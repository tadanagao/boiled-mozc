;;; boiled-mozc.el --- A wrapper to mozc.el that offers modeless input style

;; Copyright (C) 2013 Tadaaki Nagao
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Tadaaki Nagao <abtk@shitamachi.org>
;; URL: https://github.com/tadanagao/boiled-mozc
;; Keywords: mule, multilingual, input method, Japanese
;; Version: 0.3

;;; Commentary:

;; ゆでもずく!
;;
;; boiled-mozc.el wraps ("boil"s) mozc.el to offer modeless input style,
;; where you can type a Romaji sequence without activating the Mozc
;; input method and then just hit \M-o or \C-o to obtain its Hiragana
;; and Kana-Kanji conversion, respectively.
;;
;; To use boiled-mozc.el, just add the following code into your .emacs:
;;
;;   (autoload 'boiled-mozc-rK-conv "boiled-mozc"
;;     "Romaji to Kana-Kanji conversion" t)
;;   (autoload 'boiled-mozc-rhkR-conv "boiled-mozc"
;;     "Romaji to Hiragana conversion" t)
;;   (global-set-key "\C-o" 'boiled-mozc-rK-conv)
;;   (global-set-key "\M-o" 'boiled-mozc-rhkR-conv)

;; The idea of "boil" is taken from respectable forerunners' works, thus
;; acknowledgments go to:
;;
;; Kin'ya Miura-san <miura@is.aist-nara.ac.jp> for his boiled-egg.el
;; (http://usir.kobe-c.ac.jp/boiled-egg/) that "boil"s the EGG (Tamago
;; V3) input method, and his contributors;
;;
;; Hiroaki Sengoku-san <sengoku@gcd.org> for his boiling-egg.el
;; (http://www.gcd.org/sengoku/boiling-egg/) that is a total rewrite of
;; boiled-egg.el to adapt it to Tamago V4; and
;;
;; Shunsuke OKANO-san <okano@pro.ics.tut.ac.jp> for his boiling-anthy.el
;; that is a version of boiling-egg.el modified for another Japanese
;; input method anthy.el.

;; ToDo:
;; - Better handling of single ending n. (double-nn / append "'" / etc.?)

;;; Code:

(require 'mozc)


;;;; Customization options.

(defgroup boiled-mozc nil
  "Modeless input add-on for Mozc."
  :group 'mozc
  :prefix "boiled-mozc-")

(defcustom boiled-mozc-convert-key (kbd "SPC")
  "Mozc key sequence to start Kana-Kanji conversion."
  :type 'key-sequence
  :group 'boiled-mozc)

(defcustom boiled-mozc-commit-key (kbd "<f6> RET")
  "Mozc key sequence to commit Hiragana form."
  :type 'key-sequence
  :group 'boiled-mozc)

(defcustom boiled-mozc-target-chars "[]'.,a-zA-Z@-"
  "Characters to be converted to Hiragana or Kana-Kanji.

Boiled-mozc looks for the longest sequence of the specified characters
ending at the point and hands it over to Mozc.

This value is used as an argument of `skip-chars-backward'.  See
its documentation and `skip-chars-forward' for details on the format."
  :type 'string
  :link '(function-link skip-chars-backward)
  :link '(function-link skip-chars-forward)
  :group 'boiled-mozc)


;;;; Internal variables.

(defvar boiled-mozc-input-method "japanese-mozc"
  "The input method name of Mozc.")

(defvar boiled-mozc-running-type nil
  "boiled-mozc's running mode while calling mozc.el functions.
A value of nil means boiled-mozc isn't in effect, :Hiragana means
Romaji-Hiragana conversion, and :Kanji means Kana-Kanji conversion.")
(make-variable-buffer-local 'boiled-mozc-running-type)

(defvar boiled-mozc-conv-original nil
  "The original Romaji strip that is being converted.
Kept until the conversion finishes.")
(make-variable-buffer-local 'boiled-mozc-conv-original)

(defvar boiled-mozc-conv-marker (make-marker)
  "The buffer position where the original Romaji strip begins.")
(make-variable-buffer-local 'boiled-mozc-conv-marker)

(defvar boiled-mozc-conv-type nil
  "Indicates the current form in cyclic conversion.
Its value is maintained in `boiled-mozc-rhkR-conv' and referred to in
`boiled-mozc-rK-conv'.  Possible values are nil, :Hiragana, :Katakana and
:RomajiZenkaku.")
(make-variable-buffer-local 'boiled-mozc-conv-type)

(defvar boiled-mozc-preedit nil
  "The content of Mozc's preedit area.
Used to detect when conversion completed.")
(make-variable-buffer-local 'boiled-mozc-preedit)

;; `inactivate-*' was renamed to `deactivate-*' on Emacs 24.3.
(defalias 'boiled-mozc-deactivate-input-method
  (if (not (functionp 'deactivate-input-method))
      'inactivate-input-method
    'deactivate-input-method))


;;;; Wrappers to mozc.el functions.

;; Because mozc.el doesn't offer any kind of hook, we make heavy use of
;; advices to change its behavior.

(defadvice mozc-candidate-update (around boiled-mozc-hide-candidates activate)
  "Hide the echo-area candidate list while and after conversion."
  (unless (eq boiled-mozc-running-type :Hiragana)
    ad-do-it))

(defadvice mozc-preedit-update (before boiled-mozc-preedit-update
				       (preedit &optional candidates)
				       activate compile)
  "Catch and keep Mozc's preedit content for later comparison."
  (if (eq boiled-mozc-running-type :Kanji)
      (let ((segment (mozc-protobuf-get preedit 'segment)))
	(setq boiled-mozc-preedit
	      (apply #'concat
		     (mapcar (lambda (x)
			       (mozc-protobuf-get x 'value)) segment))))))

(defadvice mozc-preedit-clear (before boiled-mozc-preedit-clear
				      activate compile)
  "Catch when Mozc's preedit is cleared for later comparison."
  (setq boiled-mozc-preedit nil))

(defadvice mozc-handle-event (around boiled-mozc-handle-event
				     (event)
				     activate compile)
  "Check if Mozc finished conversion, in which case do some clean-ups for
boiled-mozc."
  (let ((begin (point-marker))
	(prev-preedit boiled-mozc-preedit))
    ad-do-it
    (when (eq boiled-mozc-running-type :Kanji)
      (let ((str (buffer-substring begin (point))))
	(when (and (> (length str) 0)
		   (string= prev-preedit str))
	  ;; Conversion completed
	  (mozc-clean-up-changes-on-buffer)
	  (when boiled-mozc-preedit
	    ;; A key possibly expected to be inserted was pressed. The
	    ;; event must be re-cast with the input method deactivated.
	    (setq boiled-mozc-preedit nil)
	    (mozc-fall-back-on-default-binding event))))))
  (when (and (not mozc-preedit-in-session-flag)
	     boiled-mozc-running-type)
    (mozc-clean-up-changes-on-buffer)
    (boiled-mozc-deactivate-input-method)
    (setq boiled-mozc-running-type nil)
    (if (eq (marker-position boiled-mozc-conv-marker) (point))
	;; Conversion canceled.
	(insert boiled-mozc-conv-original))))


;;;; Internal functions.

(defun boiled-mozc-search-beginning ()
  "Search backward from point for the beginning of a Romaji strip."
  (let* ((pos (point))
	 (mark (mark t))
	 (bol (line-beginning-position))
	 (begin (catch 'begin
		  (save-excursion
		    (if (and mark (<= bol mark) (< mark pos)
			     (not (re-search-backward "[^!-~]" mark t)))
			(throw 'begin mark)))
		  (save-excursion
		    (if (skip-chars-backward boiled-mozc-target-chars bol)
			(throw 'begin (point))))
		  pos)))
    (set-marker boiled-mozc-conv-marker begin)
    (setq boiled-mozc-conv-original (buffer-substring begin pos))))


;;;; Interactive conversion functions.

;;;###autoload
(defun boiled-mozc-rK-conv ()
  "Romaji to Kanji conversion."
  (interactive "*")
  (unless (and (eq last-command 'boiled-mozc-rhkR-conv)
	       boiled-mozc-conv-type)
    (boiled-mozc-search-beginning))
  (delete-region boiled-mozc-conv-marker (point))
  (setq boiled-mozc-running-type :Kanji)
  (activate-input-method boiled-mozc-input-method)
  (mapc #'mozc-handle-event
	(vconcat boiled-mozc-conv-original boiled-mozc-convert-key)))

;;;###autoload
(defun boiled-mozc-rhkR-conv ()
  "Romaji(Hankaku) - Hiragana - Katakana - Romaji(Zenkaku) cyclic conversion."
  (interactive "*")
  (cond
   ((or (not (eq last-command this-command))
	(null boiled-mozc-conv-type))
    (boiled-mozc-search-beginning)
    (setq boiled-mozc-running-type :Hiragana)
    (delete-region boiled-mozc-conv-marker (point))
    (activate-input-method boiled-mozc-input-method)
    (mapc #'mozc-handle-event
	  (vconcat boiled-mozc-conv-original boiled-mozc-commit-key))
    (boiled-mozc-deactivate-input-method)
    (setq boiled-mozc-conv-type :Hiragana))
   ((eq boiled-mozc-conv-type :Hiragana)
    (japanese-katakana-region boiled-mozc-conv-marker (point))
    (setq boiled-mozc-conv-type :Katakana))
   ((eq boiled-mozc-conv-type :Katakana)
    (delete-region boiled-mozc-conv-marker (point))
    (insert boiled-mozc-conv-original)
    (japanese-zenkaku-region boiled-mozc-conv-marker (point))
    (setq boiled-mozc-conv-type :RomajiZenkaku))
   ((eq boiled-mozc-conv-type :RomajiZenkaku)
    (japanese-hankaku-region boiled-mozc-conv-marker (point))
    (setq boiled-mozc-conv-type nil))
   ))


(provide 'boiled-mozc)

;; Local Variables:
;; coding: utf-8
;; End:

;;; boiled-mozc.el ends here
