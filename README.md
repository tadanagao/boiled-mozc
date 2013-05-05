boiled-mozc
===========

ゆでもずく!
A wrapper to `mozc.el` that offers modeless ("boil"ed) input style.

Synopsis
--------

`boiled-mozc.el` wraps("boil"s) `mozc.el` to offer modeless input style,
where you can type a Romaji sequence without activating the Mozc
input method and then just hit `\M-o` or `\C-o` to obtain its Hiragana
and Kana-Kanji conversion, respectively.

How to Set Up
-------------

To use `boiled-mozc.el`, just add the following code into your `.emacs`:

    (autoload 'boiled-mozc-rK-conv "boiled-mozc"
      "Romaji to Kana-Kanji conversion" t)
    (autoload 'boiled-mozc-rhkR-conv "boiled-mozc"
      "Romaji to Hiragana conversion" t)
    (global-set-key "\C-o" 'boiled-mozc-rK-conv)
    (global-set-key "\M-o" 'boiled-mozc-rhkR-conv)

Author
------

Copyright (C) 2013 Tadaaki Nagao
All rights reserved.

Licensed under the terms of the 2-clause BSD license.
See `boiled-mozc.el` for further details.

Acknowledgments
---------------

The idea of "boil" is based on respectable forerunners' works, thus
acknowledgments go to:

Kin'ya Miura-san <miura@is.aist-nara.ac.jp> for his `boiled-egg.el`
(http://usir.kobe-c.ac.jp/boiled-egg/) that "boil"s the EGG (Tamago
V3) input method, and his contributors;

Hiroaki Sengoku-san <sengoku@gcd.org> for his `boiling-egg.el`
(http://www.gcd.org/sengoku/boiling-egg/) that is a total rewrite of
boiled-egg.el to adapt it to Tamago V4; and

Shunsuke OKANO-san <okano@pro.ics.tut.ac.jp> for his `boiling-anthy.el`
that is a version of `boiling-egg.el` modified for another Japanese
input method `anthy.el`.
