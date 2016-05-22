;;; ig-fonts.el --- Set right fonts

;;; Commentary:
;; Set right Unicode fonts and overrides for specific ranges.
;; Based on great work by Roland Walker from
;; https://github.com/rolandwalker/unicode-fonts
;; Uses only ranges that were interesting to me, greatly simplifying the font
;; handling, speeding up the processing, but leaving high degree of
;; customizability.

;; No checking of any kind is used for fonts.  Ensure they are installed.
;; http://sourceforge.net/projects/dejavu/files/dejavu/2.35/dejavu-fonts-ttf-2.35.tar.bz2
;; http://users.teilar.gr/~g1951d/Symbola.zip
;; https://github.com/googlei18n/noto-fonts/blob/master/unhinted/NotoSansSymbols-Regular.ttf?raw=true
;; https://github.com/googlei18n/noto-fonts/blob/master/unhinted/NotoSansEgyptianHieroglyphs-Regular.ttf?raw=true

;;; Code:



;; Fonts mapping
;; Enlarge for the glyphs to be distinguishable
(setq ig-fonts-mapping '
      (("Noto Sans Symbols-16"

	;; https://en.wikipedia.org/wiki/Aegean_Numbers_%28Unicode_block%29
	(#x10100 . #x1013F); "Aegean Numbers"
	;; https://en.wikipedia.org/wiki/Alchemical_Symbols_%28Unicode_block%29
	(#x1F700 . #x1F77F); "Alchemical Symbols"
	;; https://en.wikipedia.org/wiki/Ancient_Greek_Musical_Notation
	(#x1D200 . #x1D24F); "Ancient Greek Musical Notation"
	;; https://en.wikipedia.org/wiki/Ancient_Greek_Numbers
	(#x10140 . #x1018F); "Ancient Greek Numbers"
	;; https://en.wikipedia.org/wiki/Ancient_Symbols
	(#x10190 . #x101CF); "Ancient Symbols"
	;; https://en.wikipedia.org/wiki/Braille_Patterns
	(#x2800 . #x28FF); "Braille Patterns"
	;; https://en.wikipedia.org/wiki/Byzantine_Musical_Symbols
	(#x1D000 . #x1D0FF); "Byzantine Musical Symbols"
	;; https://en.wikipedia.org/wiki/Combining_Diacritical_Marks_for_Symbols
	(#x20D0 . #x20FF); "Combining Diacritical Marks for Symbols"
	;; https://en.wikipedia.org/wiki/Control_Pictures
	(#x2400 . #x243F); "Control Pictures"
	;; https://en.wikipedia.org/wiki/Counting_Rod_Numerals
	(#x1D360 . #x1D37F); "Counting Rod Numerals"
	;; https://en.wikipedia.org/wiki/Enclosed_Alphanumerics
	(#x2460 . #x24FF); "Enclosed Alphanumerics"
	;; https://en.wikipedia.org/wiki/Enclosed_Ideographic_Supplement
	(#x1F200 . #x1F2FF); "Enclosed Ideographic Supplement"
	;; https://en.wikipedia.org/wiki/Letterlike_Symbols
	(#x2100 . #x214F); "Letterlike Symbols"
	;; https://en.wikipedia.org/wiki/Mahjong_Tiles_%28Unicode_block%29
	(#x1F000 . #x1F02F); "Mahjong Tiles"
	;; https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols
	(#x1D400 . #x1D7FF); "Mathematical Alphanumeric Symbols"
	;; https://en.wikipedia.org/wiki/Mathematical_Operators
	(#x2200 . #x22FF); "Mathematical Operators"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Mathematical_Symbols-A
	(#x27C0 . #x27EF); "Miscellaneous Mathematical Symbols-A"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Mathematical_Symbols-B
	(#x2980 . #x29FF); "Miscellaneous Mathematical Symbols-B"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Symbols
	(#x2600 . #x26FF); "Miscellaneous Symbols"
	;; https://en.wikipedia.org/wiki/Optical_Character_Recognition_%28Unicode_block%29
	(#x2440 . #x245F); "Optical Character Recognition"
	;; https://en.wikipedia.org/wiki/Specials_%28Unicode_block%29
	(#xFFF0 . #xFFFF); "Specials"
	;; https://en.wikipedia.org/wiki/Supplemental_Mathematical_Operators
	(#x2A00 . #x2AFF); "Supplemental Mathematical Operators"
	)

       ("Symbola-16"

	;; https://en.wikipedia.org/wiki/Combining_Diacritical_Marks
	(#x0300 . #x036F); "Combining Diacritical Marks"
	;; https://en.wikipedia.org/wiki/Combining_Half_Marks
	(#xFE20 . #xFE2F); "Combining Half Marks"
	;; https://en.wikipedia.org/wiki/Currency_Symbols_%28Unicode_block%29
	(#x20A0 . #x20CF); "Currency Symbols"
	;; https://en.wikipedia.org/wiki/Cyrillic_%28Unicode_block%29
	(#x0400 . #x04FF); "Cyrillic"
	;; https://en.wikipedia.org/wiki/Dingbat#Unicode
	(#x2700 . #x27BF); "Dingbats"
	;; https://en.wikipedia.org/wiki/Domino_Tiles
	(#x1F030 . #x1F09F); "Domino Tiles"
	;; https://en.wikipedia.org/wiki/Emoticons_%28Unicode_block%29
	(#x1F600 . #x1F64F); "Emoticons"
	;; https://en.wikipedia.org/wiki/Enclosed_Alphanumeric_Supplement
	(#x1F100 . #x1F1FF); "Enclosed Alphanumeric Supplement"
	;; https://en.wikipedia.org/wiki/General_Punctuation_%28Unicode_block%29
	(#x2000 . #x206F); "General Punctuation"
	;; https://en.wikipedia.org/wiki/Geometric_Shapes
	(#x25A0 . #x25FF); "Geometric Shapes"
	;; https://en.wikipedia.org/wiki/Geometric_Shapes_Extended
	(#x1F780 . #x1F7FF); "Geometric Shapes Extended"
	;; https://en.wikipedia.org/wiki/Greek_and_Coptic
	(#x0370 . #x03FF); "Greek and Coptic"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Symbols_and_Arrows
	(#x2B00 . #x2BFF); "Miscellaneous Symbols and Arrows"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Symbols_and_Pictographs
	(#x1F300 . #x1F5FF); "Miscellaneous Symbols and Pictographs"
	;; https://en.wikipedia.org/wiki/Miscellaneous_Technical
	(#x2300 . #x23FF); "Miscellaneous Technical"
	;; https://en.wikipedia.org/wiki/Musical_Symbols_%28Unicode_block%29
	(#x1D100 . #x1D1FF); "Musical Symbols"
	;; https://en.wikipedia.org/wiki/Number_Forms
	(#x2150 . #x218F); "Number Forms"
	;; https://en.wikipedia.org/wiki/Ornamental_Dingbats
	(#x1F650 . #x1F67F); "Ornamental Dingbats"
	;; https://en.wikipedia.org/wiki/Playing_cards_in_Unicode
	(#x1F0A0 . #x1F0FF); "Playing Cards"
	;; https://en.wikipedia.org/wiki/Supplemental_Arrows-A
	(#x27F0 . #x27FF); "Supplemental Arrows-A"
	;; https://en.wikipedia.org/wiki/Supplemental_Arrows-B
	(#x2900 . #x297F); "Supplemental Arrows-B"
	;; https://en.wikipedia.org/wiki/Supplemental_Arrows-C
	(#x1F800 . #x1F8FF); "Supplemental Arrows-C"
	;; https://en.wikipedia.org/wiki/Supplemental_Punctuation
	(#x2E00 . #x2E7F); "Supplemental Punctuation"
	;; https://en.wikipedia.org/wiki/Supplemental_Symbols_and_Pictographs
	(#x1F900 . #x1F9FF); "Supplemental Symbols and Pictographs"
	;; https://en.wikipedia.org/wiki/Transport_and_Map_Symbols
	(#x1F680 . #x1F6FF); "Transport and Map Symbols"
	;; https://en.wikipedia.org/wiki/Vertical_Forms
	(#xFE10 . #xFE1F); "Vertical Forms"
	)
       
       ("Noto Sans Egyptian Hieroglyphs-32"

	;; https://en.wikipedia.org/wiki/Egyptian_Hieroglyphs_%28Unicode_block%29
	(#x13000 . #x1342F); "Egyptian Hieroglyphs"
	)))



;; Override
;;;###autoload
(defun ig-set-font-overrides ()
  "Change fontset font according to definitions in ig-fonts-mapping.
No checks of any kind are done (font existence, etc.)"
  (let (ig-font-name)
    (dolist (font-override ig-fonts-mapping)
      (setq ig-font-name (car font-override))
      (dolist (ig-font-range (cdr font-override))
	(set-fontset-font "fontset-default"
			  (cons (decode-char 'ucs (car ig-font-range)) (decode-char 'ucs (cdr ig-font-range)))
			  (font-spec :name ig-font-name :registry "iso10646-1")
			  nil 'append)))))



;; UI fonts
;;;###autoload
(defun ig-set-ui-fonts ()
  "Set UI fonts as needed."
  ;; Set font for regular Emacs and for emacsclient.
  ;; Don't use (when (member "DejaVu Sans Mono" (font-family-list)),
  ;; as it won't work with daemon
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
  ;; specify font for Unicode characters ranges. Works only by duplicating:
  ;; for regular Emacs
  (ig-set-font-overrides)
  ;; and for emacsclient
  (add-hook 'before-make-frame-hook 'ig-set-font-overrides))

(provide 'ig-fonts)

;;; ig-fonts.el ends here
