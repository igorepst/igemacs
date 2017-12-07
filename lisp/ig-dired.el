;;; -*- lexical-binding: t -*-
;;; ig-dired.el --- Dired configuration

;;; Commentary:
;; Configuration of dired mode.

;;; Code:

;; Note that the space used in time style format is actually a Unicode
;; char U+2008 PUNCTUATION SPACE to overcome a bug
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18875
(defconst ig-time-style-space (string ?\u2008) "Punctuation space Unicode char.")
;; Although %R == %H:%M (see `man date`), the former causes problems with
;; `dired-insert-subdir`, where 'R' substring is replaced
(defconst ig-ls-switches (concat
			  "--group-directories-first --time-style=+%d/%m/%y"
			  ig-time-style-space "%H:%M -ahFl") "'ls' switches.")

;;;###autoload
(defun ig-dired-sort (variant &optional reverse)
  "Sort dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat (purecopy ig-ls-switches) " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

(defun ig-get-zb (arg)
  "Get Zenburn color with name ARG."
  (cdr (assoc arg zenburn-default-colors-alist)))

(use-package dired-rainbow
  :after dired
  :config
  (setq dired-rainbow-date-regexp (concat "\\(?:[0-3][0-9]/[0-1][0-9]/[0-9][0-9]"
					  ig-time-style-space "[0-2][0-9]:[0-5][0-9]\\)"))
  
  (defconst ig-src-files-extensions
    '("ahk" "asm" "asp" "aspx" "bat" "cfg" "cnf" "css" "csv" "dmp" "el" "htm" "html" "inc"
      "ini" "jad" "java" "js" "json" "jsp" "lib" "log" "o" "php" "pro" "rc" "reg" "rss"
      "scpt" "src" "usr" "vbs" "xhtml" "xml" "xsd" "xsl" "xslt") "Source code files.")
  (defconst ig-images-files-extensions
    '("3d" "3ds" "abr" "ai" "ani" "arw" "bmp" "c4d" "cdr" "cdt" "cpt" "cr2" "crw" "csh"
      "dcr" "dds" "dgn" "dib" "dng" "drw" "dt2" "dwfx" "dwg" "dxf" "emf" "emz" "eps" "fla"
      "fpx" "gif" "icns" "ico" "icon" "ics" "jpeg" "jpg" "lcf" "max" "mdi" "mrw" "nef"
      "odg" "orf" "pcd" "pcx" "pic" "png" "psb" "psd" "psdx" "pts" "ptx" "pzl" "raf" "raw"
      "rw2" "sda" "skp" "stl" "sup" "svg" "tga" "thm" "tif" "tiff" "u3d" "vsdx" "wbmp"
      "wdp" "webp" "wmf" "x_t") "Images files.")
  (defconst ig-archives-files-extensions
    '("000" "001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "7z" "7z.001"
      "7z.002" "7z.003" "7zip" "a00" "a01" "a02" "a03" "a04" "a05" "ace" "air" "apk"
      "appxbundle" "arc" "arj" "asec" "bar" "bin" "c00" "c01" "c02" "c03" "cab" "cbr"
      "cbz" "ccd" "cso" "cue" "daa" "dao" "deb" "dlc" "dmg" "gz" "gzip" "hqx" "img"
      "inv" "ipa" "iso" "isz" "jar" "mdf" "mds" "mdx" "msu" "nbh" "nrg" "pak" "r00"
      "r01" "r02" "r03" "r04" "r05" "r06" "r07" "r08" "r09" "r10" "rar" "rpm" "sis"
      "sisx" "sit" "sitd" "sitx" "tao" "tar" "tar.gz" "tc" "tgz" "toast" "uax" "uif"
      "vcd" "webarchive" "xap" "z01" "z02" "z03" "z04" "z05" "zab" "zip") "Archives files.")
  (defconst ig-exec-files-extensions
    '("alx" "app" "application" "appx" "blf" "btm" "cmd" "com" "cpl" "dat" "dll" "drv"
      "dump" "elf" "evtx" "exe" "gadget" "idx" "inf" "jse" "kext" "key" "lnk" "msi" "mui"
      "ocx" "prg" "rom" "scr" "sfcache" "shs" "so" "swp" "sys" "vbe" "vxd" "wsf" "wsh")
    "Executable files.")
  (defconst ig-docs-files-extensions
    '("1st" "abw" "accdb" "accdt" "ashx" "atom" "aww" "azw" "azw3" "azw4" "bc" "bc!"
      "chm" "class" "cnt" "crdownload" "csv" "dbx" "djvu" "doc" "docm" "docx" "dot"
      "dotm" "dotx" "download" "eml" "eng" "epub" "fb2" "gdoc" "gsheet" "gslides"
      "iba" "ibooks" "ind" "indd" "keynote" "lit" "lrc" "lst" "mdb" "mht" "mobi"
      "mpd" "mpp" "mpt" "nfo" "numbers" "odf" "ods" "odt" "oft" "one" "onepkg"
      "opml" "ott" "oxps" "pages" "part" "partial" "pdf" "plist" "pmd" "pot" "potx"
      "pps" "ppsx" "ppt" "pptm" "pptx" "prn" "ps" "pst" "pub" "pwi" "rep" "rtf"
      "sdc" "sdd" "sdw" "snp" "srt" "sub" "sxc" "sxw" "tbl" "text" "thmx" "torrent"
      "tpl" "txt" "vsd" "wlmp" "wpd" "wps" "wri" "xls" "xlsm" "xlsx" "xps") "Document files.")
  (defconst ig-media-files-extensions
    '("264" "3g2" "3ga" "3gp" "aac" "aiff" "amr" "ape" "arf" "asf" "asx" "avi" "bik"
      "cda" "dash" "dvf" "dvr" "flac" "flv" "gp4" "gp5" "gpx" "h264" "logic" "m2t"
      "m2ts" "m3u" "m4a" "m4b" "m4p" "m4v" "midi" "mkv" "mod" "mov" "mp3" "mp4"
      "mpeg" "mpg" "mswmm" "mts" "mu8" "ogg" "ogv" "pcm" "prproj" "rec" "rmvb" "snd"
      "sng" "swf" "tod" "tp" "ts" "vob" "wav" "webm" "wma" "wmv" "wpl") "Media files.")

  (defun ig-dired-rainbow--get-face (face-props)
    `(:foreground ,(ig-get-zb face-props)))
  (advice-add 'dired-rainbow--get-face :override #'ig-dired-rainbow--get-face)
  (dired-rainbow-define ig-src-files-extensions "zenburn-yellow" ig-src-files-extensions t)
  (dired-rainbow-define ig-images-files-extensions "zenburn-green+2" ig-images-files-extensions t)
  (dired-rainbow-define ig-archives-files-extensions "zenburn-red" ig-archives-files-extensions t)
  (dired-rainbow-define ig-exec-files-extensions "zenburn-cyan" ig-exec-files-extensions t)
  (dired-rainbow-define ig-docs-files-extensions "zenburn-blue+1" ig-docs-files-extensions t)
  (dired-rainbow-define ig-media-files-extensions "zenburn-blue-2" ig-media-files-extensions t)
  (dired-rainbow-define-chmod ig-exec-files-chmod "zenburn-magenta" "-.*x.*" t)
  (advice-remove 'dired-rainbow--get-face #'ig-dired-rainbow--get-face))

;;;###autoload
(defun ig-open-dired-2pane (&optional file1 file2)
  "Open dired on two panes."
  (interactive)
  (select-frame (make-frame))
  (set-frame-parameter nil 'fullscreen 'maximized)
  (dired (or file1 ig-home-dir))
  (goto-line 1)
  (split-window-horizontally)
  (find-file-other-window (or file2 ig-home-dir)))

(use-package dired
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	dired-dwim-target t
	dired-use-ls-dired t
	;; We MUST now override the following regexp.
	;; There is a regular space in its end
	directory-listing-before-filename-regexp
	(purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			  directory-listing-before-filename-regexp)))
  
  (set-face-attribute 'dired-flagged nil :inherit nil :foreground "black" :background "red")
  (set-face-attribute 'dired-header nil :inherit nil :bold t :underline t)
  (set-face-attribute 'dired-ignored nil :inherit nil)
  (set-face-attribute 'dired-symlink nil :inherit nil)
  (set-face-attribute 'dired-warning nil :inherit nil) ;;Not in use
  (set-face-attribute 'dired-marked nil :inherit nil :foreground "black"
		      :background (ig-get-zb "zenburn-orange") :bold t)
  (set-face-attribute 'dired-mark nil :inherit 'dired-marked)
  (set-face-attribute 'dired-perm-write nil :inherit nil :foreground "magenta")
  
  (defun ig-dired-sort-set-mode-line (args)
    "Override mode name. ARGS are not used"
    (setq mode-name
	  (concat
	   (cond ((string-match-p
		   "-v$" dired-actual-switches)
		  "Dir name")
		 ((string-match-p
		   "-t$" dired-actual-switches)
		  "Dir time")
		 ((string-match-p
		   "-S$" dired-actual-switches)
		  "Dir size")
		 ((string-match-p
		   "-X$" dired-actual-switches)
		  "Dir ext")
		 (t
		  (concat "Dired " dired-actual-switches)))
	   (if (string-match-p "^--reverse" dired-actual-switches)
	       " ↑" " ↓")))
    (force-mode-line-update))
  (advice-add 'dired-sort-set-mode-line :around #'ig-dired-sort-set-mode-line)
  (ig-dired-sort "-v" nil)
  (require 'dired-rainbow)

  (defun ig-maybe-find-alternate-file(file)
    "Find FILE, but if the current buffer is shown in another window, don't kill it."
    (let ((wins (get-buffer-window-list)))
      (if (and wins (< 1 (length wins)))
	  (find-file file) (find-alternate-file file))))
  (defun ig-dired-maybe-visit-new-buffer()
    "Always leave one Dired buffer open."
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
	  (ig-maybe-find-alternate-file file)
	(find-file (file-name-sans-versions file t)))))
  (defun ig-dired-mouse-find-file (event)
    "In Dired, visit the file or directory name you click on in the same window."
    (interactive "e")
    (let ((window (posn-window (event-end event)))
	  (pos (posn-point (event-end event))))
      ;; In case the active window is NOT where the click was done,
      ;; as the focus stays where it was
      (select-window window)
      (set-buffer (window-buffer window))
      (goto-char pos)
      (ig-dired-maybe-visit-new-buffer)))
  (defun ig-dired-xdg-open ()
    (interactive)
    (let ((file (dired-get-file-for-visit))
	  (process-connection-type nil))
      (start-process "" nil "xdg-open" file)))
  :general
  (:keymaps 'dired-mode-map
	    "z" '(lambda() (interactive) (require 'ig-hydra) (ig-hydra-dired-sort/body))
	    [mouse-2] #'ig-dired-mouse-find-file
	    [remap dired-find-file] '(lambda() (interactive) (ig-dired-maybe-visit-new-buffer))
	    [remap dired-up-directory] '(lambda() (interactive) (ig-maybe-find-alternate-file ".."))))

(provide 'ig-dired)

