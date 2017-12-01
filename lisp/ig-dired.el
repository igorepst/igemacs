;;; ig-dired.el --- Dired configuration

;;; Commentary:
;; Configuration of dired mode.

;;; Code:

;; Note that the space used in time style format is actually a Unicode
;; char U+2008 PUNCTUATION SPACE to overcome a bug
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18875
(defconst ig-time-style-space (string ?\u2008) "Punctuation space Unicode char.")
(defconst ig-ls-switches (concat
			  "--group-directories-first --time-style=+%d/%m/%y"
			  ig-time-style-space "%R -AhFl") "'ls' switches.")

;;;###autoload
(defun ig-dired-sort (variant &optional reverse)
  "Sort dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat (purecopy ig-ls-switches) " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

;;'(("Video files" . ("264" "3g2" "3gp" "arf" "asf" "asx" "avi" "bik" "dash" "dat" "dvr" "flv" "h264" "m2t" "m2ts" "m4v" "mkv" "mod" "mov" "mp4" "mpeg" "mpg" "mswmm" "mts" "ogv" "prproj" "rec" "rmvb" "swf" "tod" "tp" "ts" "vob" "webm" "wmv" ))("Audio files" . ("3ga" "aac" "aiff" "amr" "ape" "arf" "asf" "asx" "cda" "dvf" "flac" "gp4" "gp5" "gpx" "logic" "m4a" "m4b" "m4p" "midi" "mp3" "ogg" "pcm" "rec" "snd" "sng" "uax" "wav" "wma" "wpl" "zab" ))("Bitmap images" . ("bmp" "dds" "dib" "dng" "dt2" "emf" "gif" "ico" "icon" "jpeg" "jpg" "pcx" "pic" "png" "psd" "psdx" "raw" "tga" "thm" "tif" "tiff" "wbmp" "wdp" "webp" ))("Digital camera RAW photos" . ("arw" "cr2" "crw" "dcr" "dng" "fpx" "mrw" "nef" "orf" "pcd" "ptx" "raf" "raw" "rw2" ))("Vector graphics" . ("ai" "cdr" "csh" "drw" "emz" "odg" "pic" "sda" "svg" "swf" "wmf" ))("Graphics file types" . ("abr" "ai" "ani" "cdt" "cpt" "djvu" "eps" "fla" "icns" "ico" "icon" "mdi" "odg" "pic" "psb" "psd" "pzl" "sup" "vsdx" ))("3D graphics" . ("3d" "3ds" "c4d" "dgn" "dwfx" "dwg" "dxf" "lcf" "max" "pro" "pts" "skp" "stl" "u3d" "x_t" ))("Font files" . ("eot" "otf" "ttc" "ttf" "woff" ))("Documents" . ("abw" "aww" "chm" "cnt" "dbx" "djvu" "doc" "docm" "docx" "dot" "dotm" "dotx" "epub" "gp4" "ind" "indd" "key" "keynote" "mht" "mpp" "mpt" "odf" "ods" "odt" "ott" "oxps" "pages" "pdf" "pmd" "pot" "potx" "pps" "ppsx" "ppt" "pptm" "pptx" "prn" "prproj" "ps" "pub" "pwi" "rep" "rtf" "sdd" "sdw" "shs" "snp" "sxw" "tpl" "vsd" "wlmp" "wpd" "wps" "wri" "xps" ))("Simple text files" . ("1st" "alx" "application" "asp" "csv" "eng" "htm" "html" "log" "lrc" "lst" "nfo" "opml" "plist" "pts" "reg" "rep" "rtf" "srt" "sub" "tbl" "text" "txt" "xml" "xsd" "xsl" "xslt" ))("E-book files" . ("azw" "azw3" "azw4" "cbr" "cbz" "epub" "fb2" "iba" "ibooks" "lit" "mobi" "pdf" ))("Spreadsheets" . ("numbers" "ods" "sdc" "sxc" "xls" "xlsm" "xlsx" ))("Microsoft Office files" . ("accdb" "accdt" "doc" "docm" "docx" "dot" "dotm" "dotx" "mdb" "mpd" "mpp" "mpt" "oft" "one" "onepkg" "pot" "potx" "pps" "ppsx" "ppt" "pptm" "pptx" "pst" "pub" "snp" "thmx" "vsd" "vsdx" "xls" "xlsm" "xlsx" ))("Game related files" . ("big" "bik" "cab" "dat" "dds" "hi" "lng" "pak" "res" "sav" "save" "SC2Replay" "scn" "scx" "uax" "wotreplay" "wowpreplay" ))("Emulation related files" . ("dat" "g64" "gb" "gba" "mbz" "n64" "nds" "nes" "rom" "smc" "smd" "srm" "v64" ))("Virtualization software related files" . ("ova" "ovf" "pvm" "vdi" "vhd" "vmdk" "vmem" "vmwarevm" "vmx" ))("Internet related files" . ("ashx" "asp" "aspx" "atom" "bc" "bc!" "class" "crdownload" "css" "dlc" "download" "download" "download" "eml" "flv" "gdoc" "gif" "gsheet" "gslides" "htm" "html" "js" "json" "jsp" "mht" "opml" "part" "partial" "php" "png" "rss" "swf" "torrent" "webm" "webp" "xap" "xhtml" "xml" "xsd" "xsl" "xslt" ))("Email files" . ("dbx" "eml" "ldif" "mht" "msg" "pst" "vcf" ))("File extensions blocked by mail clients" . ("app" "asp" "bat" "chm" "cnt" "com" "cpl" "eml" "exe" "gadget" "inf" "js" "lnk" "mdb" "msi" "prg" "reg" "scr" "shs" "tmp" "vbs" ))("Possibly dangerous files" . ("bat" "bin" "chm" "class" "com" "cpl" "dll" "drv" "exe" "jar" "js" "lnk" "ocx" "pcx" "scr" "shs" "swf" "sys" "vbs" "vxd" "wmf" ))("Archives" . ("001" "002" "003" "004" "005" "006" "007" "008" "009" "010" "7z" "7z.001" "7z.002" "7z.003" "7zip" "a00" "a01" "a02" "a03" "a04" "a05" "ace" "air" "apk" "appxbundle" "arc" "arj" "asec" "bar" "bin" "c00" "c01" "c02" "c03" "cab" "cbr" "cbz" "cso" "deb" "dlc" "gz" "gzip" "hqx" "inv" "ipa" "isz" "jar" "msu" "nbh" "pak" "part1.exe" "part1.rar" "part2.rar" "r00" "r01" "r02" "r03" "r04" "r05" "r06" "r07" "r08" "r09" "r10" "rar" "rpm" "sis" "sisx" "sit" "sitd" "sitx" "tar" "tar.gz" "tgz" "uax" "webarchive" "xap" "z01" "z02" "z03" "z04" "z05" "zab" "zip" ))("Backup files" . ("bak" "bbb" "bkf" "bkp" "dbk" "gho" "ipd" "iso" "json" "mdbackup" "nba" "nbf" "nbu" "nco" "nrg" "old" "rar" "rom" "sbf" "sbu" "spb" "tib" "wbcat" "zip" ))("Disk images" . ("000" "bin" "ccd" "cue" "daa" "dao" "dmg" "img" "img" "iso" "isz" "mdf" "mds" "mdx" "nrg" "tao" "tc" "toast" "uif" "vcd" ))("Mobile phone related files" . ("apk" "asec" "bbb" "crypt" "ipa" "ipd" "ipsw" "mdbackup" "nbh" "nbu" "npf" "pkpass" "rem" "rsc" "sbf" "sis" "sisx" "thm" "vcf" "xap" ))("Mobile ringtones" . ("aac" "aiff" "amr" "m4a" "midi" "mp3" "wav" "wma" ))("Financial files" . ("gdb" "ofx" "qif" ))("Database related files" . ("accdb" "accdt" "csv" "db" "dbf" "fdb" "gdb" "idx" "mdb" "mdf" "sdf" "sql" "sqlite" "wdb" "xml" ))("GIS, GPS, map file types" . ("gpx" "kml" "kmz" "map" ))("Program executables" . ("air" "app" "application" "appx" "bat" "bin" "com" "cpl" "deb" "dll" "elf" "exe" "jar" "js" "lnk" "msi" "part1.exe" "prg" "rpm" "shs" "vbs" "xap" ))("System file types" . ("alx" "blf" "cpl" "dat" "dll" "drv" "dump" "evtx" "gadget" "idx" "inf" "kext" "key" "mui" "ocx" "reg" "rom" "scr" "sfcache" "swp" "sys" "vxd" ))("Various presets or theme files" . ("alx" "ccd" "cnf" "contact" "cue" "deskthemepack" "ics" "ifo" "lnk" "lrtemplate" "m3u" "m3u8" "mui" "plist" "pls" "pro" "skn" "svp" "theme" "themepack" "thm" "thmx" "trm" "wba" ))("Plugins" . ("crx" "plugin" "safariextz" "xpi" ))("Source code files" . ("asm" "asp" "aspx" "bat" "htm" "inc" "jad" "java" "js" "json" "jsp" "lib" "o" "php" "rc" "rss" "scpt" "src" "vbs" "xml" "xsd" "xsl" "xslt" ))("Configuration files" . ("cfg" "cnf" "ini" "pro" "reg" "usr" ))("Binary files" . ("dmp" "log" "rom" ))("Encrypted files" . ("asec" "crypt" "ksd" "pfx" "pkpass" "rem" "tc" ))("Temporary files" . ("!ut" "adadownload" "bc" "bc!" "blf" "cache" "crdownload" "dmp" "download" "download" "download" "part" "partial" "temp" "tmp" ))("Various file types" . ("3dr" "cal" "dat" "dct" "dic" "dmp" "eng" "gbk" "lng" "log" "md5" "msmessagestore" "prj" "ref" "rep" "rsc" "tbl" "template" "upd" "upg" )))



(use-package dired-rainbow
  :config
  (defconst ig-images-files-extensions
    '("bmp" "jpg" "jpeg" "png" "gif" "tif" "tiff" "psd" "psp" "wmf" "emf" "ppm" "pgm" "pbm" "pnm" "svg" "ai" "eps" "ico")
    "Images files.")
  (defconst ig-archives-files-extensions
    '("rar" "zip" "7z" "sqx" "gz" "tgz" "tar" "ace" "arj" "lha" "uc2" "lzma" "bz2" "z" "uc2" "taz" "lzh" "xz")
    "Archives files.")
  (defconst ig-exec-files-extensions
    '("exe" "com" "jar" "bat" "cmd" "ahk" "btm" "vbs" "vbe" "js" "jse" "wsf" "wsh" "msi")
    "Executable files.")
  (defconst ig-docs-files-extensions
    '("doc" "docx" "odt" "xls" "xlsx" "ods" "pdf" "djvu")
    "Document files.")
  (defconst ig-media-files-extensions
    '("avi" "vob" "mpg" "mpeg" "mkv" "mp4" "wmv" "webm" "flv" "ogv" "ogm" "divx" "m2v" "h264" "aac" "flac" "mp3" "wma" "mp4" "m4a" "mpa" "wav" "mid" "ac3" "mka" "cda")
    "Media files.")
  (setq dired-rainbow-date-regexp (concat "\\(?:[0-3][0-9]/[0-1][0-9]/[0-9][0-9]" ig-time-style-space "[0-2][0-9]:[0-5][0-9]\\)"))
  (dired-rainbow-define ig-media-files-extensions "#6fb7d8" ig-media-files-extensions)
  (dired-rainbow-define ig-images-files-extensions "#9397b7" ig-images-files-extensions)
  (dired-rainbow-define ig-archives-files-extensions "#9fb696" ig-archives-files-extensions)
  (dired-rainbow-define ig-docs-files-extensions "#21a184" ig-docs-files-extensions)
  (dired-rainbow-define ig-exec-files-extensions "#d787a5" ig-exec-files-extensions)
  (dired-rainbow-define-chmod ig-exec-files-chmod "#d787a5" "-.*x.*"))
;;  (dired-rainbow-define-chmod ig-dirs-chmod (alist-get "zenburn-fg" zenburn-default-colors-alist) "d"))
;;(dired-rainbow-define-chmod ig-dirs-chmod "#00aef4" "d"))

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

