;;; emms-mark-ext.el --- Extra functions for emms-mark-mode and emms-tag-edit-mode

;; Filename: emms-mark-ext.el
;; Description: Extra functions for emms-mark-mode and emms-tag-edit-mode
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2010
;; Version: 0.2
;; Last-Updated: 2013-05-28 03:20:06
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/emms-mark-ext
;; Keywords: convenience multimedia
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((emms "3.0"))
;;
;; Features that might be required by this library:
;;
;; emms
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1DPsbd286Xq8kch5t62DfppUbcL3e6PqnT
;;
;; How to create documentation and distribute package:
;;
;;     1) Remember to add ;;;###autoload magic cookies if possible
;;     2) Generate a bitcoin address for donations with shell command: bitcoin getaccountaddress emms-mark-ext
;;        and place address after "Commentary:" above.
;;     3) Use org-readme-top-header-to-readme to create initial Readme.org file.
;;     4) Use M-x auto-document to insert descriptions of commands and documents
;;     5) Create documentation in the Readme.org file:
;;        - Use org-mode features for structuring the data.
;;        - Divide the commands into different categories and create headings
;;          containing org lists of the commands in each category.
;;        - Create headings with any other extra information if needed (e.g. customization).
;;     6) In this buffer use org-readme-to-commentary to fill Commentary section with
;;        documentation from Readme.org file.
;;     7) Make any necessary adjustments to the documentation in this file (e.g. remove the installation
;;        and customization sections added in previous step since these should already be present).
;;     8) Use org-readme-marmalade-post and org-readme-convert-to-emacswiki to post
;;        the library on Marmalade and EmacsWiki respectively.
;; 
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `emms-mark-mark-tagged'
;;    Mark all tracks whose TAG field matches REGEXP.
;;  `emms-tag-editor-alter-notes-tag'
;;    Alter arbitrary word tags to the info-note tag of tracks.
;;  `emms-tag-editor-clear-field'
;;    Clear contents of a field for all tracks in tags editor.
;;

;;; Commentary:

;; `emms-mark-mark-tagged' can be used to mark based on a tag. It works at
;; `emms-mark-mode', if it's not in `emms-mark-mode', `emms-mark-mode' will 
;; be turned on automatically.

;; When in `emms-mark-mode', the following keybindings are defined for search
;; based on tags:
;; '/ m'    `emms-mark-mark-tagged'

;; `emms-tag-editor-alter-notes-tag' can be used to add/remove word from
;; tracks info-note tag. And the added tag will be separated by ":".



;;; Installation:
;;
;; Put emms-mark-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'emms-mark-ext)

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `emms-tag-editor-notes-word-list'
;;    List of default words for the info-note field of the emms-tag-editor buffer.
;;    default = nil

;;
;; All of the above can customized by:
;;      M-x customize-group RET emms-mark-ext RET
;;

;;; Change log:
;;
;; 2013/05/28
;;      * Updated for upload to Marmalade 
;;
;; 2010
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require

(require 'emms-mark)

;;; Code:

(defcustom emms-tag-editor-notes-word-list nil
  "List of default words for the info-note field of the emms-tag-editor buffer.
This list is used by the emms-tag-editor-alter-notes-tag function when the user is prompted 
for words to add to the notes field."
  :type '(repeat string)
  :group 'emms-tag-editor)


(defun emms-mark-ext-hook ()
  (define-prefix-command 'emms-mark-tag-match-map)
  (define-key emms-mark-mode-map (kbd "/") 'emms-mark-tag-match-map)
  (define-key emms-mark-mode-map (kbd "/ m") 'emms-mark-mark-tagged)
  (define-key emms-mark-mode-map (kbd "/ a") 'emms-mark-mark-artist)
  (define-key emms-mark-mode-map (kbd "/ c") 'emms-mark-mark-composer)
  (define-key emms-mark-mode-map (kbd "/ p") 'emms-mark-mark-performer)
  (define-key emms-mark-mode-map (kbd "/ t") 'emms-mark-mark-title)
  (define-key emms-mark-mode-map (kbd "/ l") 'emms-mark-mark-album)
  (define-key emms-mark-mode-map (kbd "/ y") 'emms-mark-mark-year))

(add-hook 'emms-playlist-mode-hook 'emms-mark-ext-hook)

(defun emms-mark-mark-tagged (tag regexp arg)
  "Mark all tracks whose TAG field matches REGEXP.
A prefix argument means to unmark them instead.
NOTE: if emms-mark-mode is not turned on, this function will
turn it on."
  (interactive
   (list (emms-completing-read "Search tag: "
			       (mapcar (lambda (targ)
					 (list (symbol-name (car targ))))
				       emms-tag-editor-tags)
			       nil t)
	 (read-from-minibuffer (if current-prefix-arg
				   "Unmark tracks with artist matching regexp: "
				 "Mark tracks with artist matching regexp: "))
	 current-prefix-arg))
  (emms-mark-mode)
  (let ((emms-mark-char (if arg ?\040 ?*)))
    (save-excursion
      (save-restriction
	(if (and mark-active transient-mark-mode)
	    (narrow-to-region (region-beginning) (region-end)))
	(goto-char (point-min))
	(emms-walk-tracks
	  (let* ((track (emms-playlist-track-at (point)))
		 (field (emms-track-get track (intern tag))))
	    (when (string-match-p regexp (or field ""))
	      (emms-mark-track 1))))))))

(loop for tag in '("artist" "composer" "performer" "title" "album" "year")
      do (eval `(defun ,(intern (concat "emms-mark-mark-" tag)) (regexp arg)
                  ,(concat "Mark all tracks with " tag "-tag matching REGEXP.
A prefix argument means to unmark them instead.
NOTE: if emms-mark-mode is not turned on, this function will
turn it on.")
                  (interactive (list (read-from-minibuffer
                                      (if current-prefix-arg
                                          ,(concat "Unmark tracks with " tag " matching regexp: ")
                                        ,(concat "Mark tracks with " tag " matching regexp: ")))
                                     current-prefix-arg))
                  (emms-mark-mark-tagged ,(concat "info-" tag) regexp arg))))

(defun emms-tag-editor-read-tags-completing nil
  "Read words with completion one by one until `RET' is hit twice consecutively, and return them as a list of strings.
Completion candidates are obtained from emms-tag-editor-notes-word-list, but new words that are not in this list can
also be entered. If a new word is entered then the user is prompted to add this word to emms-tag-editor-notes-word-list,
and then after all words are added, they are prompted to save this list."
  (let* ((word (completing-read "Word (press TAB to complete, RET to finish): " emms-tag-editor-notes-word-list nil nil))
	 words changed)
    (while (not (string= "" word))
      (add-to-list 'words word)
      (if (and (not (member word emms-tag-editor-notes-word-list)) (y-or-n-p "Add new word list?"))
	  (progn (add-to-list 'emms-tag-editor-notes-word-list word)
		 (setq changed t)))
      (setq word (completing-read "Word: " emms-tag-editor-notes-word-list nil nil)))
    (if (and changed (y-or-n-p "Save changed word list?"))
	(custom-save-all))
    words))


(defun emms-tag-editor-alter-notes-tag (words arg)
  "Alter arbitrary word tags to the info-note tag of tracks.
The info-tag will have a list of words separated by \":\".
If a prefix arg is supplied then the words should be removed from the
info-note tag for each track.
If region is selected then only alter fields within region.
WORDS should be a list of words (as strings) to add/remove. 
If nil then the words will be prompted for from the user with completion, until a blank is entered.
At each prompt the user can either enter one of the default words in emms-tag-editor-word-list or a new word.
If a new word is entered the user is prompted to add it to emms-tag-editor-word-list, where it will be saved."
  (interactive
   (list (emms-tag-editor-read-tags-completing) current-prefix-arg))
  (dolist (word words)  
    (save-excursion
      (save-restriction
	(if (and mark-active transient-mark-mode)
	    (narrow-to-region (region-beginning) (region-end)))
	(goto-char (point-min))
	(while (re-search-forward "^info-note[^:\n]*" nil t)
	  (let ((curr-note (buffer-substring (point) (line-end-position))))
	    (if arg
		(progn
		  (let ((tags (split-string curr-note "[:]"))
			newtags)
		    (dolist (tag tags)
		      (unless (or (string= tag word)
				  (string-match-p "^[ \t]+$" tag)
				  (string-equal "" tag))
			(push tag newtags)))
		    (delete-region (point) (line-end-position))
		    (when newtags
		      (insert ":"))
		    (insert (mapconcat #'(lambda (a) a) (nreverse newtags) ":"))
		    (when newtags
		      (insert ":"))))
	      (progn
		(goto-char (line-end-position))
		(if (char-equal (char-before (line-end-position)) ?:)
		    (insert (concat word ":"))
		  (insert (concat ":" word ":")))))))))))

(defun emms-tag-editor-clear-field (field)
  "Clear contents of a field for all tracks in tags editor.
If region is selected then only alter fields within region."
  (interactive
   (list (emms-completing-read "Search tag: "
			       (mapcar (lambda (targ)
					 (list (symbol-name (car targ))))
				       emms-tag-editor-tags)
			       nil t)))
  (save-excursion
    (save-restriction
      (if (and mark-active transient-mark-mode)
	  (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward (concat "^" field " += ") nil t)
	(if (not (equal (point) (line-end-position))) 
	    (kill-line))))))

(provide 'emms-mark-ext)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "emms-mark-ext.el" (buffer-name) (buffer-string) "update")

;;; emms-mark-ext.el ends here
