;;; emms-ext.el --- Extension functions to EMMS

;; Copyright (C) 2010  

;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Keywords: emms, music

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `emms-tag-mark-regexp' can be used to mark based on a tag. It works at
;; `emms-mark-mode', if it's not in `emms-mark-mode', `emms-mark-mode' will 
;; be turned on automatically.

;; When in `emms-mark-mode', the following keybindings are defined for search
;; based on tags:
;; '/ m'    `emms-tag-mark-regexp'

;; TODO : we can also define other utility functions to search for different fields
;; such as `emms-tag-mark-regexp-artist', `emms-tag-mark-regexp-album', etc.

;; `emms-tag-editor-alter-notes-tag' can be used to add/remove word from
;; tracks info-note tag. And the added tag will be separated by ":".

;;; Code:
(require 'emms-mark)

;; TODO:
;; / a for artist
;; / C composer
;; / p performer
;; / t title
;; / l album
;; / y year
;; etc

(defcustom emms-tag-editor-notes-word-list '("dub" "good")
  "List of default words for the info-note field of the emms-tag-editor buffer.
This list is used by the emms-tag-editor-alter-notes-tag function when the user is prompted 
for words to add to the notes field."
  :type '(repeat string)
  :group 'emms-tag-editor)

(defun emms-ext-hook ()
  (define-key emms-mark-mode-map (kbd "/ m") 'emms-tag-mark-regexp))

(add-hook 'emms-playlist-mode-hook 'emms-ext-hook)

(defun emms-tag-mark-regexp (tag regexp arg)
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
				   "UnMark tracks matching: "
				 "Mark tracks matching: "))
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



;; folowing functions were copied from emms-extension.el by Andy Stewart (available on emacswiki)
(defun emms-playlist-play-filename ()
  "Return the filename the current play."
  (cdr (assoc 'name (emms-playlist-current-selected-track))))

(defun emms-tag-editor-next-same-field (&optional reverse)
  "Jump to next same field."
  (interactive)
  (let (filed-name)
    (save-excursion
      (beginning-of-line)
      (if (search-forward-regexp "^[^ ]*[ \t]+= " (line-end-position) t)
          (setq filed-name (buffer-substring (match-beginning 0) (match-end 0)))))
    (when filed-name
      (if (null reverse)
          (search-forward-regexp filed-name (point-max) t)
        (beginning-of-line)
        (search-backward-regexp filed-name (point-min) t))
      (goto-char (match-end 0)))))

(defun emms-tag-editor-prev-same-field ()
  "Jump to previous same field."
  (interactive)
  (emms-tag-editor-next-same-field t))

(defun emms-first-mark-track ()
  "Jump to first mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-min))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-last-mark-track ()
  "Jump to last mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-max))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-next-mark-track ()
  "Jump to next mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (bolp)
        (forward-char +1))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No next mark track."))))

(defun emms-prev-mark-track ()
  "Jump to previous mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (not (bolp))
        (beginning-of-line))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No previous mark track."))))

(defun emms-playlist-current-title ()
  "Return the filename the current play."
  (cdr (assoc 'info-title (emms-playlist-track-at))))





(provide 'emms-ext)
;;; emms-ext.el ends here
