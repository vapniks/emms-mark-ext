;;; emms-ext.el --- Extension functions to EMMS

;; Copyright (C) 2010  

;; Author: 
;; Keywords: 

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

(provide 'emms-ext)
;;; emms-ext.el ends here
