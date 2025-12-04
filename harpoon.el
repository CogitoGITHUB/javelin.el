;;; harpoon.el --- Bookmarks on steroids    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Otávio Schwanck, 2025 Damian Barabonkov

;; Author: Otávio Schwanck <otavioschwanck@gmail.com>
;; Keywords: tools languages
;; Homepage: https://github.com/otavioschwanck/harpoon.el
;; Version: 0.5
;; Package-Requires: ((emacs "27.2") (f "0.20.0") (hydra "0.14.0") (project "0.8.1"))

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

;; This is a plugin base on harpoon from vim (by ThePrimeagen).  Is like a
;; bookmark manager on steroids.
;; You can easily add, reorder and delete bookmarks.  The bookmarks are
;; separated by project and branch.

;;; Changelog
;;; 0.5
;;; Fix when project is not loaded
;;;
;;; 0.4
;;; Added hydra support

;;; Code:
(require 'f)
(require 'subr-x)

(defun harpoon--default-project-package ()
  "Return the default project package."
  (if (featurep 'projectile) 'projectile 'project))

(defgroup harpoon nil
  "Organize bookmarks by project and branch."
  :group 'tools)

(defcustom harpoon-without-project-function 'harpoon--package-name
  "When project is not found, use this function instead."
  :type 'string)

(defcustom harpoon-cache-file (concat user-emacs-directory ".local/harpoon/")
  "Where the cache will be saved."
  :type 'string)

(defcustom harpoon-project-package (harpoon--default-project-package)
  "Project package to access project functions."
  :type 'symbol)

(defcustom harpoon-separate-by-branch t
  "Harpoon separated by branch."
  :type 'boolean)

(defvar harpoon-cache '()
  "Cache for harpoon.")

(defvar harpoon-cache-loaded nil
  "Cache for harpoon.")

(defun harpoon-project-root-function ()
  "Get the project root."
  (cond
   ((eq harpoon-project-package 'projectile) (when (fboundp 'projectile-project-root) (projectile-project-root)))
   ((eq harpoon-project-package 'project) (expand-file-name (when (fboundp 'project-root) (project-root (project-current)))))))

(defun harpoon--current-file-directory ()
  "Return current directory path sanitized."
  (harpoon--sanitize (file-name-directory buffer-file-name)))

(defun harpoon--has-project ()
  "Get the project name."
  (let ((project-name (harpoon--get-project-name)))
    (not (or (string= project-name "") (string= project-name "-") (string= project-name nil)))))

(defun harpoon--get-project-name ()
  "Get the harpoon project name."
  (condition-case nil (cond
                       ((eq harpoon-project-package 'projectile) (when (fboundp 'projectile-project-name) (projectile-project-name)))
                       ((eq harpoon-project-package 'project) (harpoon--get-project-name-for-project)))
    (error nil)))

(defun harpoon-project-name-function ()
  "Get the project name."
  (if (harpoon--has-project) (harpoon--get-project-name) (funcall harpoon-without-project-function)))

(defun harpoon--get-project-name-for-project ()
  "Return projects name for project."
  (let* ((splitted-project-path (split-string (project-root (project-current)) "/"))
         (splitted-length (length splitted-project-path))
         (project-name (nth (- splitted-length 2) splitted-project-path)))
    project-name))

(defun harpoon--get-branch-name ()
  "Get the branch name for harpoon."
  (car (split-string
        (shell-command-to-string
         (concat "cd " (harpoon-project-root-function) "; git rev-parse --abbrev-ref HEAD")) "\n")))

(defun harpoon--cache-key ()
  "Key to save current file on cache."
  (if (harpoon--has-project) (if harpoon-separate-by-branch
                                 (concat (harpoon--sanitize (harpoon-project-name-function))
                                         "#"
                                         (harpoon--sanitize (harpoon--get-branch-name)))
                               (harpoon--sanitize (harpoon-project-name-function)))
    (harpoon--sanitize (harpoon-project-name-function))))

(defun harpoon--create-directory ()
  "Create harpoon cache dir if doesn't exist."
  (unless (f-directory? harpoon-cache-file)
    (make-directory harpoon-cache-file t)))

(defun harpoon--file-name ()
  "File name for harpoon on current project."
  (concat harpoon-cache-file (harpoon--cache-key) ".json"))

(defun harpoon--read-json ()
  "Read and parse the harpoon JSON file.
Returns a list of alists with `harpoon_number' and `filepath' keys."
  (if (file-exists-p (harpoon--file-name))
      (condition-case nil
          (json-parse-string (f-read (harpoon--file-name) 'utf-8)
                             :object-type 'alist
                             :array-type 'list)
        (error '()))
    '()))

(defun harpoon--write-json (data)
  "Write DATA to the harpoon JSON file.
DATA should be a list of alists with `harpoon_number' and `filepath' keys."
  (harpoon--create-directory)
  (f-write-text (json-serialize data) 'utf-8 (harpoon--file-name)))

(defun harpoon--get-filepath-by-number (harpoon-number)
  "Get the filepath for a given HARPOON-NUMBER.
Returns nil if not found."
  (let ((data (harpoon--read-json)))
    (alist-get 'filepath
               (seq-find (lambda (item)
                           (= (alist-get 'harpoon_number item) harpoon-number))
                         data))))

(defun harpoon--set-filepath-by-number (harpoon-number filepath)
  "Set FILEPATH for a given HARPOON-NUMBER.
Updates existing entry or adds a new one."
  (let* ((data (harpoon--read-json))
         (existing (seq-find (lambda (item)
                               (= (alist-get 'harpoon_number item) harpoon-number))
                             data)))
    (if existing
        ;; Update existing entry
        (setf (alist-get 'filepath existing) filepath)
      ;; Add new entry
      (push `((harpoon_number . ,harpoon-number) (filepath . ,filepath)) data))
    (harpoon--write-json data)))

(defun harpoon--remove-by-number (harpoon-number)
  "Remove entry with HARPOON-NUMBER from the harpoon list."
  (let ((data (harpoon--read-json)))
    (harpoon--write-json
     (seq-remove (lambda (item)
                   (= (alist-get 'harpoon_number item) harpoon-number))
                 data))))

(defun harpoon--get-all-filepaths ()
  "Get all filepaths from harpoon, sorted by harpoon_number.
Returns a list of filepaths."
  (let ((data (harpoon--read-json)))
    (mapcar (lambda (item) (alist-get 'filepath item))
            (seq-sort (lambda (a b)
                        (< (alist-get 'harpoon_number a)
                           (alist-get 'harpoon_number b)))
                      data))))

(defun harpoon--next-available-number ()
  "Get the next available harpoon number."
  (let* ((data (harpoon--read-json))
         (numbers (mapcar (lambda (item) (alist-get 'harpoon_number item)) data)))
    (if numbers (1+ (apply #'max numbers)) 1)))

(defun harpoon--buffer-file-name ()
  "Parse harpoon file name."
  (if (harpoon--has-project) (s-replace-regexp (harpoon-project-root-function) "" (buffer-file-name)) (buffer-file-name)))

(defun harpoon--sanitize (string)
  "Sanitize word to save file.  STRING: String to sanitize."
  (s-replace-regexp "/" "---" string))

;;;###autoload
(defun harpoon-go-to (harpoon-number)
  "Go to specific file on harpoon by HARPOON-NUMBER."
  (require 'project)
  (let* ((file-name (harpoon--get-filepath-by-number harpoon-number))
         (full-file-name (when file-name
                           (if (and (fboundp 'project-root) (harpoon--has-project))
                               (concat (or harpoon--project-path (harpoon-project-root-function)) file-name)
                             file-name))))
    (cond
     ((null file-name)
      (message "No file harpooned to position %d" harpoon-number))
     ((file-exists-p full-file-name)
      (find-file full-file-name))
     (t
      (message "%s not found." full-file-name)))))

(defun harpoon--delete (harpoon-number)
  "Delete an item on harpoon. HARPOON-NUMBER: Position to delete."
  (harpoon--remove-by-number harpoon-number)
  (message "Deleted harpoon position %d" harpoon-number))


;;;###autoload
(defun harpoon-delete-1 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 1))

;;;###autoload
(defun harpoon-delete-2 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 2))

;;;###autoload
(defun harpoon-delete-3 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 3))

;;;###autoload
(defun harpoon-delete-4 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 4))

;;;###autoload
(defun harpoon-delete-5 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 5))

;;;###autoload
(defun harpoon-delete-6 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 6))

;;;###autoload
(defun harpoon-delete-7 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 7))

;;;###autoload
(defun harpoon-delete-8 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 8))

;;;###autoload
(defun harpoon-delete-9 ()
  "Delete item harpoon on position 1."
  (interactive)
  (harpoon--delete 9))

;;;###autoload
(defun harpoon-go-to-1 ()
  "Go to file 1 on harpoon."
  (interactive)
  (harpoon-go-to 1))

;;;###autoload
(defun harpoon-go-to-2 ()
  "Go to file 2 on harpoon."
  (interactive)
  (harpoon-go-to 2))

;;;###autoload
(defun harpoon-go-to-3 ()
  "Go to file 3 on harpoon."
  (interactive)
  (harpoon-go-to 3))

;;;###autoload
(defun harpoon-go-to-4 ()
  "Go to file 4 on harpoon."
  (interactive)
  (harpoon-go-to 4))

;;;###autoload
(defun harpoon-go-to-5 ()
  "Go to file 5 on harpoon."
  (interactive)
  (harpoon-go-to 5))

;;;###autoload
(defun harpoon-go-to-6 ()
  "Go to file 6 on harpoon."
  (interactive)
  (harpoon-go-to 6))

;;;###autoload
(defun harpoon-go-to-7 ()
  "Go to file 7 on harpoon."
  (interactive)
  (harpoon-go-to 7))

;;;###autoload
(defun harpoon-go-to-8 ()
  "Go to file 8 on harpoon."
  (interactive)
  (harpoon-go-to 8))

;;;###autoload
(defun harpoon-go-to-9 ()
  "Go to file 9 on harpoon."
  (interactive)
  (harpoon-go-to 9))

(defun harpoon-assign-to (harpoon-number)
  "Assign the current buffer to a specific position in harpoon.
HARPOON-NUMBER: The position (1-9) to assign the current file to."
  (require 'project)
  (let ((file-to-add (harpoon--buffer-file-name)))
    (harpoon--set-filepath-by-number harpoon-number file-to-add)
    (message "Assigned %s to harpoon position %d" file-to-add harpoon-number)))

;;;###autoload
(defun harpoon-assign-to-1 ()
  "Assign current buffer to position 1 on harpoon."
  (interactive)
  (harpoon-assign-to 1))

;;;###autoload
(defun harpoon-assign-to-2 ()
  "Assign current buffer to position 2 on harpoon."
  (interactive)
  (harpoon-assign-to 2))

;;;###autoload
(defun harpoon-assign-to-3 ()
  "Assign current buffer to position 3 on harpoon."
  (interactive)
  (harpoon-assign-to 3))

;;;###autoload
(defun harpoon-assign-to-4 ()
  "Assign current buffer to position 4 on harpoon."
  (interactive)
  (harpoon-assign-to 4))

;;;###autoload
(defun harpoon-assign-to-5 ()
  "Assign current buffer to position 5 on harpoon."
  (interactive)
  (harpoon-assign-to 5))

;;;###autoload
(defun harpoon-assign-to-6 ()
  "Assign current buffer to position 6 on harpoon."
  (interactive)
  (harpoon-assign-to 6))

;;;###autoload
(defun harpoon-assign-to-7 ()
  "Assign current buffer to position 7 on harpoon."
  (interactive)
  (harpoon-assign-to 7))

;;;###autoload
(defun harpoon-assign-to-8 ()
  "Assign current buffer to position 8 on harpoon."
  (interactive)
  (harpoon-assign-to 8))

;;;###autoload
(defun harpoon-assign-to-9 ()
  "Assign current buffer to position 9 on harpoon."
  (interactive)
  (harpoon-assign-to 9))

;;;###autoload
(defun harpoon-go-to-next ()
  "Go to the next file in harpoon."
  (interactive)
  (let* ((data (harpoon--read-json))
         (sorted-data (seq-sort (lambda (a b)
                                  (< (alist-get 'harpoon_number a)
                                     (alist-get 'harpoon_number b)))
                                data))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) sorted-data))
         (current-file (harpoon--buffer-file-name))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (next-index (mod (+ current-index 1) (length files)))
         (next-item (nth next-index sorted-data)))
    (when next-item
      (harpoon-go-to (alist-get 'harpoon_number next-item)))))

;;;###autoload
(defun harpoon-go-to-prev ()
  "Go to the previous file in harpoon."
  (interactive)
  (let* ((data (harpoon--read-json))
         (sorted-data (seq-sort (lambda (a b)
                                  (< (alist-get 'harpoon_number a)
                                     (alist-get 'harpoon_number b)))
                                data))
         (files (mapcar (lambda (item) (alist-get 'filepath item)) sorted-data))
         (current-file (harpoon--buffer-file-name))
         (current-index (or (cl-position current-file files :test 'string=) -1))
         (prev-index (mod (+ current-index (length files) -1) (length files)))
         (prev-item (nth prev-index sorted-data)))
    (when prev-item
      (harpoon-go-to (alist-get 'harpoon_number prev-item)))))

;;;###autoload
(defun harpoon-add-file ()
  "Add current file to harpoon."
  (interactive)
  (let* ((file-to-add (harpoon--buffer-file-name))
         (data (harpoon--read-json))
         (existing (seq-find (lambda (item)
                               (string= (alist-get 'filepath item) file-to-add))
                             data)))
    (if existing
        (message "This file is already on harpoon.")
      (let ((next-num (harpoon--next-available-number)))
        (harpoon--set-filepath-by-number next-num file-to-add)
        (message "File added to harpoon at position %d." next-num)))))

;;;###autoload
(defun harpoon-quick-menu-hydra ()
  "Open harpoon quick menu with hydra."
  (interactive)
  (require 'hydra)
  (let ((candidates (harpoon--hydra-candidates "harpoon-go-to-")))
    (eval `(defhydra harpoon-hydra (:exit t :column 1)
             "

        ██╗  ██╗ █████╗ ██████╗ ██████╗  ██████╗  ██████╗ ███╗   ██╗
        ██║  ██║██╔══██╗██╔══██╗██╔══██╗██╔═══██╗██╔═══██╗████╗  ██║
        ███████║███████║██████╔╝██████╔╝██║   ██║██║   ██║██╔██╗ ██║
        ██╔══██║██╔══██║██╔══██╗██╔═══╝ ██║   ██║██║   ██║██║╚██╗██║
        ██║  ██║██║  ██║██║  ██║██║     ╚██████╔╝╚██████╔╝██║ ╚████║
        ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝      ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝
                                                            "
             ,@candidates
             ("SPC" harpoon-toggle-quick-menu "Open Menu" :column "Other Actions")
             ("d" harpoon-delete-item "Delete some harpoon" :column "Other Actions")
             ("f" harpoon-toggle-file "Open Harpoon File" :column "Other Actions")
             ("c" harpoon-clear "Clear Harpoon" :column "Other Actions")
             ("s" harpoon-add-file "Save Current File to Harpoon" :column "Other Actions"))))

  (when (fboundp 'harpoon-hydra/body) (harpoon-hydra/body)))


(defun harpoon--hydra-candidates (method)
  "Candidates for hydra. METHOD = Method to execute on harpoon item."
  (let* ((data (harpoon--read-json))
         (sorted-data (seq-sort (lambda (a b)
                                  (< (alist-get 'harpoon_number a)
                                     (alist-get 'harpoon_number b)))
                                data))
         (full-candidates (seq-take sorted-data 9)))
    (mapcar (lambda (item)
              (let ((num (alist-get 'harpoon_number item))
                    (filepath (alist-get 'filepath item)))
                (list (format "%s" num)
                      (intern (concat method (format "%s" num)))
                      (harpoon--format-item-name filepath)
                      :column (if (< num 6) "1-5" "6-9"))))
            full-candidates)))

(defun harpoon--format-item-name (item)
  "Format item on harpoon. ITEM = Item to be formated.
FULL-CANDIDATES:  Candidates to be edited."
  (if (string-match-p "/" item)
      (let ((splitted-item (split-string item "/")))
        (harpoon--already-includes-text item splitted-item)) item))

(defun harpoon--already-includes-text (item splitted-item)
  "Return the name to be used on hydra.
ITEM = Full item.  SPLITTED-ITEM = Item splitted.
FULL-CANDIDATES = All candidates to look."
  (let ((file-base-name (nth (- (length splitted-item) 1) splitted-item))
        (candidates (seq-take (harpoon--get-all-filepaths) 9)))
    (if (member file-base-name (mapcar (lambda (x)
                                         (nth (- (length (split-string x "/")) 1) (split-string x "/")))
                                       (delete item candidates)))
        (concat file-base-name " at " (string-join (butlast splitted-item) "/"))
      file-base-name)))


;;;###autoload
(defun harpoon-delete-item ()
  "Delete items on harpoon."
  (interactive)
  (let ((candidates (harpoon--hydra-candidates "harpoon-delete-")))
    (eval `(defhydra harpoon-delete-hydra (:exit t :column 1 :color red)
             "

   /0000000\\
   | 00000 |
   | | | | |
   | TRASH |
   | | | | |
   \\-------/

Select items to delete:
"
             ,@candidates
             ("SPC" harpoon-quick-menu-hydra "Back to harpoon" :column "Other Actions")
             ("q" hydra-keyboard-quit "Quit" :column "Other Actions"))))

  (when (fboundp 'harpoon-delete-hydra/body) (harpoon-delete-hydra/body)))


(defun harpoon--package-name ()
  "Return harpoon package name."
  "harpoon")

;;;###autoload
;;;###autoload
(defun harpoon-toggle-file ()
  "Open harpoon JSON file for viewing/editing."
  (interactive)
  (harpoon--create-directory)
  ;; Ensure file exists with empty array if it doesn't
  (unless (file-exists-p (harpoon--file-name))
    (harpoon--write-json '()))
  (find-file (harpoon--file-name)))

;;;###autoload
(defun harpoon-toggle-quick-menu ()
  "Open quickmenu."
  (interactive)
  (let ((result (harpoon--fix-quick-menu-items)))
    (when (and result (not (string-equal result "")))
      (find-file (if (harpoon--has-project) (concat (harpoon-project-root-function) (harpoon--remove-number result))
                   (harpoon--remove-number result))))))

(defun harpoon--remove-number (file)
  "Remove number of the file. FILE = Filename to remove the number."
  (nth 1 (split-string file " - ")))

(defun harpoon--fix-quick-menu-items ()
  "Fix harpoon quick menu items."
  (let* ((data (harpoon--read-json))
         (sorted-data (seq-sort (lambda (a b)
                                  (< (alist-get 'harpoon_number a)
                                     (alist-get 'harpoon_number b)))
                                data))
         (items (mapcar (lambda (item)
                          (format "%d - %s"
                                  (alist-get 'harpoon_number item)
                                  (alist-get 'filepath item)))
                        sorted-data)))
    (completing-read "Harpoon to file: " items)))

;;;###autoload
(defun harpoon-clear ()
  "Clear harpoon files."
  (interactive)
  (when (yes-or-no-p "Do you really want to clear harpoon file? ")
    (harpoon--write-json '())
    (message "Harpoon cleaned.")))

(provide 'harpoon)
;;; harpoon.el ends here
