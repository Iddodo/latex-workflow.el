;;; latex-workflow.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Ido Merenstein
;;
;; Author: Ido Merenstein <m.ido@campus.technion.ac.il>
;; Maintainer: Ido Merenstein <m.ido@campus.technion.ac.il>
;; Created: June 30, 2023
;; Modified: June 30, 2023
;; Version: 0.0.1
;; Keywords: latex convenience data docs extensions unix
;; Homepage: https://github.com/Iddodo/latex-workflow.ek
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;; This is still a work in progress, and will be changed often.
;;
;;  Description
;;  The intent of this package is to help create a more comfortable
;;  experience of writing lively and colorful LaTeX documents inside Emacs.
;;
;;; Code:


;; Default location to locate template directory
(defvar latex-workflow-template-path (concat (file-name-directory buffer-file-name)
                                             "templates/"))

;; List of currently available templates
(defvar latex-workflow-templates-alist
  `((tikz-figure . ((input-line . 4)
                    (input-function . latex-workflow-tikz-figure-template-input)))
    (tikz-standalone . ((input-line . 5)
                        (input-function . nil)))))



(defun latex-workflow-template-assoc (template)
  (let ((template-alist (assoc template latex-workflow-templates-alist)))
    (unless template-alist
      (error "LaTeX template %s not found" template))
    template-alist))

(defun latex-workflow-parse-template (template &optional input)
  (let-alist (latex-workflow-template-assoc template)
    (with-temp-buffer
      (insert-file-contents
       (concat latex-workflow-template-path (symbol-name template) ".tex"))
      (when .input-function
        (forward-line .input-line)
        (insert (funcall .input-function input)))
      (buffer-string))))

(defun latex-workflow-insert-template (template &optional input)
  (interactive)
  (set-mark-command nil)
  (insert (latex-workflow-parse-template template input))
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))



(defun latex-workflow-create-tikz-figure ()
  "Create a new TiKZ figure as a standalone file, insert to current buffer
  and open in new buffer.
  Currently only supports creation of figure insdide a \"figures\" subfolder."
  (interactive)
  ;; Create figures directory if it doesn't exist
  (unless (file-exists-p "figures")
    (make-directory "figures"))
  (let* ((file-path (concat (read-string "Figure name: " "figures/") ".tex"))
         (file-name (file-name-nondirectory file-path)))
    ;; Check if file already exists
    (when (file-exists-p file-path)
      (let ((overwrite (y-or-n-p (concat "File " file-name " already exists. Overwrite?"))))
        (unless overwrite
          (error "File already exists."))
        ;; Delete file if overwrite is true
        (delete-file file-path)
        ;; Kill buffer if it exists
        (when (get-file-buffer file-path)
          (kill-buffer (get-file-buffer file-path)))))
    ;; Insert figure to current buffer in a new line
    (latex-workflow-insert-template 'tikz-figure file-path)
    ;; Open the file
    (find-file file-path))
  ;; Insert TiKZ figure template
  (latex-workflow-insert-template 'tikz-standalone)
  ;; Save the file
  (save-buffer)
  ;; Put cursor in line 6
  (forward-line (latex-workflow-template-line-number 'tikz-standalone))
  (LaTeX-indent-line)
  ;; Switch to evil insert mode
  (when (featurep 'evil)
    (evil-insert 1)))

(defun latex-workflow-tikz-figure-template-input (tikz-file)
  "Input function for TiKZ figures."
  (concat "\\input{" (file-name-sans-extension tikz-file) "}"))


(defun latex-workflow-template-line-number (template)
  "Fetch input line of some template."
  (alist-get 'input-line (latex-workflow-template-assoc template)))


(defun latex-workflow-copy-tikz-figure ()
  "Copy an existing Tikz figure."
  (interactive)
  (let ((fig-list (mapcar 'file-name-sans-extension
                          (directory-files "figures/" nil (rx ".tex" eos)))))
    (unless fig-list
      (error "No figures to copy."))

    (let* ((selected-fig (completing-read "Copy figure: " fig-list))
           (selected-fig-path (concat "figures/" selected-fig ".tex"))
           (new-fig (read-string "New figure name: " selected-fig))
           (new-fig-path (concat "figures/" new-fig ".tex")))

      (when (file-exists-p new-fig-path)
        (unless (y-or-n-p (concat "Figure " new-fig " already exists. Overwrite?"))
          (error "Figure already exists."))
        (delete-file new-fig-path))

      (copy-file selected-fig-path new-fig-path)
      (latex-workflow-insert-template 'tikz-figure (concat "figures/" new-fig))
      (find-file new-fig-path))))

(defun latex-workflow-insert-existing-figure ()
  "Insert some existing figure."
  (interactive)
  (let ((fig-list (mapcar 'file-name-sans-extension
                          (directory-files "figures/" nil (rx ".tex" eos)))))
    (unless fig-list
      (error "No figures in \"figures/\" folder."))

    (let* ((selected-fig (completing-read "Figure: " fig-list)))

      (latex-workflow-insert-template 'tikz-figure (concat "figures/" selected-fig)))))


(defun latex-workflow-insert-clipboard-image ()
  "Paste image from clipboard to Latex, add support for later renaming."
  (interactive)
  ;; Create asset folder if it doesn't exist
  (unless (file-exists-p "assets")
    (make-directory "assets"))
  ;; Remember current image name in case user wants to rename it
  (let ((current-image-name (concat "assets/" (make-temp-name "image-") ".png")))
    ;; Paste image from clipboard to file
    (shell-command (concat "pngpaste " current-image-name))
    ;; Insert image to LaTeX buffer
    (insert (concat "\\includegraphics{" current-image-name "}"))))


(defun latex-workflow-includegraphics-rename ()
  "Rename image file of a general includegraphics command at point
  (where cursor currently is)."
  (interactive)
  (beginning-of-line)
  (let ((end-of-line (line-end-position)))
    (when (re-search-forward "\\\\includegraphics{\\(.*?\\)}" end-of-line t)
      ;; Edit file name sans extension, then put the extension back
      ;; otherwise the extension will be lost
      ;; Also, delete the old file name from the read-string and only keep its path
      (let* ((current-image-name (match-string 1))
             (new-image-name-no-extension
              (read-string "New image name: "(file-name-directory current-image-name)))
             (new-image-name (concat new-image-name-no-extension "." (file-name-extension current-image-name))))
        (rename-file current-image-name new-image-name)
        (replace-match new-image-name nil nil nil 1)))))


(provide 'latex-workflow)
;;; latex-workflow.el ends here
