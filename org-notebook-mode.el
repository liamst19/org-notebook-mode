;;; org-notebook minor-mode

(make-variable-buffer-local
 (defvar org-notebook-mode-on nil
   "Toggle org notebook view mode"))

(make-variable-buffer-local
 (defvar org-notebook-mode-buffer nil
   ""))

(make-variable-buffer-local
 (defvar org-notebook-mode-subtree-buffer nil
   ""))

(defvar org-notebook-window-direction 'right
  "The direction of org-notebook window will be created.")

(defun org-notebook-move-back ()
  "Switch back to the original buffer window from the subtree buffer window."
  ;; I wonder if there is another way to do this, where
  ;; the window with the original buffer can be specified.
  (cond ((eq org-notebook-window-direction 'right)
         (windmove-left))
        ((eq org-notebook-window-direction 'left)
         (windmove-right))
        ((eq org-notebook-window-direction 'above)
         (windmove-down))
        ((eq org-notebook-window-direction 'below)
         (windmove-up))))

(defun org-notebook-toggle-view ()
  "Toggles org-notebook view mode"
  (interactive)
  (if org-notebook-mode-on
      ;; Turn off
      (progn
        (setq org-notebook-mode-on nil)
        ;; Get rid of the subtree window
        (delete-window (get-buffer-window org-notebook-mode-subtree-buffer))
        ;; Get rid of the subtree buffer
        (kill-buffer org-notebook-mode-subtree-buffer))
    ;; If view mode is off, turn on
    (progn
      (setq org-notebook-mode-on t))))

(defun org-notebook-clone-subtree-other-buffer ()
  "Clones original buffer to the new window, and show and expand only the subtree."
  (clone-indirect-buffer-other-window org-notebook-mode-subtree-buffer t)
  (org-narrow-to-subtree)
  (outline-show-entry)
  (show-children))

(defun org-notebook-get-window ()
  "Clear and focus on the subtree window if it exists, split a new one, or replace the subtree window from another org buffer."
  (if (not (get-buffer-window org-notebook-mode-subtree-buffer))
    (progn
       (let ((subtree-regexp "-subtree$"))
         (dolist (buf (buffer-list))
           (progn
             (if (string-match-p subtree-regexp (buffer-name buf))
                 (progn
                   (delete-window (get-buffer-window buf))
                   (kill-buffer buf)))))
         (split-window nil nil org-notebook-window-direction)))
      ;; Get rid of the subtree content
      (kill-buffer org-notebook-mode-subtree-buffer)))

;; Indirect orgtree, reuse buffer
(defun org-subtree-to-indirect-buffer ()
  "Opens the subtree in a new indirect buffer."
  (interactive)
  (if org-notebook-mode-on
      (progn
        ;; Make sure current buffer is the original and not the subtree
        (if (eq org-notebook-mode-buffer (current-buffer))
            (progn
              (setq org-notebook-mode-subtree-buffer (concat (buffer-name) "-subtree"))
              ;; Subtree opens only when cursor is at an org-header
              (if (org-at-heading-p)
                  (progn
                    ;; Split or reuse window
                    (org-notebook-get-window)
                    ;; Generate content in the subtree buffer
                    (org-notebook-clone-subtree-other-buffer)
                    ;; Move back to the original buffer
                    (org-notebook-move-back)
                    )))))))

;;;###autoload
(define-minor-mode org-notebook-mode
  "Minor mode for quickly browsing through org-mode notes."
  :init-value nil
  :lighter " notebook"
  :keymap (let ((map  (make-sparse-keymap)))
            (define-key map (kbd "C-c o") 'org-notebook-toggle-view)
            map)
  (add-hook 'post-command-hook 'org-subtree-to-indirect-buffer)
  (setq org-notebook-mode-buffer (current-buffer)))

;;;###autoload
(add-hook 'org-mode-hook 'org-notebook-mode)

(provide 'org-notebook-mode)
