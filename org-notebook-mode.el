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

(defcustom org-notebook-window-direction 'right
  "The direction of org-notebook window will be created.")

(defun org-notebook-toggle-view ()
  "Toggles org-notebook view mode"
  (interactive)
  (if org-notebook-mode-on
      ;; Turn off
      (progn
        (setq org-notebook-mode-on nil)
        (let ((buf (get-buffer org-notebook-mode-subtree-buffer))
              (buf-win (get-buffer-window org-notebook-mode-subtree-buffer)))
          ;; Get rid of the subtree window
          (when buf-win (delete-window buf-win))
          ;; Get rid of the subtree buffer
          (when buf (kill-buffer org-notebook-mode-subtree-buffer))))
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
    (if (get-buffer-window org-notebook-mode-subtree-buffer)
        ;; If subtree window preexists, get rid of the subtree content
        (kill-buffer org-notebook-mode-subtree-buffer)
      ;; If there is no subtree window for the buffer,
      (progn
        ;; Get a list of all open buffers
        (dolist (buf (buffer-list))
          (progn
            ;; If there are other buffers with '-subtree' suffix
            (if (string-match-p "-subtree$" (buffer-name buf))
                (progn
                  ;; If the '-subtree buffer' has a window open, close it
                  (when (get-buffer-window buf) (delete-window (get-buffer-window buf)))
                  ;; Kill the '-subtree' buffer
                  (when buf (kill-buffer buf))))))
        ;; Open a new window for the subtree
        (split-window nil nil org-notebook-window-direction))))

;; Indirect orgtree, reuse buffer
(defun org-notebook-subtree-to-indirect-buffer ()
  "Opens the subtree in a new indirect buffer."
  (interactive)
  (let ((orig-win (get-buffer-window)))
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
                (select-window orig-win)))))))

(defun org-notebook-view ()
  "If view mode is toggled, opens subtree window on header focus"
  (if org-notebook-mode-on
      (org-notebook-subtree-to-indirect-buffer)))

;;;###autoload
(define-minor-mode org-notebook-mode
  "Minor mode for quickly browsing through org-mode notes."
  :init-value nil
  :lighter " notebook"
  :keymap (let ((map  (make-sparse-keymap)))
            (define-key map (kbd "C-c o") 'org-notebook-toggle-view)
            (define-key map (kbd "S-<return>") 'org-notebook-subtree-to-indirect-buffer)
            map)
  (add-hook 'post-command-hook 'org-notebook-view)
  (setq org-notebook-mode-buffer (current-buffer)))

;;;###autoload
(add-hook 'org-mode-hook 'org-notebook-mode)

(provide 'org-notebook-mode)
