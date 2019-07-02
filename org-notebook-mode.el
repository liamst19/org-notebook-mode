;;; org-notebook minor-mode

(make-variable-buffer-local
 (defvar org-notebook-mode-on nil
   "Toggle org notebook view mode"))

(defun org-toggle-notebook-view ()
  "Toggles Notebook view mode"
  (interactive)
  (if org-notebook-mode-on
      (progn
        (setq org-notebook-mode-on nil)
        (let ((ind-buf "org-subtree-indirect"))
          (delete-window (get-buffer-window ind-buf))
          (kill-buffer ind-buf)))
    (setq org-notebook-mode-on t)))

;; Indirect orgtree, reuse buffer
(defun org-subtree-to-indirect-buffer ()
  "Opens the subtree in a new indirect buffer."
  (interactive)
  (if org-notebook-mode-on
      (let ((ind-buf "org-subtree-indirect"))
         (if (org-at-heading-p)
             (progn
               (if (get-buffer-window ind-buf)
                   (kill-buffer ind-buf)
                 (split-window nil nil 'right))
               (clone-indirect-buffer-other-window ind-buf t)
               (org-narrow-to-subtree)
               (outline-show-entry)
               (show-children)
               (windmove-left))))))

;;;###autoload
(define-minor-mode org-notebook-mode
  "Toggle org notebook view mode"
  :init-value nil
  :lighter " notebook"
   (add-hook 'post-command-hook 'org-subtree-to-indirect-buffer))

;;;###autoload
(add-hook 'org-mode-hook 'org-notebook-mode)

(provide 'org-notebook-mode)
