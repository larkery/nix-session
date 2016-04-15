;;; epass-authinfo.el --- Advise authinfo to use epass

;; Copyright (C) me

;;; Commentary:

;; Advises `netrc-credentials' to use gpg org files.

;;; Code:

(defcustom epass-database-paths '("~/.passwords")
  "Paths to epass database files."
  :group 'epass)

(defun epass-load-netrc-credentials (machine ports)
  "Load all the credentials for MACHINE and PORTS."
  (let (result
        result-port
        (paths (copy-sequence epass-database-paths)))
    (while paths
      (let ((file (pop paths)))
        (when (file-exists-p file)
          (if (file-directory-p file)
              (dolist (child (directory-files file t ".*\\.org\\(\\.gpg\\)?"))
                (push child paths))
            (with-current-buffer (find-file-noselect file)
              (dolist (answer (org-element-map (org-element-parse-buffer 'headline)
                                  'headline
                                (lambda (element)
                                  (let ((p   (org-element-property :PASSWORD element))
                                        (u   (org-element-property :USERNAME element))
                                        (m   (org-element-property :MACHINE element))
                                        (por (org-element-property :PORT element)))
                                    (when (and (string= machine m)
                                               (or (not ports)
                                                   (memq p ports)))

                                      (list por u p))))))
                ;; if the port is better than the port we have already
                ;; then update what we have.

                (let ((port-index (position (car answer) ports)))
                  (setq result
                        (or
                         (and ports
                              port-index
                              (> port-index result-port)
                              result)
                         (cdr answer))
                        result-port
                        (min (or result-port 1000) (or port-index 1000)))))
              )))))
    (message "%s" result)
    result
    ))


(defun epass-netrc-credentials (orig machine &rest ports)
  "Amend the result of ORIG for MACHINE and PORTS."
  (or
   (epass-load-netrc-credentials machine ports)
   (apply orig (cons machine ports))))


(advice-add 'netrc-credentials :around #'epass-netrc-credentials)

(provide 'epass-authinfo)

;;; epass-authinfo.el ends here
