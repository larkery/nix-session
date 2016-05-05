;;; epass-authinfo.el --- Advise authinfo to use epass

;; Copyright (C) me

;;; Commentary:

;; Advises `netrc-credentials' to use gpg org files.

;;; Code:


(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'cl-macs)
(require 'auth-source)

(defcustom epass-database-paths '("~/.passwords")
  "Paths to epass database files."
  :group 'epass)

(defvar epass-interesting-properties '(:PASSWORD :URL :USERNAME :MACHINE :PORT :SHARE :DOMAIN))

(defun epass-entry->alist (plist)
  (let (result)
    (while plist
      (when (memq (car plist) epass-interesting-properties)
        (push (cons (car plist) (cadr plist)) result))
      (setq plist (cddr plist)))
    result))

(defun epass-map (f)
  (let ((paths (copy-sequence epass-database-paths)))
    (while paths
      (let ((file (pop paths)))
        (when (file-exists-p file)
          (if (file-directory-p file)
              (dolist (child (directory-files file t ".*\\.org\\(\\.gpg\\)?")) (push child paths))
            (with-current-buffer (find-file-noselect file)
              (org-element-map (org-element-parse-buffer 'headline)
                  'headline
                (lambda (element)
                  (let ((entry (epass-entry->alist (nth 1 element))))
                    (when entry (funcall f entry)))
                  t)))))))))

(defun epass-host (alist)
  (let ((machine (cdr (assoc :MACHINE alist)))
        (url (epass-url alist)))
    (or machine
        (if url (url-host (url-generic-parse-url url))))))

(defun epass-port (alist)
  (let ((port (cdr (assoc :PORT alist)))
        (url (epass-url alist)))
    (or port
        (if url (number-to-string (url-port (url-generic-parse-url url)))))))

(defun epass-user (alist)
  (cdr (assoc :USERNAME alist)))

(defun epass-password (alist)
  (cdr (assoc :PASSWORD alist)))

(defun epass-url (alist)
  (cdr (assoc :URL alist)))

(defun epass-load-netrc-credentials (machine ports)
  "Load all the credentials for MACHINE and PORTS."

  (let (result)
    (epass-map
     (lambda (alist)
       (let* ((emachine (epass-host alist))
              (eport (epass-port alist)))
         (when (and (string= emachine machine)
                    (or (not ports)
                        (member eport ports))
                    (or (not result)
                        (and ports
                             (> (position eport ports)
                                (position (car result) ports)))))
           (setq result (list eport
                              (epass-user alist)
                              (epass-password alist)))
           ))))
    (cdr result)))

(defun epass-netrc-credentials (orig machine &rest ports)
  "Amend the result of ORIG for MACHINE and PORTS."
  (or
   (epass-load-netrc-credentials machine ports)
   (apply orig (cons machine ports))))

(advice-add 'netrc-credentials :around #'epass-netrc-credentials)

;; we also want to patch auth-source-search

;; https://github.com/DamienCassou/auth-password-store/blob/master/auth-password-store.el

(cl-defun auth-epass-search (&rest spec
                                  &key backend type host user port
                                  &allow-other-keys)
  "Given a property list SPEC, return search matches from the :backend.
 See `auth-source-search' for details on SPEC."

  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid password-store search: %s %s")
  (if (listp host)
      (setq host (remove-if-not #'identity host))
    (if host
        (progn
          (when (cl-position ?: host)
            ;; so, we have a port number. we will prefer this
            (save-match-data
              (let ((parts (split-string host ":")))
                (setq host (car parts)
                      port (cadr parts)))))
          (setq host (list host)))))

  (if (numberp port)
      (setq port (number-to-string port)))

  (let (result)
    (epass-map
     (lambda (match)
       (let ((h (epass-host match))
             (p (epass-port match))
             (u (epass-user match))
             (s (epass-password match)))

         (when (and (or (not host)
                        (member h host))
                    (or (not port)
                        (string= port p))
                    (or (not user)
                        (string= user u)))

           (push
            (list
             :host h
             :port p
             :user u
             ;; may need to be a lambda
             :secret s)
            result)))))
    result)
)

 ;;;###autoload
(defun auth-epass-enable ()
  "Enable auth-password-store."
  ;; To add password-store to the list of sources, evaluate the following:
  (add-to-list 'auth-sources 'epass-store)
  ;; clear the cache (required after each change to #'auth-pass-search)
  (auth-source-forget-all-cached)
  )

(defvar auth-epass-backend
  (auth-source-backend "epass"
                       :source "." ;; not used
                       :type 'epass
                       :search-function #'auth-epass-search)
  "Auth-source backend for password-store.")

(defun auth-epass-backend-parse (entry)
  "Create a password-store auth-source backend from ENTRY."
  (when (eq entry 'epass-store)
    (auth-source-backend-parse-parameters entry auth-epass-backend)))

(advice-add 'auth-source-backend-parse :before-until #'auth-epass-backend-parse)

(auth-epass-enable)

(provide 'epass-authinfo)

;;; epass-authinfo.el ends here
