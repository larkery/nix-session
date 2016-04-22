(require 'org-element)
(require 'json)

(defvar epass-element-prefix "")
(defvar epass-interesting-properties '(:PASSWORD :URL :USERNAME :MACHINE :PORT :SHARE :DOMAIN))

(defun epass-properties (plist)
  (let (result)
    (while plist

      (when (memq (car plist) epass-interesting-properties)
        (push (cons (car plist) (cadr plist)) result))
      (setq plist (cddr plist)))

    result))

(defun epass-element-princ-json (element)
  (org-element-map element 'headline
    (lambda (x)
      (let ((name (org-element-property :raw-value x))
            (p (org-element-property :PASSWORD x))
            (u (org-element-property :URL x))
            (l (org-element-property :USERNAME x)))
       (if (or p u l) ;; leaf node
           (progn
             (princ "\n")
             (princ (json-encode
                     `((name . ,(concat epass-element-prefix "/" name))
                       ,@(epass-properties (nth 1 x)))))
             (princ "\n"))

         ;; higher up node
         (let ((epass-element-prefix (concat epass-element-prefix "/" name)))
           (epass-element-princ-json (org-element-contents x))))))
    nil nil 'headline
    ))

(defun epass-file-princ-json (file)
  (when (file-exists-p file)
    (if (file-directory-p file)
        (dolist (child (directory-files file t ".*\\.org\\(\\.gpg\\)?"))
          (epass-file-princ-json child))
      (with-current-buffer (find-file-noselect file)
        (let ((epass-element-prefix file))
          (epass-element-princ-json (org-element-parse-buffer 'headline))))
      )))
