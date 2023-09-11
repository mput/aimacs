;;; aimacs.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Maxim P
;;
;; Author: Maxim P <putintsev@gmail.com>
;; Maintainer: Maxim P <putintsev@gmail.com>
;; Created: September 10, 2023
;; Modified: September 10, 2023
;; Version: 0.0.1
;; Keywords: chat gpt
;; Homepage: https://github.com/mput/aimacs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defmacro aimacs-comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun aimacs-create-and-show-res (buf-name body mode)
  (message "mode: %s" mode)
  (let ((b (get-buffer-create buf-name)))
    (with-current-buffer b
      (erase-buffer)
      (insert body)
      (goto-char 0)
      (when mode
        (funcall mode)))
    (display-buffer b)))


(ignore-errors
  (load-env-vars "~/projects/personal/aimacs/.env")
  (set-popup-rule! "^\\*aimacs.*" :size 0.4 :side 'bottom :select nil :quit nil))



(defconst aimacs-oai-comp-api-base "https://api.openai.com/v1/chat/completions")
(defconst aimacs-api-key (getenv "GPT_API_KEY"))


(defvar aimacs-model "gpt-3.5-turbo")

(setq aimacs-templates
  '(((name . "markdown grammar correction")
     (system . "You will be provided with statements, and your task is to convert them to standard English.
Provide the result only in markdown format while trying to preserve the markup!
Explain you corrections in markdown comment bellow corrected text, provide explanation step by step.")
     (user . "{{region}}")
     (result-mode . markdown-mode))
    ((name . "refact clojure")
     (system . "Here is current namespace with Clojure code:
{{-buffer}}
Refact the next function, explain you decisions.")
     (user . "{{region}}")
     (result-mode . clojure-mode))))


(defun aimacs-replace-in-template (s s-region s-buffer)
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (while (re-search-forward "{{\\(.*\\)}}" nil t)
      (let* ((match (match-string-no-properties 1))
             (replace (pcase match
                        ("region" s-region)
                        ("buffer" s-buffer)
                        (_ ""))))
        (replace-match replace)))
    (buffer-substring (point-min) (point-max))))


(defun aimacs-resolve-template (tname)
  (let* ((rec (assoc `(name . ,tname) aimacs-templates #'equal))
         (region (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   ""))
         (buffer (buffer-substring (point-min) (point-max))))
    (mapcar (lambda (p)
              (cons (car p)
                    (if (stringp (cdr p))
                        (aimacs-replace-in-template (cdr p) region buffer)
                      (cdr p))))
            rec)))



(aimacs-comment
 (message "%s"(alist-get 'result-mode (aimacs-resolve-template "markdown grammar correction")))

 (aimacs-resolve-template "refact clojure")
 )

(defun aimacs-query-and-show (t-name)
  (let* ((template (aimacs-resolve-template t-name))
         (messages (->> '(system user)
                        (mapcar (lambda (n)
                                  (let ((v (alist-get n template)))
                                    (when v
                                      `((role . ,n)
                                        (content . , v))))))
                        (remq nil)))
         (res-mode (alist-get 'result-mode template))
         (url-request-method "POST")
         (url-request-data  (json-encode `((model . ,aimacs-model)
                                           (messages  . ,messages))))
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(format "Bearer %s" aimacs-api-key)))))
    (url-retrieve aimacs-oai-comp-api-base
                  (lambda (status res-mode)
                    (condition-case nil
                        (let* ((jbody (progn
                                        (goto-char url-http-end-of-headers)
                                        (json-read)))
                               (res (->> (aref (alist-get 'choices jbody) 0)
                                         (alist-get 'message)
                                         (alist-get 'content))))
                          (aimacs-create-and-show-res (format "*aimacs-%s*" (random 1000)) res res-mode))
                      (error
                       (pop-to-buffer (current-buffer))))
                    (message "Done."))
                  (list res-mode))))


;; (encode-coding-string "🚧 *dsd" 'utf-8)

(defun aimacs-complete ()
  (interactive)
  (aimacs-query-and-show (completing-read "Specify Template: "
                                   (mapcar (lambda (v) (alist-get 'name v) ) aimacs-templates))))


(aimacs-comment
 (aimacs-create-and-show-res "*aimacs*" "Body here!")
 (aimacs-query-and-show "I'm writing plugin for emacs which will allow to make some interractions with chat gpt!"))


(provide 'aimacs)
;;; aimacs.el ends here

