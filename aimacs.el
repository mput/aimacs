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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun create-and-show-res
    (buf-name body)
  (let ((b (get-buffer-create buf-name)))
    (with-current-buffer b
      (erase-buffer)
      (insert body)
      (goto-char 0))
    (pop-to-buffer b)))

(ignore-errors
  (set-popup-rule! "^\\*aimacs.*" :size 0.4 :side 'bottom :select nil :quit nil))


(defconst oai-comp-api-base "https://api.openai.com/v1/chat/completions")
(defconst api-key (getenv "GPT_API_KEY"))

(defun query-and-show (user-q)
  (let ((url-request-method "POST")
        (url-request-data  (json-encode `((model . "gpt-4")
                                          (messages ((role . "system")
                                                     (content . "You will be provided with statements, and your task is to convert them to standard English.
Provide the result only in markdown format while trying to preserve the markup!
Explain you corrections in markdown comment bellow corrected text, provide explanation step by step."))
                                                    ((role . "user")
                                                     (content . ,user-q))))))
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(format "Bearer %s" api-key)))))
    (url-retrieve oai-comp-api-base
                  (lambda (&rest rest-var)
                    (let* ((jbody (progn
                                    (goto-char url-http-end-of-headers)
                                    (json-read)))
                           (res (->> (aref (alist-get 'choices jbody) 0)
                                     (alist-get 'message)
                                     (alist-get 'content))))
                      (create-and-show-res "*aimacs*" res))
                    (message "Done.")))) )


(comment
 (create-and-show-res "*aimacs*" "Body here!")
 (query-and-show "I'm writing plugin for emacs which will allow to make some interractions with chat gpt!")
 )

(defun aimacs-complete ()
    (interactive)
    (if (region-active-p)
        (query-and-show (buffer-substring (region-beginning) (region-end)))
      (message "Only works on region!")))

(provide 'aimacs)
;;; aimacs.el ends here
