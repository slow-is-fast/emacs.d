;; Keep track of loading time
(defconst emacs-start-time (current-time))
(require 'package)
;; initalize all ELPA packages
; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                        ; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)                        
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
                            emacs-start-time))))
(message "Loaded packages in %.3fs" elapsed))

;; UTF-8 as default encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; add this especially on Windows, else python output problem
(set-terminal-coding-system 'utf-8-unix)

;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
    (load custom-file))

(require 'org)
(org-babel-load-file
(expand-file-name "settings.org"
     user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                            emacs-start-time))))
(put 'upcase-region 'disabled nil)
(message "Loading settings...done (%.3fs)" elapsed))