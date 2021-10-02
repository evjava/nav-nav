;;; nav-nav.el --- Quick path navigation based on Hydra + Helm -*- coding: utf-8;

;; Copyright (C) 2021 Eugene Vagin

;; Author: Eugene Vagin <evjava@yandex.ru>
;; Created: 02 Oct 2021
;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;; Keywords: navigation helm hydra
;; URL: https://github.com/evjava/nav-nav

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'hydra)
(require 'helm)
(require 'dash)

(setq nav-nav-all-keys (-map #'char-to-string 
                            (append 
                             (cl-loop for i from 0 to 26 collect (+ ?a i))
                             (cl-loop for i from 0 to 26 collect (+ ?A i))
                             '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\) ?_ ?+)
                             '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))

(defvar nav-nav-file nil 
  "the file of the kind <\n'(\n  (\"key1\" \"path1\")\n  (\"key2\" \"path2\")\n...\n))\n> for quick navigation")
(defvar nav-nav-en-layout "us" "english layout")
(defvar nav-nav-is-switch-keyboard nil
  "Switches to english layout before showing hydra and switches back after if `xkblayout-state` installed (2 layouts expected)")

(defun nav-nav (&optional arg)
  """ Shows hydra which 
  - if arg not passed: navigates to specific path
  - if arg     passed: inserts and send path (may be suitable for shell)
  Extra commands:
  - `f11` - shows missing paths
  - `f12` - shows free keys
  - `?`   - leads to showing helm with paths
  Also:
  - updates visit time selected key/path 
  """
  (interactive "p")
  (if (null nav-nav-file)
      (message "`nav-nav-file` not installed!")
    (nav-nav-0 arg)))

(defun nav-nav-0 (arg)
  (let* ((commands (nav-nav-commands-from-path nav-nav-file))
         (key-commands (--filter (not (null (car it))) commands))
         (shell-go (not (eq arg 1)))
         (switch (and nav-nav-is-switch-layout (not (nav-nav-is-en-layout)))))
    (if switch (nav-nav-next-layout))
    (call-interactively
     (eval `(defhydra nav-nav-hydra (:columns 2 :exit t :foreign-keys warn)
              "nav-nav"
              ,@(mapcar (lambda (c) (list (car c) `(nav-nav-navigate-callback ,(cdr c) ,shell-go ,switch) (cdr c))) key-commands)
              ("?" (nav-nav-navigate-helm ,shell-go ,switch) "helm")
              ("<f11>" nav-nav-navigate-show-missing-paths "*Missing paths*")
              ("<f12>" nav-nav-navigate-show-free-keys "*Free keys*")
              )))))

(defun nav-nav-commands-from-path-raw (path)
  (let* ((str-commands (get-string-from-file path))
         (commands-raw (cadr (read str-commands))))
    commands-raw))

(defun nav-nav-commands-from-path (path)
  (let* ((commands-raw (nav-nav-commands-from-path-raw path))
         (commands (--map (apply 'cons (cl-subseq it 0 2)) commands-raw)))
    commands))

(defun nav-nav-navigate-helm (shell-go switch) 
  (interactive)
  (flet ((open (x) (nav-nav-navigate-callback (cadr (s-split-up-to " " x 1)) shell-go switch)))
    (let ((paths (--map (format "%s %s" (car it) (cdr it)) (nav-nav-commands-from-path nav-items-file))))
      (helm :sources (helm-build-sync-source "nav-nav:"
                       :candidates paths
                       :action '(("Open" . open)))))))

(defun nav-nav-navigate-callback (path shell-go switch-layout)
  (interactive)
  (if shell-go
      (let ((directory 
             (if (file-directory-p path) 
                 path
               (file-name-directory path))))
        (insert (concat "cd " directory))
        (comint-send-input))
    (cond
     ((string-prefix-p "*" path) (switch-to-buffer path))
     (t (progn
          (find-file path)
          (eq major-mode 'emacs-lisp-mode)
          (if (eq major-mode 'dired-mode) (revert-buffer))))))
  (if switch-layout (nav-nav-next-layout))
  (nav-nav-update-entry-by-path path))

(defun nav-nav-patch-entry (str-entry)
  (let* ((entry (read str-entry))
         (time (format-time-string "%Y-%m-%dT%H:%M" (current-time)))
         (new-entry (append (cl-subseq entry 0 2) `(,time))))
    (concat " " (prin1-to-string new-entry))))

(defun nav-nav-update-entry-by-path (path)
  (let* ((str-commands (get-string-from-file nav-items-file))
         (regexp (format "^.*%s.*$" path))
         (entry-match (s-match regexp str-commands)))
    (if entry-match
        (let* ((old-entry (car entry-match))
               (new-entry (nav-nav-patch-entry old-entry))
               (new-str-commands (replace-regexp-in-string old-entry new-entry str-commands))
               (new-commands-0 (prin1-to-string new-str-commands))
               (len (length new-commands-0))
               (new-commands-1 (substring-no-properties new-commands-0 1 (1- len)))
               (new-commands (s-replace "\\\"" "\"" new-commands-1)))
          (with-temp-file nav-items-file
            (insert new-commands))))))

(defun nav-nav-navigate-show-free-keys ()
  (interactive)
  (let* ((str-commands (get-string-from-file nav-items-file))
         (commands-raw (cadr (read str-commands)))
         (used-keys (--filter (not (equal it ".")) (-map #'car commands-raw)))
         (free-keys (--filter (not (-contains? used-keys it)) nav-nav-all-keys)))
    (message "free keys: %s" free-keys)))

(defun nav-nav-navigate-show-missing-paths ()
  (interactive)
  (let* ((str-commands (get-string-from-file nav-items-file))
         (commands-raw (cadr (read str-commands)))
         (missing-commands (--filter (not (file-exists-p (cadr it))) commands-raw))
         (joined-missing-commands (s-join "\n" (-map 'prin1-to-string missing-commands))))
    (message "Missing paths:\n%s" joined-missing-commands)))

;; reverse-im doesn't work with hydra :(
;; https://github.com/a13/reverse-im.el/issues/17
(defun nav-nav-is-en-layout ()
  (equal (shell-command-to-string "xkblayout-state print %s") nav-nav-en-layout))

(defun nav-nav-next-layout ()
  (shell-command-to-string "xkblayout-state set +1"))

(provide 'nav-nav)
