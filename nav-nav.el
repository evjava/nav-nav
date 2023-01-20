;;; nav-nav.el --- Quick path navigation based on Hydra + Helm -*- coding: utf-8;

;; Copyright (C) 2021 Eugene Tagin

;; Author: Eugene Tagin <evjava@yandex.ru>
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

(setq
 nav-nav-all-keys
 (-map #'char-to-string
       (cl-flet ((chars (ch-from cnt) (cl-loop for i from ch-from to (+ ch-from cnt) collect i)))
         (append  (chars ?a 26) (chars ?A 26) (chars ?0 9)
                  '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\( ?\) ?_ ?+)))))

(setq
 nav-nav-actions-map-default
 '((1 . (lambda (path)
          (cond
           ((string-prefix-p "*" path) (switch-to-buffer path))
           (t (progn
                (find-file path)
                (if (eq major-mode 'dired-mode) (revert-buffer)))))))
   (4 . (lambda (path)
          (let* ((directory
                  (if (file-directory-p path) path (file-name-directory path))))
            (insert (concat "cd " directory))
            (comint-send-input))))))

(defvar nav-nav-file nil 
  "File with list of key-path pairs for quick navigation")
(defvar nav-nav-en-layout "us" "english layout")
(defvar nav-nav-is-switch-keyboard nil
  "Switches to english layout before showing hydra and switches back after if `xkblayout-state` installed (2 layouts expected)")
(defvar nav-nav-actions-map nav-nav-actions-map-default
  "Actions for passed arguments")
(defvar nav-nav-default-action #'find-file
  "Default action")

(defun nav-nav-paths (path)
  (with-temp-buffer
    (insert-file-contents path)
    (read (buffer-string))))

(defun nav-nav (&optional arg)
  """ Shows hydra
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
    (nav-nav-0 (or arg 1))))

(defun nav-nav-0 (arg)
  (let* ((commands (nav-nav-paths nav-nav-file))
         (key-commands-0 (--filter (not (null (car it))) commands))
         (string>> (lambda (s1 s2) (if (null s1) nil (string> s1 s2))))
         (key-commands-1 (-sort (-on string>> #'caddr) key-commands-0))
         (key-commands (--map (cons (car it) (cadr it)) key-commands-1))
         (switch (and nav-nav-is-switch-layout (not (nav-nav-is-en-layout)))))
    (when switch
      (nav-nav-next-layout))
    (call-interactively
     (eval `(defhydra nav-nav-hydra (:columns 2 :exit t :foreign-keys warn)
              "nav-nav"
              ,@(mapcar (lambda (c) (list (car c) `(nav-nav-navigate-callback ,(car c) ,(cdr c) ,arg ,switch) (cdr c))) key-commands)
              ("?" (nav-nav-navigate-helm ,arg ,switch) "helm")
              ("<f11>" nav-nav-navigate-show-missing-paths "*Missing paths*")
              ("<f12>" nav-nav-navigate-show-free-keys "*Free keys*")
              )))))

(defun nav-nav-navigate-helm (arg switch)
  (interactive)
  (let* ((open #'(lambda (x)
                   (let* ((kv (s-split-up-to " " x 1)))
                     (nav-nav-navigate-callback (car kv) (cadr kv) arg switch))))
         (commands (nav-nav-paths nav-nav-file))
         (paths (--map (format "%s %s" (car it) (cadr it)) commands))
         (res (helm :sources (helm-build-sync-source "nav-nav:"
                               :candidates paths
                               :action `(("Open" . ,open)))))
         ) res))

(defun nav-nav-cur-buffer-path ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun nav-nav-navigate-callback (key path arg switch-layout)
  (interactive)
  (let* ((callback (alist-get arg nav-nav-actions-map nav-nav-default-action)))
    (funcall callback path)
    (if switch-layout (nav-nav-next-layout))
    (nav-nav-update-entry-by-key key #'nav-nav-update-access-time)))

(defun nav-nav-update-access-time (entry)
  (let* ((ctime (current-time))
         (stime (format-time-string "%Y-%m-%dT%H:%M" ctime))
         (res (list (car entry) (cadr entry) stime))
         ) res))

(defun nav-nav-update-path (key new-path)
  (nav-nav-update-entry-by-key
   key
   (lambda (entry)
     (list (car entry) new-path))))

(defun nav-nav-update-entry-by-key (key callback)
  (let* ((commands (nav-nav-paths nav-nav-file))
         (eq-predicate (lambda (e) (equal (car e) key)))
         (paths-upd (-map-when eq-predicate callback commands))
         (paths-parts (--map (format " %S" it) paths-upd))
         (str-paths-upd (s-concat "(\n" (s-join "\n" paths-parts) "\n)")))
    (with-temp-file nav-nav-file
      (insert str-paths-upd))))

(defun nav-nav-navigate-show-free-keys ()
  (interactive)
  (let* ((commands (nav-nav-paths nav-nav-file))
         (used-keys (--filter (not (equal it ".")) (-map #'car commands)))
         (free-keys (--filter (not (-contains? used-keys it)) nav-nav-all-keys)))
    (message "free keys: %s" free-keys)))

(defun nav-nav-navigate-show-missing-paths ()
  (interactive)
  (let* ((commands (nav-nav-paths nav-nav-file))
         (missing-commands (--filter (not (file-exists-p (cadr it))) commands))
         (joined-missing-commands (s-join "\n" (-map 'prin1-to-string missing-commands)))
         (path-msg (if missing-commands
                       (format "Missing paths:\n%s" joined-missing-commands)
                     "No missing paths")))
    (message path-msg)))

(defun nav-nav-sort-by-access ()
  (interactive)
  (let* ((paths (nav-nav-paths nav-nav-file)Б)
         (commands-sorted (-sort (-on #'string-lessp #'caddr) paths))
         (commands-sorted-str (--map (format "%s" it) commands-sorted)))
    (helm :sources (helm-build-sync-source "nav-nav (sorted by access):"
                     :candidates commands-sorted-str))))

;; reverse-im doesn't work with hydra :(
;; https://github.com/a13/reverse-im.el/issues/17
(defun nav-nav-is-en-layout ()
  (condition-case nil
      (equal (shell-command-to-string "xkblayout-state print %s") nav-nav-en-layout)
    (error t)))

(defun nav-nav-next-layout ()
  (shell-command-to-string "xkblayout-state set +1"))

(provide 'nav-nav)
