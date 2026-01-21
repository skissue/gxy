;;; org-resnap.el --- Insert reMarkable screenshots into Org -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/skissue/gxy
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Insert screenshots from the reMarkable paper tablet into Org Mode attachments
;; via reSnap: https://github.com/cloudsftp/reSnap.

;;; Code:

(require 'org-attach)
(require 'ol)
(require 'url)

(defgroup org-resnap nil
  "Insert reMarkable screenshots into Org."
  :group 'org
  :prefix "org-resnap-")

(defconst org-resnap--script-url
  "https://raw.githubusercontent.com/cloudsftp/reSnap/latest/reSnap.sh"
  "URL to download the reSnap script from.")

(defcustom org-resnap-command 'auto
  "Location of the reSnap script.
When set to `auto', the script will be downloaded automatically
to `org-resnap-download-path' if not already present."
  :type '(choice (const :tag "Auto-download" auto)
                 (string :tag "Path to reSnap")))

(defcustom org-resnap-download-path
  (expand-file-name "reSnap.sh" user-emacs-directory)
  "Path to download the reSnap script to when `org-resnap-command' is `auto'."
  :type 'string)

(defcustom org-resnap-preview t
  "Whether to call `org-link-preview' after inserting the link."
  :type 'boolean)

(defconst org-resnap--local-ip "10.11.99.1"
  "Default IP address of the reMarkable when connected via USB.")

(defcustom org-resnap-external-ip nil
  "External IP address of the reMarkable for WiFi connections.
Used as fallback when USB connection is not available."
  :type '(choice (const :tag "None" nil)
                 (string :tag "IP address")))

(defun org-resnap--ensure-script ()
  "Automatically download the reSnap script if needed.
Sets `org-resnap-command' to the path of the downloaded script."
  (unless (file-exists-p org-resnap-download-path)
    (message "Downloading reSnap...")
    (url-copy-file org-resnap--script-url org-resnap-download-path)
    (set-file-modes org-resnap-download-path #o755))
  (setq org-resnap-command org-resnap-download-path))

(defun org-resnap--local-reachable-p ()
  "Return non-nil if the reMarkable is reachable at the local USB IP.
Checks if any local interface has an IP in the reMarkable USB subnet."
  (zerop (call-process-shell-command
          "ip -4 addr show | grep -q 'inet 10\\.11\\.99\\.'")))

(defun org-resnap--get-ip ()
  "Return the IP address to use for connecting to the reMarkable.
Returns the local USB IP if reachable, otherwise the external IP.
Signals an error if neither is available."
  (cond
   ((org-resnap--local-reachable-p) org-resnap--local-ip)
   (org-resnap-external-ip org-resnap-external-ip)
   (t (user-error "reMarkable not connected via USB and `org-resnap-external-ip' is not set"))))

;;;###autoload
(defun org-resnap-screenshot ()
  "Take a screenshot, attach it, and insert the link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org Mode buffer"))
  (when (eq org-resnap-command 'auto)
    (org-resnap--ensure-script))
  (let ((ip (org-resnap--get-ip)))
    (message "Taking screenshot from %s..." ip)
    (let* ((temp-file (make-temp-file "org-resnap-" nil ".png"))
           (output-buffer (get-buffer-create "*org-resnap*"))
           (exit-code (with-current-buffer output-buffer
                        (erase-buffer)
                        (call-process org-resnap-command nil t nil
                                      "-n" "-s" ip "-o" temp-file))))
      (unless (zerop exit-code)
        (delete-file temp-file)
        (display-buffer output-buffer)
        (user-error "Calling reSnap failed with exit code %d" exit-code))
      (kill-buffer output-buffer)
      (let ((link (car (org-attach-attach temp-file nil 'mv))))
        (insert (org-link-make-string-for-buffer link))
        (when org-resnap-preview
          (org-link-preview))))))

(provide 'org-resnap)

;;; org-resnap.el ends here
