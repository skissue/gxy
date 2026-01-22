;;; elfeed-protocol-miniflux.el --- Miniflux protocol for elfeed -*- lexical-binding: t; -*-

;;; Commentary:
;; Miniflux protocol for elfeed.

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'elfeed)
(require 'elfeed-protocol-common)

;;; Code:

(defcustom elfeed-protocol-miniflux-maxsize 1000
  "Maximize entries size for each request."
  :group 'elfeed-protocol
  :type 'integer)

(defcustom elfeed-protocol-miniflux-star-tag 'star
  "Default star tag for Miniflux entry.
If one entry set or remove the tag,
then the starred state in Miniflux will be synced, too."
  :group 'elfeed-protocol
  :type 'symbol)

(defcustom elfeed-protocol-miniflux-update-with-modified-time t
  "Determine default update method for Miniflux.
If t will update since last modified time, and if nil will update since last entry id."
  :group 'elfeed-protocol
  :type 'boolean)

(defcustom elfeed-protocol-miniflux-sync-status-on-update t
  "If non-nil, sync read/starred status from server on each update.
This uses the Miniflux `changed_after` API parameter (requires Miniflux 2.0.49+)
to fetch entries whose status changed since last sync, allowing status changes
made on other devices to be reflected in elfeed."
  :group 'elfeed-protocol
  :type 'boolean)

(defcustom elfeed-protocol-miniflux-fetch-category-as-tag t
  "If true, tag the Miniflux feed category to feed item."
  :group 'elfeed-protocol
  :type 'boolean)

(defvar elfeed-protocol-miniflux-categories (make-hash-table :test 'equal)
  "Category list from Miniflux, will be used to tag entries.")

(defvar elfeed-protocol-miniflux-feeds (make-hash-table :test 'equal)
  "Feed list from Miniflux, will be filled before updating operation.")

(defun elfeed-protocol-miniflux--get-last-sync-time (proto-id)
  "Get last sync time for PROTO-ID.
This tracks when we last synced read/starred status from the server,
separate from :last-modified which tracks new entries by publication time."
  (let ((last-sync (elfeed-protocol-get-feed-meta-data proto-id :last-sync-time)))
    (if last-sync last-sync 0)))

(defun elfeed-protocol-miniflux--set-last-sync-time (proto-id timestamp)
  "Set last sync time for PROTO-ID to TIMESTAMP."
  (elfeed-protocol-set-feed-meta-data proto-id :last-sync-time timestamp))

(defconst elfeed-protocol-miniflux-api-base "/v1")
(defconst elfeed-protocol-miniflux-api-feeds (concat elfeed-protocol-miniflux-api-base "/feeds"))
(defconst elfeed-protocol-miniflux-api-categories (concat elfeed-protocol-miniflux-api-base "/categories"))
(defconst elfeed-protocol-miniflux-api-entries (concat elfeed-protocol-miniflux-api-base "/entries"))
(defconst elfeed-protocol-miniflux-api-entries-status (concat elfeed-protocol-miniflux-api-base "/entries"))
(defconst elfeed-protocol-miniflux-api-entry-toggle-bookmark (concat elfeed-protocol-miniflux-api-base "/entries/%s/bookmark"))
(defconst elfeed-protocol-miniflux-api-feed-entries (concat elfeed-protocol-miniflux-api-base "/feeds/%s/entries"))
(defconst elfeed-protocol-miniflux-api-feed-mark-read (concat elfeed-protocol-miniflux-api-base "/feeds/%s/mark-all-as-read"))

(defun elfeed-protocol-miniflux-id (url)
  "Get miniflux protocol id with URL."
  (elfeed-protocol-id "miniflux" url))

(defun elfeed-protocol-miniflux--init-headers (url &optional put-json)
  "Get http request headers with authorization and user agent information.
URL should contains user and password fields, if not, will query in the related
feed properties.  Will set content type to json if PUT-JSON is not nil."
  (let* ((proto-id (elfeed-protocol-miniflux-id url))
         (api-token (elfeed-protocol-meta-password proto-id))
         (headers `(("User-Agent" . ,elfeed-user-agent))))
    (when (not api-token)
      (elfeed-log 'error "elfeed-protocol-miniflux: missing API token"))
    (push `("X-Auth-Token" . ,api-token) headers)
    (when put-json
      (push `("Content-Type" . "application/json") headers))
    headers))

(defmacro elfeed-protocol-miniflux-with-fetch (url method data &rest body)
  "Just like `elfeed-with-fetch' but special for Miniflux HTTP request.
URL is the target url, METHOD is the HTTP method (GET, PUT, POST, etc.),
DATA is in string format for PUT/POST requests.
Optional argument BODY is the rest Lisp code after operation finished."
  (declare (indent defun))
  `(let* ((use-curl elfeed-use-curl) ; capture current value in closure
          (headers (elfeed-protocol-miniflux--init-headers ,url ,data))
          (no-auth-url (elfeed-protocol-no-auth-url ,url))
          (cb (lambda (status)
                (if (elfeed-is-status-error status use-curl)
                    (let ((print-escape-newlines t))
                      (elfeed-handle-http-error
                       no-auth-url
                       (if use-curl elfeed-curl-error-message status)))
                  (progn
                    (unless use-curl
                      (elfeed-move-to-first-empty-line)
                      (set-buffer-multibyte t))
                    (when elfeed-protocol-log-trace
                      (elfeed-log 'debug "elfeed-protocol-miniflux: %s" (buffer-string)))
                    (elfeed-protocol-miniflux--parse-result ,@body)
                    (unless use-curl
                      (kill-buffer)))))))
     (if use-curl
         (cond
          (,data
           (elfeed-curl-enqueue no-auth-url cb :headers headers
                                :method ,method :data ,data))
          ((string= ,method "PUT")
           (elfeed-curl-enqueue no-auth-url cb :headers headers
                                :method "PUT"))
          (t
           (elfeed-curl-enqueue no-auth-url cb :headers headers)))
       (cond
        (,data
         (let ((url-request-extra-headers headers)
               (url-request-method ,method)
               (url-request-data ,data))
           (url-retrieve no-auth-url cb () t t)))
        ((string= ,method "PUT")
         (let ((url-request-extra-headers headers)
               (url-request-method "PUT"))
           (url-retrieve no-auth-url cb () t t)))
        (t
         (let ((url-request-extra-headers headers))
           (url-retrieve no-auth-url cb () t t)))))))

(defmacro elfeed-protocol-miniflux--parse-result (&rest body)
  "Parse Miniflux api result JSON buffer.
Will eval rest BODY expressions at end."
  (declare (indent defun))
  `(let* ((content (buffer-string))
          (result (condition-case err
                      (json-read)
                    (error
                     (elfeed-log 'debug "elfeed-protocol-miniflux: JSON parse error: %s, content: %s" err content)
                     nil)))
          (error-message (and result (map-elt result 'error_message))))
     (if error-message
         (elfeed-log 'error "elfeed-protocol-miniflux: %s" error-message)
       ,@body)))

(defmacro elfeed-protocol-miniflux-fetch-prepare (host-url &rest body)
  "Ensure feed list updated before expressions.
HOST-URL is the host name of Miniflux server.  And will eval rest
BODY expressions at end."
  (declare (indent defun))
  `(elfeed-protocol-miniflux--update-feed-list
    ,host-url (lambda () ,@body)))

(defun elfeed-protocol-miniflux--update-categories-list (host-url &optional callback)
  "Update Miniflux server categories list.
HOST-URL is the host name of Miniflux server.  Will call CALLBACK at end."
  (elfeed-log 'debug "elfeed-protocol-miniflux: update category list")
  (elfeed-protocol-miniflux-with-fetch
    (concat host-url elfeed-protocol-miniflux-api-categories)
    "GET"
    nil
    (elfeed-protocol-miniflux--parse-categories host-url result)
    (when callback (funcall callback))))

(defun elfeed-protocol-miniflux--parse-categories (host-url content)
  "Parse the categories JSON buffer and cache the result.
HOST-URL is the host name of Miniflux server.  CONTENT is the result JSON content
by http request.  Return cached `elfeed-protocol-miniflux-categories'."
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (categories (if (vectorp content) content (vector))))
    (puthash proto-id categories elfeed-protocol-miniflux-categories)
    (elfeed-log 'debug "elfeed-protocol-miniflux: found %s categories" (length categories))
    elfeed-protocol-miniflux-categories))

(defun elfeed-protocol-miniflux--get-category-name (host-url category-id)
  "Return category name from HOST-URL for CATEGORY-ID."
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (categories (gethash proto-id elfeed-protocol-miniflux-categories))
         (category (catch 'found
                     (let* ((length (length categories)))
                       (dotimes (i length)
                         (let* ((item (elt categories i))
                                (id (map-elt item 'id))
                                (title (map-elt item 'title)))
                           (when (eq id category-id)
                             (throw 'found title))))))))
    category))

(defun elfeed-protocol-miniflux--parse-feeds (host-url content)
  "Parse the feeds JSON buffer and fill results to db.
HOST-URL is the host name of Miniflux server.  CONTENT is the result JSON
content by http request.  Return `elfeed-protocol-miniflux-feeds'."
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (feeds (if (vectorp content) content (vector))))
    (puthash proto-id feeds elfeed-protocol-miniflux-feeds)
    (cl-loop for feed across feeds do
             (let* ((feed-url (map-elt feed 'feed_url))
                    (feed-id (elfeed-protocol-format-subfeed-id
                              proto-id feed-url))
                    (feed-title (elfeed-cleanup (map-elt feed 'title)))
                    (feed-db (elfeed-db-get-feed feed-id)))
               (setf (elfeed-feed-url feed-db) feed-id
                     (elfeed-feed-title feed-db) feed-title)))
    (elfeed-log 'debug "elfeed-protocol-miniflux: found %s feeds" (length feeds))
    elfeed-protocol-miniflux-feeds))

(defun elfeed-protocol-miniflux--update-feed-list (host-url &optional callback)
  "Update Miniflux feed list.
HOST-URL is the host name of Miniflux server.  Will call CALLBACK at end."
  (elfeed-log 'debug "elfeed-protocol-miniflux: update feed list")
  (let* ((url (concat host-url elfeed-protocol-miniflux-api-feeds))
         (parse-feeds-func (lambda ()
                             (elfeed-protocol-miniflux-with-fetch
                               url "GET" nil
                               (elfeed-protocol-miniflux--parse-feeds host-url result)
                               (when callback (funcall callback))))))
    (if elfeed-protocol-miniflux-fetch-category-as-tag
        (elfeed-protocol-miniflux--update-categories-list host-url parse-feeds-func)
      (funcall parse-feeds-func))))

(defun elfeed-protocol-miniflux--get-subfeed-url (host-url feed-id)
  "Get sub feed url for the Miniflux protocol feed HOST-URL and FEED-ID."
  (let* ((url (catch 'found
                (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
                       (feeds (gethash proto-id elfeed-protocol-miniflux-feeds))
                       (length (length feeds)))
                  (dotimes (i length)
                    (let* ((feed (elt feeds i))
                           (id (map-elt feed 'id))
                           (url (map-elt feed 'feed_url)))
                      (when (eq id feed-id)
                        (throw 'found url))))))))
    (unless url
      (setq url elfeed-protocol-unknown-feed-url)
      (elfeed-log 'warn "elfeed-protocol-miniflux: no subfeed for feed id %s, fallback to unknown feed" feed-id))
    url))

(defun elfeed-protocol-miniflux--get-subfeed-id (host-url feed-url)
  "Get sub feed id for the Miniflux protocol feed HOST-URL and FEED-URL."
  (let* ((id (catch 'found
               (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
                      (feeds (gethash proto-id elfeed-protocol-miniflux-feeds))
                      (length (length feeds)))
                 (dotimes (i length)
                   (let* ((feed (elt feeds i))
                          (id (map-elt feed 'id))
                          (url (map-elt feed 'feed_url)))
                     (when (string= url feed-url)
                       (throw 'found id))))))))
    (unless id
      (elfeed-log 'error "elfeed-protocol-miniflux: no subfeed for feed url %s" feed-url))
    id))

(defun elfeed-protocol-miniflux--get-feed-category-id (host-url feed-id)
  "Get feed category id for the Miniflux protocol feed HOST-URL and FEED-ID."
  (catch 'found
    (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
           (feeds (gethash proto-id elfeed-protocol-miniflux-feeds))
           (length (length feeds)))
      (dotimes (i length)
        (let* ((feed (elt feeds i))
               (id (map-elt feed 'id))
               (category (map-elt feed 'category))
               (category-id (and category (map-elt category 'id))))
          (when (eq id feed-id)
            (throw 'found category-id)))))))

(defun elfeed-protocol-miniflux-entry-p (entry)
  "Check if specific ENTRY is fetched from Miniflux."
  (let* ((proto-id (elfeed-protocol-entry-protocol-id entry))
         (proto-type (when proto-id (elfeed-protocol-type proto-id))))
    (string= proto-type "miniflux")))

(defun elfeed-protocol-miniflux--parse-date (date-string)
  "Parse ISO 8601 DATE-STRING to seconds since epoch as integer.
Returns an integer because `elfeed-new-date-for-entry' only handles
integer timestamps correctly (floats cause it to fall back to current time)."
  (if (and date-string (stringp date-string))
      (condition-case nil
          (truncate (float-time (parse-iso8601-time-string date-string)))
        (error
         ;; Fallback for dates without timezone
         (condition-case nil
             (truncate (float-time (date-to-time date-string)))
           (error (truncate (float-time))))))
    (truncate (float-time))))

(defun elfeed-protocol-miniflux--parse-entries (host-url content &optional mark-state callback)
  "Parse the entries JSON buffer and fill results to elfeed db.
HOST-URL is the host name of Miniflux server.  CONTENT is the result JSON content
by http request.  If MARK-STATE is nil, then just not update the :last-modified,
:first-entry-id and :last-entry-id values.  If CALLBACK is not nil, will call it
with the result entries as argument.  Return parsed entries."
  (if (> (hash-table-count elfeed-protocol-miniflux-feeds) 0)
      (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
             (unread-num 0)
             (starred-num 0)
             (begin-time (time-to-seconds))
             (min-first-entry-id (elfeed-protocol-get-first-entry-id proto-id))
             (max-last-entry-id (elfeed-protocol-get-last-entry-id proto-id))
             (max-last-modified (elfeed-protocol-get-last-modified proto-id))
             ;; entries come in {total: N, entries: [...]} or as array directly
             (items (cond
                     ((and (listp content) (map-elt content 'entries))
                      (map-elt content 'entries))
                     ((vectorp content) content)
                     (t (vector))))
             entries)
        (elfeed-log 'debug "elfeed-protocol-miniflux: parsing entries, first-entry-id: %d last-entry-id: %d last-modified: %d"
                    (elfeed-protocol-get-first-entry-id proto-id)
                    (elfeed-protocol-get-last-entry-id proto-id)
                    (elfeed-protocol-get-last-modified proto-id))
        (setq entries
              (cl-loop for item across items collect
                       (pcase-let* (((map id title author content url hash
                                          ('feed_id feed-id)
                                          ('published_at published-at)
                                          status starred enclosures feed)
                                     item)
                                    (id (if (stringp id) (string-to-number id) id))
                                    (feed-id (if (stringp feed-id) (string-to-number feed-id) feed-id))
                                    (feed-url (elfeed-protocol-miniflux--get-subfeed-url host-url feed-id))
                                    (pub-date (elfeed-protocol-miniflux--parse-date published-at))
                                    (unread (string= status "unread"))
                                    (starred (not (eq starred :json-false)))

                                    ;; Get category from embedded feed object or from cache
                                    (category-obj (or (and feed (map-elt feed 'category))
                                                      nil))
                                    (category-id (or (and category-obj (map-elt category-obj 'id))
                                                     (elfeed-protocol-miniflux--get-feed-category-id host-url feed-id)))
                                    (category-name (when elfeed-protocol-miniflux-fetch-category-as-tag
                                                     (or (and category-obj (map-elt category-obj 'title))
                                                         (elfeed-protocol-miniflux--get-category-name host-url category-id))))

                                    (namespace (elfeed-url-to-namespace feed-url))
                                    (full-id (cons namespace (elfeed-cleanup hash)))
                                    (original (elfeed-db-get-entry full-id))
                                    (original-date (and original
                                                        (elfeed-entry-date original)))
                                    (autotags (elfeed-protocol-feed-autotags proto-id feed-url))
                                    (fixtags (elfeed-normalize-tags autotags elfeed-initial-tags))
                                    (tags (progn
                                            (unless unread
                                              (setq fixtags (delete 'unread fixtags)))
                                            (when starred
                                              (push elfeed-protocol-miniflux-star-tag fixtags))
                                            (when category-name
                                              (push (intern category-name) fixtags))
                                            fixtags))
                                    ;; Parse enclosures
                                    (parsed-enclosures
                                     (when (and enclosures (> (length enclosures) 0))
                                       (cl-loop for enc across enclosures collect
                                                (list (map-elt enc 'url)
                                                      (map-elt enc 'mime_type)
                                                      (or (map-elt enc 'size) 0)))))
                                    (db-entry (elfeed-entry--create
                                               :title (elfeed-cleanup title)
                                               :id full-id
                                               :feed-id (elfeed-protocol-format-subfeed-id
                                                         proto-id feed-url)
                                               :link (elfeed-cleanup url)
                                               :tags tags
                                               :date (elfeed-new-date-for-entry
                                                      original-date pub-date)
                                               :enclosures parsed-enclosures
                                               :content content
                                               :content-type 'html
                                               :meta `(,@(elfeed-protocol-build-meta-author author)
                                                       ,@(list :protocol-id proto-id
                                                               :id id
                                                               :guid-hash hash
                                                               :feed-id feed-id
                                                               :starred starred)))))
                         (when unread (setq unread-num (1+ unread-num)))
                         (when starred (setq starred-num (1+ starred-num)))

                         ;; force override unread and star tags without repeat sync operation
                         (when original
                           (if unread (elfeed-tag-1 original 'unread)
                             (elfeed-untag-1 original 'unread))
                           (if starred (elfeed-tag-1 original elfeed-protocol-miniflux-star-tag)
                             (elfeed-untag-1 original elfeed-protocol-miniflux-star-tag)))

                         ;; calculate the last modified time and first/last entry id
                         (when (or (< min-first-entry-id 0) (< id min-first-entry-id))
                           (setq min-first-entry-id id))
                         (when (or (< max-last-entry-id 0) (> id max-last-entry-id))
                           (setq max-last-entry-id id))
                         (when (> pub-date max-last-modified)
                           (setq max-last-modified pub-date))

                         (dolist (hook elfeed-new-entry-parse-hook)
                           (run-hook-with-args hook :miniflux item db-entry))
                         db-entry)))
        (elfeed-db-add entries)
        (when callback (funcall callback entries))

        ;; update last modified time and first/last entry id
        (when (and mark-state (>= min-first-entry-id 0))
          (elfeed-protocol-set-first-entry-id proto-id min-first-entry-id))
        (when (and mark-state (>= max-last-entry-id 0))
          (elfeed-protocol-set-last-entry-id proto-id max-last-entry-id))
        (when (and mark-state (> max-last-modified 0))
          (elfeed-protocol-set-last-modified proto-id max-last-modified))

        (elfeed-log 'debug "elfeed-protocol-miniflux: parsed %s entries(%d unread, %d starred) with %fs, first-entry-id: %d last-entry-id: %d last-modified: %d"
                    (length entries) unread-num starred-num
                    (- (time-to-seconds) begin-time)
                    (elfeed-protocol-get-first-entry-id proto-id)
                    (elfeed-protocol-get-last-entry-id proto-id)
                    (elfeed-protocol-get-last-modified proto-id))
        entries)
    (progn
      (elfeed-log 'error "elfeed-protocol-miniflux: elfeed-protocol-miniflux-feeds is nil, please call elfeed-protocol-miniflux--update-feed-list first")
      nil)))

(defun elfeed-protocol-miniflux--do-update (host-url action &optional arg callback)
  "Real Miniflux updating operations.
HOST-URL is the host name of Miniflux server, and user field
authentication info is always required so could find the related
protocol feed id correctly.  ACTION could be init, update-since-time,
update-subfeed, update-since-id, or sync-status.  For init, will fetch
unread and starred entries.  For update-subfeed, will fetch entries for
special sub feed, the ARG is the feed id.  For update-since-id, will
fetch all entries after the provided entry id.  For update-since-time
means only update entries since the special modified time, the ARG is
the timestamp.  For sync-status, will fetch entries changed since last
sync time (requires Miniflux 2.0.49+).  If CALLBACK is not nil, will
call it with the result entries as argument."
  (elfeed-log 'debug "elfeed-protocol-miniflux: update entries with action %s, arg %s" action arg)
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (url-base (concat host-url elfeed-protocol-miniflux-api-entries))
         (mark-state t)
         url-opt)
    (unless elfeed--inhibit-update-init-hooks
      (run-hooks 'elfeed-update-init-hooks))
    (cond
     ;; initial sync, fetch unread entries
     ((eq action 'init)
      (elfeed-protocol-set-last-modified proto-id 0)
      (elfeed-protocol-set-first-entry-id proto-id -1)
      (elfeed-protocol-set-last-entry-id proto-id -1)
      (elfeed-protocol-miniflux--set-last-sync-time proto-id 0)
      (elfeed-protocol-clean-pending-ids proto-id)
      (setq url-opt (format "%s?status=unread&limit=%d&order=published_at&direction=desc"
                            url-base elfeed-protocol-miniflux-maxsize)))
     ;; update entries since last modified time
     ((eq action 'update-since-time)
      (let ((timestamp (truncate arg)))
        (setq url-opt (format "%s?limit=%d&order=published_at&direction=asc&published_after=%d"
                              url-base elfeed-protocol-miniflux-maxsize timestamp))))
     ;; update entries since special entry id (using offset)
     ((eq action 'update-since-id)
      (let ((offset (if arg arg (max 0 (elfeed-protocol-get-last-entry-id proto-id)))))
        (setq url-opt (format "%s?limit=%d&order=id&direction=asc&after_entry_id=%d"
                              url-base elfeed-protocol-miniflux-maxsize offset))))
     ;; update entries for special sub feed
     ((eq action 'update-subfeed)
      (setq mark-state nil)
      (setq url-opt (format "%s?status=unread&limit=%d&order=published_at&direction=desc"
                            (concat host-url (format elfeed-protocol-miniflux-api-feed-entries arg))
                            elfeed-protocol-miniflux-maxsize)))
     ;; sync status changes from server (entries changed since last sync)
     ((eq action 'sync-status)
      (setq mark-state nil) ; don't update entry tracking, just sync status
      (let ((last-sync (elfeed-protocol-miniflux--get-last-sync-time proto-id)))
        (setq url-opt (format "%s?changed_after=%d&limit=%d&order=changed_at&direction=asc"
                              url-base last-sync elfeed-protocol-miniflux-maxsize)))))
    (elfeed-protocol-miniflux-with-fetch url-opt "GET" nil
      (elfeed-protocol-miniflux--parse-entries host-url result mark-state callback)
      ;; Update last-sync-time after successful fetch
      (when (or (eq action 'init) (eq action 'sync-status))
        (elfeed-protocol-miniflux--set-last-sync-time proto-id (truncate (float-time))))
      (run-hook-with-args 'elfeed-update-hooks host-url))
    (when (eq action 'init)
      ;; initial sync, also fetch starred entries
      (let ((url-starred (format "%s?starred=true&limit=%d&order=published_at&direction=desc"
                                 url-base elfeed-protocol-miniflux-maxsize)))
        (elfeed-protocol-miniflux-with-fetch url-starred "GET" nil
          ;; do not remember the last-modified for starred entries
          (elfeed-protocol-miniflux--parse-entries host-url result nil callback)
          (run-hook-with-args 'elfeed-update-hooks url-starred))))))

(defun elfeed-protocol-miniflux-reinit (host-url)
  "Retry initial sync operation.
Will fetch all unread and starred entries from Miniflux.
HOST-URL is the host name of Miniflux server.  This may take a long
time, ensure `elfeed-curl-timeout' is big enough."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-miniflux-fetch-prepare
   host-url
   (elfeed-protocol-miniflux--do-update host-url 'init)))

(defun elfeed-protocol-miniflux-update-since-timestamp (host-url &optional timestamp)
  "Update entries since special timestamp.
HOST-URL is the host name of Miniflux server.  TIMESTAMP is the seconds
since 1970-01-01 00:00:00 UTC, the default timestamp just point to 1 hour ago."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (unless timestamp
    (setq timestamp (- (time-to-seconds) (* 1 3600))))
  (elfeed-protocol-miniflux-fetch-prepare
   host-url
   (elfeed-protocol-miniflux--do-update host-url 'update-since-time timestamp)))

(defun elfeed-protocol-miniflux-update-since-id (host-url &optional id)
  "Fetch entries after special id.
HOST-URL is the host name of Miniflux server.  If ID not provided, will
update since the last entry id."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-miniflux-fetch-prepare
   host-url
   (elfeed-protocol-miniflux--do-update host-url 'update-since-id id)))

(defun elfeed-protocol-miniflux-sync-from-server (host-url)
  "Sync read/starred status changes from Miniflux server.
HOST-URL is the host name of Miniflux server.  This fetches entries
that have been modified (e.g., marked read/unread or starred/unstarred)
since the last sync, allowing changes made on other devices or the web
interface to be reflected in elfeed.  Requires Miniflux 2.0.49+."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (elfeed-protocol-miniflux-fetch-prepare
   host-url
   (elfeed-protocol-miniflux--do-update host-url 'sync-status)))

(defun elfeed-protocol-miniflux-update-older (host-url)
  "Fetch older entries which entry id less than :first-entry-id.
HOST-URL is the host name of Miniflux server."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (first-entry-id (elfeed-protocol-get-first-entry-id proto-id))
         (since-id (- first-entry-id elfeed-protocol-miniflux-maxsize)))
    (elfeed-protocol-miniflux-update-since-id host-url since-id)))

(defun elfeed-protocol-miniflux-mark-read (host-url id)
  "Notify special entry as read.
HOST-URL is the host name of Miniflux server.  ID is the target entry id."
  (let* ((url (concat host-url elfeed-protocol-miniflux-api-entries-status))
         (data (json-encode `((entry_ids . ,(vector id))
                              (status . "read")))))
    (elfeed-log 'debug "elfeed-protocol-miniflux: mark read, id: %s" id)
    (elfeed-protocol-miniflux-with-fetch url "PUT" data)))

(defun elfeed-protocol-miniflux-mark-unread (host-url id)
  "Notify special entry as unread.
HOST-URL is the host name of Miniflux server.  ID is the target entry id."
  (let* ((url (concat host-url elfeed-protocol-miniflux-api-entries-status))
         (data (json-encode `((entry_ids . ,(vector id))
                              (status . "unread")))))
    (elfeed-log 'debug "elfeed-protocol-miniflux: mark unread, id: %s" id)
    (elfeed-protocol-miniflux-with-fetch url "PUT" data)))

(defun elfeed-protocol-miniflux-mark-starred (host-url entry)
  "Notify special entry as starred.
HOST-URL is the host name of Miniflux server.  ENTRY is the target entry object.
Since Miniflux uses a toggle endpoint, we check current state first."
  (let* ((id (elfeed-meta entry :id))
         (currently-starred (elfeed-meta entry :starred)))
    ;; Only toggle if not already starred
    (unless currently-starred
      (let ((url (concat host-url (format elfeed-protocol-miniflux-api-entry-toggle-bookmark id))))
        (elfeed-log 'debug "elfeed-protocol-miniflux: toggle starred on, id: %s" id)
        (elfeed-protocol-miniflux-with-fetch url "PUT" nil
          ;; Update the cached starred state
          (setf (elfeed-meta entry :starred) t))))))

(defun elfeed-protocol-miniflux-mark-unstarred (host-url entry)
  "Notify special entry as unstarred.
HOST-URL is the host name of Miniflux server.  ENTRY is the target entry object.
Since Miniflux uses a toggle endpoint, we check current state first."
  (let* ((id (elfeed-meta entry :id))
         (currently-starred (elfeed-meta entry :starred)))
    ;; Only toggle if currently starred
    (when currently-starred
      (let ((url (concat host-url (format elfeed-protocol-miniflux-api-entry-toggle-bookmark id))))
        (elfeed-log 'debug "elfeed-protocol-miniflux: toggle starred off, id: %s" id)
        (elfeed-protocol-miniflux-with-fetch url "PUT" nil
          ;; Update the cached starred state
          (setf (elfeed-meta entry :starred) nil))))))

(defun elfeed-protocol-miniflux-mark-read-multi (host-url ids)
  "Notify multiple entries to be read.
HOST-URL is the host name of Miniflux server.  IDS is the target entry ids."
  (let* ((url (concat host-url elfeed-protocol-miniflux-api-entries-status))
         (data (json-encode `((entry_ids . ,(vconcat ids))
                              (status . "read")))))
    (when ids
      (elfeed-log 'debug "elfeed-protocol-miniflux: mark read multi, ids: %s" ids)
      (elfeed-protocol-miniflux-with-fetch url "PUT" data))))

(defun elfeed-protocol-miniflux-mark-unread-multi (host-url ids)
  "Notify multiple entries to be unread.
HOST-URL is the host name of Miniflux server.  IDS is the target entry ids."
  (let* ((url (concat host-url elfeed-protocol-miniflux-api-entries-status))
         (data (json-encode `((entry_ids . ,(vconcat ids))
                              (status . "unread")))))
    (when ids
      (elfeed-log 'debug "elfeed-protocol-miniflux: mark unread multi, ids: %s" ids)
      (elfeed-protocol-miniflux-with-fetch url "PUT" data))))

(defun elfeed-protocol-miniflux-sync-pending-ids (host-url)
  "Sync pending read/unread/starred/unstarred entry states to Miniflux server.
HOST-URL is the host name of Miniflux server."
  (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
         (pending-read-ids (elfeed-protocol-get-pending-ids proto-id :pending-read))
         (pending-unread-ids (elfeed-protocol-get-pending-ids proto-id :pending-unread))
         (pending-starred (elfeed-protocol-get-pending-ids proto-id :pending-starred))
         (pending-unstarred (elfeed-protocol-get-pending-ids proto-id :pending-unstarred)))
    (when pending-read-ids
      (elfeed-protocol-miniflux-mark-read-multi host-url pending-read-ids))
    (when pending-unread-ids
      (elfeed-protocol-miniflux-mark-unread-multi host-url pending-unread-ids))
    ;; For starred/unstarred, we stored the full entry objects
    (dolist (entry pending-starred)
      (when (elfeed-protocol-miniflux-entry-p entry)
        (elfeed-protocol-miniflux-mark-starred host-url entry)))
    (dolist (entry pending-unstarred)
      (when (elfeed-protocol-miniflux-entry-p entry)
        (elfeed-protocol-miniflux-mark-unstarred host-url entry)))
    (elfeed-protocol-clean-pending-ids proto-id)))

(defun elfeed-protocol-miniflux-append-pending-ids (host-url entries tag action)
  "Sync unread and starred tag states to Miniflux server.
HOST-URL is the host name of Miniflux server.  ENTRIES is the target
entry objects.  TAG is the action tag, for example unread and
`elfeed-protocol-miniflux-star-tag', ACTION could be add or remove."
  (when entries
    (let* ((proto-id (elfeed-protocol-miniflux-id host-url))
           (ids (cl-loop for entry in entries
                         when (elfeed-protocol-miniflux-entry-p entry)
                         collect (elfeed-meta entry :id))))
      (cond
       ((eq action 'add)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-unread ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-read ids))
         ((eq tag elfeed-protocol-miniflux-star-tag)
          ;; For starring, store entry objects so we can check :starred state
          (let ((star-entries (cl-loop for entry in entries
                                        when (elfeed-protocol-miniflux-entry-p entry)
                                        collect entry)))
            (elfeed-protocol-append-pending-ids proto-id :pending-starred star-entries)
            (elfeed-protocol-remove-pending-ids proto-id :pending-unstarred star-entries)))))
       ((eq action 'remove)
        (cond
         ((eq tag 'unread)
          (elfeed-protocol-append-pending-ids proto-id :pending-read ids)
          (elfeed-protocol-remove-pending-ids proto-id :pending-unread ids))
         ((eq tag elfeed-protocol-miniflux-star-tag)
          (let ((star-entries (cl-loop for entry in entries
                                        when (elfeed-protocol-miniflux-entry-p entry)
                                        collect entry)))
            (elfeed-protocol-append-pending-ids proto-id :pending-unstarred star-entries)
            (elfeed-protocol-remove-pending-ids proto-id :pending-starred star-entries)))))))))

(defun elfeed-protocol-miniflux-pre-tag (host-url entries &rest tags)
  "Sync unread, starred states before tags added.
HOST-URL is the host name of Miniflux server.  ENTRIES is the target
entry objects.  TAGS is the tags are adding now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      unless (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-miniflux-append-pending-ids host-url entries-modified tag 'add)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-miniflux-sync-pending-ids host-url)))

(defun elfeed-protocol-miniflux-pre-untag (host-url entries &rest tags)
  "Sync unread, starred states before tags removed.
HOST-URL is the host name of Miniflux server.  ENTRIES is the target entry
objects.  TAGS is the tags are removing now."
  (dolist (tag tags)
    (let* ((entries-modified (cl-loop for entry in entries
                                      when (elfeed-tagged-p tag entry)
                                      collect entry)))
      (elfeed-protocol-miniflux-append-pending-ids host-url entries-modified tag 'remove)))
  (unless elfeed-protocol-lazy-sync
    (elfeed-protocol-miniflux-sync-pending-ids host-url)))

(defun elfeed-protocol-miniflux-update-subfeed (host-url feed-url &optional callback)
  "Update sub feed in Miniflux.
HOST-URL is the host name of Miniflux server, FEED-URL is the target
sub feed url, if CALLBACK is not nil will call it with the result
entries as argument."
  (interactive)
  (let* ((feed-id (elfeed-protocol-miniflux--get-subfeed-id host-url feed-url)))
    (when feed-id
      (elfeed-protocol-miniflux--do-update host-url 'update-subfeed feed-id callback))))

(defun elfeed-protocol-miniflux-update (host-or-subfeed-url &optional callback)
  "Miniflux protocol updater.
HOST-OR-SUBFEED-URL could be the host name of Miniflux server, and
user field is optional, for example \"https://myhost.com\".  And
HOST-OR-SUBFEED-URL also could be the sub feed url, too, for example
\"https://myhost.com::https://subfeed.com/rss\".  If first time run,
it will initial sync operation, or will only fetch the updated entries
since last modified.  If `elfeed-protocol-miniflux-sync-status-on-update'
is non-nil, will also sync read/starred status changes from server.
If CALLBACK is not nil will call it with the result entries as argument."
  (interactive (list (elfeed-protocol-url
                      (completing-read "Protocol Feed: " (elfeed-protocol-feed-list)))))
  (let* ((host-url (elfeed-protocol-host-url host-or-subfeed-url))
         (subfeed-url (elfeed-protocol-subfeed-url host-or-subfeed-url))
         (proto-id (elfeed-protocol-miniflux-id host-url)))
    (elfeed-protocol-add-unknown-feed proto-id) ; add unknown feed for fallback
    (elfeed-protocol-miniflux-sync-pending-ids host-url)
    (if subfeed-url
        (elfeed-protocol-miniflux-update-subfeed host-url subfeed-url callback)
      (let* ((last-modified (elfeed-protocol-get-last-modified proto-id))
             (last-entry-id (elfeed-protocol-get-last-entry-id proto-id)))
        (elfeed-protocol-miniflux-fetch-prepare
         host-url
         (progn
           ;; Sync status changes from server if enabled and not first run
           (when (and elfeed-protocol-miniflux-sync-status-on-update
                      (> last-modified 0))
             (elfeed-protocol-miniflux--do-update host-url 'sync-status))
           ;; Then fetch new entries
           (if (> last-modified 0)
               (if elfeed-protocol-miniflux-update-with-modified-time
                   (elfeed-protocol-miniflux--do-update host-url 'update-since-time last-modified callback)
                 (elfeed-protocol-miniflux--do-update host-url 'update-since-id last-entry-id callback))
             (elfeed-protocol-miniflux--do-update host-url 'init nil callback))))))))

(provide 'elfeed-protocol-miniflux)

;;; elfeed-protocol-miniflux.el ends here
