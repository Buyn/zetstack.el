;;; zetstack.el --- A lean, double-linked Zettelkasten stack on Org properties -*- lexical-binding: t; -*-

;; Author: Max & Acid Burn
;; Version: 3.0.0
;; Keywords: outlines, hypermedia, zettelkasten, stack

;;; Commentary:
;; A ruthless, unbloated Zettelkasten stack implementation using Org mode
;; property drawers (=:PREV:=, =:NEXT:=) and immutable timestamp IDs.
;; Features dynamic link parsing, auto-renaming, and Ivy integration.

;;; Code:

(require 'org)

(defgroup zetstack nil
  "A double-linked Zettelkasten stack built on Org mode."
  :group 'org
  :prefix "zetstack-")

(defcustom zetstack-directory "~/Dropbox/orgs/zettelkasten/"
  "Directory where Zetstack files are stored."
  :type 'directory
  :group 'zetstack)

(defun zetstack--current-id ()
  "Extract the 15-digit timestamp ID from the current buffer's filename,
or fall back to the :ID: property in the Org property drawer."
  (or
   ;; Vector 1: Try extracting from filename
   (let ((filepath (buffer-file-name)))
     (when filepath
       (let ((basename (file-name-nondirectory filepath)))
         (when (string-match "\\([0-9]\\{15\\}\\)" basename)
           (match-string 1 basename)))))
   ;; Vector 2: Fallback to Org property drawer if filename isn't set yet
   (org-entry-get nil "ID" t)))

(defun zetstack--find-file-by-id (id)
  "Find and return the file path for a given ID in =zetstack-directory'."
  (let* ((files (directory-files zetstack-directory t (concat "^" id)))
         (target (car files)))
        (if (and target (file-exists-p target))
            target
            nil)))

(defun zetstack--get-clean-title (filepath)
  "Extract the pure title by stripping the 15-digit ID, double dash, and extension using safe string slicing."
  (let* ((filename (file-name-nondirectory filepath))
         (no-ext (file-name-sans-extension filename)))
    (if (and (>= (length no-ext) 17)
             (string= "--" (substring no-ext 15 17)))
        (substring no-ext 17)
      no-ext)))

(defun zetstack--files-alist ()
  "Return an alist of (Display Name . FilePath) for all valid Zetstack notes,
ignoring autosaves, lock files, and non-org junk."
  (delq nil
        (mapcar (lambda (f)
                  (let ((base (file-name-nondirectory f)))
                    (unless (or (string-prefix-p ".#" base)
                                (string-suffix-p "~" base))
                      (cons (zetstack--get-clean-title f) f))))
                (directory-files zetstack-directory t "\\.org$"))))

(defun zetstack--get-first-headline ()
  "Extract the text of the first real Org headline, skipping property drawers."
  (save-excursion
    (goto-char (point-min))
    ;; If we are at a property drawer, jump past it to the :END:
    (when (re-search-forward "^:END:" nil t)
      (forward-line 1))
    ;; Now search for the first headline starting with one or more asterisks
    (if (re-search-forward "^\\*+\\s-+\\(.+\\)" nil t)
        (match-string-no-properties 1)
      ;; Fallback if no headline exists yet
      "untitled")))

(defun zetstack--extract-id-from-file (filepath)
  "Safely extract the 15-digit ID from a Zetstack file—checking filename first, 
then falling back to reading the :ID: property from the file buffer directly."
  (let* ((basename (file-name-nondirectory filepath)))
    (cond
     ;; 1. Try extracting from filename prefix
     ((string-match "^\\([0-9]\\{15\\}\\)" basename)
      (match-string 1 basename))
     ;; 2. Fallback: Open buffer invisibly and grab :ID: property from root
     (t
      (with-current-buffer (find-file-noselect filepath)
        (save-excursion
          (goto-char (point-min))
          (org-entry-get nil "ID" t)))))))

;;; Core New Node Factory (Strict Root-Level Property Drawer)
(defun zetstack--create-new (timestamp title &optional extra-properties)
  "Create and initialize a raw new Zetstack file on disk with TIMESTAMP, TITLE,
and an optional alist of EXTRA-PROPERTIES to inject directly into the root property drawer."
  (let* ((dir zetstack-directory)
         (clean-title-str (replace-regexp-in-string "[^a-zA-Z0-9-_]+" "-" title))
         (filename (expand-file-name (concat timestamp "--" (downcase clean-title-str) ".org") dir))
         (buf (create-file-buffer filename)))
    (with-current-buffer buf
      (setq buffer-file-name filename)
      (org-mode)
      ;; Force insertion at the absolute top of the file: Root Property Drawer
      (goto-char (point-min))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" timestamp))
      ;; Insert any extra root properties (like PREV or NEXT)
      (when extra-properties
        (dolist (prop extra-properties)
          (insert (format ":%s: %s\n" (car prop) (cdr prop)))))
      (insert ":END:\n\n")
      ;; Now write the first headline cleanly without any properties attached to it
      (insert (format "* %s\n" title))
      (goto-char (point-max))
      (save-buffer))
    buf))

;;; Core Relative Factory Helper (Chaining Atomic Primitives)
(defun zetstack--create-relative (relation-type)
  "Spawn and link a relative Zettel node using atomic primitives.
RELATION-TYPE can be 'next or 'prev."
  (let* ((current-buf (current-buffer))
         (current-id (with-current-buffer current-buf
                       (save-excursion
                         (goto-char (point-min))
                         (zetstack--current-id))))
         (timestamp (format-time-string "%Y%m%dT%H%M%S"))
         (is-next (eq relation-type 'next))
         (default-title (if is-next "Untitled Next" "Untitled Prev"))
         (new-buf (zetstack--create-new timestamp default-title nil)))

    ;; 1. If current node lacks an ID, mint one for it now
    (unless current-id
      (with-current-buffer current-buf
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:PROPERTIES:" nil t)
              (org-entry-put nil "ID" timestamp)
            (progn
              (goto-char (point-min))
              (insert ":PROPERTIES:\n:ID: " timestamp "\n:END:\n\n")))
          (setq current-id timestamp))))

    ;; 2. Switch to the newly created buffer
    (switch-to-buffer new-buf)

    ;; 3. Surgically insert and wire it relative to the previous current node's ID
    (zetstack--insert-current-at-id relation-type current-id)

    (message "Spawned and wired relative %s Zetstack node: %s" relation-type timestamp)))

;;;###autoload
(defun zetstack--goto-relative (prop-name)
  "Generic helper to jump to a relative Zettel node specified by PROP-NAME ('PREV' or 'NEXT').
If the target node does not exist on disk, prompt to create it."
  (let* ((target-id (org-entry-get nil prop-name t))
         (current-id (zetstack--current-id))
         ;; Determine clean label and inverse property on the fly
         (is-next (string= prop-name "NEXT"))
         (label (if is-next "Next" "Previous"))
         (inverse-prop (if is-next "PREV" "NEXT")))
    (if target-id
        (let ((target-file (zetstack--find-file-by-id target-id)))
          (zetstack-rename-and-save)
          (if target-file
              (find-file target-file)
            ;; Missing file: prompt to create the node
            (when (y-or-n-p (format "%s node %s does not exist on disk. Create it? " 
                                    label target-id))
              (let* ((new-file (expand-file-name (concat target-id "--untitled.org") zetstack-directory))
                     (new-buf (find-file new-file)))
                (with-current-buffer new-buf
                  (org-mode)
                  (org-entry-put nil "ID" target-id)
                  (when current-id
                    (org-entry-put nil inverse-prop current-id))
                  (insert (format ":PROPERTIES:\n:ID: %s\n%s:END:\n\n* Untitled %s Node\n"
                                  target-id
                                  (if current-id (format ":%s: %s\n" inverse-prop current-id) "")
                                  label))
                  (save-buffer))))))
      (message "No :%s: pointer found in property drawer." prop-name))))

(defun zetstack--remove-current ()
  "Heal the doubly-linked list pointers around the current node 
without deleting the file itself. Connects PREV and NEXT nodes if both exist,
or clears pointers if it's an edge node."
  (let* ((current-id (zetstack--current-id))
         (prev-id (org-entry-get nil "PREV" t))
         (next-id (org-entry-get nil "NEXT" t))
         (dir zetstack-directory))
    ;; 1. If PREV exists, update its NEXT pointer
    (when prev-id
      (let ((prev-file (zetstack--find-file-by-id prev-id)))
        (when prev-file
          (message "prev-id : %s" prev-file)
          (with-current-buffer (find-file-noselect prev-file)
            (save-excursion
              (goto-char (point-min))
              (if next-id
                  (org-entry-put nil "NEXT" next-id)
                (org-entry-delete nil "NEXT"))
              (save-buffer))))))
    ;; 2. If NEXT exists, update its PREV pointer
    (when next-id
      (let ((next-file (zetstack--find-file-by-id next-id)))
        (when next-file
          (message "next-file : %s" next-file)
          (with-current-buffer (find-file-noselect next-file)
            (save-excursion
              (goto-char (point-min))
              (if prev-id
                  (org-entry-put nil "PREV" prev-id)
                (org-entry-delete nil "PREV"))
              (save-buffer))))))
    (message "Healed Zettel heap around ID: %s" current-id)))

;;; Core Splicing Fix: Symmetrical Bi-Directional Pointer Re-linking
(defun zetstack--insert-current-at-id (relation-type target-id)
  "Surgically remove current node from its old neighborhood, 
then insert it relative (RELATION-TYPE is 'next or 'prev) to TARGET-ID,
correctly severing and updating all adjacent pointer drawers atomically."
  (let* ((current-id (zetstack--current-id))
         (current-file (buffer-file-name))
         (target-file (zetstack--find-file-by-id target-id))
         (is-next (eq relation-type 'next)))
    
    (unless target-file
      (user-error "Target node ID %s does not exist on disk!" target-id))
    (when (string= current-id target-id)
      (user-error "Cannot splice a node into itself, Max!"))

    ;; 1. Heal/Remove current node from its old neighborhood first
    (zetstack--remove-current)

    ;; 2. Read target's current state and its existing adjacent neighbor
    (let* ((target-old-adj-id (with-current-buffer (find-file-noselect target-file)
                                (org-entry-get nil (if is-next "NEXT" "PREV") t))))

      ;; 3. Set Current node's PREV and NEXT properties symmetrically
      (save-excursion
        (goto-char (point-min))
        (if is-next
            (progn
              (org-entry-put nil "PREV" target-id)
              (if target-old-adj-id
                  (org-entry-put nil "NEXT" target-old-adj-id)
                (org-entry-delete nil "NEXT")))
          (progn
            (org-entry-put nil "NEXT" target-id)
            (if target-old-adj-id
                (org-entry-put nil "PREV" target-old-adj-id)
              (org-entry-delete nil "PREV"))))
        (save-buffer))

      ;; 4. Update Target node's property to point to Current node
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          (goto-char (point-min))
          (if is-next
              (org-entry-put nil "NEXT" current-id)
            (org-entry-put nil "PREV" current-id))
          (save-buffer)))

      ;; 5. Update the old adjacent neighbor's opposing pointer to point to Current node
      (when target-old-adj-id
        (let ((adj-file (zetstack--find-file-by-id target-old-adj-id)))
          (when adj-file
            (with-current-buffer (find-file-noselect adj-file)
              (save-excursion
                (goto-char (point-min))
                (if is-next
                    (org-entry-put nil "PREV" current-id)
                  (org-entry-put nil "NEXT" current-id))
                (save-buffer)))))))

    (message "Successfully spliced node [%s] %s target [%s] with full boundary linkage." 
             current-id (if is-next "after" "before") target-id)))

;;; Core Factory Fix for Inserting New Node Relative to Target Name
(defun zetstack--insert-new-at-name (relation-type)
  "Interactively prompt for an existing Zettel via Ivy, create a brand new 
Zettel node using =zetstack--create-new=, and surgically splice it 
(RELATION-TYPE can be 'next or 'prev) relative to the selected target."
  (let* ((files (zetstack--files-alist)))
    (if (null files)
        (message "No Zetstack files found in directory!")
      (let* ((is-next (eq relation-type 'next))
             (prompt-label (if is-next "Insert NEW AFTER Zettel: " "Insert NEW BEFORE Zettel: "))
             (selected (completing-read prompt-label files nil t))
             (filepath (cdr (assoc selected files))))
        (when filepath
          (let* ((target-id (zetstack--extract-id-from-file filepath))
                 (timestamp (format-time-string "%Y%m%dT%H%M%S"))
                 (default-title (if is-next "Untitled Next" "Untitled Prev"))
                 ;; 1. Create the new buffer cleanly with empty properties first
                 (new-buf (zetstack--create-new timestamp default-title nil)))
            
            ;; 2. Switch to the new buffer so it becomes current
            (switch-to-buffer new-buf)

            ;; 3. Now invoke the robust topological spicer!
            (zetstack--insert-current-at-id relation-type target-id)
            
            (message "Successfully spawned, wired, and spliced new node [%s] %s [%s]." 
                     timestamp (if is-next "after" "before") target-id)))))))

;;;###autoload
(defun zetstack-create-next ()
  "Create a new Zettel node linked as a next of the current stack node."
  (interactive)
  (zetstack--create-relative 'next))

;;;###autoload
(defun zetstack-create-prev ()
  "Create a new Zettel node linked as a prev of the current stack node."
  (interactive)
  (zetstack--create-relative 'prev))

;;;###autoload
(defun zetstack-insert-new-next-to-name ()
  "Interactively select a target Zettel via Ivy, create a brand new Zettel, 
and insert/splice it immediately AFTER the target."
  (interactive)
  (zetstack--insert-new-at-name 'next))

;;;###autoload
(defun zetstack-insert-new-prev-to-name ()
  "Interactively select a target Zettel via Ivy, create a brand new Zettel, 
and insert/splice it immediately BEFORE the target."
  (interactive)
  (zetstack--insert-new-at-name 'prev))

;;;###autoload
(defun zetstack-insert-current-at-name-next ()
  "Select a target Zettel via Ivy (clean titles), and insert the current 
node immediately AFTER it."
  (interactive)
  (let* ((files (zetstack--files-alist)))
    (if (null files)
        (message "No Zetstack files found!")
      (let* ((selected (completing-read "Insert AFTER Zettel: " files nil t))
             (filepath (cdr (assoc selected files))))
        (when filepath
          (let ((target-id (zetstack--extract-id-from-file filepath)))
            (if target-id
                (zetstack--insert-current-at-id 'next target-id)
              (message "Error: Could not extract ID from %s" filepath))))))))

;;;###autoload
(defun zetstack-insert-current-at-name-prev ()
  "Select a target Zettel via Ivy (clean titles), and insert the current 
node immediately BEFORE it."
  (interactive)
  (let* ((files (zetstack--files-alist)))
    (if (null files)
        (message "No Zetstack files found!")
      (let* ((selected (completing-read "Insert BEFORE Zettel: " files nil t))
             (filepath (cdr (assoc selected files))))
        (when filepath
          (let ((target-id (zetstack--extract-id-from-file filepath)))
            (if target-id
                (zetstack--insert-current-at-id 'prev target-id)
              (message "Error: Could not extract ID from %s" filepath))))))))

;;;###autoload
(defun zetstack-rename-and-save ()
  "Check if filename matches first real headline title; rename and save if mismatch."
  (interactive)
  (let ((filepath (buffer-file-name))
        (id (zetstack--current-id)))
    (unless filepath
      (user-error "Buffer is not visiting a file yet! Save or create the node first."))
    (unless id
      (user-error "Current buffer does not have a valid 15-digit Zetstack ID!"))
    (let* ((dir (file-name-directory filepath))
           (headline-text (zetstack--get-first-headline))
           (clean-title (replace-regexp-in-string 
                         "[^a-zA-Z0-9-_]+" "-" 
                         (string-trim headline-text)))
           (expected-filename (concat dir id "--" (downcase clean-title) ".org")))
      (save-buffer)
      (unless (string= (expand-file-name filepath) (expand-file-name expected-filename))
        (rename-file filepath expected-filename)
        (set-visited-file-name expected-filename t t)
        (message "Zetstack auto-renamed to: %s" expected-filename)))))

;;;###autoload
(defun zetstack-goto-prev ()
  "Jump to the previous Zetstack node via the :PREV: property."
  (interactive)
  (zetstack--goto-relative "PREV"))

;;;###autoload
(defun zetstack-goto-next ()
  "Jump to the next Zetstack node via the :NEXT: property."
  (interactive)
  (zetstack--goto-relative "NEXT"))

;;;###autoload
(defun zetstack-open-by-name ()
  "Open a Zetstack node using Ivy, displaying clean titles without IDs."
  (interactive)
  (let* ((files (zetstack--files-alist)))
    (if (null files)
        (message "No valid Zetstack files found in directory!")
      (let* ((selected (completing-read "Open Zetstack: " files nil t))
             (filepath (cdr (assoc selected files))))
        (when filepath
          (find-file filepath))))))

;;;###autoload
(defun zetstack-add-link ()
  "Select an existing Zetstack via Ivy and insert an ID link at point."
  (interactive)
  (let* ((files (zetstack--files-alist)))
    (if (null files)
        (message "No Zetstack files found in directory!")
      (let* ((selected (completing-read "Link Zetstack: " files nil t))
             (filepath (cdr (assoc selected files))))
        (when filepath
          (let ((id (zetstack--extract-id-from-file filepath)))
            (if id
                (insert (format "[[zlink:%s][%s]]" id selected))
              (message "Error: Could not resolve a valid 15-digit ID for file: %s" filepath))))))))

;;;###autoload
(defun zetstack-add-stub-link (&optional description)
  "Insert a zlink stub. If DESCRIPTION is provided (e.g. from Elisp code), 
use it directly; otherwise, interactively prompt the user for a description.
Automatically mints a 15-digit timestamp ID and formats a clean zlink."
  (interactive)
  (let* ((desc (or description (read-string "Enter Stub Description: ")))
         (timestamp (format-time-string "%Y%m%dT%H%M%S"))
         (clean-desc (replace-regexp-in-string "[^a-zA-Z0-9-_]+" "-" (string-trim desc))))
    (insert (format "[[zlink:%s][Stub: %s]]" timestamp desc))
    (message "Minted gateway stub ID [%s] with description: %s" timestamp desc)))

;;;###autoload
(defun zetstack-open-link-at-point (id)
  "Custom link opener handling zlinks by ID prefix.
Scans the directory for the full filename starting with ID, and opens it.
If missing, extracts the link's description text directly from point."
  (let* ((matching-files (directory-files zetstack-directory t (concat "^" id)))
         (target-file (car matching-files)))
    (if (and target-file (file-exists-p target-file))
        (find-file target-file)
      ;; Extract description directly from the current line's zlink syntax
      (let* ((line-text (thing-at-point 'line t))
             (title (if (and line-text (string-match (format "\\[\\[zlink:%s\\]\\[\\([^]]+\\)\\]\\]" id) line-text))
                        (match-string 1 line-text)
                      "Untitled Stub")))
        (when (y-or-n-p (format "Stub node [%s] ('%s') has no file on disk. Create it? " id title))
          (let* ((new-buf (zetstack--create-new id title)))
            (switch-to-buffer new-buf)
            (message "Minted stub node [%s] with title: %s" id title)))))))

;;;###autoload
(defun zetstack-remove-current-file ()
  "Prompt user, heal the Zettel stack pointers, delete the underlying file,
and kill the current buffer."
  (interactive)
  (let* ((filepath (buffer-file-name))
         (current-id (zetstack--current-id)))
    (unless filepath
      (user-error "Current buffer is not visiting a file!"))
    (when (y-or-n-p (format "Permanently delete current Zettel [%s] and heal stack pointers? " current-id))
      ;; 1. Heal the pointer topology first while we are still in the buffer
      (zetstack--remove-current)
      ;; 2. Kill the buffer
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      ;; 3. Delete the file from disk
      (when (file-exists-p filepath)
        (delete-file filepath))
      (message "Zetstack node %s successfully pruned and destroyed." current-id))))

(org-link-set-parameters "zlink" :follow #'zetstack-open-link-at-point)

(provide 'zetstack)
;;; zetstack.el ends here
