;;; helm-dictionary.el --- lookup words with `dictionary.el' via `helm'
;;
;;; Commentary:
;;
;;; Code:

(unless (require 'dictionary nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `dictionary'"))

(unless (require 'helm nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `helm'"))

(defface helm-dictionary-name
  '((((background dark)) :foreground "RosyBrown")
    (((background light)) :foreground "SlateGray"))
  "Face used for dictionary name."
  :group 'helm-buffers-faces)

(defvar helm-dictionary-column-distance 50)


(defun helm-dictionary--display-entry (word dict)
  "Return string composed of WORD and DICT suitable for display in `helm' buffer."
  (let ((disp-word (if (> (length word) (- helm-dictionary-column-distance 5))
                       (concat
                        (string-trim-right
                         (truncate-string-to-width
                          word
                          (- helm-dictionary-column-distance 8)))
                        "...")
                     word))
        (disp-dict (concat "(from "
                           (propertize dict 'face 'helm-dictionary-name)
                           ")")))

    (concat disp-word
            (apply 'concat
                   (make-list
                    (- helm-dictionary-column-distance
                       (length disp-word))
                    " "))
            disp-dict)))

(defun helm-dictionary--sort-results (results)
  "Return sorted RESULTS.  Helm will not sort results by default."
  (cl-sort results
           (lambda (a b) (or (< (length a) (length b))
                        (and (= (length a) (length b)) (string-lessp a b))))
           :key '(lambda (r) (car (cdr r)))))

(defun helm-dictionary-match-words (query)
  "Produce the list of matching words for QUERY according to dictionary server queries."
  (let ((results
         (mapcar
          (lambda (entry)
            (let ((word (car entry))
                  (dict (cdr entry)))
              (cons (helm-dictionary--display-entry word dict) entry)))
          (append
           (dictionary-do-matching
            query "*" "." 'helm-get-dict-match-sources)
           (ignore-errors
             (dictionary-do-search query "*" 'helm-get-dict-search-sources))))))
    (helm-dictionary--sort-results results)
    ))

(defun helm-dictionary-search-words ()
  "Produce the list of matching words for according to dictionary server queries."
  (let ((query helm-input))
    (if (> (length query) 2)
        (let* ((results
                (mapcar
                 (lambda (entry)
                   (let ((word (car entry))
                         (dict (cdr entry)))
                     (cons (helm-dictionary--display-entry word dict)
                           entry)))
                 (dictionary-do-matching
                  query "*" "substring" 'helm-get-dict-match-sources))))
          (helm-dictionary--sort-results results)
          )
      nil)))

(defun helm-dictionary-lookup-word (entry)
  "Lookup the definition of ENTRY using the ENTRY's dictionary."
  (dictionary-new-search entry))

(defun helm-get-dict-match-sources (reply)
  "Get the dictionary results for REPLY."
  (let ((word-list (dictionary-simple-split-string (dictionary-read-answer) "\n+"))
        (result nil))
    (mapc (lambda (item)
            (let* ((entry-list (dictionary-split-string item))
                   (dictionary (car entry-list))
                   (word (cadr entry-list))
                   (hash (assoc dictionary result)))
              (when dictionary
                (if hash
                    (setcdr hash (cons word (cdr hash)))
                  (add-to-list 'result  (cons word dictionary))))))
          word-list)
    result))

(defun helm-get-dict-search-sources (reply)
  "Get the dictionary results for REPLY."
    (let (word-entries)
      (setq reply (dictionary-read-reply-and-split))
      (while (dictionary-check-reply reply 151)
        (let* ((reply-list (dictionary-reply-list reply))
               (dictionary (nth 2 reply-list))
               (description (nth 3 reply-list))
               (word (nth 1 reply-list)))
          (add-to-list 'word-entries (cons word dictionary))
          (dictionary-read-answer)
          (setq reply (dictionary-read-reply-and-split))))
      word-entries
      ))

(defun helm-dictionary-explore (hde-word)
  "Explore words similar to HDE-WORD via `helm' and `dictionary'."
  (interactive (list (let ((query (if (use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning)
                                      (region-end))
                                   (substring-no-properties (or (word-at-point) "")))))
                       (read-from-minibuffer
                        "Match Word: "
                        ;; remove the 's' at the end of query if it's there
                        (if (equal (car (last (split-string query "" t))) "s")
                            (mapconcat 'identity (-butlast (split-string query "" t)) "")
                          query)))))
  (helm :sources (helm-build-sync-source "Helm Dictionary"
                   :candidates '(lambda () (helm-dictionary-match-words hde-word))
                   :action 'helm-dictionary-lookup-word
                   :candidate-number-limit 2500)
        :buffer "*Explore Dictionary*"))

(defun helm-dictionary-lookup ()
  "Search for definintions using an assortment of dictionaries via `helm' and `dictionary'."
  (interactive)
  (helm :sources (helm-build-async-source "Helm Dictionary"
                   :candidates-process 'helm-dictionary-search-words)
                   :action 'helm-dictionary-lookup-word
                   :candidate-number-limit 2500
                   :fuzzy-match t)
        :buffer "*Search Dictionary*")

  (provide 'helm-dictionary)

;;; helm-dictionary.el ends here
