;;; helm-dictionary.el --- lookup words with `dictionary.el' via `helm'
;;
;;; Commentary:
;;
;;; Code:

(unless (require 'dictionary nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `dictionary'"))

(unless (require 'helm nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `helm'"))

(defun helm-dictionary-match-words (word)
  "Produce the list of matching words for WORD according to dictionary server queries."
  (let ((results
         (mapcar
          (lambda (entry)
            (cons (concat (car entry) "\t(" (cdr entry) ")") entry))
          (append (dictionary-do-matching word "*" "." 'helm-get-dict-match-sources)
                  (dictionary-do-search word "*" 'helm-get-dict-search-sources)))))
    (cl-sort results
             (lambda (a b) (or (< (length a) (length b)) (and (= (length a) (length b)) (string-lessp a b))))
             :key '(lambda (r) (car (cdr r))))))

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
                  (setq result (cons
                                (cons word dictionary)
                                result))))))
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

(defun helm-dictionary-lookup (hdl-word)
  "Lookup a HDL-WORD's meaning or synonyms using `helm' and `dictionary'."
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
                   :candidates '(lambda () (helm-dictionary-match-words hdl-word))
                   :action 'helm-dictionary-lookup-word
                   :candidate-number-limit 1000)
        :buffer "*Dictionary*"
        :input hdl-word))

  (provide 'helm-dictionary)

;;; helm-dictionary.el ends here
