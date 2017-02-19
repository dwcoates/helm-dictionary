;;; helm-dictionary.el --- lookup words with `dictionary.el' via `helm'
;;
;;; Commentary:
;;
;;; Code:

(unless (require 'dictionary nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `dictionary'"))

(unless (require 'helm nil t)
  (error "Failure: Could not load `helm-dictionary' dependency `helm'"))

(defun helm-dictionary-match-words ()
  "Produce the list of matching words according to dictionary server queries."
  (if (> (length helm-input) 0)
      (mapcar
       (lambda (entry)
         (cons (concat (car entry) "\t(" (cdr entry) ")") entry))
       (dictionary-do-matching helm-input "*" "." 'helm-get-dict-sources))
    nil))

(defun helm-dictionary-lookup-word (entry)
  "Lookup the definition of ENTRY using the ENTRY's dictionary."
  (dictionary-new-search entry))

(defun helm-get-dict-sources (reply)
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


(defun helm-dictionary-lookup (word)
  "Lookup a WORD's meaning or synonyms using `helm' and `dictionary'."
  (interactive (list (let ((query (if (use-region-p)
                                     (buffer-substring-no-properties
                                      (region-beginning)
                                      (region-end))
                                   (substring-no-properties (word-at-point)))))
                       (if (equal (car (last (split-string query "" t))) "s")
                           (mapconcat 'identity (-butlast (split-string query "" t)) "")
                         query))))
  (helm :sources (helm-build-sync-source "Helm Dictionary"
                 :candidates 'helm-dictionary-match-words
                 :action 'helm-dictionary-lookup-word
                 :candidate-number-limit 1000
                 :requires-pattern 2)
        :input word
        :buffer "*Dictionary*"))

(provide 'helm-dictionary)

;;; helm-dictionary.el ends here