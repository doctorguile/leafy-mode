(define-minor-mode leafy-mode
  "Leafy mode"
  :init-value nil
  :lighter " Leafy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'request-completion-at-point)
	    (define-key map (kbd "C-c t") 'leafy-test-insert-section-after)
	    (define-key map (kbd "C-c c") 'leafy-log-context)
            map))

;; Load the python.el library
(require 'python)

(defvar leafy-python-shell nil)
(defvar leafy-record-token-statistics t)

(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `leafy-python-shell` variable."
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))
    ;; Disable native Python completion for the leafy-python-shell process
    (set-process-query-on-exit-flag leafy-python-shell nil)
    ;; (set-process-filter leafy-python-shell (symbol-value 'python-shell-filter))
    ;(set-process-filter leafy-python-shell 'python-shell-filter)
    (with-current-buffer (process-buffer leafy-python-shell)
      (setq-local python-shell-completion-native-enable nil))))

(start-python-process)

(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `my-python-shell` variable."
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))))

;; On Mac OSX, they use a BSD readline instead of GNU readline
;; So, (python-shell-internal-send-string) will return the input too, separated by ^M
(defun eval-python-integer (python-cmd)
  (let* ((res (python-shell-internal-send-string python-cmd))
	 (sep "")
	 (val (delete-non-digits (nth 1 (split-string res sep)))))
    val))

(defun delete-non-digits (string)
  "Removes all non-digit characters from the given string."
  (replace-regexp-in-string "[^[:digit:]]" "" string))

(defun python-quote-argument (string)
  (replace-regexp-in-string "[']" "\\\\\\&" string))
  ;;

;; Define a function to count the number of tokens in a string
(defun count-tokens (string)
  "Count the number of tokens in a string using tiktoken."
  (let ((cmd (concat "import tiktoken; enc = tiktoken.get_encoding('gpt2'); print(len(enc.encode('" (python-quote-argument string) "')));\n")))
    (string-to-number (eval-python-integer cmd))))

(defun leafy-get-sections ()
  "Extracts each section in the buffer as a (tag, title, content) triplet."
  (let ((sections '()))
    (org-element-map (org-element-parse-buffer) 'section
      (lambda (section)
        (let* ((headline (org-element-property :parent section))
               (title (org-element-property :title headline))
               (content-start (org-element-property :contents-begin section))
               (content-end (org-element-property :contents-end section))
	       (excluded-tags (append '("PROPERTIES") nil));org-element-exclude-tags))
	       (paragraphs (org-element-map section 'paragraph 'identity))
	       (content (apply 'concat
			 (mapcar
			  (lambda (paragraph)
			    (org-no-properties
			     (org-element-interpret-data paragraph)))
			  paragraphs)))
	       ;;(section-remove-properties (progn (org-element-map section '(property-drawer)
	       ;;				   (lambda (drawer) (org-element-extract-element drawer)))
	       ;;				 (buffer-substring-no-properties
	       ;;				  (org-element-property :contents-begin section)
	       ;;				  (org-element-property :contents-end section))))
               ;;(without-properties (org-element-interpret-data (org-element-contents section)))
	       ;;(without-properties (org-element-extract-element section '("PROPERTIES")))
               ;;(content (buffer-substring-no-properties content-start content-end))
	       ;;(content-with-properties (org-element-interpret-data
		;;			 (org-element-extract-element
		;;			  section)))
	       ;;(content (org-element-normalize-string content-with-properties))
               (tag (if (member "assistant" (org-element-property :tags headline))
                        "assistant"
                      (if (member "system" (org-element-property :tags headline))
                          "system"
                        "user")))
	       )
          (push (list tag (format "%s\n%s" title content)) sections))))
    (reverse sections)))
;; (delete-non-digits "aouaoueo5aoeuaoeuaoeu")

;; (python-shell-send-setup-code)
;; (setq python-shell-send-line-send-wait nil)
;; Call the count-tokens function with a string argument
;; (c ount-tokens "This is a' + str(2+2) + ' test.")
;; (eval-python-integer "print(2+2)")
;; (count-tokens "This is a test.")
;; (python-shell-internal-send-string "print(2+2)")
;; (process-list)
;; (shell-quote-argument "aoeuaoeu")
;; (process-filter (python-shell-get-process))
;; (set-process-filter (python-shell-get-process) #'python-shell-output-filter)

;; (comint-send-string (get-buffer-process "*Python*") "print(2+2)\n" :noecho)

;;(shell-quote-argument "This is a test.")

;;(let ((process-name "Python"))
;;  (when (get-process process-name)
;;    (delete-process (get-process process-name))))

(defvar chatgpt-buffer
  (get-buffer-create "*ChatGPT*")
  "The buffer to log messages from the `do-chatgpt-request` function.")

(defun leafy-validate-chatgpt-response (response)
  (when (eq (caar response) 'error)
      (user-error "ChatGPT API Error: %S" (alist-get 'message (car response)))))

(defun leafy-extract-chatgpt-usage-statistics (response)
  (let* ((usage (alist-get 'usage response)))
    (unless (null usage)
      ;; (message "%S\n" usage)
      (let* ((input-tokens (alist-get 'prompt_tokens usage))
	     (output-tokens (alist-get 'completion_tokens usage))
	     (billed-tokens (alist-get 'total_tokens usage))
	     )
	;; (message "@@@@ foo\n")
	`((:input-tokens . ,input-tokens)
	  (:output-tokens . ,output-tokens)
	  (:billed-tokens . ,billed-tokens))))))

(defun foo-test ()
  (interactive)
  (let* ((statistics '((:input-tokens . 2924) (:output-tokens . 40) (:billed-tokens . 2964)))
	 (input-tokens (alist-get :input-tokens statistics))
	 (output-tokens (alist-get :output-tokens statistics))
	 (billed-tokens (alist-get :billed-tokens statistics))
	 )
    (message "@@@@ {statistics:%S} {input-tokens:%S} {output-tokens:%S} {billed-tokens:%S}\n" statistics input-tokens output-tokens billed-tokens)
    (message "@@@@@@ %S\n" `(tick-meta-counter "input-tokens" ,input-tokens))
    (tick-meta-counter "input-tokens" input-tokens)
    (tick-meta-counter "output-tokens" output-tokens)
    (tick-meta-counter "billed-tokens" billed-tokens)
    ))

;; (tick-meta-counter "input-tokens" (alist-get :input-tokens '((:input-tokens . 2924) (:output-tokens . 28) (:billed-tokens . 2952))))
(defun leafy-tick-token-counters (response)
  (message "@@@ leafy-tick-token-counters %S\n" response)
  (when leafy-record-token-statistics
    (let ((statistics (leafy-extract-chatgpt-usage-statistics response)))
      (message "@@@@ {statistics:%S}}\n" statistics)
      (unless (null statistics)
	(let ((input-tokens (alist-get :input-tokens statistics))
	      (output-tokens (alist-get :input-tokens statistics))
	      (billed-tokens (alist-get :billed-tokens statistics))
	      )
	  (message "@@@@ {input-tokens:%S} {output-tokens:%S} {billed-tokens:%S}\n" input-tokens output-tokens billed-tokens)
	  (message "@@@@@@ %S\n" `(tick-meta-counter "input-tokens" ,input-tokens))
	  (tick-meta-counter "input-tokens" input-tokens)
	  (tick-meta-counter "output-tokens" output-tokens)
	  (tick-meta-counter "billed-tokens" billed-tokens)
	  )))))

(defun leafy-do-chatgpt-request (chatgpt-buffer nodes)
  "Send the given list of nodes to the OpenAI Chat API and return a list of completions.
Messages are logged to the `chatgpt-buffer`."
  (let* ((messages (mapcar (lambda (node) `((role . ,(or (nth 0 node) "user"))
					    (content . ,(nth 1 node))))
			   nodes))
         (payload `((model . "gpt-3.5-turbo-0301")
                    (messages . ,messages)
                    (n . 1)
                    ;;(stop . [".", "!", "?"])
                    (max_tokens . 1024)))
         (url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " leafy-api-key))))
         (url-request-method "POST")
         (url-request-extra-headers headers)
         (url-request-data (json-encode payload))
	 )
    ;; Send the request to the OpenAI API
    (with-current-buffer (url-retrieve-synchronously url t t)
      ;; Parse the response and return the completions
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((result (json-read)))
        ;; Write the response to the chatgpt-buffer	
	(princ (format "Request: %S\n" payload) chatgpt-buffer)
	(princ (format "Response: %S\n" result) chatgpt-buffer)
	result))))

(defun extract-chatgpt-response-message (response)
  (let* ((choices (cdr (assoc 'choices response)))
	 (choice (car (aref choices 0)))
	 (content (cdr (assoc 'content choice)))
	 )
    content
    ))
    ;;

;; Test the do-chatgpt-request function
;; (leafy-do-chatgpt-request chatgpt-buffer '((nil "Hello, how are you?") (nil "I'm doing well, thank you.")))
;; 
;; (with-output-to-temp-buffer "*ChatGPT Result*"
;;   (let* ((response (leafy-do-chatgpt-request chatgpt-buffer
;; 				       '((nil "Hello, how are you?") ("assistant" "I'm doing well, thank you."))))
;; 	(result (extract-chatgpt-response-message response)))
;;     (prin1 result)))


(defun leafy-log-context ()
  "Log the given context to the `chatgpt-buffer`."
  (interactive)
  (save-excursion
    (princ (format "Context: %S\n" (leafy-get-context)) chatgpt-buffer))
  )

;; (defun leafy-get-immediate-context ()
;;   "Collects just the contents the current section into a ChatGPT-ready context"
;;   (let ((node (org-element-context)))
;;     (when (eq 'headline (org-element-type node))
;;       (let* ((entry (org-get-entry))
;;              (text entry))
;; 	(when text
;;           (list (cons 'text text)))))))
;; (defun leafy-get-immediate-context ()
;;   "Context for the immediate element"
;;   (leafy-get-element-context (org-element-at-point)))

;; (defun leafy-get-section-for-headline (headline)
;;   "Given a headline element, returns the enclosing section element."
;;   (let* ((headline-begin (org-element-property :begin headline))
;;          (section (org-element-map (org-element-parse-buffer) 'section
;;                     (lambda (s) (when (and (> (org-element-property :begin s) headline-begin)
;;                                           (< (org-element-property :end s) (org-element-property :end headline)))
;;                                   s))
;;                     nil 'first-match)))
;;     section))
;; 
;; (defun leafy-get-section-for-headline (headline)
;;   "Given an org-mode HEADLINE element, returns the corresponding section element."
;;   (org-element-property :parent headline))
;; 
;; (defun leafy-get-section-for-paragraph (paragraph)
;;   "Returns the section object enclosing the given paragraph."
;;   (let ((section (org-with-point-at (org-element-property :begin paragraph)
;;                    (org-element-at-point))))
;;     (while (not (eq (org-element-type section) 'section))
;;       (setq section (org-element-property :parent section)))
;;     section))

(defun leafy-intervals-contain-p (start1 end1 start2 end2)
  "Check if the interval [START1, END1] is contained within [START2, END2]."
  (and (<= start2 start1)
       (<= end1 end2)))

(defun leafy-get-section-for-element (element)
  "Given an Org mode paragraph element, return the section element that encloses it."
  (let ((section nil))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (and (leafy-intervals-contain-p (org-element-property :begin element) (org-element-property :end element)
					      (org-element-property :begin headline) (org-element-property :end headline))
                                               
                   (or (null section)
                       (leafy-intervals-contain-p (org-element-property :begin headline) (org-element-property :end headline)
						  (org-element-property :begin section) (org-element-property :end section))))
                                                  
          (setq section headline)))
      nil nil t)
    section))

(defun leafy-get-context ()
  "Return the ChatGPT context for the current cursor point."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (null element)
	(message "Null element")
      (leafy-get-context-at-element element))))

(defun leafy-get-context-at-element (element)
  "Return a list of all headings and their titles up to the top-level heading, along with their paragraphs."
  (let* (;; (whole-buffer-text (buffer-substring-no-properties 1 (point-max)))
	 (all-sections (leafy-get-sections))
	 (cursor-section (leafy-get-section-for-element element))
	 (cursor-section-text (buffer-substring-no-properties (org-element-property :begin cursor-section)
							      (org-element-property :end cursor-section)))
	 )
    `(,@all-sections
      ;; ("user" ,whole-buffer-text)
      ("user" ,cursor-section-text))))

(defun leafy-insert-chatgpt-response-after (title response)
  "Insert a new section with TITLE and CONTENT immediately after the current element."
  (let* ((section (org-element-context))
	 (response-body (extract-chatgpt-response-message response))
	 (usage (alist-get 'usage response))
	 (prompt-tokens (alist-get 'prompt_tokens usage))
	 (completion-tokens (alist-get 'completion_tokens usage))
	 (billed-tokens (alist-get 'total_tokens usage))
	)
    (org-insert-heading-after-current)
    (let ((new-heading (org-element-at-point)))
      (insert title)
      (insert "\n")
      (insert response-body)
      (save-excursion
	(org-with-point-at (org-element-property :begin new-heading)
	  (org-set-tags-to "assistant")
	  (org-set-property "input-tokens" (format "%S" prompt-tokens))
	  (org-set-property "output-tokens" (format "%S" completion-tokens))
	  (org-set-property "billed-tokens" (format "%S" billed-tokens))
	  )))
    ;;(org-entry-put (point) "LEAFY_CONTEXT" nil)
    (outline-show-subtree)))

(defun leafy-test-insert-section-after ()
  "Test function to insert a new section tagged with \"assistant\" after the current element."
  (interactive)
  (leafy-insert-section-after "foo" "bar"))

(defun leafy-get-element-context (element)
  "Context for the given element"
  (let* ((node (org-element-context element))
	 (begin-idx (org-element-property :contents-begin node))
	 (end-idx (org-element-property :contents-end node))
	 (text (buffer-substring begin-idx end-idx))
	 )
    text
    ))

(defun leafy-get-parents (node)
  "Collects the parent hierarchy of NODE in the form of a list of chat turns."
  (let ((parents nil))
    (while (setq node (org-up-heading-safe node))
      (push (cons 'heading (org-element-property :title node)) parents))
    parents))

(defun request-completion-at-point ()
  "Send the current node and main text of every node up to the top-level to ChatGPT for completion."
  (interactive)
  (let* ((context (leafy-get-context))
	 (response (leafy-do-chatgpt-request chatgpt-buffer context))
	 )
    (leafy-validate-chatgpt-response response)
    (leafy-tick-token-counters response)
    (leafy-insert-chatgpt-response-after "ChatGPT response" response)))

;; Define a function to search for a tag within an org-heading
(defun heading-with-tag-p (tag)
  (member "my-tag" (org-get-tags)))

(defun get-tagged-section (tag)
  "Returns the section with the specified TAG."
  (interactive "sTag: ")
  (let ((sections '()))
    (org-map-entries
     (lambda ()
       (when (org-at-heading-p)
	 (when (member tag (org-get-tags))
	   (push (org-element-subtree-complete (org-element-at-point)) sections)))))
    sections))

;; Use org-map-entries to retrieve a list of headings with the "my-tag" tag
(defun get-headings-with-tag ()
  (let ((tagged-headings '()))
    (org-map-entries
     (lambda ()
       (when (heading-with-tag-p)
         (push org-heading-components tagged-headings))))
    tagged-headings))

(defun get-property-drawer-pom (drawer-name)
  (let ((found-pom nil))
    (org-element-map (org-element-parse-buffer) 'property-drawer
      (lambda (drawer)
	(let ((pom (org-element-property :begin drawer)))
	  (when (string= drawer-name (org-entry-get pom "drawer-name"))
	    ;; (message (format "@@@@@@ - %S\n" drawer))
	    (setq found-pom pom)))))
    found-pom))


(defun get-property-drawer-value (drawer-name key)
  "Returns the value of the first property drawer with the specified TAG."
  (let ((pom (get-property-drawer-pom drawer-name)))
    (when pom (org-entry-get pom key))))

(defun set-property-drawer-value (drawer-name key value &optional (create-drawer? t))
  "Sets the value of the first property drawer with the specified TAG"
  (let ((pom (get-property-drawer-pom drawer-name)))
    (if pom
	(org-entry-put pom key value)
      (when create-drawer?
	(org-insert-property-drawer)
	(org-entry-put (point) "drawer-name" drawer-name)
	(org-entry-put (point) key value))
      )))

(defun get-meta-property (key) (get-property-drawer-value "meta" key))
(defun set-meta-property (key value) (set-property-drawer-value "meta" key value))
(defun tick-meta-counter (key dx)
  (let* ((x0 (string-to-number (or (get-meta-property key) "0")))
	 (x1 (+ x0 dx)))
    (set-meta-property key (number-to-string x1))))
