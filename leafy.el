(define-minor-mode leafy-mode
  "Leafy mode"
  :init-value nil
  :lighter " Leafy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'request-completion-at-point)
	    (define-key map (kbd "C-c t") 'leafy-test-insert-section-after)
	    (define-key map (kbd "C-c c") 'leafy-log-context)
            map))

(require 'cl-lib)

;; Load the python.el library
(require 'python)

(defvar leafy-python-shell nil)
(defvar leafy-record-token-statistics t)
(defvar leafy-chatgpt-context-size 4096)
(defvar leafy-chatgpt-output-size-reservation 1024)

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
	 (val (delete-non-digits (car (last (split-string res sep)))))
	 )
    val))

(defun delete-non-digits (string)
  "Removes all non-digit characters from the given string."
  (replace-regexp-in-string "[^[:digit:]]" "" string))

;;(defun python-quote-argument (string)
;;  (replace-regexp-in-string "[']" "\\\\\\&" (or string "")))
;;
(defun python-quote-argument (string)
  (replace-regexp-in-string "['\n]" (lambda (match) (if (string= match "'") "\\\\'" "\\\\n")) (or string "")))
(ert-deftest test-python-quote-argument ()
  (should (equal (python-quote-argument nil) ""))
  )

;; Define a function to count the number of tokens in a string
(defun leafy-count-tokens (string)
  "Count the number of tokens in a string using tiktoken."
  (let ((cmd (concat "import tiktoken; enc = tiktoken.get_encoding('gpt2'); print(len(enc.encode('" (python-quote-argument string) "')));\n")))
    (string-to-number (eval-python-integer cmd))))

(ert-deftest test-leafy-count-tokens ()
  (should (equal (leafy-count-tokens "the quick brown fox") 4))
  (should (equal (leafy-count-tokens "a b c d e") 5))
  (should (equal (leafy-count-tokens "") 0))
  (should (equal (leafy-count-tokens "Section 1\nText for section 1.") 8))
  ;;(should (equal (leafy-count-tokens nil) 0))
  )

(defun leafy-get-sections ()
  "Extracts each section in the buffer as a (level, property drawer, tag, title, content) tuple."
  (let ((sections '()))
    (org-element-map (org-element-parse-buffer) 'section
      (lambda (section)
        (let* ((headline (org-element-property :parent section))
               (title (org-element-property :title headline))
               (content-start (org-element-property :contents-begin section))
               (content-end (org-element-property :contents-end section))
	       (drawer (leafy-get-property-drawer-for-element-section section))
	       (excluded-tags (append '("PROPERTIES") nil));org-element-exclude-tags))
	       (paragraphs (org-element-map section 'paragraph 'identity))
	       (content (apply 'concat
			 (mapcar
			  (lambda (paragraph)
			    (org-no-properties
			     (org-element-interpret-data paragraph)))
			  paragraphs)))
	       (level 0)
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
          (push (list level drawer tag (format "%s" title) (format "%s" content)) sections))))
    (reverse sections)))

(defun leafy-sections-to-chatgpt (sections)
  "Returns a list of (tag, title + content) for a list of sections."
  (mapcar (lambda (section)
            (list (nth 2 section)
                  (concat (nth 3 section) "\n" (nth 4 section))))
          sections))

(defun leafy-section-count-tokens (section)
  "Counts the tokens in the concatenated title and content of a section."
  (let* ((chatgpt-section (leafy-sections-to-chatgpt (list section)))
         (title-and-content (cadr (car chatgpt-section))))
    (leafy-count-tokens title-and-content)))

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

;; (tick-meta-counter "input-tokens" (alist-get :input-tokens '((:input-tokens . 2924) (:output-tokens . 28) (:billed-tokens . 2952))))
(defun leafy-tick-token-counters (response)
  ;;(message "@@@ leafy-tick-token-counters %S\n" response)
  (when leafy-record-token-statistics
    (let ((statistics (leafy-extract-chatgpt-usage-statistics response)))
      ;;(message "@@@@ {statistics:%S}}\n" statistics)
      (unless (null statistics)
	(let ((input-tokens (alist-get :input-tokens statistics))
	      (output-tokens (alist-get :output-tokens statistics))
	      (billed-tokens (alist-get :billed-tokens statistics))
	      )
	  ;;(message "@@@@ {input-tokens:%S} {output-tokens:%S} {billed-tokens:%S}\n" input-tokens output-tokens billed-tokens)
	  ;;(message "@@@@@@ %S\n" `(tick-meta-counter "input-tokens" ,input-tokens))
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
                    (max_tokens . ,leafy-chatgpt-output-size-reservation)))
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
	 (prompt-context-size (- leafy-chatgpt-context-size leafy-chatgpt-output-size-reservation))
	 (dropped-sections (leafy-drop-sections all-sections prompt-context-size))
	 (chatgpt-sections (leafy-sections-to-chatgpt all-sections))
	 (cursor-section (leafy-get-section-for-element element))
	 (cursor-section-text (buffer-substring-no-properties (org-element-property :begin cursor-section)
							      (org-element-property :end cursor-section)))
	 )
    `(,@chatgpt-sections
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

(defun leafy-get-sibling-nodes (node)
  "Get all sibling nodes of a given NODE."
  (when node
    (let ((parent (org-element-property :parent node)))
      (org-element-map parent (org-element-type node)
        (lambda (sibling)
          (unless (eq sibling node)
            sibling))
      nil 'tree))))
(defun leafy-prioritize (ctx)
  "Drops sections according to priority list until token limit is reached, and returns the updated context."
  (let ((cur-section (car ctx))
        (limit leafy-chatgpt-context-size)
        (tokens 0)
        (result nil))
    (dolist (section (cdr ctx))
      (let ((priority (leafy-get-priority section))
            (level (car section)))
        (when (or (not (equal level (car cur-section)))
                  (not (member section (leafy-get-sibling-nodes cur-section)))
                  (< priority (leafy-get-priority cur-section)))
          (setq ctx (delete section ctx))))
      (setq tokens (+ tokens (length section)))
      (when (> tokens limit)
        (setq tokens limit))
      (setq result (cons section result)))
    (reverse result)))

(defun leafy-get-property-from-element-section (element property)
  "Extracts PROPERTY from an `org-element-property' section of ELEMENT."
  (let* ((section (leafy-get-section-for-element element))
         (section-property (org-element-property section property)))
    (when section-property
      (org-strip-properties section-property))))

(defun leafy-get-priority (section)
  "Returns the numeric priority of SECTION based on the `:priority' property."
  (let ((priority-str (leafy-get-property-from-element-section section :priority)))
    (when priority-str
      (string-to-number priority-str))))

(defun leafy-with-property-drawer (element action)
  (save-excursion
    (org-with-point-at (org-element-property :begin (org-element-for-section element))
      (action))))

(defun leafy-get-property-drawer-for-element-section (element-section)
  "Returns the property drawer for the given ELEMENT-SECTION as an
   alist of property-value pairs."
  (let ((drawer (org-element-property :drawer element-section)));;(org-element-property :parent element-section))))
    (when (and drawer (string= (org-element-property :drawer-name drawer) "PROPERTIES"))
      (let* ((eol (org-element-property :end drawer))
             (bol (save-excursion
                    (goto-char eol)
                    (forward-line -1)
                    (line-beginning-position)))
             (inside (buffer-substring-no-properties (+ 1 bol) (- eol 2)))
             (pairs (split-string inside "\n")))
        (mapcar (lambda (pair)
                  (let* ((parts (split-string pair ":"))
                         (property (car parts))
                         (value (string-trim (cadr parts))))
                    (cons property value)))
                pairs)))))

(defun leafy-get-property-drawer-for-element-section (section-element)
  "Given an Org mode section ELEMENT, returns the contents of its property drawer, if it has one. If there is no property drawer, returns nil."
  (let ((properties (org-entry-properties (org-element-property :begin section-element) 'standard)))
    ;; (message "@@@@@@ %S\n" section-element)
    (when properties
      (let ((drawer (cl-find-if (lambda (p) (string= (car p) "PROPERTIES")) properties)))
        (when drawer (cdr drawer))))))

(defun leafy-get-property-drawer-for-element-section (element-section)
  "Returns the property drawer for the given ELEMENT-SECTION as an
   alist of property-value pairs."
  
  (let ((drawer (org-element-property :drawer element-section)));;(org-element-property :parent element-section))))
    (when (and drawer (string= (org-element-property :drawer-name drawer) "PROPERTIES"))
      (let* ((eol (org-element-property :end drawer))
             (bol (save-excursion
                    (goto-char eol)
                    (forward-line -1)
                    (line-beginning-position)))
             (inside (buffer-substring-no-properties (+ 1 bol) (- eol 2)))
             (pairs (split-string inside "\n")))
        (mapcar (lambda (pair)
                  (let* ((parts (split-string pair ":"))
                         (property (car parts))
                         (value (string-trim (cadr parts))))
                    (cons property value)))
                pairs)))))
(defun leafy-get-property-drawer-for-element-section (element)
  "Get the first property drawer in the section that encloses the given ELEMENT."
  (let* ((section (leafy-get-section-for-element element))
         (drawer (org-element-map section 'drawer
                   (lambda (d) (when (string= "PROPERTIES" (org-element-property :drawer-name d)) d))
                   nil t)))
    (when drawer (buffer-substring (org-element-property :contents-begin drawer) (org-element-property :contents-end drawer)))))
;; (leafy-get-property-drawer-for-element-section (leafy-get-section-for-element (org-element-at-point)))

(defun leafy-get-property-drawer-for-element-section (element)
  "Get the first property drawer in the section that encloses the given ELEMENT.
Returns the properties as an alist."
  (let* ((section (leafy-get-section-for-element element))
         (drawer (org-element-map section 'drawer
                   (lambda (d) (when (string= "PROPERTIES" (org-element-property :drawer-name d)) d))
                   nil t)))
    drawer))

(defun leafy-extract-properties (property-drawer)
  "Extract key-value pairs from the given PROPERTY-DRAWER string."
  (let ((properties ()))
    (while (string-match "^\\s-*:\\([[:alnum:]\\-]+\\):\\s-*\\(.+\\)\\s-*$" property-drawer)
      (push (cons (intern (downcase (match-string 1 property-drawer)))
		  (match-string 2 property-drawer)) properties)
      (setq property-drawer (substring property-drawer (match-end 0))))
    (reverse properties)))

(require 'ert)
(require 'org)

(ert-deftest leafy-test-get-property-drawer-for-element-section ()
  "Test `leafy-get-property-drawer-for-element-section`."
  (with-temp-buffer
    (insert "
* Section with property drawer

  This is some text.

  :PROPERTIES:
  :input-tokens: 100
  :output-tokens: 50
  :END:

  And this is some more text.
")
    (let* ((tree (org-element-parse-buffer))
           (section (org-element-map tree 'section 'identity nil t)))
      (should (equal ;;(org-element-property :drawer section) ;;section
		     (leafy-extract-properties (leafy-get-property-drawer-for-element-section section))
                     '((input-tokens . "100")
                       (output-tokens . "50")))))))

(defun leafy-get-priority (&optional element)
  "Get the priority of the section that encloses the given ELEMENT."
  (let ((section (leafy-get-section-for-element element)))
    (when section
      (let* ((property-drawer (leafy-get-property-drawer-for-element-section element))
             (priority (org-entry-get property-drawer "priority")))
	;; (message "@@@@ %S:%s" property-drawer priority)
        (cond ((not priority) 0)
              ((string-match-p "\\(\\+\\([0-9]+\\)\\)" priority)
               (string-to-number (match-string 2 priority)))
              ((string-to-number priority))
              (t 0))))))

(ert-deftest leafy-test-get-priority ()
  (with-temp-buffer
    (insert "
* Top-level heading
  :PROPERTIES:
  :PRIORITY: 3
  :END:
** Subheading
*** Sub-subheading
    ")
    (goto-char (point-min))
    (let ((sections (leafy-get-sections))
          (top-level-priority 0)
          (subheading-priority 0)
          (sub-subheading-priority 0))
      (dolist (section sections)
        (pcase section
          (`(1 ,drawer ,tag ,title ,content)
           (setq top-level-priority (leafy-get-priority drawer))
           (should (equal top-level-priority 0)))
          (`(2 ,drawer ,tag ,title ,content)
           (setq subheading-priority (leafy-get-priority drawer))
           (should (equal subheading-priority 3)))
          (`(3 ,drawer ,tag ,title ,content)
           (setq sub-subheading-priority (leafy-get-priority drawer))
           (should (equal sub-subheading-priority 0))))))))


(defun leafy-set-priority (element priority)
  (let ((property-drawer (leafy-get-property-drawer-for-element-section element)))
    (org-entry-put property-drawer "priority" (number-to-string priority))))

(ert-deftest test-leafy-set-priority ()
  "Test if leafy-set-priority sets the priority property of a section correctly."
  (with-temp-buffer
    (org-mode)
    (insert "
* Top-level heading
  :PROPERTIES:
  :PRIORITY: 3
  :END:
** Subheading
*** Sub-subheading
")
    (goto-char (point-min))
    (let ((sections (leafy-get-sections)))
      (dolist (section sections)
        (pcase section
          (`(1 ,drawer ,tag ,title ,content)
           (should (equal 0 (leafy-get-priority drawer)))
	   (leafy-set-priority drawer 5)
	   (should (equal 5 (leafy-get-priority drawer)))
	   )
          (`(2 ,drawer ,tag ,title ,content)
	   (should (equal 3 (leafy-get-priority drawer)))
	   (leafy-set-priority drawer 10)
	   (should (equal 10 (leafy-get-priority drawer)))
	   )
          (`(3 ,drawer ,tag ,title ,content)
	   (should (equal 0 (leafy-get-priority drawer)))
	   (leafy-set-priority drawer 15)
	   (should (equal 15 (leafy-get-priority drawer)))
	   ))))))

(defun leafy-print-costs ()
  "Estimate how much $$$$ you've spent on the OpenAI so far"
  (interactive)
  (let* ((input-tokens (string-to-number (get-meta-property "input-tokens")))
	 (billed-tokens (string-to-number (get-meta-property "billed-tokens")))
	 (multiplier (/ 0.002 1000))
	 )
    (message "You have spent %s, %.0f%% of which was context" (* multiplier billed-tokens) (/ (* 100.0 input-tokens) billed-tokens))))

(cl-defun leafy-drop-one-section (sections)
  "Returns the index to remove of the last section that increases indentation."
  (let ((len (length sections))
        (indentation-level -1)
        (found-section-index nil))
    (dotimes (i len)
      (let ((j (- len i 1)))
        (when (> (car (nth j sections)) indentation-level)
          (setq found-section-index j)
          (setq indentation-level (car (nth j sections))))))
    (or found-section-index 0)))

(defun delete-at (list index)
  "Delete the element at INDEX in LIST and return the modified list."
  (if (= index 0)
      (cdr list)
    (let ((previous (nthcdr (1- index) list)))
      (setcdr previous (cdr (cdr previous)))
      list)))

(defun remove-at (list index)
  "Return a new list with the element at INDEX removed."
  (append (cl-subseq list 0 index)
          (cl-subseq list (1+ index))))

(ert-deftest test-leafy-drop-one-section ()
  "Test for `leafy-drop-one-section` function."
  (let ((sections '((1 nil nil "Section 1" "Text for section 1.")
                    (2 nil nil "Subsection 1" "Text for subsection 1.")
                    (2 nil nil "Subsection 2" "Text for subsection 2.")
                    (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                    (1 nil nil "Section 2" "Text for section 2."))))
    ;; Drop non-keeper
    (let* ((idx (leafy-drop-one-section sections))
           (modified-sections (remove-at sections idx)))
      (should (equal idx 3))
      (should (equal modified-sections
                     '((1 nil nil "Section 1" "Text for section 1.")
                       (2 nil nil "Subsection 1" "Text for subsection 1.")
                       (2 nil nil "Subsection 2" "Text for subsection 2.")
                       (1 nil nil "Section 2" "Text for section 2.")))))))

 (defun leafy-drop-sections (sections context-size)
  "Drops sections one by one until the list fits within the CONTEXT-SIZE limit.
  If the original list is smaller than the limit, returns the original list.
  Otherwise, returns the new list with dropped sections."
  (let ((new-sections sections))
    (while (and (> (length new-sections) 1) (> (apply #'+ (mapcar (lambda (s) (leafy-count-tokens (nth 4 s))) new-sections)) context-size))
      (let ((index-to-remove (leafy-drop-one-section new-sections)))
        (setq new-sections (remove-at new-sections index-to-remove))))
    new-sections))

(defun leafy-section-count-tokens (section)
  "Count the number of tokens in a section using `leafy-count-tokens`."
  (let ((section-string (concat (nth 3 section) "\n" (nth 4 section))))
    ;; (message "Section string: %S" section-string)
    (leafy-count-tokens section-string)))

(defun leafy-count-tokens (string)
  "Count the number of tokens in a string using tiktoken."
  (let ((cmd (concat "import tiktoken; enc = tiktoken.get_encoding('gpt2'); print(len(enc.encode('" (python-quote-argument string) "')));\n")))
    ;; (message "Input string: %S" string)
    ;; (message "Command: %S" cmd)
    (string-to-number (eval-python-integer cmd))))

(defun leafy-drop-sections (sections context-size)
  "Drops sections one by one until the list fits within the CONTEXT-SIZE limit.
  If the original list is smaller than the limit, returns the original list.
  Otherwise, returns the new list with dropped sections."
  (let ((new-sections sections))
    (while (and (> (length new-sections) 1) (> (apply #'+ (mapcar #'leafy-section-count-tokens new-sections)) context-size))
      (let ((index-to-remove (leafy-drop-one-section new-sections)))
        ;; (message "Dropping index: %d" index-to-remove)
        ;; (message "Before: %S" new-sections)
        (setq new-sections (leafy-delete-at index-to-remove new-sections))
        ;; (message "After: %S" new-sections)
	))
    new-sections))

(ert-deftest test-leafy-drop-sections ()
  (let* ((sections '((1 nil nil "Section 1" "Text for section 1.")
                     (2 nil nil "Subsection 1" "Text for subsection 1.")
                     (2 nil nil "Subsection 2" "Text for subsection 2.")
                     (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                     (1 nil nil "Section 2" "Text for section 2.")))
         (context-size 25) ;; Set the context size to 35, forcing at least two sections to be dropped
         (result (leafy-drop-sections sections context-size)))
    (message "result: %S" result)
    (should (equal (length result) 3)))) ;; Expecting only 3 sections to remain
