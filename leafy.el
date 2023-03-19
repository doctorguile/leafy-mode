(define-minor-mode leafy-mode
  "Leafy mode"
  :init-value nil
  :lighter " Leafy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'request-completion-at-point)
	    (define-key map (kbd "C-c t") 'leafy-test-insert-section-after)
	    (define-key map (kbd "C-c c") 'leafy-log-context)
            map))
(require 'org)
(require 'org-element)
(require 'leafy--utils)
(require 'leafy--org-helpers)

;;(load-file (expand-file-name "./org-helpers.el"));; (file-name-directory buffer-file-name)))



(require 'cl-lib)

;; Load the python.el library
(require 'python)

;; Start a Python shell for leafy
(defvar leafy-python-shell (python-shell-get-or-create-process))
(defvar leafy-record-token-statistics t)
(defvar leafy-chatgpt-context-size 4096)
(defvar leafy-chatgpt-output-size-reservation 1024)
(defvar leafy-priority-key "CONTEXT-PRIORITY")

(defun leafy-max-prompt-size () (- leafy-chatgpt-context-size leafy-chatgpt-output-size-reservation))


(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `leafy-python-shell` variable."
  (when (process-live-p leafy-python-shell)
    (kill-process leafy-python-shell))
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))
    ;; Disable native Python completion for the leafy-python-shell process
    (set-process-query-on-exit-flag leafy-python-shell nil)
    ;; (set-process-filter leafy-python-shell (symbol-value 'python-shell-filter))
    ;; (set-process-filter leafy-python-shell 'python-shell-filter)
    (with-current-buffer (process-buffer leafy-python-shell)
      (setq-local python-shell-completion-native-enable nil))))

(defun start-python-process ()
  "Starts a new Python process if one doesn't exist and sets `my-python-shell` variable."
  (unless (process-live-p leafy-python-shell)
    (setq leafy-python-shell (run-python))))

(defun run-python-command (command)
  "Run a Python command and return its output as a string."
  (shell-command-to-string (concat "python3 -c " (shell-quote-argument command))))

(defvar run-python-command-cache (make-hash-table :test 'equal :size 1000)
  "Hash table to store the last 1000 command results.")

(defun run-python-command-pure (command)
  "Run a Python command and return its output as a string.
Memoize the result and store the last 1000 command results."
  (let ((cached-result (gethash command run-python-command-cache)))
    (if cached-result
        cached-result
      (let ((result (run-python-command command)))
        (puthash command result run-python-command-cache)
        result))))

(ert-deftest test-run-python-command-pure ()
  (should (equal (run-python-command-pure "print(1 + 2)") "3\n"))
  (should (equal (gethash "print(1 + 2)" run-python-command-cache) "3\n")))

;; On Mac OSX, they use a BSD readline instead of GNU readline
;; So, (python-shell-internal-send-string) will return the input too, separated by ^M
;; So, use the RESULT: indicator to tag the result and regex for it.
;; Command should be "pure" - side-effect free
(defun eval-python-integer (python-cmd)
  (let* (;;(res (python-shell-internal-send-string python-cmd))
	 (res (run-python-command-pure python-cmd))
         (result-regexp "RESULT:\\s-*\\([[:digit:]]+\\)")
         (val (if (string-match result-regexp res)
		  (string-to-number (match-string 1 res))
		(user-error "eval-python-integer: Unable to find result in string: %S" res)
		)))
    val))

;;
(defun python-quote-argument (string)
  (replace-regexp-in-string "['\n]" (lambda (match) (if (string= match "'") "\\\\'" "\\\\n")) (or string "")))

(ert-deftest test-python-quote-argument ()
  (should (equal (python-quote-argument nil) ""))
  )

;; Define a function to count the number of tokens in a string
(defun leafy-count-tokens (string)
  "Count the number of tokens in a string using tiktoken."
  (let ((cmd (concat "import tiktoken; enc = tiktoken.get_encoding('gpt2'); print('RESULT:',len(enc.encode('" (python-quote-argument string) "')));\n")))
    (let ((result (eval-python-integer cmd)))
      (when (> result 10000)
	(user-error "ERROR: Tokens too large! Tokens:%d string:<%s> cmd:<%s>" result string cmd))
      result)))

(ert-deftest test-leafy-count-tokens ()
  (should (equal (leafy-count-tokens "the quick brown fox") 4))
  (should (equal (leafy-count-tokens "a b c d e") 5))
  (should (equal (leafy-count-tokens "") 0))
  (should (equal (leafy-count-tokens "Section 1\nText for section 1.") 8))
  (should (equal (leafy-count-tokens "(Leafy Metadata)\n") 8))
  (should (equal (leafy-count-tokens "(Leafy Metadata)\nnil") 9))
  ;;(should (equal (leafy-count-tokens nil) 0))
  )

(defun leafy-get-sections ()
  "Extracts each section in the buffer as a (level, property alist, tag, title, content) tuple."
  (let ((sections '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (let* ((title (substring-no-properties (car (org-element-property :title headline))))
               (level (org-element-property :level headline))
               (tag (if (member "assistant" (org-element-property :tags headline))
                        "assistant"
                      (if (member "system" (org-element-property :tags headline))
                          "system"
                        "user")))
               (properties (leafy-headline-to-alist headline))
               (section (org-element-map (org-element-contents headline) 'section 'identity nil t 'headline))
	       (content (or (when section
                              (leafy-section-body section))
                            "")))
          (push (list level properties tag title (format "%s" content)) sections))))
    (reverse sections)))

(defun leafy-section-to-chatgpt (section)
  "Returns a (tag, title + content) for a section"
  (pcase section
    (`(,_level ,_properties ,tag ,title ,content)
     (list tag (leafy-join (list title content) "\n")))))

(defun leafy-sections-to-chatgpt (sections)
  "Returns a list of (tag, title + content) for a list of sections."
  (mapcar #'leafy-section-to-chatgpt sections))

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
      (let* ((input-tokens (alist-get 'prompt_tokens usage))
	     (output-tokens (alist-get 'completion_tokens usage))
	     (billed-tokens (alist-get 'total_tokens usage))
	     )
	`((:input-tokens . ,input-tokens)
	  (:output-tokens . ,output-tokens)
	  (:billed-tokens . ,billed-tokens))))))

;; (tick-meta-counter "input-tokens" (alist-get :input-tokens '((:input-tokens . 2924) (:output-tokens . 28) (:billed-tokens . 2952))))
(defun leafy-tick-token-counters (response)
  (when leafy-record-token-statistics
    (let ((statistics (leafy-extract-chatgpt-usage-statistics response)))
      (unless (null statistics)
	(let ((input-tokens (alist-get :input-tokens statistics))
	      (output-tokens (alist-get :output-tokens statistics))
	      (billed-tokens (alist-get :billed-tokens statistics))
	      )
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


(defun leafy-context-entry-tokens (ctx)
  (unless (= (length ctx) 2)
    (user-error "leafy-context-entry-tokens: Strange context:<%S>" ctx))
  (let ((body (leafy-join ctx " ")))
    (leafy-count-tokens body)))

(defun leafy-log-context ()
  "Log the given context to the `chatgpt-buffer`."
  (interactive)
  (save-excursion
    (cl-flet ((join-line (chatgpt-context) (leafy-join chatgpt-context ":")))
      (let* ((ctxs (leafy-get-context))
	     (token-counts (mapcar #'leafy-context-entry-tokens ctxs))
	     (tokens-count (apply #'+ token-counts))
	     )
	;;(message "Bodies:<%S>" bodies)
	(message "Tokens: %d(%S)" tokens-count token-counts)
	(princ (format "Context: %S\n" ctxs) chatgpt-buffer)))))

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

(defun leafy-get-context (&optional buffer)
  "Return the ChatGPT context for the current cursor point."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (null element)
	(user-error "Null element")
      (leafy-get-context-at-element element buffer))))

(defun leafy-get-context-at-element (element &optional buffer)
  "Return a list of all headings and their titles up to the top-level heading, along with their paragraphs."
  (with-current-buffer (or buffer (current-buffer))
    (let* (
	   ;; (whole-buffer-text (buffer-substring-no-properties 1 (point-max)))
	   (all-sections (leafy-get-sections))
	   (cursor-section (leafy-get-section-for-element element))
	   (cursor-section-text (buffer-substring-no-properties (org-element-property :begin cursor-section)
								(org-element-property :end cursor-section)))
	   ;; Always include the last section, so deduct its tokens from max allowed.
	   (max-prompt-size (- (leafy-max-prompt-size) (leafy-count-tokens cursor-section-text)))
	   (dropped-sections (leafy-drop-sections all-sections max-prompt-size))
	   (chatgpt-sections (leafy-sections-to-chatgpt dropped-sections))
	   )
      `(,@chatgpt-sections
	;; ("user" ,whole-buffer-text)
	("user" ,cursor-section-text)))))

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
	    (setq found-pom pom)))))
    found-pom))


(defun get-property-drawer-value (drawer-name key)
  "Returns the value of the first property drawer with the specified TAG."
  (let ((pom (get-property-drawer-pom drawer-name)))
    (when pom (org-entry-get pom key))))

(cl-defun set-property-drawer-value (drawer-name key value &optional (create-drawer? t))
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
         (section-property (org-element-property property section)))
    (when section-property
      (org-strip-properties section-property))))

(defun leafy-with-property-drawer (element action)
  (save-excursion
    (org-with-point-at (org-element-property :begin (org-element-for-section element))
      (action))))

(defun leafy-get-property-drawer-under-element (element)
  "Get the first property drawer in the section that encloses the given ELEMENT."
  (let* ((drawer (org-element-map element 'drawer
                   (lambda (d) (when (string= "PROPERTIES" (org-element-property :drawer-name d)) d))
                   nil t)))
    (when drawer
      (buffer-substring (org-element-property :contents-begin drawer) (org-element-property :contents-end drawer)))))

(defun leafy-get-property-drawer-for-element-section (element)
  "Get the first property drawer in the section that encloses the given ELEMENT."
  (leafy-get-property-drawer-under-element (leafy-get-section-for-element element)))

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

(defun leafy-get-priority (props)
  (string-to-number (leafy-get-key props leafy-priority-key "0")))

(ert-deftest leafy-test-get-priority ()
  (with-temp-buffer
    (insert "
* Top-level heading
  :PROPERTIES:
  :CONTEXT-PRIORITY: 3
  :END:
** Subheading
*** Sub-subheading
    ")
    (goto-char (point-min))
    (let ((sections (leafy-get-sections)))
      (ert-info ((format "Sections: %S" sections))
	(should (equal 3 (length sections))))
      (dolist (section sections)
	(ert-info ((format "Section: %S" section))
	  (pcase section
	    (`(1 ,props ,tag ,title ,content)
	     (should (equal 3 (leafy-get-priority props))))
	    (`(2 ,props ,tag ,title ,content)
	     (should (equal 0 (leafy-get-priority props))))
	    (`(3 ,props ,tag ,title ,content)
	     (should (equal 0 (leafy-get-priority props))))))))))

(defun leafy-set-priority (props priority)
  "Set the PRIORITY property of the given HEADLINE to the specified value."
  (leafy-set-key props leafy-priority-key (number-to-string priority)))

(ert-deftest test-leafy-set-priority ()
  "Test if leafy-set-priority sets the priority property of a section correctly."
  (cl-flet ((test-case (org-content section-id initial-priority new-priority)
                       (with-temp-buffer
                         (org-mode)
                         (insert org-content)
                         (goto-char (point-min))
                         (let* ((sections (leafy-get-sections))
                                (section (nth (- section-id 1) sections)))
                           (pcase section
                             (`(,_level ,props ,_tag ,_title ,_content)
			      (should (equal initial-priority (leafy-get-priority props)))
			      (leafy-set-priority props new-priority)
			      (should (equal new-priority (leafy-get-priority props)))))))))
    ;; Case 1: Top-level heading
    (test-case "* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:"
               1 3 5)
    ;; Case 2: Subheading with no property drawer
    (test-case "* Top-level heading
** Subheading"
               2 0 10)
    ;; Case 3: Sub-subheading
    (test-case "* Top-level heading
** Subheading
*** Sub-subheading
:PROPERTIES:
:CONTEXT-PRIORITY: 5
:END:"
               3 5 15)))

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

(ert-deftest test-leafy-drop-one-section ()
  "Test for `leafy-drop-one-section` function."
  (let ((sections '((1 nil nil "Section 1" "Text for section 1.")
                    (2 nil nil "Subsection 1" "Text for subsection 1.")
                    (2 nil nil "Subsection 2" "Text for subsection 2.")
                    (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                    (1 nil nil "Section 2" "Text for section 2."))))
    ;; Drop non-keeper
    (let* ((idx (leafy-drop-one-section sections))
           (modified-sections (leafy-remove-at sections idx)))
      (should (equal idx 3))
      (should (equal modified-sections
                     '((1 nil nil "Section 1" "Text for section 1.")
                       (2 nil nil "Subsection 1" "Text for subsection 1.")
                       (2 nil nil "Subsection 2" "Text for subsection 2.")
                       (1 nil nil "Section 2" "Text for section 2.")))))))

(defun leafy-section-count-tokens (section)
  "Count the number of tokens in a section using `leafy-count-tokens`."
  (leafy-context-entry-tokens (leafy-section-to-chatgpt section)))

 (defun leafy-drop-sections (sections context-size)
  "Drops sections one by one until the list fits within the CONTEXT-SIZE limit.
  If the original list is smaller than the limit, returns the original list.
  Otherwise, returns the new list with dropped sections."
  (let ((new-sections (copy-sequence sections)))
    (cl-flet ((count-sections (ctx) (apply #'+ (mapcar #'leafy-section-count-tokens ctx))))
      (while (and (> (length new-sections) 1)
		  (> (count-sections new-sections) context-size))
	(let* ((index-to-remove (leafy-drop-one-section new-sections))
	       (section (nth index-to-remove new-sections))
	       (section-tokens (leafy-section-count-tokens section))
	       )
	  ;; (message "Dropping section with %d tokens" section-tokens)
          (setq new-sections (leafy-remove-at new-sections index-to-remove))))
      (message "leafy-drop-sections: Before:%d After:%d context:%d" (count-sections sections) (count-sections new-sections) context-size)
      new-sections)))

(ert-deftest test-leafy-drop-sections-1 ()
  (let* ((sections '((1 nil nil "Section 1" "Text for section 1.")
                     (2 nil nil "Subsection 1" "Text for subsection 1.")
                     (2 nil nil "Subsection 2" "Text for subsection 2.")
                     (3 nil nil "Subsubsection 1" "Text for subsubsection 1.")
                     (1 nil nil "Section 2" "Text for section 2.")))
         (context-size 25) ;; Set the context size to 35, forcing at least two sections to be dropped
         (result (leafy-drop-sections sections context-size)))
    (should (equal 46 (apply #'+ (mapcar #'leafy-section-count-tokens sections)))) ;; Before dropping any sections
    ;; (message "result: %S" result)
    (should (equal (length result) 3)))) ;; Expecting only 3 sections to remain

(ert-deftest test-leafy-drop-sections-2 ()
  "Test for `leafy-drop-sections` function."
  (let ((sections '((0 nil "user" "(Leafy Metadata)" "")
                    (0 nil "user" "(Project Summary)" "This project involves using the ChatGPT model from OpenAI to assist with project planning and organization within an org-mode file.")
                    (0 nil "user" "(Functions:)" "`leafy-get-section-for-element`: Given an Org mode paragraph element, returns the section element that encloses it.")
                    (0 nil "user" "(Project)" "Hello ChatGPT. Today we are working on the first item in the todo list that I expect is in your context. Can you repeat it so I know you can see it?")))
        (context-size 3072))
    (let ((result (leafy-drop-sections sections context-size)))
      (should (equal (length result) 4))
      (should (equal result sections)))))

(ert-deftest test-leafy-drop-sections-3 ()
  (with-temp-buffer
    (insert "
* Leafy Metadata
* Project Summary
* todo items
** Immediate items
*** TODO Prepare a context in a temporary buffer.
*** TODO Automatically strip out context to fit within the limit.
** Backlog
*** TODO Include current price of project in the status bar.
*** TODO Allow selecting between different OpenAI models.
*** TODO ChatGPT operator templates acting on code blocks
*** TODO Recursive ChatGPT templates
* Chat
** Project
*** ChatGPT Response :assistant:
aoeu
")
    (org-mode)
    (let ((sections (leafy-get-context (current-buffer)))
          (expected '(("user" "Leafy Metadata")
                      ("user" "Project Summary")
                      ("user" "todo items")
                      ("user" "Immediate items")
                      ("user" "Prepare a context in a temporary buffer.")
                      ("user" "Automatically strip out context to fit within the limit.")
                      ("user" "Backlog")
                      ("user" "Include current price of project in the status bar.")
                      ("user" "Allow selecting between different OpenAI models.")
                      ("user" "ChatGPT operator templates acting on code blocks")
                      ("user" "Recursive ChatGPT templates")
                      ("user" "Chat")
                      ("user" "Project")
                      ("assistant" "ChatGPT Response\naoeu\n")
		      ("user" "*** ChatGPT Response :assistant:\naoeu\n")
		      )))
      ;; (message "test-leafy-drop-sections-3 sections:<%S>" (leafy-get-context))
      (should (equal sections expected)))))

(ert-deftest test-minified-org-document ()
  (with-temp-buffer
    (insert "
* Leafy Metadata
:PROPERTIES:
:drawer-name: meta
:input-tokens: 215172
:output-tokens: 35780
:billed-tokens: 250952
:END:

* Project Summary

* todo items
** Immediate items
*** TODO Prepare a context in a temporary buffer.
*** TODO Automatically strip out context to fit within the limit.

** Backlog
*** TODO Include current price of project in the status bar.
*** TODO Allow selecting between different OpenAI models.
*** TODO ChatGPT operator templates acting on code blocks
*** TODO Recursive ChatGPT templates

* Chat

** Project

*** ChatGPT Response                                               :assistant:
:PROPERTIES:
:input-tokens: 422
:output-tokens: 42
:billed-tokens: 464
:END:
aoeu
")
    (org-mode)
    (let ((sections (leafy-get-context))
          (expected '(("user" "Leafy Metadata")
                      ("user" "Project Summary")
                      ("user" "todo items")
                      ("user" "Immediate items")
                      ("user" "Prepare a context in a temporary buffer.")
                      ("user" "Automatically strip out context to fit within the limit.")
                      ("user" "Backlog")
                      ("user" "Include current price of project in the status bar.")
                      ("user" "Allow selecting between different OpenAI models.")
                      ("user" "ChatGPT operator templates acting on code blocks")
                      ("user" "Recursive ChatGPT templates")
                      ("user" "Chat")
                      ("user" "Project")
                      ("assistant" "ChatGPT Response\naoeu\n")
		      ("user" "*** ChatGPT Response                                               :assistant:\n:PROPERTIES:
:input-tokens: 422
:output-tokens: 42
:billed-tokens: 464
:END:
aoeu
")
		      )))
      (should (equal sections expected)))))

(provide 'leafy)
