(require 'org)
(require 'ert)

(defun leafy-is-ancestor (ancestor descendant)
  "Check if ANCESTOR is an ancestor of DESCENDANT in the Org element tree."
  (let ((parent (org-element-property :parent descendant)))
    (while (and parent (not (eq parent ancestor)))
      (setq parent (org-element-property :parent parent)))
    (eq parent ancestor)))

(ert-deftest test-leafy-is-ancestor ()
  "Test if leafy-is-ancestor correctly identifies ancestor-descendant relationships."
  (with-temp-buffer
    (org-mode)
    (insert "
* Top-level heading
** Subheading
*** Sub-subheading
")
    (goto-char (point-min))
    (let* ((tree (org-element-parse-buffer))
           (top-level (org-element-map tree 'headline
                        (lambda (el) (and (= (org-element-property :level el) 1) el)) nil t))
           (subheading (org-element-map tree 'headline
                         (lambda (el) (and (= (org-element-property :level el) 2) el)) nil t))
           (subsubheading (org-element-map tree 'headline
                            (lambda (el) (and (= (org-element-property :level el) 3) el)) nil t)))
      (should (leafy-is-ancestor top-level subheading))
      (should (leafy-is-ancestor top-level subsubheading))
      (should (leafy-is-ancestor subheading subsubheading))
      (should-not (leafy-is-ancestor subheading top-level))
      (should-not (leafy-is-ancestor subsubheading top-level))
      (should-not (leafy-is-ancestor subsubheading subheading)))))

(defun leafy-headline-get-property-drawer (headline)
  "Gets the property drawer for a headline"
  (let ((candidate (org-element-map headline '(property-drawer headline) (lambda (x) (unless (eq x headline) x)) nil t)))
    (when (eq (org-element-type candidate) 'property-drawer)
      candidate)))

(ert-deftest test-leafy-headline-property-drawer ()
  "Test if leafy-headline-property-drawer fetches the property drawer under a headline."
  (with-temp-buffer
    (org-mode)
    (insert "* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:
")
    (goto-char (point-min))
    (let* ((headline (org-element-map (org-element-parse-buffer) 'headline 'identity nil t))
           (property-drawer (leafy-headline-get-property-drawer headline)))
      (should (eq (org-element-type property-drawer) 'property-drawer))
      (should (equal (org-element-property :key (car (org-element-contents property-drawer))) "CONTEXT-PRIORITY"))
      (should (equal (org-element-property :value (car (org-element-contents property-drawer))) "3")))))

(defun leafy-headline-get-or-create-property-drawer (headline)
  "Gets the property drawer for a headline, creating it if necessary."
  (let ((drawer (leafy-headline-get-property-drawer headline))
        (headline-begin (org-element-property :begin headline)))
    (when (null drawer)
      (save-excursion
        (goto-char headline-begin)
        (org-insert-property-drawer))
      (setq headline (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (hl)
                         (when (= (org-element-property :begin hl) headline-begin)
                           hl))
                       nil t))
      (setq drawer (leafy-headline-get-property-drawer headline)))
    drawer))

(defun leafy-get-buffer-contents ()
  "Return the entire contents of the current buffer as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest test-leafy-headline-get-or-create-property-drawer ()
  "Test if leafy-headline-property-drawer creates a new property drawer if one doesn't exist."
  (with-temp-buffer
    (org-mode)
    (insert "* Top-level heading\n")
    (goto-char (point-min))
    (let* ((headline (org-element-map (org-element-parse-buffer) 'headline 'identity nil t))
           (property-drawer (leafy-headline-get-or-create-property-drawer headline)))
      (should (eq (org-element-type property-drawer) 'property-drawer)))))

(ert-deftest test-leafy-headline-get-or-create-property-drawer-no-drawer-with-child-drawer ()
  "Test if leafy-headline-get-or-create-property-drawer handles a headline with no property drawer that has a headline with a property drawer as child."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent headline
** Child headline
:PROPERTIES:
:CONTEXT-PRIORITY: 5
:END:
")
    (goto-char (point-min))
    (let* ((headlines (org-element-map (org-element-parse-buffer) 'headline 'identity))
           (parent-headline (nth 0 headlines))
           (child-headline (nth 1 headlines))
           (parent-drawer (leafy-headline-get-or-create-property-drawer parent-headline))
           (child-drawer (leafy-headline-get-or-create-property-drawer child-headline)))
      (should (eq (org-element-type parent-drawer) 'property-drawer))
      (should (eq (org-element-type child-drawer) 'property-drawer))
      (should (equal (org-element-property :key (car (org-element-contents child-drawer))) "CONTEXT-PRIORITY"))
      (should (equal (org-element-property :value (car (org-element-contents child-drawer))) "5")))))

(defun leafy-headline-to-alist (headline)
  "Convert a headline with a property drawer into an alist of key/value pairs."
  (let ((properties '()))
    (push (cons :headline headline) properties)
    
    (let* ((property-drawer (leafy-headline-get-property-drawer headline)))
      (when property-drawer
	(org-element-map property-drawer 'node-property
          (lambda (node-property)
            (let ((key (org-element-property :key node-property))
                  (value (org-element-property :value node-property)))
              (push (cons (intern key) value) properties))))))
    properties))

(ert-deftest test-leafy-headline-to-alist ()
  (let* ((headline '(headline (:raw-value "Top-level heading" :begin 2 :end 97 :pre-blank 0 :contents-begin 22 :contents-end 93 :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 1 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 2 :PRIORITY "3" :title ("Top-level heading"))
                              (section (:begin 22 :end 60 :contents-begin 22 :contents-end 60 :post-blank 0 :post-affiliated 22)
                                       (property-drawer (:begin 22 :end 60 :contents-begin 37 :contents-end 52 :post-blank 0 :post-affiliated 22)
                                                        (node-property (:key "PRIORITY" :value "3" :begin 37 :end 52 :post-blank 0 :post-affiliated 37))))))
         (alist (leafy-headline-to-alist headline))
         (expected `((PRIORITY . "3")
		     (:headline . ,headline))))
    (should (equal alist expected))))

(ert-deftest test-leafy-headline-to-alist-2 ()
  "Test if leafy-headline-to-alist combines properties from multiple headlines into the same alist."
  (with-temp-buffer
    (org-mode)
    (insert "
* Top-level heading
:PROPERTIES:
:CUSTOM-A: 1
:END:
** Subheading
:PROPERTIES:
:CUSTOM-B: 2
:END:
")
    (goto-char (point-min))
    (let* ((tree (org-element-parse-buffer))
           (headlines (org-element-map tree 'headline 'identity))
           (top-level-alist (leafy-headline-to-alist (nth 0 headlines)))
           (subheading-alist (leafy-headline-to-alist (nth 1 headlines))))
      (should (equal "1" (alist-get 'CUSTOM-A top-level-alist)))
      (should (null (alist-get 'CUSTOM-B top-level-alist)))
      (should (equal "2" (alist-get 'CUSTOM-B subheading-alist)))
      (should (null (alist-get 'CUSTOM-A subheading-alist))))))


(defun leafy-get-property-drawer (headline)
  "Get the property drawer underneath the given HEADLINE element."
  (let ((first-drawer (org-element-map headline 'property-drawer #'identity nil t)))
    (when (leafy-is-ancestor headline first-drawer)
      first-drawer)))

(defun leafy-get-org-element-pom (element)
  (org-element-property :begin element))

(defun leafy-headline-get-property-drawer-pom (element)
  (leafy-get-org-element-pom (leafy-headline-get-property-drawer element)))
  ;;(leafy-get-org-element-pom element))

(ert-deftest test-leafy-get-org-element-pom ()
  (with-temp-buffer
    (org-mode)
    (insert "* Test Headline\n")
    (let* ((org-data (org-element-parse-buffer))
           (headline (org-element-map org-data 'headline #'identity nil t)))
      (should (equal (leafy-get-org-element-pom headline) 1)))))

(ert-deftest test-leafy-get-property-drawer-pom ()
  (with-temp-buffer
    (org-mode)
    (insert "* Test Headline\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :PRIORITY: A\n")
    (insert "  :END:\n")
    (let* ((org-data (org-element-parse-buffer))
           (headline (org-element-map org-data 'headline #'identity nil t))
           (property-drawer-pom (leafy-headline-get-property-drawer-pom headline)))
      (should (equal property-drawer-pom 17)))))

(ert-deftest test-leafy-get-property-drawer-pom-2 ()
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
	     (let ((pom (leafy-headline-get-property-drawer-pom (alist-get :headline props))))
	       (should (= 22 pom))
	       (should (equal "3" (org-entry-get pom "CONTEXT-PRIORITY" t))))
	     )
	    (`(2 ,props ,tag ,title ,content)
	     (should (null (leafy-headline-get-property-drawer-pom (alist-get :headline props)))))
	    (`(3 ,props ,tag ,title ,content)
	     (should (null (leafy-headline-get-property-drawer-pom (alist-get :headline props)))))))))))

(ert-deftest test-org-entry-get-with-hardcoded-pom ()
  (with-temp-buffer
    (insert "
* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:
** Subheading
*** Sub-subheading
    ")
    (org-mode)
    (let ((pom 22)) ; Hardcoded pom (one character before the property drawer)
      (save-excursion
	(goto-char pom)
	(should (equal "3" (org-entry-get pom "CONTEXT-PRIORITY")))))))

(ert-deftest test-org-entry-get-with-headline-pom ()
  (with-temp-buffer
    (insert "
* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:
** Subheading
*** Sub-subheading
:PROPERTIES:
:CONTEXT-PRIORITY: 5
:END:
    ")
    (org-mode)
    (let ((pom (org-find-exact-headline-in-buffer "Top-level heading")))
      (save-excursion
	(goto-char pom)
	(should (equal "3" (org-entry-get pom "CONTEXT-PRIORITY")))))
    (let ((pom (org-find-exact-headline-in-buffer "Subheading")))
      (save-excursion
	(goto-char pom)
	(should (null (org-entry-get pom "CONTEXT-PRIORITY")))))
    (let ((pom (org-find-exact-headline-in-buffer "Sub-subheading")))
      (save-excursion
	(goto-char pom)
	(should (equal "5" (org-entry-get pom "CONTEXT-PRIORITY")))))
    ))

(ert-deftest test-leafy-get-custom-properties ()
  (with-temp-buffer
    (insert "
* Top-level heading
:PROPERTIES:
:LEAFY-PRIORITY: 3
:INPUT-TOKENS: 100
:OUTPUT-TOKENS: 50
:END:
** Subheading
*** Sub-subheading
    ")
    (org-mode)
    (goto-char (point-min))
    (let ((sections (leafy-get-sections)))
      (ert-info ((format "Sections: %S" sections))
	(should (equal 3 (length sections))))
      (dolist (section sections)
	(ert-info ((format "Section: %S" section))
	  (pcase section
	    (`(1 ,props ,tag ,title ,content)
	     (should (equal "3" (alist-get 'LEAFY-PRIORITY props)))
	     (should (equal "100" (alist-get 'INPUT-TOKENS props)))
	     (should (equal "50" (alist-get 'OUTPUT-TOKENS props))))
	    (`(2 ,props ,tag ,title ,content)
	     (should (= 1 (length props))))
	    (`(3 ,props ,tag ,title ,content)
	     (should (= 1 (length props))))))))))

(defun leafy-get-key (props key &optional default)
  (save-excursion
    (let* ((headline (alist-get :headline props))
	   (pom (leafy-headline-get-property-drawer-pom headline))
	   )
      (or (unless (null pom)
	    (goto-char pom)
	    (org-entry-get pom key))
	  default))))

(defun leafy-set-key (props key value)
  (save-excursion
    (let* ((headline (alist-get :headline props))
           (property-drawer-element (leafy-headline-get-or-create-property-drawer headline))
           (pom (org-element-property :begin property-drawer-element)))
      (unless (null pom)
        (goto-char pom))
      (when (null pom)
        (user-error "leafy-set-key: Null pom: <%S>" headline))
      (org-entry-put pom key value)
      (setf (alist-get :headline props) (or (leafy-reparse-element headline)
					    (user-error "leafy-set-key: Unable to reparse headline")))
      )))

(ert-deftest test-leafy-set-key ()
  "Test if leafy-set-key sets the value of a key in the property drawer."
  (with-temp-buffer
    (org-mode)
    (insert "* Test headline
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:
")
    (goto-char (point-min))
    (let* ((headline (org-element-map (org-element-parse-buffer) 'headline 'identity nil t))
           (props (leafy-headline-to-alist headline)))
      ;; Test setting an existing key
      (leafy-set-key props "CONTEXT-PRIORITY" "5")
      (should (equal (leafy-get-key props "CONTEXT-PRIORITY") "5"))

      ;; Test setting a new key
      (leafy-set-key props "NEW-KEY" "42")
      (should (equal (leafy-get-key props "NEW-KEY") "42")))))

(ert-deftest test-leafy-set-key-multiple-sections ()
  "Test if leafy-set-key sets the property value correctly for multiple sections."
  (cl-flet ((test-case (org-content section-id key initial-value new-value)
                       (with-temp-buffer
                         (org-mode)
                         (insert org-content)
                         (goto-char (point-min))
                         (let* ((sections (leafy-get-sections))
                                (section (nth (- section-id 1) sections)))
                           (pcase section
                             (`(,_level ,props ,_tag ,_title ,_content)
			      (ert-info ((format "section-id:%S" section-id))
				(should (equal initial-value (leafy-get-key props key)))
				(leafy-set-key props key new-value)
				(should (equal new-value (leafy-get-key props key))))))))))
    ;; Case 1: Top-level heading
    (test-case "* Top-level heading
:PROPERTIES:
:CONTEXT-PRIORITY: 3
:END:"
               1 "CONTEXT-PRIORITY" "3" "10")
    ;; Case 2: Subheading with no property drawer
    (test-case "* Top-level heading
** Subheading"
               2 "CONTEXT-PRIORITY" nil "")
    ;; Case 3: Sub-subheading
    (test-case "* Top-level heading
** Subheading
*** Sub-subheading
:PROPERTIES:
:CONTEXT-PRIORITY: 5
:END:"
               3 "CONTEXT-PRIORITY" "5" "15")))

(defun leafy-reparse-element (element)
  "Reparses the buffer and returns the same element after re-parsing."
  (let ((element-type (org-element-type element))
        (element-begin (org-element-property :begin element))
        (parsed-tree (org-element-parse-buffer)))
    (org-element-map parsed-tree element-type
      (lambda (e)
        (when (= (org-element-property :begin e) element-begin)
          e))
      nil t)))

(ert-deftest test-leafy-reparse-element ()
  "Test leafy-reparse-element function."
  (with-temp-buffer
    (org-mode)
    (insert "* Test headline")
    (goto-char (point-min))
    (let* ((parsed-tree (org-element-parse-buffer))
           (headline (org-element-map parsed-tree 'headline #'identity nil t))
           (reparsed-headline (leafy-reparse-element headline)))
      (should (eq (org-element-type headline) (org-element-type reparsed-headline)))
      (should (= (org-element-property :begin headline)
                 (org-element-property :begin reparsed-headline))))))

(defun leafy-section-body (section)
  "Return the content of a SECTION element as a string."
  (let* ((elements (org-element-contents section))
         (paragraphs (seq-filter (lambda (el) (eq (org-element-type el) 'paragraph)) elements)))
    (apply 'concat
           (mapcar
            (lambda (paragraph)
              (org-no-properties
               (org-element-interpret-data paragraph)))
            paragraphs))))

(ert-deftest test-leafy-section-body ()
  "Test if leafy-section-body returns the correct content of a section."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nSection content\nAnother line\n** Subheading\nSubheading content")
    (goto-char (point-min))
    (let* ((headline (org-element-map (org-element-parse-buffer) 'headline 'identity nil t))
           (section (org-element-map headline 'section 'identity nil t))
           (content (leafy-section-body section)))
      (should (equal content "Section content\nAnother line\n")))))

(defun leafy-section-body (section)
  "Extract the content of a section, excluding content from nested headlines."
  (let* ((element section)
         (paragraphs (org-element-map element 'paragraph 'identity nil nil 'headline)))
    (apply 'concat
           (mapcar (lambda (paragraph)
                     (org-no-properties (org-element-interpret-data paragraph)))
                   paragraphs))))
(defun leafy-section-body (section)
  "Extract the content of a section, excluding content from nested headlines."
  (let* ((element section)
         (content (buffer-substring-no-properties
                   (org-element-property :contents-begin element)
                   (or (org-element-property :contents-end element) (point-max))))
         (content-lines (split-string content "\n"))
         (filtered-content-lines (seq-take-while (lambda (line) (not (string-match-p "^\\*\\{1,\\}" line))) content-lines)))
    (mapconcat 'identity filtered-content-lines "\n")))
(defun leafy-section-body (section)
  "Extract the content of a section, excluding content from nested headlines."
  (let* ((element section)
         (section-end (or (org-element-property :contents-end element) (point-max)))
         (content-elements (org-element-contents element))
         (filtered-elements (seq-take-while
                             (lambda (el) (and (not (eq (org-element-type el) 'headline))
                                               (< (org-element-property :begin el) section-end)))
                             content-elements)))
    (org-element-interpret-data filtered-elements)))

(defun leafy-section-body (section)
  "Extract the content of a section, excluding content from nested headlines."
  (let ((paragraphs (org-element-map section 'paragraph 'identity nil nil t)))
    (apply 'concat
           (mapcar (lambda (paragraph)
                     (org-no-properties (org-element-interpret-data paragraph)))
                   paragraphs))))

(ert-deftest test-leafy-section-body-multiple-sections ()
  "Test if leafy-section-body returns the correct content of multiple sections."
  (with-temp-buffer
    (org-mode)
    (insert "
* Chat
foo
** Project
*** ChatGPT Response :assistant:
aoeu
")
    (goto-char (point-min))
    (let* ((headlines (org-element-map (org-element-parse-buffer) 'headline 'identity))
           (sections (mapcar (lambda (headline)
                               (org-element-map (org-element-contents headline) 'section 'identity nil t 'headline))
                             headlines))
           (contents (mapcar 'leafy-section-body sections))
           (expected '("foo\n" "" "aoeu\n")))
      (should (equal contents expected)))))

(defun leafy-element-properties (element)
  (nth 1 element))

(defun org-element-has-inherited-tag (element tag)
  "Check if the given Org-mode ELEMENT or any of its ancestors has the specified TAG."
  (if (null element)
      nil
    (let ((tags (org-element-property :tags element)))
      (or (member tag tags)
          (org-element-has-inherited-tag (org-element-property :parent element) tag)))))

(provide 'leafy--org-helpers)

(defun leafy-find-named-block (block-name)
  (org-element-map (org-element-parse-buffer) 'src-block
    (lambda (src-block)
      (when (string= (org-element-property :name src-block) block-name)
        src-block))
    ;; Ensure search stops at the first matching block
    nil t))

(defun leafy-get-block-content (block)
  (org-element-property :value block))

(defun leafy-input-from-org-block (block-name)
  (let ((block (leafy-find-named-block block-name)))
    (when block (leafy-get-block-content block))))
