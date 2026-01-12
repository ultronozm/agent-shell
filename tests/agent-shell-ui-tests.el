;;; agent-shell-ui-tests.el --- Tests for agent-shell-ui -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Prefer source over stale byte-compiled files during local test runs.
(setq load-prefer-newer t)

(require 'agent-shell-ui)

;;; Code:

(cl-defun agent-shell-ui-tests--render-chunks (chunks &key force-rebuild expanded)
  "Render CHUNKS into a single fragment and return buffer text.

When FORCE-REBUILD is non-nil, disable the fast append path so updates always
rebuild the fragment (matching the original O(n²) behavior).

When EXPANDED is non-nil, create the fragment expanded; when nil, collapsed."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R")
           (first (car chunks))
           (rest (cdr chunks))
           (render
            (lambda ()
              (agent-shell-ui-update-fragment
               (agent-shell-ui-make-fragment-model
                :namespace-id namespace-id
                :block-id block-id
                :label-left label-left
                :label-right label-right
                :body first)
               :append t
               :create-new t
               :expanded expanded
               :navigation 'never
               :no-undo t)
              (dolist (chunk rest)
                (agent-shell-ui-update-fragment
                 (agent-shell-ui-make-fragment-model
                  :namespace-id namespace-id
                  :block-id block-id
                  :label-left label-left
                  :label-right label-right
                  :body chunk)
                 :append t
                 :create-new nil
                 :navigation 'never
                 :no-undo t)))))
      (if force-rebuild
          (cl-letf (((symbol-function 'agent-shell-ui--append-fragment-body)
                     (lambda (&rest _args) nil)))
            (funcall render))
        (funcall render))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun agent-shell-ui-tests--render-chunks-stepwise (chunks &key force-rebuild expanded)
  "Render CHUNKS, returning a list of buffer texts after each update.

When FORCE-REBUILD is non-nil, disable the fast append path so each update
rebuilds the fragment (matching the original O(n^2) behavior)."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R")
           (states nil)
           (render
            (lambda ()
              (agent-shell-ui-update-fragment
               (agent-shell-ui-make-fragment-model
                :namespace-id namespace-id
                :block-id block-id
                :label-left label-left
                :label-right label-right
                :body (car chunks))
               :append t
               :create-new t
               :expanded expanded
               :navigation 'never
               :no-undo t)
              (push (buffer-substring-no-properties (point-min) (point-max)) states)
              (dolist (chunk (cdr chunks))
                (agent-shell-ui-update-fragment
                 (agent-shell-ui-make-fragment-model
                  :namespace-id namespace-id
                  :block-id block-id
                  :label-left label-left
                  :label-right label-right
                  :body chunk)
                 :append t
                 :create-new nil
                 :navigation 'never
                 :no-undo t)
                (push (buffer-substring-no-properties (point-min) (point-max)) states))
              (nreverse states))))
      (if force-rebuild
          (cl-letf (((symbol-function 'agent-shell-ui--append-fragment-body)
                     (lambda (&rest _args) nil)))
            (funcall render))
        (funcall render)))))

(defun agent-shell-ui-tests--content-store-value ()
  "Return the stored body value for the test fragment."
  (gethash "ns-b-body" agent-shell-ui--content-store))

(ert-deftest agent-shell-ui-normalize-body-test ()
  (should (equal (agent-shell-ui--normalize-body nil) nil))
  (should (equal (agent-shell-ui--normalize-body "") ""))
  (should (equal (agent-shell-ui--normalize-body "a") "a"))
  (should (equal (agent-shell-ui--normalize-body "a\n") "a\n"))
  (should (equal (agent-shell-ui--normalize-body "a\n\n") "a\n\n"))
  (should (equal (agent-shell-ui--normalize-body "a\n\n\n") "a\n\n")))

(ert-deftest agent-shell-ui-chunk-insert-text-mid-line-test ()
  (let ((chunk (agent-shell-ui--normalize-body " world")))
    ;; Appending mid-line should not introduce an extra indentation boundary.
    (should (equal (agent-shell-ui--chunk-insert-text chunk nil) " world"))
    ;; Appending at line start should include the indentation prefix.
    (should (equal (agent-shell-ui--chunk-insert-text chunk t) "   world"))))

(ert-deftest agent-shell-ui-streaming-append-matches-rebuild-test ()
  "Fast append should be text-identical to rebuild behavior."
  (let* ((chunks '("Hello"
                   " world"
                   "!\n\n"
                   "New para\n"
                   "\n"
                   "Indented:\n  - item\n"
                   "\nDone\n"))
         (fast (agent-shell-ui-tests--render-chunks chunks :expanded t))
         (rebuild (agent-shell-ui-tests--render-chunks chunks :force-rebuild t :expanded t)))
    (should (equal fast rebuild))))

(ert-deftest agent-shell-ui-streaming-append-stepwise-matches-rebuild-test ()
  "Fast append should match rebuild output after each chunk."
  (let* ((chunks '("Hello\n\n" "\n" "World"))
         (fast (agent-shell-ui-tests--render-chunks-stepwise chunks :expanded t))
         (rebuild (agent-shell-ui-tests--render-chunks-stepwise chunks :force-rebuild t :expanded t)))
    (should (equal fast rebuild))))

(ert-deftest agent-shell-ui-content-store-is-chunked-on-fast-append-test ()
  "Fast append should store the body in chunk form to avoid O(n²) concat."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body "Hello")
       :append t
       :create-new t
       :expanded t
       :navigation 'never
       :no-undo t)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body " world")
       :append t
       :create-new nil
       :navigation 'never
       :no-undo t)
      (let ((val (agent-shell-ui-tests--content-store-value)))
        (should (consp val))
        (should (eq (car val) 'chunks))))))

(ert-deftest agent-shell-ui-content-store-get-body-materializes-test ()
  "Materializing chunked content should preserve the concatenated body."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R")
           (chunks '("Hello" " world" "!\n\n" "Done\n")))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body (car chunks))
       :append t :create-new t :expanded t :navigation 'never :no-undo t)
      (dolist (chunk (cdr chunks))
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id namespace-id
          :block-id block-id
          :label-left label-left
          :label-right label-right
          :body chunk)
         :append t :create-new nil :navigation 'never :no-undo t))
      (let* ((expected (apply #'concat (mapcar #'agent-shell-ui--normalize-body chunks)))
             (body (agent-shell-ui--content-store-get-body "ns-b")))
        (should (equal body expected))
        ;; After materialization, the value should be cached as a string.
        (should (stringp (agent-shell-ui-tests--content-store-value)))))))

(ert-deftest agent-shell-ui-fast-streaming-append-toggle-disables-chunking-test ()
  "Disabling fast streaming append should fall back to rebuild behavior."
  (with-temp-buffer
    (let* ((agent-shell-ui-fast-streaming-append nil)
           (namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body "Hello")
       :append t :create-new t :expanded t :navigation 'never :no-undo t)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body " world")
       :append t :create-new nil :expanded t :navigation 'never :no-undo t)
      ;; Rebuild path stores full body as a string (not chunked).
      (should (stringp (agent-shell-ui-tests--content-store-value))))))

(ert-deftest agent-shell-ui-fast-streaming-append-falls-back-when-body-missing-test ()
  "Fast append should fall back cleanly if the fragment has no body yet."
  (with-temp-buffer
    (let* ((agent-shell-ui-fast-streaming-append t)
           (namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R"))
      ;; Create a block with labels but no body yet (placeholder indicator space).
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body nil)
       :append nil :create-new t :expanded t :navigation 'always :no-undo t)
      ;; Now stream first body chunk. This should rebuild (no existing body range),
      ;; and we should end up with a real body section containing the chunk.
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body "Hello")
       :append t :create-new nil :expanded t :navigation 'always :no-undo t)
      (let* ((block (agent-shell-ui--block-range :position (point-min)))
             (body (agent-shell-ui--nearest-range-matching-property
                    :property 'agent-shell-ui-section :value 'body
                    :from (map-elt block :start)
                    :to (map-elt block :end))))
        (should body)
        (goto-char (map-elt body :start))
        (should (search-forward "Hello" (map-elt body :end) t))))))

(ert-deftest agent-shell-ui-append-respects-collapsed-invisibility-test ()
  "Appended text should remain invisible when the fragment is collapsed."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R"))
      ;; Start collapsed.
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body "Hello")
       :append t :create-new t :expanded nil :navigation 'always :no-undo t)
      ;; Append.
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body " world")
       :append t :create-new nil :navigation 'always :no-undo t)
      ;; Body end should still be invisible.
      (let* ((block (agent-shell-ui--block-range :position (point-min)))
             (body (agent-shell-ui--nearest-range-matching-property
                    :property 'agent-shell-ui-section :value 'body
                    :from (map-elt block :start)
                    :to (map-elt block :end))))
        (should body)
        (should (eq (get-text-property (map-elt body :start) 'invisible) t))
        (should (eq (get-text-property (1- (map-elt body :end)) 'invisible) t)))
      ;; Toggle open from any position inside the block, then ensure body is visible.
      (goto-char (point-min))
      (let ((block-pos (when-let* ((match (text-property-search-forward
                                           'agent-shell-ui-state nil
                                           (lambda (_old-val new-val) new-val)
                                           t)))
                         (prop-match-beginning match))))
        (should (integerp block-pos))
        (goto-char block-pos)
        (agent-shell-ui-toggle-fragment-at-point))
      (let* ((block (agent-shell-ui--block-range :position (point-min)))
             (body (agent-shell-ui--nearest-range-matching-property
                    :property 'agent-shell-ui-section :value 'body
                    :from (map-elt block :start)
                    :to (map-elt block :end))))
        (should body)
        (should (null (get-text-property (map-elt body :start) 'invisible)))
        ;; Trailing whitespace may remain invisible even when expanded, so
        ;; check the last non-whitespace character instead.
        (save-excursion
          (goto-char (map-elt body :end))
          (should (re-search-backward "[^ \t\n]" (map-elt body :start) t))
          (should (null (get-text-property (point) 'invisible))))))))

(ert-deftest agent-shell-ui-append-to-non-collapsable-fragment-is-visible-test ()
  "Appended text should remain visible when the fragment cannot be collapsed."
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :body "Hello")
       :append t :create-new t :expanded nil :navigation 'never :no-undo t)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :body " world")
       :append t :create-new nil :navigation 'never :no-undo t)
      (let* ((block (agent-shell-ui--block-range :position (point-min)))
             (body (agent-shell-ui--nearest-range-matching-property
                    :property 'agent-shell-ui-section :value 'body
                    :from (map-elt block :start)
                    :to (map-elt block :end))))
        (should body)
        (goto-char (map-elt body :start))
        (should (search-forward "world" (map-elt body :end) t))
        (let ((pos (- (point) (length "world"))))
          (should (null (get-text-property pos 'invisible))))))))

(ert-deftest agent-shell-ui-delete-fragment-cleans-content-store-test ()
  (with-temp-buffer
    (let* ((namespace-id "ns")
           (block-id "b")
           (label-left "L")
           (label-right "R"))
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body "Hello")
       :append t :create-new t :expanded t :navigation 'never :no-undo t)
      (agent-shell-ui-update-fragment
       (agent-shell-ui-make-fragment-model
        :namespace-id namespace-id
        :block-id block-id
        :label-left label-left
        :label-right label-right
        :body " world")
       :append t :create-new nil :navigation 'never :no-undo t)
      (should (agent-shell-ui-tests--content-store-value))
      (agent-shell-ui-delete-fragment :namespace-id namespace-id :block-id block-id :no-undo t)
      (should (null (agent-shell-ui-tests--content-store-value))))))

(provide 'agent-shell-ui-tests)
;;; agent-shell-ui-tests.el ends here
