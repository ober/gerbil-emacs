;;; -*- Gerbil -*-
;;; Org list handling: ordered/unordered, checkboxes, statistics, bullet cycling.
;;; Backend-agnostic (Scintilla API only, no Qt imports).

(export #t)

(import :std/sugar
        (only-in :std/srfi/13
                 string-trim string-contains string-prefix? string-join
                 string-pad-right)
        :std/pregexp
        :std/misc/string
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-emacs/core
        :gerbil-emacs/org-parse)

;;;============================================================================
;;; List Item Detection
;;;============================================================================

(def (org-list-item? str)
  "Detect if string is a list item. Returns (values type indent marker) or (values #f #f #f).
Types: 'unordered, 'ordered, 'checkbox-unchecked, 'checkbox-checked, 'description."
  (let ((trimmed str)
        (indent (org-count-leading-spaces str)))
    (cond
      ;; Checkbox: - [ ] or - [X] or - [x]
      ((pregexp-match "^(\\s*)([-+*])\\s+\\[([ Xx])\\]\\s" str)
       => (lambda (m)
            (let ((marker (list-ref m 2))
                  (check (list-ref m 3)))
              (if (or (string=? check "X") (string=? check "x"))
                (values 'checkbox-checked indent marker)
                (values 'checkbox-unchecked indent marker)))))
      ;; Unordered: - item, + item, * item (but not heading)
      ((pregexp-match "^(\\s+)([-+*])\\s" str)
       => (lambda (m)
            (values 'unordered indent (list-ref m 2))))
      ;; Unordered at col 0 with - or + (not * which is heading)
      ((pregexp-match "^([-+])\\s" str)
       => (lambda (m)
            (values 'unordered 0 (list-ref m 1))))
      ;; Ordered: 1. item or 1) item
      ((pregexp-match "^(\\s*)(\\d+)([.)]) " str)
       => (lambda (m)
            (values 'ordered indent (string-append (list-ref m 2) (list-ref m 3)))))
      ;; Description: - term :: description
      ((pregexp-match "^(\\s*)([-+*])\\s+.+\\s+::\\s" str)
       => (lambda (m)
            (values 'description indent (list-ref m 2))))
      (else (values #f #f #f)))))

(def (org-count-leading-spaces str)
  "Count leading space characters."
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (and (< i len) (char=? (string-ref str i) #\space))
        (loop (+ i 1))
        i))))

;;;============================================================================
;;; M-RET: Context-Sensitive New Item
;;;============================================================================

(def (org-meta-return ed)
  "Context-sensitive M-RET: on heading -> new heading, on list item -> new item."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line (editor-get-line ed line-num)))
    (cond
      ;; On a heading: insert new heading at same level after subtree
      ((org-heading-line? line)
       (let* ((level (org-heading-stars-of-line line))
              (text (editor-get-text ed))
              (lines (string-split text #\newline))
              (subtree-end (org-find-subtree-end-in-text lines line-num level))
              (insert-pos (if (>= subtree-end (length lines))
                            (editor-get-text-length ed)
                            (editor-position-from-line ed subtree-end)))
              (new-heading (string-append "\n" (make-string level #\*) " ")))
         (editor-insert-text ed insert-pos new-heading)
         (editor-goto-pos ed (+ insert-pos (string-length new-heading)))))
      ;; On a list item: insert new item at same level
      (else
       (let-values (((type indent marker) (org-list-item? line)))
         (if (not type)
           ;; Not on a list: just insert newline
           (let ((eol (send-message ed SCI_GETLINEENDPOSITION line-num)))
             (editor-insert-text ed eol "\n")
             (editor-goto-pos ed (+ eol 1)))
           ;; On a list item
           (let* ((eol (send-message ed SCI_GETLINEENDPOSITION line-num))
                  (prefix (cond
                            ((eq? type 'checkbox-unchecked)
                             (string-append (make-string indent #\space) marker " [ ] "))
                            ((eq? type 'checkbox-checked)
                             (string-append (make-string indent #\space) marker " [ ] "))
                            ((eq? type 'ordered)
                             (let ((next-num (+ (or (string->number
                                                      (pregexp-replace "[.)]" marker "")) 1) 1))
                                   (suffix (if (string-contains marker ")")
                                             ")" ".")))
                               (string-append (make-string indent #\space)
                                              (number->string next-num) suffix " ")))
                            (else
                             (string-append (make-string indent #\space) marker " "))))
                  (new-text (string-append "\n" prefix)))
             (editor-insert-text ed eol new-text)
             (editor-goto-pos ed (+ eol (string-length new-text))))))))))

;;;============================================================================
;;; Bullet Cycling (C-c -)
;;;============================================================================

(def *org-bullet-cycle* '("-" "+" "*" "1." "1)"))

(def (org-cycle-list-bullet ed)
  "Cycle the list bullet at current line: - -> + -> * -> 1. -> 1) -> -"
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line (editor-get-line ed line-num)))
    (let-values (((type indent marker) (org-list-item? line)))
      (when type
        (let* ((cur-bullet (cond
                             ((eq? type 'ordered)
                              (let ((m (pregexp-match "\\d+([.)])" marker)))
                                (if m (string-append "1" (list-ref m 1)) "1.")))
                             (else marker)))
               (next-bullet (org-next-bullet cur-bullet))
               ;; Build replacement text for just the bullet
               (line-start (editor-position-from-line ed line-num))
               (line-end (send-message ed SCI_GETLINEENDPOSITION line-num))
               (new-line (org-replace-bullet line marker next-bullet type)))
          (send-message ed SCI_SETTARGETSTART line-start)
          (send-message ed SCI_SETTARGETEND line-end)
          (send-message/string ed SCI_REPLACETARGET new-line))))))

(def (org-next-bullet current)
  "Get next bullet in cycle."
  (let loop ((cycle *org-bullet-cycle*))
    (cond
      ((null? cycle) "-")  ; default
      ((string=? (car cycle) current)
       (if (null? (cdr cycle))
         (car *org-bullet-cycle*)
         (cadr cycle)))
      (else (loop (cdr cycle))))))

(def (org-replace-bullet line old-marker new-bullet type)
  "Replace bullet in a line, handling ordered number conversion."
  (cond
    ((eq? type 'ordered)
     ;; Replace "N." or "N)" with new bullet
     (pregexp-replace "\\d+[.)]" line new-bullet))
    (else
     ;; Replace single char bullet
     (let ((m (pregexp-match "^(\\s*)([-+*])(\\s)" line)))
       (if m
         (string-append (list-ref m 1) new-bullet (list-ref m 3)
                        (substring line (string-length (list-ref m 0))
                                   (string-length line)))
         line)))))

;;;============================================================================
;;; List Indentation
;;;============================================================================

(def (org-indent-list-item ed direction)
  "Indent (direction=1) or dedent (direction=-1) list item at point."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line (editor-get-line ed line-num)))
    (let-values (((type indent marker) (org-list-item? line)))
      (when type
        (let* ((new-indent (max 0 (+ indent (* direction 2))))
               (line-start (editor-position-from-line ed line-num))
               (line-end (send-message ed SCI_GETLINEENDPOSITION line-num))
               ;; Remove old indent, add new
               (stripped (string-trim line))
               (new-line (string-append (make-string new-indent #\space) stripped)))
          (send-message ed SCI_SETTARGETSTART line-start)
          (send-message ed SCI_SETTARGETEND line-end)
          (send-message/string ed SCI_REPLACETARGET new-line))))))

;;;============================================================================
;;; Ordered List Renumbering
;;;============================================================================

(def (org-renumber-list! ed)
  "Renumber ordered list items starting from current line's list."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (total (editor-get-line-count ed)))
    ;; Find start of list (walk backward while lines are list items at same indent)
    (let* ((cur-line (editor-get-line ed line-num))
           (cur-indent (org-count-leading-spaces cur-line)))
      (let ((start (let loop ((i line-num))
                     (if (and (>= i 0)
                              (let-values (((type indent marker) (org-list-item? (editor-get-line ed i))))
                                (and type (= indent cur-indent))))
                       (loop (- i 1))
                       (+ i 1)))))
        ;; Renumber from start
        (let loop ((i start) (num 1))
          (when (< i total)
            (let ((line (editor-get-line ed i)))
              (let-values (((type indent marker) (org-list-item? line)))
                (when (and (eq? type 'ordered) (= indent cur-indent))
                  (let* ((suffix (if (string-contains marker ")") ")" "."))
                         (new-marker (string-append (number->string num) suffix))
                         (new-line (pregexp-replace "\\d+[.)]" line new-marker))
                         (line-start (editor-position-from-line ed i))
                         (line-end (send-message ed SCI_GETLINEENDPOSITION i)))
                    (send-message ed SCI_SETTARGETSTART line-start)
                    (send-message ed SCI_SETTARGETEND line-end)
                    (send-message/string ed SCI_REPLACETARGET new-line)
                    (loop (+ i 1) (+ num 1))))))))))))

;;;============================================================================
;;; Checkbox Statistics
;;;============================================================================

(def (org-update-checkbox-statistics! ed)
  "Update [N/M] and [N%] cookies in parent heading or list item."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (total (editor-get-line-count ed))
         ;; Find parent heading
         (heading-line (let loop ((i line-num))
                         (cond
                           ((< i 0) #f)
                           ((org-heading-line? (editor-get-line ed i)) i)
                           (else (loop (- i 1)))))))
    (when heading-line
      (let ((heading (editor-get-line ed heading-line)))
        ;; Only update if heading contains a cookie [/] or [%]
        (when (or (string-contains heading "[/]")
                  (string-contains heading "[%]")
                  (pregexp-match "\\[\\d+/\\d+\\]" heading)
                  (pregexp-match "\\[\\d+%\\]" heading))
          ;; Count checkboxes in children
          (let* ((level (org-heading-stars-of-line heading))
                 (end-line (let ((lines (string-split (editor-get-text ed) #\newline)))
                             (org-find-subtree-end-in-text lines heading-line level)))
                 (counts (let loop ((i (+ heading-line 1)) (checked 0) (total-count 0))
                           (if (>= i (min end-line total))
                             (cons checked total-count)
                             (let ((l (editor-get-line ed i)))
                               (cond
                                 ((string-contains l "[X]")
                                  (loop (+ i 1) (+ checked 1) (+ total-count 1)))
                                 ((string-contains l "[ ]")
                                  (loop (+ i 1) checked (+ total-count 1)))
                                 (else (loop (+ i 1) checked total-count)))))))
                 (checked-count (car counts))
                 (total-boxes (cdr counts)))
            ;; Update cookies in the heading
            (let* ((new-heading heading)
                   ;; Update [N/M] cookie
                   (new-heading (pregexp-replace
                                 "\\[\\d*/\\d*\\]"
                                 new-heading
                                 (string-append "[" (number->string checked-count)
                                                "/" (number->string total-boxes) "]")))
                   ;; Update [N%] cookie
                   (new-heading (pregexp-replace
                                 "\\[\\d*%\\]"
                                 new-heading
                                 (string-append "["
                                                (number->string
                                                 (if (= total-boxes 0) 0
                                                   (quotient (* 100 checked-count) total-boxes)))
                                                "%]")))
                   (line-start (editor-position-from-line ed heading-line))
                   (line-end (send-message ed SCI_GETLINEENDPOSITION heading-line)))
              (unless (string=? new-heading heading)
                (send-message ed SCI_SETTARGETSTART line-start)
                (send-message ed SCI_SETTARGETEND line-end)
                (send-message/string ed SCI_REPLACETARGET new-heading)))))))))
