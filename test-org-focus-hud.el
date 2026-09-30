;;; test-org-focus-hud.el --- Standalone unit tests for org-focus-hud -*- lexical-binding: t -*-

(setq package-user-dir "/home/saisan/.emacs.d/elpa/31.1/develop")
(package-initialize)
(require 'org)
(require 'org-clock)
(load-file "org-focus-hud.el")

(defvar test-failures 0)

(defun assert-equal (actual expected desc)
  (if (equal actual expected)
      (message "PASS: %s" desc)
    (setq test-failures (1+ test-failures))
    (message "FAIL: %s\n  Expected: %S\n  Actual:   %S" desc expected actual)))

(defun assert-true (val desc)
  (if val
      (message "PASS: %s" desc)
    (setq test-failures (1+ test-failures))
    (message "FAIL: %s (expected non-nil, got %S)" desc val)))

;; TEST 19: Focus HUD Boundary Isolation & Target Heading Resolution
;; ============================================================================
(message "\n--- TEST 19: Focus HUD Boundary Isolation & Target Resolution ---")

(let* ((temp-file (make-temp-file "test-focus-boundary-" nil ".org"))
       (buf (find-file-noselect temp-file))
       (hud-buf (get-buffer-create "*Org Focus HUD*")))
  (unwind-protect
      (with-current-buffer buf
        (org-mode)
        (insert "* TODO Target Task\n:PROPERTIES:\n:EFFORT: 30m\n:END:\n* TODO Neighbor Task\n:LOGBOOK:\n:END:\n")
        (save-buffer)
        (let ((m-target (progn (goto-char (point-min)) (point-marker))))

          ;; 19.1: Adding checklist item 'k' to a task with NO existing checklist items
          ;; Must stay within m-target and NOT leak into Neighbor Task
          (with-current-buffer hud-buf
            (org-focus-hud-mode)
            (setq org-focus-hud--target-marker m-target)
            (org-focus-hud-refresh)
            (org-focus-hud-add-checklist "First Target Checklist"))

          (with-current-buffer buf
            (let ((content (buffer-string)))
              (assert-true (string-match "\\* TODO Target Task\n:PROPERTIES:\n:EFFORT: 30m\n:END:\n  - \\[ \\] First Target Checklist\n\\* TODO Neighbor Task" content)
                           "Test 19.1: Checklist item inserted into Target Task without leaking into Neighbor Task")))

          ;; 19.2: Adding note 'n' to Target Task (which has no LOGBOOK)
          ;; Must not leak into Neighbor Task's LOGBOOK
          (with-current-buffer hud-buf
            (org-focus-hud-add-note "Target Quick Note"))
          (with-current-buffer buf
            (let* ((m-neighbor (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward "Neighbor Task")
                                 (org-back-to-heading t)
                                 (point-marker)))
                   (target-notes (org-focus-hud--get-notes m-target))
                   (neighbor-notes (org-focus-hud--get-notes m-neighbor)))
              (assert-equal (length target-notes) 1 "Test 19.2: Exactly 1 note on Target Task")
              (assert-true (string-match-p "Target Quick Note" (car target-notes)) "Test 19.2: Target note content matches")
              (assert-equal (length neighbor-notes) 0 "Test 19.2: Neighbor Task LOGBOOK received 0 notes")))

          ;; 19.3: Adding child subtask 's'
          (with-current-buffer hud-buf
            (org-focus-hud-add-subtask "Target Child Subtask" "15m"))
          (with-current-buffer buf
            (let ((subtasks (org-focus-hud--get-subtasks m-target)))
              (assert-equal (length subtasks) 1 "Test 19.3: Target Task has 1 child subtask")
              (assert-equal (plist-get (car subtasks) :title) "Target Child Subtask" "Test 19.3: Child title matches")))

          ;; 19.4: Adding sibling task 'a'
          (with-current-buffer hud-buf
            (org-focus-hud-add-sibling "Target Sibling Task" "20m"))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (assert-true (re-search-forward "^\\* TODO Target Sibling Task.*:AUTOSCH:" nil t)
                           "Test 19.4: Sibling task inserted at level 1 with :AUTOSCH:"))))

        ;; 19.5: Target Heading Resolution: Invoking focus on an Org heading when another task is clocked
        (let ((clocked-file (make-temp-file "test-focus-clock-" nil ".org"))
              (clocked-buf nil))
          (unwind-protect
              (progn
                (setq clocked-buf (find-file-noselect clocked-file))
                (with-current-buffer clocked-buf
                  (org-mode)
                  (insert "* TODO Background Clocked Task\n")
                  (save-buffer)
                  (goto-char (point-min))
                  (org-clock-in))
                ;; Now, user is visiting buf on Neighbor Task
                (with-current-buffer buf
                  (goto-char (point-min))
                  (re-search-forward "Neighbor Task")
                  ;; Call interactive form resolution
                  (let ((resolved-marker
                         (cond
                          ((and (derived-mode-p 'org-mode)
                                (not (derived-mode-p 'org-agenda-mode))
                                (ignore-errors (save-excursion (org-back-to-heading t) (point-marker)))))
                          ((eq major-mode 'org-agenda-mode)
                           (let ((m (or (org-get-at-bol 'org-marker) (org-get-at-bol 'org-hd-marker))))
                             (and m (markerp m) (marker-buffer m) m)))
                          ((and (fboundp 'org-clock-is-active) (org-clock-is-active)
                                (boundp 'org-clock-marker) (markerp org-clock-marker) (marker-buffer org-clock-marker))
                           (org-with-point-at org-clock-marker
                             (org-back-to-heading t)
                             (point-marker)))
                          (t nil))))
                    (assert-true (and resolved-marker (equal (marker-buffer resolved-marker) buf))
                                 "Test 19.5: Active org-mode buffer heading prioritized over background clocked task")
                    (with-current-buffer (marker-buffer resolved-marker)
                      (org-with-point-at resolved-marker
                        (assert-equal (org-get-heading t t t t) "Neighbor Task"
                                     "Test 19.5: Resolved heading is Neighbor Task"))))))
            (when (fboundp 'org-clock-is-active)
              (when (org-clock-is-active) (org-clock-out nil t)))
            (when (buffer-live-p clocked-buf) (kill-buffer clocked-buf))
            (when (file-exists-p clocked-file) (delete-file clocked-file)))))
    (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p temp-file) (delete-file temp-file))))


;;; ============================================================================
;;; TEST 20: Focus HUD Time Remaining Calculation & Active Clock Detection
;;; ============================================================================
(message "\n--- TEST 20: Focus HUD Time Remaining & Active Clock Detection ---")
(let* ((temp-file (make-temp-file "org-test-focus-clock-" nil ".org"))
       (buf (find-file-noselect temp-file)))
  (unwind-protect
      (with-current-buffer buf
        (org-mode)
        ;; Task 1: 4-hour task (effort 240m), scheduled 2 days in the future (e.g. 14:00-18:00)
        ;; Previously, time remaining calculated (time-subtract end-time now), giving ~2861m!
        ;; Now, remaining time must reflect actual effort remaining (240m).
        (let* ((future-date (format-time-string "%Y-%m-%d" (time-add (current-time) (* 2 86400)))))
          (insert (format "* TODO Four Hour Future Task\nSCHEDULED: <%s Thu 14:00-18:00>\n:PROPERTIES:\n:Effort:   4:00\n:END:\n:LOGBOOK:\n:END:\n\n* TODO Secondary Task\n:PROPERTIES:\n:Effort:   1:00\n:END:\n" future-date))
          (save-buffer))
        (goto-char (point-min))
        (let ((m1 (point-marker)))
          (re-search-forward "Secondary Task")
          (org-back-to-heading t)
          (let ((m2 (point-marker)))
            ;; 20.1 Test task-clocked-p and Focus HUD before clocking in
            (assert-equal (org-auto-scheduler--task-clocked-p m1) nil
                          "Test 20.1: m1 is not clocked in initially")
            (org-focus-hud m1)
            (let ((hud-buf (get-buffer "*Org Focus HUD*")))
              (with-current-buffer hud-buf
                (assert-true (string-match-p "TIME REMAINING: 4h 00m left (240m)" (buffer-string))
                             "Test 20.1: 4-hour future task displays '4h 00m left (240m)' (not 2800+ mins)")
                (assert-true (string-match-p "\\[PAUSED / NOT CLOCKED\\]" (buffer-string))
                             "Test 20.1: [PAUSED / NOT CLOCKED] shown when not clocked in")
                (assert-true (string-match-p "14:00 – 18:00" (buffer-string))
                             "Test 20.1: Slot times rendered")
                (assert-true (not (string-match-p "14:00 – 18:00 (Today)" (buffer-string)))
                             "Test 20.1: Future slot does not claim '(Today)'")))

            ;; 20.2 Clock into m1 (drawer gets CLOCK line, org-clock-marker is inside drawer)
            (with-current-buffer buf
              (goto-char (marker-position m1))
              (org-clock-in))
            (assert-true (org-clocking-p) "Test 20.2: Clock is active")
            (assert-true (org-auto-scheduler--task-clocked-p m1)
                         "Test 20.2: org-auto-scheduler--task-clocked-p returns t for m1")
            (assert-equal (org-auto-scheduler--task-clocked-p m2) nil
                          "Test 20.2: org-auto-scheduler--task-clocked-p returns nil for m2")

            ;; 20.3 Focus HUD while clocked in
            (org-focus-hud-refresh)
            (let ((hud-buf (get-buffer "*Org Focus HUD*")))
              (with-current-buffer hud-buf
                (assert-true (string-match-p "TIME REMAINING: 4h 00m left (240m)" (buffer-string))
                             "Test 20.3: TIME REMAINING remains 240m while clock just started")
                (assert-true (not (string-match-p "\\[PAUSED / NOT CLOCKED\\]" (buffer-string)))
                             "Test 20.3: [PAUSED / NOT CLOCKED] is NOT displayed when clocked in")))

            ;; 20.4 Toggle pause via Focus HUD 'p'
            (org-focus-hud-toggle-pause)
            (assert-true (not (org-clocking-p)) "Test 20.4: Clock stopped after toggle pause")
            (let ((hud-buf (get-buffer "*Org Focus HUD*")))
              (with-current-buffer hud-buf
                (assert-true (string-match-p "\\[PAUSED / NOT CLOCKED\\]" (buffer-string))
                             "Test 20.4: [PAUSED / NOT CLOCKED] appears after toggle pause")))

            ;; 20.5 Resume clock via Focus HUD 'p'
            (org-focus-hud-toggle-pause)
            (assert-true (org-clocking-p) "Test 20.5: Clock resumed after second toggle pause")
            (assert-true (org-auto-scheduler--task-clocked-p m1)
                         "Test 20.5: Task m1 is clocked in after resume")

            ;; 20.6 Active clock time inclusion in clocked-time
            ;; Mock clock having started 30 minutes ago
            (setq org-clock-start-time (time-subtract (current-time) 1800))
            (assert-equal (org-auto-scheduler-get-clocked-time m1) 30
                          "Test 20.6: Active clock 30m elapsed included in clocked time")
            (org-focus-hud-refresh)
            (let ((hud-buf (get-buffer "*Org Focus HUD*")))
              (with-current-buffer hud-buf
                (assert-true (string-match-p "TIME REMAINING: 3h 30m left (210m)" (buffer-string))
                             "Test 20.6: TIME REMAINING updated to 3h 30m left (210m)")
                (assert-true (string-match-p "30m clocked (12%)" (buffer-string))
                             "Test 20.6: Progress shows 30m clocked (12%)")))

            ;; 20.7 Test 'o' opens task in other window and 'O' opens in current window
            (let ((hud-buf (get-buffer "*Org Focus HUD*")))
              (with-current-buffer hud-buf
                (org-focus-hud-goto-task-other-window)
                (assert-equal (current-buffer) buf
                              "Test 20.7: Focused on original org buffer after other-window jump")
                (assert-equal (point) (marker-position m1)
                              "Test 20.7: Cursor positioned on target task headline after jump")))

            ;; Clean up clock
            (when (org-clocking-p) (org-clock-out nil t)))))
    (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p temp-file) (delete-file temp-file))))


;;; ============================================================================
;;; TEST 21: Focus HUD Nested Bullets/Checklists, Refresh ('r'), and Work Log ('l' & Scroll)
;;; ============================================================================
(message "\n--- TEST 21: Focus HUD Nested Bullets/Checklists, Refresh ('r'), and Work Log ('l' & Scroll) ---")
(let* ((temp-file (make-temp-file "org-test-focus-nested-" nil ".org"))
       (buf (find-file-noselect temp-file)))
  (unwind-protect
      (with-current-buffer buf
        (org-mode)
        (insert "* TODO Multi-Level Feature Implementation\n")
        (insert "SCHEDULED: <2026-09-30 Wed 10:00-11:30>\n")
        (insert ":PROPERTIES:\n:Effort:   1:30\n:END:\n")
        (insert ":LOGBOOK:\n:END:\n\n")
        (insert "- [ ] Root checklist item A\n")
        (insert "  - Plain sub-bullet 1 (no checkbox)\n")
        (insert "  - [X] Completed sub-checklist item 2\n")
        (insert "    - [ ] Deeply nested sub-item 3\n")
        (insert "- Plain root bullet B\n\n")
        (save-buffer)

        (let* ((m (point-min-marker))
               (items (org-focus-hud--get-checklists m))
               (hud-buf (get-buffer-create "*Org Focus HUD*")))
          ;; 21.1 Checklists and bullets extraction with nested levels
          (assert-equal (length items) 5 "Test 21.1: All 5 list items extracted across body")
          (assert-equal (plist-get (nth 0 items) :level) 0 "Test 21.1: Root item A level 0")
          (assert-equal (plist-get (nth 0 items) :state) "[ ]" "Test 21.1: Root item A checkbox [ ]")
          (assert-equal (plist-get (nth 1 items) :level) 1 "Test 21.1: Plain sub-bullet 1 level 1")
          (assert-equal (plist-get (nth 1 items) :state) nil "Test 21.1: Plain sub-bullet 1 has no checkbox")
          (assert-equal (plist-get (nth 2 items) :level) 1 "Test 21.1: Sub-checklist item 2 level 1")
          (assert-equal (plist-get (nth 2 items) :state) "[X]" "Test 21.1: Sub-checklist item 2 checkbox [X]")
          (assert-equal (plist-get (nth 3 items) :level) 2 "Test 21.1: Deeply nested sub-item 3 level 2")
          (assert-equal (plist-get (nth 3 items) :state) "[ ]" "Test 21.1: Deeply nested sub-item 3 checkbox [ ]")
          (assert-equal (plist-get (nth 4 items) :level) 0 "Test 21.1: Plain root bullet B level 0")
          (assert-equal (plist-get (nth 4 items) :state) nil "Test 21.1: Plain root bullet B has no checkbox")

          ;; Render HUD
          (with-current-buffer hud-buf
            (org-focus-hud-mode)
            (setq org-focus-hud--target-marker m)
            (setq org-focus-hud--log-offset 0)
            (org-focus-hud-refresh)
            (let ((hud-str (buffer-string)))
              ;; Checklists/Bullets box rendering with indentation & plain bullet icon
              (assert-true (string-match-p "CHECKLIST \\[1/3\\]" hud-str)
                           "Test 21.1: Checklist counter shows [1/3]")
              (assert-true (string-match-p "\\[ \\] Root checklist item A" hud-str)
                           "Test 21.1: Root checklist item rendered")
              (assert-true (string-match-p "Plain sub-bullet 1 (no checkbox)" hud-str)
                           "Test 21.1: Indented plain bullet rendered")
              (assert-true (string-match-p "\\[X\\] Completed sub-checklist item 2" hud-str)
                           "Test 21.1: Nested checked item rendered")
              (assert-true (string-match-p "\\[ \\] Deeply nested sub-item 3" hud-str)
                           "Test 21.1: Deeply nested checklist item rendered")
              (assert-true (string-match-p "Plain root bullet B" hud-str)
                           "Test 21.1: Plain root bullet rendered without checkbox")
              ;; Initial Work Log empty display
              (assert-true (string-match-p "WORK LOG \\[0\\]" hud-str)
                           "Test 21.1: Work log shows 0 entries initially"))

            ;; 21.2 Toggle plain bullet to add checkbox
            (goto-char (point-min))
            (re-search-forward "Plain root bullet B")
            (beginning-of-line)
            (org-focus-hud-toggle-checklist))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (assert-true (re-search-forward "- \\[[ Xx]\\] Plain root bullet B" nil t)
                           "Test 21.2: Plain bullet toggled into checkbox in source buffer")))

          ;; 21.3 Refresh keybinding ('r') updates HUD when external notes/time changes occur
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "r"))
                        #'org-focus-hud-refresh
                        "Test 21.3: 'r' key bound to org-focus-hud-refresh")
          ;; Directly change effort in org buffer
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (org-entry-put nil "EFFORT" "2:30")
              (save-buffer)))
          ;; Invoke refresh
          (with-current-buffer hud-buf
            (org-focus-hud-refresh)
            (assert-true (string-match-p "Effort: 150m" (buffer-string))
                         "Test 21.3: Refresh updated Effort to 150m in HUD"))

          ;; 21.4 Work Log capture ('l') with inactive timestamp
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "l"))
                        #'org-focus-hud-log-work
                        "Test 21.4: 'l' key bound to org-focus-hud-log-work")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "["))
                        #'org-focus-hud-log-scroll-up
                        "Test 21.4: '[' key bound to org-focus-hud-log-scroll-up")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "]"))
                        #'org-focus-hud-log-scroll-down
                        "Test 21.4: ']' key bound to org-focus-hud-log-scroll-down")

          (with-current-buffer hud-buf
            (org-focus-hud-log-work "Implemented nested checklist support"))
          ;; Check that source buffer has inactive timestamp in LOGBOOK
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (assert-true (re-search-forward "- \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\] Implemented nested checklist support" nil t)
                           "Test 21.4: Work log entry saved with inactive timestamp in LOGBOOK")))
          ;; Check HUD reflects the new log entry
          (with-current-buffer hud-buf
            (let ((hud-str (buffer-string)))
              (assert-true (string-match-p "WORK LOG \\[1\\]" hud-str)
                           "Test 21.4: Work log header shows 1 entry")
              (assert-true (string-match-p "Implemented nested checklist support" hud-str)
                           "Test 21.4: Work log entry rendered in HUD")))

          ;; 21.5 Scrollable Work Log when entries exceed log height
          (with-current-buffer hud-buf
            ;; Add 6 more log entries (total 7 > default height 5)
            (dotimes (i 6)
              (org-focus-hud-log-work (format "Progress milestone step #%d" (1+ i))))
            (let ((hud-str (buffer-string)))
              (assert-true (string-match-p "WORK LOG \\[3-7 of 7\\]" hud-str)
                           "Test 21.5: Work log header shows [3-7 of 7] entries visible")
              (assert-true (string-match-p "Progress milestone step #6" hud-str)
                           "Test 21.5: Latest milestone #6 visible at offset 0"))
            ;; Scroll up to view older logs
            (org-focus-hud-log-scroll-up)
            (let ((hud-str (buffer-string)))
              (assert-true (string-match-p "WORK LOG \\[2-6 of 7\\]" hud-str)
                           "Test 21.5: Scroll up shifted visible window to [2-6 of 7]"))
            ;; Scroll up again
            (org-focus-hud-log-scroll-up)
            (let ((hud-str (buffer-string)))
              (assert-true (string-match-p "WORK LOG \\[1-5 of 7\\]" hud-str)
                           "Test 21.5: Scroll up shifted to oldest window [1-5 of 7]")
              (assert-true (string-match-p "Implemented nested checklist support" hud-str)
                           "Test 21.5: Oldest work log entry visible after scroll"))
            ;; Scroll down back toward newest
            (org-focus-hud-log-scroll-down)
            (let ((hud-str (buffer-string)))
              (assert-true (string-match-p "WORK LOG \\[2-6 of 7\\]" hud-str)
                           "Test 21.5: Scroll down shifted window to [2-6 of 7]")))))
    (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p temp-file) (delete-file temp-file))))

;;; ============================================================================
;;; ============================================================================
;;; TEST 22: Option 2a Item Swapping & Contextual List Item Addition in Focus HUD
;;; ============================================================================
(message "
--- TEST 22: Option 2a Item Swapping & Contextual List Item Addition in Focus HUD ---")
(let* ((temp-file (make-temp-file "org-focus-option2a-" nil ".org"))
       (buf (find-file-noselect temp-file))
       (hud-buf (get-buffer-create "*Org Focus HUD*")))
  (unwind-protect
      (progn
        (with-current-buffer buf
          (org-mode)
          (insert "* TODO Task With Intervening Notes And Media
"
                  "  :PROPERTIES:
"
                  "  :EFFORT:   1:00
"
                  "  :AUTOSCH:  t
"
                  "  :END:
"
                  "  - [ ] Top Item Alpha
"
                  "    Multi-line explanation for Alpha.
"
                  "    [[file:alpha_diagram.png]]
"
                  "
"
                  "  Unindented body paragraph discussing the module architecture.
"
                  "  [[file:module_architecture.png]]
"
                  "
"
                  "  - [ ] Bottom Item Omega
"
                  "    Notes for Omega.
")
          (save-buffer))

        (let ((m (with-current-buffer buf
                   (goto-char (point-min))
                   (point-marker))))
          ;; 22.1 Verify Keybindings for Option 2a
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-k"))
                        #'org-focus-hud-move-item-up
                        "Test 22.1: 'M-k' bound to org-focus-hud-move-item-up")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-<up>"))
                        #'org-focus-hud-move-item-up
                        "Test 22.1: 'M-<up>' bound to org-focus-hud-move-item-up")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-j"))
                        #'org-focus-hud-move-item-down
                        "Test 22.1: 'M-j' bound to org-focus-hud-move-item-down")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-<down>"))
                        #'org-focus-hud-move-item-down
                        "Test 22.1: 'M-<down>' bound to org-focus-hud-move-item-down")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-h"))
                        #'org-focus-hud-outdent-item
                        "Test 22.1: 'M-h' bound to org-focus-hud-outdent-item")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-<left>"))
                        #'org-focus-hud-outdent-item
                        "Test 22.1: 'M-<left>' bound to org-focus-hud-outdent-item")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-l"))
                        #'org-focus-hud-indent-item
                        "Test 22.1: 'M-l' bound to org-focus-hud-indent-item")
          (assert-equal (lookup-key org-focus-hud-mode-map (kbd "M-<right>"))
                        #'org-focus-hud-indent-item
                        "Test 22.1: 'M-<right>' bound to org-focus-hud-indent-item")

          ;; Render HUD
          (with-current-buffer hud-buf
            (org-focus-hud-mode)
            (setq org-focus-hud--target-marker m)
            (org-focus-hud-refresh)

            ;; 22.2 Contextual addition below current cursor item
            (goto-char (point-min))
            (assert-true (search-forward "Top Item Alpha" nil t)
                         "Test 22.2: Found Top Item Alpha in HUD")
            (beginning-of-line)
            (assert-true (get-text-property (point) 'focus-check-pos)
                         "Test 22.2: Cursor has focus-check-pos property")
            ;; Add contextual checklist item below Alpha
            (org-focus-hud-add-checklist "Substep Alpha Beta")
            ;; Cursor should now be anchored on the new item
            (assert-true (looking-at ".*Substep Alpha Beta")
                         "Test 22.2: Cursor anchored on newly inserted item"))

          ;; Verify source buffer layout for contextual insertion
          (with-current-buffer buf
            (let ((content (buffer-string)))
              ;; Substep Alpha Beta must be inserted below Alpha and its notes, before the unindented paragraph
              (let ((pos-alpha (string-match "Top Item Alpha" content))
                    (pos-diagram (string-match "alpha_diagram" content))
                    (pos-substep (string-match "Substep Alpha Beta" content))
                    (pos-unindented (string-match "Unindented body paragraph" content))
                    (pos-omega (string-match "Bottom Item Omega" content)))
                (assert-true (< pos-alpha pos-diagram) "Test 22.2: Alpha before its diagram")
                (assert-true (< pos-diagram pos-substep) "Test 22.2: Diagram before inserted substep")
                (assert-true (< pos-substep pos-unindented) "Test 22.2: Substep inserted before unindented paragraph")
                (assert-true (< pos-unindented pos-omega) "Test 22.2: Unindented paragraph before Omega"))))

          ;; 22.3 Fallback addition at end of list when cursor NOT on list item
          (with-current-buffer hud-buf
            (goto-char (point-min)) ; On header/timer, no focus-check-pos
            (assert-equal (get-text-property (point) 'focus-check-pos) nil
                          "Test 22.3: Point-min has no focus-check-pos")
            (org-focus-hud-add-checklist "End Item Zeta")
            (assert-true (looking-at ".*End Item Zeta")
                         "Test 22.3: Cursor anchored on End Item Zeta"))

          (with-current-buffer buf
            (let ((content (buffer-string)))
              (let ((pos-omega (string-match "Bottom Item Omega" content))
                    (pos-zeta (string-match "End Item Zeta" content)))
                (assert-true (< pos-omega pos-zeta)
                             "Test 22.3: End Item Zeta appended after Bottom Item Omega"))))

          ;; 22.4 Option 2a: Move item DOWN across intervening body text & images
          (with-current-buffer hud-buf
            (goto-char (point-min))
            (search-forward "Top Item Alpha")
            (beginning-of-line)
            ;; Move Alpha DOWN past "Substep Alpha Beta"
            (org-focus-hud-move-item-down)
            (assert-true (looking-at ".*Top Item Alpha")
                         "Test 22.4: Cursor anchored on Alpha after move down 1")
            ;; Move Alpha DOWN past Omega (jumping across unindented paragraph & image)
            (org-focus-hud-move-item-down)
            (assert-true (looking-at ".*Top Item Alpha")
                         "Test 22.4: Cursor anchored on Alpha after jumping across intervening body"))

          (with-current-buffer buf
            (let ((content (buffer-string)))
              ;; Verify intervening text and image remain 100% intact
              (assert-true (string-match "Unindented body paragraph discussing the module architecture\." content)
                           "Test 22.4: Intervening paragraph completely intact")
              (assert-true (string-match "module_architecture\.png" content)
                           "Test 22.4: Intervening image link completely intact")
              ;; Alpha and its diagram have moved after Omega, before Zeta
              (let ((pos-unindented (string-match "Unindented body paragraph" content))
                    (pos-omega (string-match "Bottom Item Omega" content))
                    (pos-alpha (string-match "Top Item Alpha" content))
                    (pos-alpha-diag (string-match "alpha_diagram" content))
                    (pos-zeta (string-match "End Item Zeta" content)))
                (assert-true (< pos-unindented pos-omega) "Test 22.4: Unindented paragraph before Omega")
                (assert-true (< pos-omega pos-alpha) "Test 22.4: Omega before Alpha")
                (assert-true (< pos-alpha pos-alpha-diag) "Test 22.4: Alpha diagram moved along with Alpha")
                (assert-true (< pos-alpha pos-zeta) "Test 22.4: Alpha before Zeta"))))

          ;; 22.5 Option 2a: Move item UP across intervening body text & images
          (with-current-buffer hud-buf
            (goto-char (point-min))
            (search-forward "Top Item Alpha")
            (beginning-of-line)
            ;; Move Alpha UP past Omega
            (org-focus-hud-move-item-up)
            (assert-true (looking-at ".*Top Item Alpha")
                         "Test 22.5: Cursor anchored on Alpha after move up 1")
            ;; Move Alpha UP past Substep Alpha Beta (jumping back across intervening paragraph & image)
            (org-focus-hud-move-item-up)
            (assert-true (looking-at ".*Top Item Alpha")
                         "Test 22.5: Cursor anchored on Alpha after moving up past intervening body"))

          (with-current-buffer buf
            (let ((content (buffer-string)))
              ;; Intervening text and image remain 100% intact
              (assert-true (string-match "Unindented body paragraph discussing the module architecture\." content)
                           "Test 22.5: Intervening paragraph completely intact after move up")
              (assert-true (string-match "module_architecture\.png" content)
                           "Test 22.5: Intervening image link completely intact after move up")
              (let ((pos-alpha (string-match "Top Item Alpha" content))
                    (pos-substep (string-match "Substep Alpha Beta" content))
                    (pos-unindented (string-match "Unindented body paragraph" content))
                    (pos-omega (string-match "Bottom Item Omega" content)))
                (assert-true (< pos-alpha pos-substep) "Test 22.5: Alpha moved back before substep")
                (assert-true (< pos-substep pos-unindented) "Test 22.5: Substep before unindented paragraph")
                (assert-true (< pos-unindented pos-omega) "Test 22.5: Unindented paragraph before Omega"))))

          ;; 22.6 Indent ('M-l') and Outdent ('M-h')
          (with-current-buffer hud-buf
            (goto-char (point-min))
            (search-forward "Bottom Item Omega")
            (beginning-of-line)
            (org-focus-hud-indent-item)
            (assert-true (looking-at ".*Bottom Item Omega")
                         "Test 22.6: Cursor anchored on Omega after indent"))

          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (search-forward "Bottom Item Omega")
              (beginning-of-line)
              ;; Originally indent was 2 spaces; now it should be 4 spaces
              (assert-equal (current-indentation) 4 "Test 22.6: Omega indentation increased to 4 spaces")
              ;; Continuation note of Omega should also have been shifted rigidly by 2 spaces (from 4 to 6)
              (forward-line 1)
              (assert-equal (current-indentation) 6 "Test 22.6: Omega notes indentation rigidly shifted to 6 spaces")))

          (with-current-buffer hud-buf
            (goto-char (point-min))
            (search-forward "Bottom Item Omega")
            (beginning-of-line)
            (org-focus-hud-outdent-item)
            (assert-true (looking-at ".*Bottom Item Omega")
                         "Test 22.6: Cursor anchored on Omega after outdent"))

          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (search-forward "Bottom Item Omega")
              (beginning-of-line)
              (assert-equal (current-indentation) 2 "Test 22.6: Omega indentation restored to 2 spaces")
              (forward-line 1)
              (assert-equal (current-indentation) 4 "Test 22.6: Omega notes indentation restored to 4 spaces")))))
  (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p temp-file) (delete-file temp-file))))

(if (= test-failures 0)
    (message "ALL 22 TEST SUITES PASSED PERFECTLY!")
  (message "FAILURES DETECTED: %d" test-failures))
(message "==============================================")

;;; ============================================================================
;;; TEST 5: Standalone Pure-Org Operation (No Scheduler Required)
;;; ============================================================================
(message "\n--- TEST 5: Standalone Pure-Org Operation ---")
(let* ((temp-file (make-temp-file "org-standalone-focus-" nil ".org"))
       (buf (find-file-noselect temp-file))
       (hud-buf (get-buffer-create "*Org Focus HUD*")))
  (unwind-protect
      (with-current-buffer buf
        (org-mode)
        (insert "* TODO Pure Standalone Org Task\n:PROPERTIES:\n:Effort:   1:30\n:END:\n\n- [ ] First checklist item\n- [X] Second checklist item\n")
        (save-buffer)
        (goto-char (point-min))
        (let ((m (point-marker)))
          ;; Verify standalone effort calculation without org-auto-scheduler
          (assert-equal (org-focus-hud--get-effort m) 90
                        "Test 5.1: Standalone effort parsed from :Effort: as 90 minutes")

          ;; Launch HUD directly on marker
          (org-focus-hud m)
          (with-current-buffer hud-buf
            (assert-true (string-match-p "Pure Standalone Org Task" (buffer-string))
                         "Test 5.2: Task title rendered in HUD")
            (assert-true (string-match-p "1h 30m" (buffer-string))
                         "Test 5.2: Effort rendered as 1h 30m in HUD")
            (assert-true (string-match-p "CHECKLIST \\[1/2\\]" (buffer-string))
                         "Test 5.2: Checklist counter rendered as [1/2]")

            ;; Test standalone extend (pure Org mode: bumps :Effort: property)
            (org-focus-hud-extend 30)
            (assert-equal (org-focus-hud--get-effort m) 120
                          "Test 5.3: Standalone extend bumped effort to 120 minutes (2:00)")
            (with-current-buffer buf
              (org-with-point-at m
                (assert-equal (org-entry-get nil "Effort") "2:00"
                              "Test 5.3: :Effort: property in org file updated to 2:00"))))))
    (when (buffer-live-p hud-buf) (kill-buffer hud-buf))
    (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p temp-file) (delete-file temp-file))))

;; ============================================================================
;; SUMMARY
;; ============================================================================
(if (> test-failures 0)
    (progn
      (message "\n==============================================")
      (message "TEST RUN FAILED with %d failures!" test-failures)
      (message "==============================================\n")
      (kill-emacs 1))
  (message "\n==============================================")
  (message "ALL ORG-FOCUS-HUD TEST SUITES PASSED PERFECTLY!")
  (message "==============================================\n")
  (kill-emacs 0))
