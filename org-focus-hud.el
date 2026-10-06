;;; org-focus-hud.el --- Distraction-free monotasking cockpit for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Santosh Dayapule
;; Author: SSD2019 <santosh.dayapule@gmail.com>
;; URL: https://github.com/SSD2019/org-focus-hud
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.3"))
;; Keywords: org, focus, productivity, monotasking, cockpit, timer

;; This file is part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Focus HUD is a distraction-free, monotasking execution cockpit for Org-mode.
;; It presents only the single active or currently scheduled task, keeping you
;; immersed in high-flow work without schedule drift or context switching.
;;
;; Features:
;; - Live progress bar with elapsed clock vs. planned effort
;; - In-cockpit hierarchical checklist and bullet editing
;; - Option 2a surgical item block reordering (preserving intervening text/diagrams)
;; - Quick work logging (inactive timestamps), note capture, and subtask creation
;; - Built-in Pomodoro pacing and overtime warning indicators
;; - Evil / Spacemacs seamless modal navigation
;; - Zero hard dependencies: works out of the box with standard org-clock
;;   and integrates deeply with org-auto-scheduler when available.
;;
;; Usage:
;;   (require 'org-focus-hud)
;;   M-x org-focus-hud
;;
;; In the HUD cockpit:
;;   TAB / S-TAB : Next/Prev checklist item
;;   RET         : Toggle checkbox [ ] <-> [X]
;;   M-j / M-k   : Move item down / up surgically across body paragraphs
;;   M-h / M-l   : Outdent / Indent item
;;   k           : Add contextual checklist or bullet item
;;   l           : Log work done (saved to :LOGBOOK:)
;;   [ / ]       : Scroll work log
;;   n           : Add quick note
;;   s           : Add child subtask
;;   a           : Add sibling task
;;   d           : Mark DONE and advance to next scheduled task
;;   w           : Wait on this task and advance
;;   +           : Extend effort +15m
;;   p           : Pause / resume clock
;;   o / O       : Jump to task in Org file
;;   ?           : Toggle shortcuts help legend
;;   q           : Minimize HUD

;;; Code:

(require 'org)
(require 'org-clock)
(eval-when-compile (require 'subr-x))

(defgroup org-focus-hud nil
  "Distraction-free monotasking cockpit for Org-mode."
  :group 'org
  :prefix "org-focus-hud-")

(defcustom org-focus-hud-default-effort 30
  "Default task duration in minutes when no effort is specified."
  :type 'integer
  :group 'org-focus-hud)

(defcustom org-focus-hud-waiting-states '("WAITING")
  "List of TODO states considered waiting / blocked."
  :type '(repeat string)
  :group 'org-focus-hud)

(defcustom org-focus-hud-sibling-tag "AUTOSCH"
  "Default tag to attach to sibling tasks created in the Focus HUD, or nil for none."
  :type '(choice (string :tag "Tag name") (const :tag "No tag" nil))
  :group 'org-focus-hud)

(defcustom org-focus-hud-today-tasks-function nil
  "Custom function returning a list of plists for today's scheduled tasks.
Each plist should contain :marker, :title, :start, :end, :is-done, and :state."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-extend-task-function nil
  "Custom function called to extend the current task effort by MINUTES.
Called with one argument: (minutes)."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-effort-function nil
  "Custom function returning the effort in minutes for a task marker."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-clocked-time-function nil
  "Custom function returning the clocked time in minutes for a task marker."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-pomodoro-function nil
  "Custom function returning a pomodoro spec plist for a task marker."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-task-resolver-function nil
  "Custom function returning a task marker to display in the Focus HUD."
  :type '(choice (const nil) function)
  :group 'org-focus-hud)

(defcustom org-focus-hud-on-done-hook nil
  "Hook run when a task is marked DONE in the Focus HUD."
  :type 'hook
  :group 'org-focus-hud)

;;; Helper functions for standalone Org operation:

(defun org-focus-hud--parse-scheduled-time-range (sched-str)
  "Parse scheduled string to extract start time and end time."
  (when sched-str
    (cond
     ((fboundp 'org-auto-scheduler-parse-scheduled-time-range)
      (org-auto-scheduler-parse-scheduled-time-range sched-str))
     ((string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\([^>]*\\)>" sched-str)
      (let* ((start-part (match-string 1 sched-str))
             (end-time-part (match-string 2 sched-str))
             (start-time (org-time-string-to-time start-part))
             (full-end-time-str (concat (substring start-part 0 11) end-time-part))
             (end-time (org-time-string-to-time full-end-time-str)))
        (list start-time end-time nil)))
     ((string-match "\\(<[^>]+>\\)--\\(<[^>]+>\\)" sched-str)
      (let* ((start-time (org-time-string-to-time (match-string 1 sched-str)))
             (end-time (org-time-string-to-time (match-string 2 sched-str))))
        (list start-time end-time nil)))
     ((string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+ [0-9]\\{2\\}:[0-9]\\{2\\}\\)" sched-str)
      (let ((start-time (org-time-string-to-time (match-string 1 sched-str))))
        (list start-time nil nil)))
     (t nil))))

(defun org-focus-hud--get-effort (marker)
  "Return effort in minutes for heading at MARKER."
  (if (bound-and-true-p org-focus-hud-effort-function)
      (funcall org-focus-hud-effort-function marker)
    (org-with-point-at marker
      (let ((effort-str (org-entry-get nil "Effort")))
        (if (and effort-str (not (string-empty-p effort-str)))
            (if (fboundp 'org-duration-to-minutes)
                (round (org-duration-to-minutes effort-str))
              (let* ((parts (split-string effort-str ":"))
                     (h (string-to-number (car parts)))
                     (m (if (cadr parts) (string-to-number (cadr parts)) 0)))
                (+ (* h 60) m)))
          (or org-focus-hud-default-effort 30))))))

(defun org-focus-hud--get-clocked-time (marker)
  "Return clocked minutes for heading at MARKER."
  (if (bound-and-true-p org-focus-hud-clocked-time-function)
      (funcall org-focus-hud-clocked-time-function marker)
    (let ((mins 0))
      (org-with-point-at marker
        (setq mins (round (or (org-clock-sum-current-item) 0)))
        (when (and (org-focus-hud--task-clocked-p marker)
                   (boundp 'org-clock-start-time)
                   org-clock-start-time)
          (let ((active-secs (float-time (time-subtract (current-time) org-clock-start-time))))
            (setq mins (+ mins (round (/ active-secs 60.0)))))))
      mins)))

(defun org-focus-hud--task-clocked-p (marker)
  "Return non-nil if the task at MARKER is currently clocked in."
  (and (markerp marker)
       (marker-buffer marker)
       (or (and (fboundp 'org-clocking-p) (org-clocking-p))
           (and (fboundp 'org-clock-is-active) (org-clock-is-active)))
       (let ((clock-buf (or (and (boundp 'org-clock-hd-marker) (markerp org-clock-hd-marker) (marker-buffer org-clock-hd-marker))
                            (and (boundp 'org-clock-marker) (markerp org-clock-marker) (marker-buffer org-clock-marker))))
             (clock-pos (or (and (boundp 'org-clock-hd-marker) (markerp org-clock-hd-marker) (marker-position org-clock-hd-marker))
                            (and (boundp 'org-clock-marker) (markerp org-clock-marker)
                                 (org-with-point-at org-clock-marker
                                   (ignore-errors (org-back-to-heading t) (point))))))
             (marker-pos (org-with-point-at marker
                           (ignore-errors (org-back-to-heading t) (point)))))
         (and clock-buf
              (equal clock-buf (marker-buffer marker))
              clock-pos
              marker-pos
              (= clock-pos marker-pos)))))

(defun org-focus-hud--get-pomodoro-spec (marker)
  "Return pomodoro spec for task at MARKER."
  (cond
   ((bound-and-true-p org-focus-hud-pomodoro-function)
    (funcall org-focus-hud-pomodoro-function marker))
   ((fboundp 'org-auto-scheduler-get-task-pomodoro-spec)
    (org-auto-scheduler-get-task-pomodoro-spec marker))
   (t nil)))

(defun org-focus-hud--get-today-tasks ()
  "Return today's scheduled tasks as a list of plists."
  (cond
   ((bound-and-true-p org-focus-hud-today-tasks-function)
    (funcall org-focus-hud-today-tasks-function))
   ((fboundp 'org-auto-scheduler-get-today-scheduled-tasks)
    (org-auto-scheduler-get-today-scheduled-tasks))
   (t nil)))

;;; Focus HUD: Distraction-Free Monotasking Cockpit
;;; ============================================================================

(defface org-focus-hud-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for the title in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-box-face
  '((t (:foreground "dim gray")))
  "Face for box borders in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-progress-done-face
  '((t (:foreground "#2ecc71" :weight bold)))
  "Face for completed progress in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-progress-remain-face
  '((t (:foreground "#7f8c8d")))
  "Face for remaining progress in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-overrun-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for task overrun in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-section-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for section headers in the Focus HUD."
  :group 'org-focus-hud)

(defface org-focus-hud-key-face
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for shortcut keys in the Focus HUD."
  :group 'org-focus-hud)

(defcustom org-focus-hud-refresh-interval 15
  "Interval in seconds to auto-refresh the Focus HUD progress bar."
  :type 'integer
  :group 'org-focus-hud)

(defcustom org-focus-hud-bar-width 40
  "Width in characters for the Focus HUD progress bar."
  :type 'integer
  :group 'org-focus-hud)

(defcustom org-focus-hud-auto-clock-in-on-advance t
  "When non-nil, automatically clock into the next scheduled task upon pressing `d` in Focus HUD."
  :type 'boolean
  :group 'org-focus-hud)

(defcustom org-focus-hud-follow-active-clock t
  "When non-nil, automatically switch the Focus HUD to newly clocked tasks."
  :type 'boolean
  :group 'org-focus-hud)

(defcustom org-focus-hud-show-help nil
  "Whether to display the shortkey legend by default in the Focus HUD cockpit.
Defaults to nil (hidden until `?' is toggled)."
  :type 'boolean
  :group 'org-focus-hud)

(defface org-focus-hud-transient-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for in-progress or partial items in the Focus HUD."
  :group 'org-focus-hud)

(defcustom org-focus-hud-log-height 5
  "Number of visible lines for the fixed-size scrollable Work Log section in Focus HUD."
  :type 'integer
  :group 'org-focus-hud)

(defcustom org-focus-hud-compact t
  "When non-nil, render Focus HUD in compact mode with reduced whitespace.
Eliminates blank lines between sections and avoids padding empty rows."
  :type 'boolean
  :group 'org-focus-hud)

(defcustom org-focus-hud-section-spacing 0
  "Number of blank lines between sections in the Focus HUD cockpit.
0 means compact (no empty line between sections).
1 means spacious (one empty line between sections)."
  :type 'integer
  :group 'org-focus-hud)

(defun org-focus-hud--section-sep ()
  "Return newline string separator between sections based on spacing settings."
  (let ((spacing (if org-focus-hud-compact
                     (or org-focus-hud-section-spacing 0)
                   (max 1 (or org-focus-hud-section-spacing 1)))))
    (make-string (1+ (max 0 spacing)) ?\n)))

(defvar-local org-focus-hud--log-offset 0
  "Buffer-local scroll offset for the fixed-size Work Log section in Focus HUD.
0 means showing the most recent entries.")

(defvar-local org-focus-hud--show-help nil
  "Buffer-local flag indicating whether the shortcuts help is toggled visible.")

(defvar org-focus-hud--timer nil
  "Timer for updating the Focus HUD buffer.")

(defvar-local org-focus-hud--target-marker nil
  "Buffer-local marker of the task currently being tracked in the Focus HUD.")

(defvar org-focus-hud--inhibit-clock-hooks nil
  "Non-nil to inhibit Focus HUD clock hooks from running.")

(defun org-focus-hud--calculate-levels (items)
  "Calculate hierarchical nesting :level (0, 1, 2, ...) for ITEMS based on :indent."
  (let ((indent-stack '())
        (res '()))
    (dolist (item items)
      (let ((ind (or (plist-get item :indent) 0)))
        (while (and indent-stack (> (car indent-stack) ind))
          (pop indent-stack))
        (unless (member ind indent-stack)
          (if (or (null indent-stack) (> ind (car indent-stack)))
              (push ind indent-stack)
            (setq indent-stack (list ind))))
        (let ((level (max 0 (1- (length indent-stack)))))
          (push (append item (list :level level)) res))))
    (nreverse res)))

(defun org-focus-hud--body-start (body-end)
  "Return position where task body begins, after all planning and initial drawers."
  (org-back-to-heading t)
  (org-end-of-meta-data nil)
  (while (and (< (point) body-end)
              (or (looking-at "^[ \t]*$")
                  (looking-at "^[ \t]*:[A-Za-z0-9_-]+:[ \t]*$")))
    (if (looking-at "^[ \t]*:[A-Za-z0-9_-]+:[ \t]*$")
        (if (re-search-forward "^[ \t]*:END:.*$" body-end t)
            (forward-line 1)
          (forward-line 1))
      (forward-line 1)))
  (min (point) body-end))

(defun org-focus-hud--extract-item-effort (text)
  "Extract effort in minutes and clean text from TEXT.
Returns a cons cell (CLEAN-TEXT . EFFORT-MINS).
Matches trailing [2h], [90m], [1.5h], [1h30m], [2:00], [est: 2h], etc."
  (if (and text
           (string-match
            "[ \t]*\\\[\\(?:est:[ \t]*\\)?\\(?:\\([0-9]+\\(?:\\.[0-9]+\\)?\\)[ \t]*[hH]\\(?:[ \t]*\\([0-9]+\\)[ \t]*[mM]\\)?\\|\\([0-9]+\\)[ \t]*[mM]\\|\\([0-9]+\\):\\([0-9]\\{2\\}\\)\\)\\\][ \t]*$"
            text))
      (let* ((match-start (match-beginning 0))
             (clean-text (string-trim (substring text 0 match-start)))
             (h-str (match-string 1 text))
             (hm-str (match-string 2 text))
             (m-str (match-string 3 text))
             (colon-h (match-string 4 text))
             (colon-m (match-string 5 text))
             (mins
              (cond
               ((and colon-h colon-m)
                (+ (* (string-to-number colon-h) 60)
                   (string-to-number colon-m)))
               (m-str
                (string-to-number m-str))
               (h-str
                (let ((h (string-to-number h-str))
                      (m (if hm-str (string-to-number hm-str) 0)))
                  (round (+ (* h 60) m))))
               (t nil))))
        (cons (if (string-empty-p clean-text) text clean-text) mins))
    (cons text nil)))

(defun org-focus-hud--format-effort-human (mins)
  "Format MINS into a human-readable effort string like `2h', `1h 30m', or `45m'."
  (let ((m (or mins 0)))
    (cond
     ((<= m 0) "0m")
     ((>= m 60)
      (let ((h (/ m 60))
            (rem (% m 60)))
        (if (= rem 0)
            (format "%dh" h)
          (format "%dh %02dm" h rem))))
     (t (format "%dm" m)))))

(defun org-focus-hud--format-badge-effort (mins)
  "Format MINS into a concise badge string like `2:00' or `0:45'."
  (let ((m (or mins 0)))
    (format "%d:%02d" (/ m 60) (% m 60))))

(defvar org-focus-hud--active-checklist-table (make-hash-table :test 'equal)
  "Hash table mapping task marker key (BUF . POS) to active item plist:
(:pos POS :start-clock START-CLOCK :start-time START-TIME :title TITLE).")

(defun org-focus-hud--get-active-key (marker)
  "Return cache key for task at MARKER."
  (when (and marker (markerp marker) (marker-buffer marker))
    (cons (buffer-name (marker-buffer marker)) (marker-position marker))))

(defun org-focus-hud--get-item-elapsed (marker item)
  "Return elapsed minutes for active checklist ITEM under task at MARKER."
  (let* ((key (org-focus-hud--get-active-key marker))
         (active-data (when key (gethash key org-focus-hud--active-checklist-table)))
         (item-pos (plist-get item :pos)))
    (if (and active-data (equal (plist-get active-data :pos) item-pos))
        (let* ((start-clock (plist-get active-data :start-clock))
               (current-clock (org-focus-hud--get-clocked-time marker)))
          (max 0 (- current-clock (or start-clock current-clock))))
      (if (and key (string= (or (plist-get item :state) "") "[-]"))
          (let ((cur-clock (org-focus-hud--get-clocked-time marker)))
            (puthash key (list :pos item-pos
                               :start-clock cur-clock
                               :start-time (current-time)
                               :title (plist-get item :clean-text))
                     org-focus-hud--active-checklist-table)
            0)
        0))))

(defun org-focus-hud--get-item-breadcrumb (all-items cur-item)
  "Return ancestor title for CUR-ITEM in ALL-ITEMS, or nil if at root level."
  (let* ((cur-pos (plist-get cur-item :pos))
         (cur-level (or (plist-get cur-item :level) 0)))
    (when (> cur-level 0)
      (let* ((idx (cl-position cur-pos all-items :key (lambda (it) (plist-get it :pos))))
             (parent-item (when (and idx (> idx 0))
                            (cl-find-if (lambda (it)
                                          (< (or (plist-get it :level) 0) cur-level))
                                        (nreverse (cl-subseq all-items 0 idx))))))
        (when parent-item
          (plist-get parent-item :clean-text))))))

(defun org-focus-hud--analyze-checklists (items)
  "Annotate ITEMS with hierarchical metrics:
:has-children, :children-effort, :all-children-done, :any-child-transient."
  (let* ((len (length items))
         (annotated (mapcar #'copy-sequence items)))
    (dotimes (i len)
      (let* ((cur (nth i annotated))
             (cur-level (or (plist-get cur :level) 0))
             (has-children nil)
             (leaf-efforts 0)
             (all-done t)
             (any-transient nil)
             (has-box-children nil)
             (j (1+ i)))
        (while (and (< j len) (> (or (plist-get (nth j annotated) :level) 0) cur-level))
          (setq has-children t)
          (let* ((desc (nth j annotated))
                 (desc-level (or (plist-get desc :level) 0))
                 (next-desc-level (when (< (1+ j) len)
                                    (or (plist-get (nth (1+ j) annotated) :level) 0)))
                 (is-desc-leaf (or (null next-desc-level) (<= next-desc-level desc-level)))
                 (desc-state (plist-get desc :state)))
            (when (and is-desc-leaf (plist-get desc :effort-mins))
              (setq leaf-efforts (+ leaf-efforts (plist-get desc :effort-mins))))
            (when desc-state
              (setq has-box-children t)
              (unless (string-match-p "\\[[Xx]\\]" desc-state)
                (setq all-done nil))
              (when (string-match-p "\\[[-]\\]" desc-state)
                (setq any-transient t))))
          (setq j (1+ j)))
        (setq cur (plist-put cur :has-children has-children))
        (setq cur (plist-put cur :children-effort leaf-efforts))
        (setq cur (plist-put cur :all-children-done (and has-box-children all-done)))
        (setq cur (plist-put cur :any-child-transient any-transient))
        (setcar (nthcdr i annotated) cur)))
    annotated))

(defun org-focus-hud--log-work-silent (marker work-text)
  "Log WORK-TEXT under :LOGBOOK: of task at MARKER without user prompts."
  (when (and marker (markerp marker) (marker-buffer marker)
             (not (string-empty-p (string-trim (or work-text "")))))
    (let ((ts (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (org-with-point-at marker
        (org-back-to-heading t)
        (save-excursion
          (let* ((body-end (save-excursion
                             (or (and (org-goto-first-child) (point))
                                 (and (outline-next-heading) (point))
                                 (point-max))))
                 (meta-end (save-excursion
                             (org-back-to-heading t)
                             (org-end-of-meta-data t)
                             (min (point) body-end))))
            (goto-char (marker-position marker))
            (org-back-to-heading t)
            (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" body-end t)
                (progn
                  (if (re-search-forward "^[ \t]*:END:[ \t]*$" body-end t)
                      (goto-char (match-beginning 0))
                    (goto-char body-end))
                  (insert (format "  - %s %s\n" ts (string-trim work-text))))
              (goto-char meta-end)
              (unless (bolp) (insert "\n"))
              (insert "  :LOGBOOK:\n"
                      (format "  - %s %s\n" ts (string-trim work-text))
                      "  :END:\n"))
            (when (buffer-file-name (buffer-base-buffer))
              (save-buffer))))))))

(defun org-focus-hud--get-checklists (marker)
  "Return a list of checklist and bullet items for task at MARKER.
Captures all plain list items (with checkboxes or plain bullets)
across the node body, including indentation level for nested structure.
Drawers (like :LOGBOOK: or :PROPERTIES:) and timestamped log notes are excluded.
Each item is a plist:
  (:pos POS :box-pos BOX-POS :indent INDENT :bullet BULLET :state STATE :text TEXT :level LEVEL)."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let* ((items '())
           (body-end (save-excursion
                       (or (and (org-goto-first-child) (point))
                           (and (outline-next-heading) (point))
                           (point-max))))
           (start-pos (save-excursion
                        (org-focus-hud--body-start body-end))))
      (save-excursion
        (goto-char start-pos)
        (while (< (point) body-end)
          (cond
           ;; Skip drawers (like :LOGBOOK:, :PROPERTIES:, etc.) if any appear in body
           ((looking-at "^[ \t]*:[A-Za-z0-9_-]+:[ \t]*$")
            (if (re-search-forward "^[ \t]*:END:.*$" body-end t)
                (forward-line 1)
              (forward-line 1)))
           ;; Skip stray drawer end lines
           ((looking-at "^[ \t]*:END:?.*$")
            (forward-line 1))
           ;; Skip stray clock lines
           ((looking-at "^[ \t]*CLOCK:")
            (forward-line 1))
           ;; Match plain list items (bullets or checkboxes)
           ((looking-at "^\\([ \t]*\\)\\([-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\(?:\\(\\[[ Xx-]\\]\\)[ \t]+\\)?\\(.*\\)$")
            (let* ((indent (length (match-string 1)))
                   (bullet (match-string 2))
                   (is-heading (and (string= bullet "*") (= indent 0)))
                   (box-pos (when (match-beginning 3) (match-beginning 3)))
                   (state (match-string-no-properties 3))
                   (raw-text (string-trim (match-string-no-properties 4)))
                   (pos (or box-pos (match-beginning 2)))
                   (effort-pair (org-focus-hud--extract-item-effort raw-text))
                   (clean-text (car effort-pair))
                   (effort-mins (cdr effort-pair))
                   ;; Exclude log/note entries from checklist & bullet outline
                   (is-log-or-note
                    (or (string-match-p "^Note taken on \\[" raw-text)
                        (string-match-p "^State \"[^\"]+\"" raw-text)
                        (string-match-p "^\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" raw-text)
                        (string-match-p "^\\[[0-9]\\{2\\}:[0-9]\\{2\\}\\]" raw-text))))
              (unless (or is-heading is-log-or-note)
                (push (list :pos pos
                            :box-pos box-pos
                            :indent indent
                            :bullet bullet
                            :state state
                            :text raw-text
                            :clean-text clean-text
                            :effort-mins effort-mins)
                      items)))
            (forward-line 1))
           (t
            (forward-line 1)))))
      (org-focus-hud--calculate-levels (nreverse items)))))

(defun org-focus-hud--get-subtasks (marker)
  "Return a list of immediate child subtasks for task at MARKER.
Each item is a plist (:pos POS :state STATE :title TITLE)."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let ((items '()))
      (save-excursion
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        (when (org-goto-first-child)
          (let ((done-loop nil))
            (while (not done-loop)
              (let ((st (org-get-todo-state))
                    (hl (org-get-heading t t t t))
                    (p (point)))
                (push (list :pos p :state (or st "TODO") :title (or hl "Untitled")) items))
              (unless (org-goto-sibling)
                (setq done-loop t))))))
      (nreverse items))))

(defun org-focus-hud--get-notes (marker)
  "Return up to 4 recent notes for task at MARKER.
Extracts quick notes from the task body and :LOGBOOK: drawer captured via `n`."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let* ((notes '())
           (body-end (save-excursion
                       (or (and (org-goto-first-child) (point))
                           (and (outline-next-heading) (point))
                           (point-max))))
           (meta-end (save-excursion
                       (org-back-to-heading t)
                       (org-end-of-meta-data t)
                       (min (point) body-end))))
      (save-excursion
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        ;; Only match Org "Note taken on [...]" or short-time quick notes "[HH:MM]":
        (while (re-search-forward "^[ \t]*- \\(?:Note taken on \\[\\([0-9][^]]*\\)\\]\\|\\[\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]\\)\\(?: \\\\\\\\n[ \t]*\\)?\\(.*\\)$" body-end t)
          (let* ((ts (or (match-string-no-properties 1) (match-string-no-properties 2)))
                 (body (match-string-no-properties 3))
                 (short-ts (if (string-match "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" ts)
                               (match-string 1 ts)
                             ts))
                 (note-text (string-trim body)))
            (unless (string-empty-p note-text)
              (push (format "[%s] %s" short-ts note-text) notes)))))
      (save-excursion
        (goto-char meta-end)
        (while (re-search-forward "^[ \t]*- \\[\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\][ \t]+\\(.*\\)$" body-end t)
          (let ((ts (match-string-no-properties 1))
                (text (match-string-no-properties 2)))
            (unless (member (format "[%s] %s" ts text) notes)
              (push (format "[%s] %s" ts text) notes)))))
      (let ((res (nreverse notes)))
        (if (> (length res) 4)
            (last res 4)
          res)))))

(defun org-focus-hud--get-logs (marker)
  "Return all timestamped work logs and notes for task at MARKER.
Extracts items from the task's :LOGBOOK: drawer and body:
- Inactive timestamp logs: - [YYYY-MM-DD Day HH:MM] ...
- Clock notes: - Note taken on [...] \\ ...
- State transitions: - State \"...\" from \"...\" [...]
Each item is a plist (:ts TIMESTAMP :text TEXT :formatted STR)."
  (org-with-point-at marker
    (org-back-to-heading t)
    (let* ((logs '())
           (body-end (save-excursion
                       (or (and (org-goto-first-child) (point))
                           (and (outline-next-heading) (point))
                           (point-max)))))
      (save-excursion
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        ;; Scan for inactive timestamp logs with dates [YYYY-MM-DD ...] or Note taken:
        (while (re-search-forward "^[ \t]*- \\(?:Note taken on \\)?\\(\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\]\\)\\(?: \\\\\\\\n[ \t]*\\)?\\(.*\\)$" body-end t)
          (let* ((ts (match-string-no-properties 1))
                 (body (match-string-no-properties 2))
                 (log-text (string-trim body)))
            (unless (string-empty-p log-text)
              (push (list :ts ts :text log-text :formatted (format "%s %s" ts log-text)) logs))))
        ;; Scan for state transitions:
        (goto-char (marker-position marker))
        (org-back-to-heading t)
        (while (re-search-forward "^[ \t]*- State \"\\([^\"]+\\)\"[ \t]+from[ \t]+\"\\([^\"]+\\)\"[ \t]+\\(\\[[0-9][^]]*\\]\\)[ \t]*\\(.*\\)$" body-end t)
          (let* ((st-to (match-string-no-properties 1))
                 (st-from (match-string-no-properties 2))
                 (ts (match-string-no-properties 3))
                 (extra (string-trim (or (match-string-no-properties 4) "")))
                 (text (if (string-empty-p extra)
                           (format "State %s → %s" st-from st-to)
                         (format "State %s → %s: %s" st-from st-to extra))))
            (push (list :ts ts :text text :formatted (format "%s %s" ts text)) logs))))
      (nreverse logs))))

(defun org-focus-hud--resolve-task (&optional override-marker)
  "Resolve the active task for the Focus HUD.
Returns a plist with task details or nil if no active task found."
  (let* ((marker
          (cond
           ((and override-marker (markerp override-marker) (marker-buffer override-marker))
            (org-with-point-at override-marker
              (org-back-to-heading t)
              (point-marker)))
           ((and (fboundp 'org-clock-is-active) (org-clock-is-active)
                 (boundp 'org-clock-marker) (markerp org-clock-marker) (marker-buffer org-clock-marker))
            (org-with-point-at org-clock-marker
              (org-back-to-heading t)
              (point-marker)))
           (t
            (let* ((today-tasks (ignore-errors (org-focus-hud--get-today-tasks)))
                   (now (current-time))
                   (current-task nil)
                   (earliest-pending nil))
              (dolist (tk today-tasks)
                (unless (plist-get tk :is-done)
                  (let ((start (plist-get tk :start))
                        (end (plist-get tk :end)))
                    (if (and start end (time-less-p start now) (time-less-p now end))
                        (setq current-task tk)
                      (unless earliest-pending
                        (setq earliest-pending tk))))))
              (let ((m (plist-get (or current-task earliest-pending) :marker)))
                (when (and m (markerp m) (marker-buffer m))
                  (org-with-point-at m
                    (org-back-to-heading t)
                    (point-marker)))))))))
    (when (and marker (markerp marker) (marker-buffer marker))
      (org-with-point-at marker
        (let* ((title (or (org-get-heading t t t t) "Untitled Task"))
               (state (org-get-todo-state))
               (category (or (org-entry-get nil "CATEGORY")
                             (ignore-errors (org-get-category))
                             "General"))
               (parent-title
                (save-excursion
                  (if (org-up-heading-safe)
                      (org-get-heading t t t t)
                    category)))
               (sched-str (org-entry-get nil "SCHEDULED"))
               (range (when sched-str (org-focus-hud--parse-scheduled-time-range sched-str)))
               (start-time (nth 0 range))
               (end-time (nth 1 range))
               (raw-effort (or (ignore-errors (org-focus-hud--get-effort marker))
                               (when (and start-time end-time)
                                 (round (/ (float-time (time-subtract end-time start-time)) 60)))
                               (or org-focus-hud-default-effort 30)))
               (effort-mins (round raw-effort))
               (clocked-mins (round (or (ignore-errors (org-focus-hud--get-clocked-time marker)) 0)))
               (is-clocked (org-focus-hud--task-clocked-p marker))
               (pomo-spec (when (fboundp 'org-focus-hud--get-pomodoro-spec)
                            (org-focus-hud--get-pomodoro-spec marker)))
               (checklists (org-focus-hud--get-checklists marker))
               (subtasks (org-focus-hud--get-subtasks marker))
               (notes (org-focus-hud--get-notes marker))
               (logs (org-focus-hud--get-logs marker)))
          (list :marker marker
                :title title
                :state state
                :category category
                :parent parent-title
                :sched-str sched-str
                :start-time start-time
                :end-time end-time
                :effort effort-mins
                :clocked clocked-mins
                :is-clocked is-clocked
                :pomodoro pomo-spec
                :checklists checklists
                :subtasks subtasks
                :notes notes
                :logs logs))))))

(defun org-focus-hud--render (task-info)
  "Render TASK-INFO into the current buffer."
  (let* ((title (plist-get task-info :title))
         (parent (plist-get task-info :parent))
         (effort (max 1 (plist-get task-info :effort)))
         (clocked (plist-get task-info :clocked))
         (start-time (plist-get task-info :start-time))
         (end-time (plist-get task-info :end-time))
         (is-clocked (plist-get task-info :is-clocked))
         (pomo (plist-get task-info :pomodoro))
         (now (current-time))
         (time-str (format-time-string "%H:%M · %a %b %d" now))
         (today-str (format-time-string "%Y-%m-%d" now))
         (is-today-slot (and end-time (string= (format-time-string "%Y-%m-%d" end-time) today-str)))
         ;; Overrun calculations:
         ;; 1. Slot overrun: task was scheduled for a slot today, now is past scheduled end, and clocked in
         (slot-overrun-mins (when (and is-today-slot (time-less-p end-time now) is-clocked)
                              (round (/ (float-time (time-subtract now end-time)) 60))))
         ;; 2. Effort overrun: clocked time exceeds estimated effort
         (effort-overrun-mins (when (> clocked effort)
                                (- clocked effort)))
         (overrun-mins (or slot-overrun-mins effort-overrun-mins))
         (is-overrun (and overrun-mins (> overrun-mins 0)))
         (pct (min 100 (round (/ (* (float clocked) 100.0) effort))))
         (bar-width org-focus-hud-bar-width)
         (filled-chars (min bar-width (round (* (/ (float pct) 100.0) bar-width))))
         (empty-chars (max 0 (- bar-width filled-chars)))
         ;; Remaining effort is effort minus clocked time (not wall-clock distance to future slots!)
         (rem-mins (if is-overrun
                       0
                     (max 0 (- effort clocked)))))

    ;; 1. Header Box
    (insert "╭" (make-string 77 ?─) "╮\n")
    (let* ((prefix (if is-overrun "⚠️  OVERRUN: " "🎯 FOCUS: "))
           (full-title (concat prefix title))
           (max-title-len (- 77 4 (length time-str)))
           (trunc-title (if (> (length full-title) max-title-len)
                            (concat (substring full-title 0 (- max-title-len 3)) "...")
                          full-title))
           (pad (make-string (max 0 (- 77 4 (length trunc-title) (length time-str))) ?\s)))
      (insert "│ "
              (propertize trunc-title 'face (if is-overrun 'org-focus-hud-overrun-face 'org-focus-hud-header-face))
              pad " [" time-str "] │\n"))
    (insert "╰" (make-string 77 ?─) "╯" (org-focus-hud--section-sep))

    ;; 2. Metadata Lines
    (insert "  " (propertize "PROJECT:" 'face 'bold) "  " parent "\n")
    (let ((slot-str (cond
                     ((and start-time end-time)
                      (if is-today-slot
                          (format "%s – %s (Today)"
                                  (format-time-string "%H:%M" start-time)
                                  (format-time-string "%H:%M" end-time))
                        (format "%s – %s (%s)"
                                (format-time-string "%H:%M" start-time)
                                (format-time-string "%H:%M" end-time)
                                (format-time-string "%a %b %d" start-time))))
                     (start-time
                      (format "%s (%s)"
                              (format-time-string "%H:%M" start-time)
                              (if is-today-slot "Today" (format-time-string "%a %b %d" start-time))))
                     (t "Flexible / Not Scheduled"))))
      (insert "  " (propertize "SLOT:   " 'face 'bold) "  " slot-str
              (format " · Effort: %dm" effort)
              (if pomo
                  (format " · Pomodoro: 🍅 [%dm/%dm]" (plist-get pomo :work) (plist-get pomo :break))
                "")
              (org-focus-hud--section-sep)))

    ;; 3. Progress Bar & Pacing
    (let* ((bar-str (concat
                    "["
                    (propertize (make-string filled-chars ?█) 'face 'org-focus-hud-progress-done-face)
                    (propertize (make-string empty-chars ?░) 'face 'org-focus-hud-progress-remain-face)
                    "]"))
          (rem-str (cond
                    ((<= rem-mins 0) "0m left")
                    ((>= rem-mins 60)
                     (format "%dh %02dm left (%dm)" (/ rem-mins 60) (% rem-mins 60) rem-mins))
                    (t (format "%dm left" rem-mins))))
          (status-str (cond
                       (is-overrun
                        (propertize (if slot-overrun-mins
                                        (format "⚠️ OVERRUN: +%dm past scheduled end!" overrun-mins)
                                      (format "⚠️ OVERRUN: +%dm over planned effort!" overrun-mins))
                                    'face 'org-focus-hud-overrun-face))
                       ((not is-clocked)
                        (format "TIME REMAINING: %s  " rem-str))
                       (t
                        (format "TIME REMAINING: %s" rem-str)))))
      (insert "  " status-str
              (if (not is-clocked)
                  (concat (if (string-suffix-p " " status-str) "" "  ")
                          (propertize "[PAUSED / NOT CLOCKED]" 'face 'org-focus-hud-overrun-face))
                "")
              "\n")
      (insert "  " bar-str (format " %dm clocked (%d%%)" clocked pct) (org-focus-hud--section-sep)))

    ;; 4. Checklist & Outline Bullets Box
    (let* ((raw-items (plist-get task-info :checklists))
           (items (org-focus-hud--analyze-checklists raw-items))
           (leaf-items (cl-remove-if (lambda (x) (plist-get x :has-children)) items))
           (box-items (cl-remove-if-not (lambda (x) (plist-get x :state)) items))
           (done-cnt (cl-count-if (lambda (x) (string-match-p "\\[[Xx]\\]" (plist-get x :state))) box-items))
           (tot-cnt (length box-items))
           (parent-effort (or (plist-get task-info :effort) 0))
           (leaf-done (cl-reduce #'+ (mapcar (lambda (x)
                                               (if (and (plist-get x :state)
                                                        (string-match-p "\\[[Xx]\\]" (plist-get x :state)))
                                                   (or (plist-get x :effort-mins) 0)
                                                 0))
                                             leaf-items)))
           (leaf-todo (cl-reduce #'+ (mapcar (lambda (x)
                                               (if (or (null (plist-get x :state))
                                                       (not (string-match-p "\\[[Xx]\\]" (plist-get x :state))))
                                                   (or (plist-get x :effort-mins) 0)
                                                 0))
                                             leaf-items)))
           (envelope-reserves (cl-reduce #'+ (mapcar (lambda (x)
                                                       (if (and (plist-get x :has-children)
                                                                (plist-get x :effort-mins))
                                                           (max 0 (- (plist-get x :effort-mins)
                                                                     (or (plist-get x :children-effort) 0)))
                                                         0))
                                                     items)))
           (done-effort leaf-done)
           (todo-effort (+ leaf-todo envelope-reserves))
           (planned-effort (+ done-effort todo-effort))
           (reserve (if (> parent-effort 0) (- parent-effort planned-effort) 0))
           (has-estimates (> planned-effort 0))
           (hdr (if (> tot-cnt 0)
                    (format "┌─ CHECKLIST [%d/%d] " done-cnt tot-cnt)
                  (if items
                      (format "┌─ CHECKLIST & OUTLINE (%d items) " (length items))
                    "┌─ CHECKLIST & OUTLINE ")))
           (hdr-line (concat hdr (make-string (max 0 (- 78 (length hdr) 1)) ?─) "┐")))
      (insert "  " (propertize hdr-line 'face 'org-focus-hud-box-face) "\n")
      (if (null items)
          (insert "  " (propertize "│ (No checklist or bullet items. Press 'k' to add one)" 'face 'org-focus-hud-box-face)
                  (make-string (max 0 (- 78 52)) ?\s)
                  (propertize "│" 'face 'org-focus-hud-box-face) "\n")
        ;; Render Concept B Allocation Bar if any item has an estimate
        (when has-estimates
          (let* ((bar-w 20)
                 (is-overrun (and (> parent-effort 0) (< reserve 0)))
                 (deficit (if is-overrun (abs reserve) 0))
                 (bar-str
                  (if (<= parent-effort 0)
                      (let* ((done-chars (min bar-w (round (* bar-w (/ (float done-effort) (max 1 planned-effort))))))
                             (todo-chars (- bar-w done-chars)))
                        (concat (propertize (make-string done-chars ?█) 'face 'org-focus-hud-progress-done-face)
                                (propertize (make-string todo-chars ?░) 'face 'org-focus-hud-transient-face)))
                    (if is-overrun
                        (let* ((budget-ratio (/ (float parent-effort) planned-effort))
                               (budget-chars (round (* bar-w budget-ratio)))
                               (deficit-chars (max 1 (- bar-w budget-chars)))
                               (adj-budget (- bar-w deficit-chars))
                               (done-chars (min adj-budget (round (* bar-w (/ (float done-effort) planned-effort)))))
                               (todo-chars (max 0 (- adj-budget done-chars))))
                          (concat (propertize (make-string done-chars ?█) 'face 'org-focus-hud-progress-done-face)
                                  (propertize (make-string todo-chars ?░) 'face 'org-focus-hud-transient-face)
                                  (propertize (make-string deficit-chars ?▓) 'face 'org-focus-hud-overrun-face)))
                      (let* ((done-chars (round (* bar-w (/ (float done-effort) parent-effort))))
                             (todo-chars (round (* bar-w (/ (float todo-effort) parent-effort))))
                             (clamped-todo (min todo-chars (- bar-w done-chars)))
                             (reserve-chars (max 0 (- bar-w done-chars clamped-todo))))
                        (concat (propertize (make-string done-chars ?█) 'face 'org-focus-hud-progress-done-face)
                                (propertize (make-string clamped-todo ?░) 'face 'org-focus-hud-transient-face)
                                (propertize (make-string reserve-chars ?·) 'face 'org-focus-hud-progress-remain-face))))))
                 (info-text
                  (if (<= parent-effort 0)
                      (format "%s Done · %s Left (no :EFFORT: set)"
                              (org-focus-hud--format-effort-human done-effort)
                              (org-focus-hud--format-effort-human todo-effort))
                    (if is-overrun
                        (format "%s planned · ⚠️ +%s deficit / %s"
                                (org-focus-hud--format-effort-human planned-effort)
                                (org-focus-hud--format-effort-human deficit)
                                (org-focus-hud--format-effort-human parent-effort))
                      (format "%s Done · %s Left · %s Reserve / %s"
                              (org-focus-hud--format-effort-human done-effort)
                              (org-focus-hud--format-effort-human todo-effort)
                              (org-focus-hud--format-effort-human reserve)
                              (org-focus-hud--format-effort-human parent-effort)))))
                 (alloc-prefix "ALLOC: [")
                 (alloc-suffix "] ")
                 (inner-w 73)
                 (text-with-bar (concat alloc-prefix (make-string bar-w ?X) alloc-suffix info-text))
                 (pad-count (max 0 (- inner-w (string-width text-with-bar))))
                 (alloc-line (concat "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                                     alloc-prefix bar-str alloc-suffix
                                     (if is-overrun
                                         (propertize info-text 'face 'org-focus-hud-overrun-face)
                                       info-text)
                                     (make-string pad-count ?\s)
                                     (propertize "│\n" 'face 'org-focus-hud-box-face)))
                 (sep-line (concat "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                                   (propertize (make-string inner-w ?─) 'face 'org-focus-hud-box-face)
                                   (propertize "│\n" 'face 'org-focus-hud-box-face))))
            (insert alloc-line)
            (insert sep-line)))
        (dolist (item items)
          (let* ((st (plist-get item :state))
                 (has-box (not (null st)))
                 (has-children (plist-get item :has-children))
                 (children-effort (or (plist-get item :children-effort) 0))
                 (is-done (or (and has-box (string-match-p "\\[[Xx]\\]" st))
                              (and has-children (plist-get item :all-children-done))))
                 (is-transient (or (and has-box (string-match-p "\\[[-]\\]" st))
                                   (and has-children (plist-get item :any-child-transient) (not is-done))))
                 (is-active-clocked (and (not has-children) has-box (string-match-p "\\[[-]\\]" st)))
                 (level (or (plist-get item :level) 0))
                 (indent-str (make-string (* level 2) ?\s))
                 (prefix-ptr (if is-active-clocked "▶ " "  "))
                 (bullet-sym (if has-box
                                 (cond (is-done "[X]")
                                       (is-transient "[-]")
                                       (t "[ ]"))
                               (if (string-match-p "^[0-9]" (or (plist-get item :bullet) ""))
                                   (plist-get item :bullet)
                                 "•")))
                 (raw-text (plist-get item :text))
                 (clean-text (or (plist-get item :clean-text) raw-text))
                 (effort-mins (plist-get item :effort-mins))
                 (badge-str
                  (if has-children
                      (if effort-mins
                          (if (> children-effort effort-mins)
                              (format "[%s / %s ⚠️]"
                                      (org-focus-hud--format-badge-effort children-effort)
                                      (org-focus-hud--format-badge-effort effort-mins))
                            (format "[%s / %s]"
                                    (org-focus-hud--format-badge-effort children-effort)
                                    (org-focus-hud--format-badge-effort effort-mins)))
                        (when (> children-effort 0)
                          (format "[Σ %s]" (org-focus-hud--format-badge-effort children-effort))))
                    (when effort-mins
                      (if is-done
                          (format "[✓ %s]" (org-focus-hud--format-badge-effort effort-mins))
                        (format "[%s]" (org-focus-hud--format-badge-effort effort-mins))))))
                 (badge-face
                  (if has-children
                      (if (and effort-mins (> children-effort effort-mins))
                          'org-focus-hud-overrun-face
                        'org-focus-hud-key-face)
                    (when effort-mins
                      (if is-done
                          'org-focus-hud-progress-done-face
                        'org-focus-hud-key-face))))
                 (timer-str
                  (when is-active-clocked
                    (let* ((elapsed (org-focus-hud--get-item-elapsed (plist-get task-info :marker) item))
                           (has-overrun (and effort-mins (> elapsed effort-mins)))
                           (pause-suffix (if (not is-clocked) " (paused)" "")))
                      (if has-overrun
                          (format "⏱️ ⚠️ %s (+%s)%s"
                                  (org-focus-hud--format-effort-human elapsed)
                                  (org-focus-hud--format-effort-human (- elapsed effort-mins))
                                  pause-suffix)
                        (if effort-mins
                            (format "⏱️ %s / %s%s"
                                    (org-focus-hud--format-effort-human elapsed)
                                    (org-focus-hud--format-badge-effort effort-mins)
                                    pause-suffix)
                          (format "⏱️ %s%s" (org-focus-hud--format-effort-human elapsed) pause-suffix))))))
                 (timer-face
                  (when is-active-clocked
                    (if (not is-clocked)
                        'shadow
                      (let* ((elapsed (org-focus-hud--get-item-elapsed (plist-get task-info :marker) item))
                             (has-overrun (and effort-mins (> elapsed effort-mins))))
                        (if has-overrun
                            'org-focus-hud-overrun-face
                          'org-focus-hud-transient-face)))))
                 (inner-w 73)
                 (left-prefix (format "%s%s%s " indent-str prefix-ptr bullet-sym))
                 (left-len (string-width left-prefix))
                 (badge-len (if badge-str (string-width badge-str) 0))
                 (timer-len (if timer-str (+ (string-width timer-str) 1) 0))
                 (right-len (+ badge-len timer-len))
                 (avail-text (max 0 (- inner-w left-len (if (> right-len 0) (1+ right-len) 0))))
                 (display-text (if (> (string-width clean-text) avail-text)
                                   (concat (substring clean-text 0 (max 0 (- avail-text 3))) "...")
                                 clean-text))
                 (pad-count (max 0 (- inner-w left-len (string-width display-text) right-len)))
                 (bullet-face (if has-box
                                  (cond (is-done 'org-focus-hud-progress-done-face)
                                        (is-transient 'org-focus-hud-transient-face)
                                        (t 'bold))
                                'org-focus-hud-section-face))
                 (line-str (propertize
                            (concat "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                                    indent-str
                                    (if is-active-clocked
                                        (propertize prefix-ptr 'face 'org-focus-hud-overrun-face)
                                      prefix-ptr)
                                    (propertize bullet-sym 'face bullet-face)
                                    " "
                                    (if is-done (propertize display-text 'face 'shadow) display-text)
                                    (make-string pad-count ?\s)
                                    (if timer-str (concat (propertize timer-str 'face timer-face) " ") "")
                                    (if badge-str (propertize badge-str 'face badge-face) "")
                                    (propertize "│" 'face 'org-focus-hud-box-face)
                                    "\n")
                            'focus-check-pos (plist-get item :pos)
                            'focus-check-text raw-text
                            'focus-has-box has-box
                            'focus-marker (plist-get task-info :marker)
                            'mouse-face 'highlight
                            'help-echo (if has-box
                                           "RET cycle [ ] → [-] → [X] · 'f' focus/clock · 'e' edit"
                                         "RET add checkbox · 'e' edit"))))
            (insert line-str))))
      (insert "  " (propertize (concat "└" (make-string 76 ?─) "┘") 'face 'org-focus-hud-box-face) (org-focus-hud--section-sep)))

    ;; 5. Subtasks Box
    (let ((subtasks (plist-get task-info :subtasks)))
      (when subtasks
        (let* ((hdr "┌─ SUBTASKS (CHILD TODOS) ")
               (hdr-line (concat hdr (make-string (max 0 (- 78 (length hdr) 1)) ?─) "┐")))
          (insert "  " (propertize hdr-line 'face 'org-focus-hud-box-face) "\n")
          (dolist (st subtasks)
            (let* ((state (plist-get st :state))
                   (st-title (plist-get st :title))
                   (is-done (member state (or org-done-keywords '("DONE"))))
                   (content (format "• [%s] %s" state st-title))
                   (max-len (- 78 6))
                   (trunc (if (> (length content) max-len)
                              (concat (substring content 0 (- max-len 3)) "...")
                            content))
                   (padding (make-string (max 0 (- 78 (length trunc) 5)) ?\s)))
              (insert "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                      (propertize (format "• [%s]" state) 'face (if is-done 'org-focus-hud-progress-done-face 'org-focus-hud-section-face))
                      " "
                      (if is-done (propertize st-title 'face 'shadow) st-title)
                      padding
                      (propertize "│" 'face 'org-focus-hud-box-face)
                      "\n")))
          (insert "  " (propertize (concat "└" (make-string 76 ?─) "┘") 'face 'org-focus-hud-box-face) (org-focus-hud--section-sep)))))

    ;; 6. Recent Notes Box (Quick notes from 'n')
    (let ((notes (plist-get task-info :notes)))
      (when notes
        (let* ((hdr "┌─ RECENT NOTES ")
               (hdr-line (concat hdr (make-string (max 0 (- 78 (length hdr) 1)) ?─) "┐")))
          (insert "  " (propertize hdr-line 'face 'org-focus-hud-box-face) "\n")
          (dolist (note notes)
            (let* ((max-len (- 78 6))
                   (trunc (if (> (length note) max-len)
                              (concat (substring note 0 (- max-len 3)) "...")
                            note))
                   (padding (make-string (max 0 (- 78 (length trunc) 5)) ?\s)))
              (insert "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                      trunc padding
                      (propertize "│" 'face 'org-focus-hud-box-face)
                      "\n")))
          (insert "  " (propertize (concat "└" (make-string 76 ?─) "┘") 'face 'org-focus-hud-box-face) (org-focus-hud--section-sep)))))

    ;; 7. Work Log Box (Fixed size scrollable, captured via 'l')
    (let* ((logs (plist-get task-info :logs))
           (tot-cnt (length logs))
           (h (max 1 (or org-focus-hud-log-height 5)))
           (max-off (max 0 (- tot-cnt h)))
           (eff-off (max 0 (min (or org-focus-hud--log-offset 0) max-off)))
           (start-idx (max 0 (- tot-cnt h eff-off)))
           (end-idx (min tot-cnt (+ start-idx h)))
           (visible-logs (if (> tot-cnt 0) (cl-subseq logs start-idx end-idx) nil))
           (has-older (> start-idx 0))
           (has-newer (< end-idx tot-cnt))
           (hdr (cond
                 ((= tot-cnt 0)
                  "┌─ WORK LOG [0] (Press 'l' to log) ")
                 ((<= tot-cnt h)
                  (format "┌─ WORK LOG [%d] (Press 'l' to log) " tot-cnt))
                 (t
                  (format "┌─ WORK LOG [%d-%d of %d] %s%s(Scroll: [ / ]) "
                          (1+ start-idx) end-idx tot-cnt
                          (if has-older "▲ " "")
                          (if has-newer "▼ " "")))))
           (hdr-line (concat hdr (make-string (max 0 (- 78 (length hdr) 1)) ?─) "┐")))
      ;; Keep local offset clamped to valid bounds
      (setq org-focus-hud--log-offset eff-off)
      (insert "  " (propertize hdr-line 'face 'org-focus-hud-box-face) "\n")
      (if (= tot-cnt 0)
          (progn
            (let* ((msg "│ (No log entries yet. Press 'l' to capture work done with timestamp)")
                   (pad (make-string (max 0 (- 78 (length msg) 1)) ?\s)))
              (insert "  " (propertize msg 'face 'org-focus-hud-box-face)
                      pad
                      (propertize "│" 'face 'org-focus-hud-box-face) "\n"))
            (unless org-focus-hud-compact
              (dotimes (_ (1- h))
                (insert "  " (propertize "│" 'face 'org-focus-hud-box-face)
                        (make-string 76 ?\s)
                        (propertize "│" 'face 'org-focus-hud-box-face) "\n"))))
        ;; Render visible window of log entries
        (dolist (item visible-logs)
          (let* ((item-str (if (listp item)
                               (or (plist-get item :formatted)
                                   (format "%s %s" (or (plist-get item :ts) "") (or (plist-get item :text) "")))
                             (format "%s" item)))
                 (max-len (- 78 4))
                 (trunc (if (> (length item-str) max-len)
                            (concat (substring item-str 0 (- max-len 3)) "...")
                          item-str))
                 (pad (make-string (max 0 (- 78 (length trunc) 3)) ?\s)))
            (insert "  " (propertize "│ " 'face 'org-focus-hud-box-face)
                    (if (string-match "^\\[[^\\]]+\\]" trunc)
                        (concat (propertize (match-string 0 trunc) 'face 'org-focus-hud-key-face)
                                (substring trunc (match-end 0)))
                      trunc)
                    pad
                    (propertize "│" 'face 'org-focus-hud-box-face)
                    "\n")))
        ;; Fill remaining lines if visible-logs < h
        (unless org-focus-hud-compact
          (dotimes (_ (- h (length visible-logs)))
            (insert "  " (propertize "│" 'face 'org-focus-hud-box-face)
                    (make-string 76 ?\s)
                    (propertize "│" 'face 'org-focus-hud-box-face) "\n"))))
      (insert "  " (propertize (concat "└" (make-string 76 ?─) "┘") 'face 'org-focus-hud-box-face) (org-focus-hud--section-sep)))

    ;; 8. Keybindings Footer Table (toggled with '?')
    (if org-focus-hud--show-help
        (progn
          (insert "  " (propertize "CAPTURE (Zero context switching)   ACTIONS & PACING" 'face 'org-focus-hud-section-face) "\n")
          (insert "  " (propertize (concat (make-string 33 ?─) "  " (make-string 42 ?─)) 'face 'org-focus-hud-box-face) "\n")
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[k]" 'face 'org-focus-hud-key-face) " + Checklist / bullet item")
                          (concat (propertize "[RET]" 'face 'org-focus-hud-key-face) " Cycle [ ] → [-] → [X]")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[f]" 'face 'org-focus-hud-key-face) " Focus item ([-] ⏱️)")
                          (concat (propertize "[e]" 'face 'org-focus-hud-key-face) "   Edit item / estimate")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[M-j]" 'face 'org-focus-hud-key-face) " / "
                                  (propertize "[M-k]" 'face 'org-focus-hud-key-face) " Move item down / up")
                          (concat (propertize "[r]" 'face 'org-focus-hud-key-face) "   Refresh node details")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[M-h]" 'face 'org-focus-hud-key-face) " / "
                                  (propertize "[M-l]" 'face 'org-focus-hud-key-face) " Outdent / Indent item")
                          (concat (propertize "[d]" 'face 'org-focus-hud-key-face) "   Mark DONE & Advance")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[l]" 'face 'org-focus-hud-key-face) " + Log Work Done ([timestamp])")
                          (concat (propertize "[w]" 'face 'org-focus-hud-key-face) "   Wait on this & Advance")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[n]" 'face 'org-focus-hud-key-face) " + Quick Note")
                          (concat (propertize "[+]" 'face 'org-focus-hud-key-face) "   Extend +15m")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[s]" 'face 'org-focus-hud-key-face) " + Child Subtask")
                          (concat (propertize "[p]" 'face 'org-focus-hud-key-face) "   Pause / Resume Clock")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[a]" 'face 'org-focus-hud-key-face) " + Sibling Task (after this)")
                          (concat (propertize "[q]" 'face 'org-focus-hud-key-face) "   Minimize HUD")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[[]" 'face 'org-focus-hud-key-face) " / "
                                  (propertize "[]]" 'face 'org-focus-hud-key-face) " Scroll Work Log")
                          (concat (propertize "[?]" 'face 'org-focus-hud-key-face) " Hide shortcuts help")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[o]" 'face 'org-focus-hud-key-face) " Open in Other Window")
                          (concat (propertize "[O]" 'face 'org-focus-hud-key-face) " Jump to Org File")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[z]" 'face 'org-focus-hud-key-face) " Toggle compact view")
                          (concat (propertize "[c]" 'face 'org-focus-hud-key-face) "   Clock into today's task"))))
      (insert "  " (propertize "[?]" 'face 'org-focus-hud-key-face)
              " " (propertize "Shortcuts" 'face 'shadow)
              "  ·  "
              (propertize "[c]" 'face 'org-focus-hud-key-face)
              " " (propertize "Clock-in" 'face 'shadow)
              "  ·  "
              (propertize "[z]" 'face 'org-focus-hud-key-face)
              " " (propertize (if org-focus-hud-compact "Spacious" "Compact") 'face 'shadow)
              "  ·  "
              (propertize "[l]" 'face 'org-focus-hud-key-face)
              " " (propertize "Log" 'face 'shadow)
              "  ·  "
              (propertize "[r]" 'face 'org-focus-hud-key-face)
              " " (propertize "Refresh" 'face 'shadow)
              "  ·  "
              (propertize "[q]" 'face 'org-focus-hud-key-face)
              " " (propertize "Minimize" 'face 'shadow)
              "\n"))))

(defun org-focus-hud--render-standby ()
  "Render standby view when no active or scheduled task is detected."
  (let ((time-str (format-time-string "%H:%M · %a %b %d")))
    (insert "╭" (make-string 77 ?─) "╮\n")
    (insert "│ ⏸️  ORG FOCUS HUD STANDBY"
            (make-string (max 0 (- 77 27 (length time-str))) ?\s)
            "[" time-str "] │\n")
    (insert "╰" (make-string 77 ?─) "╯" (org-focus-hud--section-sep))
    (insert "  NO ACTIVE OR SCHEDULED TASK DETECTED RIGHT NOW." (org-focus-hud--section-sep))
    (insert "  " (propertize "ACTIONS:" 'face 'org-focus-hud-section-face) "\n")
    (insert "  " (propertize (make-string 75 ?─) 'face 'org-focus-hud-box-face) "\n")
    (insert (format "  %s  Clock into a task from today's agenda\n"
                    (propertize "[c]" 'face 'org-focus-hud-key-face)))
    (insert (format "  %s  Refresh Focus HUD\n"
                    (propertize "[r]" 'face 'org-focus-hud-key-face)))
    (insert (format "  %s  Refresh Focus HUD\n"
                    (propertize "[g]" 'face 'org-focus-hud-key-face)))
    (insert (format "  %s  Close Focus HUD\n"
                    (propertize "[q]" 'face 'org-focus-hud-key-face)))
    (if org-focus-hud--show-help
        (progn
          (insert (format "  %s  Hide shortcuts help\n\n"
                          (propertize "[?]" 'face 'org-focus-hud-key-face)))
          (insert "  " (propertize "ALL FOCUS SHORTCUTS:" 'face 'org-focus-hud-section-face) "\n")
          (insert "  " (propertize (concat (make-string 33 ?─) "  " (make-string 42 ?─)) 'face 'org-focus-hud-box-face) "\n")
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[k]" 'face 'org-focus-hud-key-face) " + Checklist item")
                          (concat (propertize "[RET]" 'face 'org-focus-hud-key-face) " Toggle checklist item [ ] ↔ [X]")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[e]" 'face 'org-focus-hud-key-face) " Edit item / estimate")
                          (concat (propertize "[d]" 'face 'org-focus-hud-key-face) "   Mark DONE & Advance")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[n]" 'face 'org-focus-hud-key-face) " + Quick Note")
                          (concat (propertize "[d]" 'face 'org-focus-hud-key-face) "   Mark DONE & Advance")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[s]" 'face 'org-focus-hud-key-face) " + Child Subtask")
                          (concat (propertize "[w]" 'face 'org-focus-hud-key-face) "   Wait on this & Advance")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[a]" 'face 'org-focus-hud-key-face) " + Sibling Task (after this)")
                          (concat (propertize "[+]" 'face 'org-focus-hud-key-face) "   Extend +15m")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[o]" 'face 'org-focus-hud-key-face) " Open in Other Window")
                          (concat (propertize "[p]" 'face 'org-focus-hud-key-face) "   Pause / Resume Clock")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[O]" 'face 'org-focus-hud-key-face) " Jump to Org File")
                          (concat (propertize "[q]" 'face 'org-focus-hud-key-face) "   Minimize HUD")))
          (insert (format "  %-35s  %-42s\n"
                          (concat (propertize "[z]" 'face 'org-focus-hud-key-face) " Toggle compact view")
                          (concat (propertize "[c]" 'face 'org-focus-hud-key-face) "   Clock into today's task"))))
      (insert (format "  %s  Show all shortcuts\n"
                      (propertize "[?]" 'face 'org-focus-hud-key-face))))))

(defun org-focus-hud-refresh (&optional interactive-p)
  "Re-render the Focus HUD buffer, preserving cursor position if possible.
When INTERACTIVE-P is non-nil (e.g. called via `r' or `g'), echoes a confirmation."
  (interactive "p")
  (let ((buf (get-buffer "*Org Focus HUD*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let* ((orig-pos (point))
               (orig-check-pos (get-text-property (point) 'focus-check-pos))
               (task-info (org-focus-hud--resolve-task org-focus-hud--target-marker)))
          (when (and task-info (plist-get task-info :marker))
            (setq org-focus-hud--target-marker (plist-get task-info :marker)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (if task-info
                (org-focus-hud--render task-info)
              (org-focus-hud--render-standby))
            ;; Restore cursor to matching checklist if possible, else orig-pos
            (if orig-check-pos
                (let ((found nil))
                  (goto-char (point-min))
                  (while (and (not found) (not (eobp)))
                    (if (equal (get-text-property (point) 'focus-check-pos) orig-check-pos)
                        (setq found t)
                      (forward-line 1)))
                  (unless found
                    (goto-char (min orig-pos (point-max)))))
              (goto-char (min orig-pos (point-max)))))))
      (when interactive-p
        (message "Refreshed Focus HUD node details.")))))

(defun org-focus-hud-toggle-checklist (&optional arg)
  "Toggle checkbox state or focus of item at point in the Focus HUD.
With optional prefix ARG, forces binary toggle between [ ] and [X].
Without ARG, cycles:
  [ ] (Todo) → [-] (Active In-Progress & Timer) → [X] (Done & Log) → [ ]
When transitioning to [X], automatically logs milestone and duration
to :LOGBOOK: without intrusive prompts. If point is on a plain bullet, adds [ ]."
  (interactive "P")
  (let* ((pos (get-text-property (point) 'focus-check-pos))
         (has-box (get-text-property (point) 'focus-has-box))
         (check-text (get-text-property (point) 'focus-check-text))
         (m (or (get-text-property (point) 'focus-marker)
                org-focus-hud--target-marker)))
    (if (and pos m (markerp m) (marker-buffer m))
        (progn
          (org-with-point-at m
            (let* ((key (org-focus-hud--get-active-key m))
                 (all-items (org-focus-hud--get-checklists m))
                 (cur-item (or (cl-find-if (lambda (it) (equal (plist-get it :pos) pos)) all-items)
                               (when check-text
                                 (cl-find-if (lambda (it) (string= (plist-get it :text) check-text)) all-items)))))
            (save-excursion
              (if cur-item
                  (setq pos (plist-get cur-item :pos)))
              (goto-char pos)
              (beginning-of-line)
              (if (not has-box)
                  ;; Plain bullet: add [ ]
                  (if (looking-at "^\\([ \t]*[-+*]\\)[ \t]+")
                      (replace-match "\\1 [ ] ")
                    (org-toggle-checkbox '(4)))
                ;; Checkbox exists: match state
                (if (looking-at "^\\([ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\)\\[\\([ Xx-]\\)\\]")
                    (let* ((cur-st (match-string-no-properties 2))
                           (prefix (match-string 1)))
                      (cond
                       ;; If prefix arg given: standard 2-way toggle [ ] ↔ [X]
                       (arg
                        (if (string-match-p "[Xx]" cur-st)
                            (replace-match (concat prefix "[ ]"))
                          (replace-match (concat prefix "[X]"))))
                       ;; [ ] → [-] (Start active focus)
                       ((string= cur-st " ")
                        (replace-match (concat prefix "[-]"))
                        (save-match-data
                          (dolist (other all-items)
                            (when (and (not (equal (plist-get other :pos) pos))
                                       (string= (plist-get other :state) "[-]"))
                              (save-excursion
                                (goto-char (plist-get other :pos))
                                (beginning-of-line)
                                (when (looking-at "^\\([ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\)\\[-\\]")
                                  (replace-match "\\1[ ]"))))))
                        ;; Automatically clock in to parent task if not already clocked in
                        (unless (org-focus-hud--task-clocked-p m)
                          (save-match-data
                            (org-with-point-at m
                              (let ((org-focus-hud--inhibit-clock-hooks t))
                                (org-clock-in)))))
                        (when key
                          (puthash key (list :pos pos
                                             :start-clock (org-focus-hud--get-clocked-time m)
                                             :start-time (current-time)
                                             :title (plist-get cur-item :clean-text))
                                   org-focus-hud--active-checklist-table))
                        (message "Active focus: %s (Clocked in & Timer started)" (or (plist-get cur-item :clean-text) "Item")))
                       ;; [-] → [X] (Complete & auto-log milestone)
                       ((string= cur-st "-")
                        (replace-match (concat prefix "[X]"))
                        (let* ((active-data (when key (gethash key org-focus-hud--active-checklist-table)))
                               (start-clock (when active-data (plist-get active-data :start-clock)))
                               (cur-clock (org-focus-hud--get-clocked-time m))
                               (elapsed (if start-clock (max 0 (- cur-clock start-clock)) 0))
                               (effort (plist-get cur-item :effort-mins))
                               (clean-title (or (plist-get cur-item :clean-text) "Item"))
                               (breadcrumb (org-focus-hud--get-item-breadcrumb all-items cur-item))
                               (full-title (if breadcrumb (format "%s > %s" breadcrumb clean-title) clean-title))
                               (dur-str (if (> elapsed 0) (format "%dm" elapsed) "<1m"))
                               (est-str (when effort (format " · est: %s" (org-focus-hud--format-effort-human effort))))
                               (log-msg (format "Completed: %s (%s%s)" full-title dur-str (or est-str ""))))
                          (org-focus-hud--log-work-silent m log-msg)
                          (when key (remhash key org-focus-hud--active-checklist-table))
                          (message "Completed: %s (%s). Milestone logged." full-title dur-str)))
                       ;; [X] → [ ] (Reopen)
                       (t
                        (replace-match (concat prefix "[ ]"))
                        (when key (remhash key org-focus-hud--active-checklist-table))
                        (message "Reopened: %s" (or (plist-get cur-item :clean-text) "Item")))))
                  (org-toggle-checkbox)))
              (when (buffer-file-name (buffer-base-buffer))
                (save-buffer)))))
          (org-focus-hud-refresh))
      (message "No checklist or bullet item at point. Press 'k' to add one."))))

(defun org-focus-hud-focus-checklist ()
  "Set the checklist item at point as active in-progress ([-]) and start micro-timer.
If the item is already [-], pauses it back to [ ]. Clears active state from other items."
  (interactive)
  (let* ((pos (get-text-property (point) 'focus-check-pos))
         (has-box (get-text-property (point) 'focus-has-box))
         (check-text (get-text-property (point) 'focus-check-text))
         (m (or (get-text-property (point) 'focus-marker)
                org-focus-hud--target-marker)))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist item"))
    (org-with-point-at m
      (let* ((key (org-focus-hud--get-active-key m))
             (all-items (org-focus-hud--get-checklists m))
             (cur-item (or (cl-find-if (lambda (it) (equal (plist-get it :pos) pos)) all-items)
                           (when check-text
                             (cl-find-if (lambda (it) (string= (plist-get it :text) check-text)) all-items)))))
        (unless cur-item
          (user-error "Current item could not be located in task"))
        (setq pos (plist-get cur-item :pos))
        (save-excursion
          (goto-char pos)
          (beginning-of-line)
          (if (not has-box)
              (if (looking-at "^\\([ \t]*[-+*]\\)[ \t]+")
                  (progn
                    (replace-match "\\1 [-] ")
                    ;; Automatically clock in to parent task if not already clocked in
                    (unless (org-focus-hud--task-clocked-p m)
                      (save-match-data
                        (org-with-point-at m
                          (let ((org-focus-hud--inhibit-clock-hooks t))
                            (org-clock-in)))))
                    (when key
                      (puthash key (list :pos pos
                                         :start-clock (org-focus-hud--get-clocked-time m)
                                         :start-time (current-time)
                                         :title (plist-get cur-item :clean-text))
                               org-focus-hud--active-checklist-table))
                    (message "Active focus: %s (Clocked in & Timer started)" (or (plist-get cur-item :clean-text) "Item")))
                (org-toggle-checkbox '(4)))
            (if (looking-at "^\\([ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\)\\[\\([ Xx-]\\)\\]")
                (let ((prefix (match-string 1))
                      (cur-st (match-string-no-properties 2)))
                  (if (string= cur-st "-")
                      (progn
                        (replace-match (concat prefix "[ ]"))
                        (when key (remhash key org-focus-hud--active-checklist-table))
                        (message "Paused focus: %s" (or (plist-get cur-item :clean-text) "Item")))
                    (replace-match (concat prefix "[-]"))
                    (save-match-data
                      (dolist (other all-items)
                        (when (and (not (equal (plist-get other :pos) pos))
                                   (string= (plist-get other :state) "[-]"))
                          (save-excursion
                            (goto-char (plist-get other :pos))
                            (beginning-of-line)
                            (when (looking-at "^\\([ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\)\\[-\\]")
                              (replace-match "\\1[ ]"))))))
                    ;; Automatically clock in to parent task if not already clocked in
                    (unless (org-focus-hud--task-clocked-p m)
                      (save-match-data
                        (org-with-point-at m
                          (let ((org-focus-hud--inhibit-clock-hooks t))
                            (org-clock-in)))))
                    (when key
                      (puthash key (list :pos pos
                                         :start-clock (org-focus-hud--get-clocked-time m)
                                         :start-time (current-time)
                                         :title (plist-get cur-item :clean-text))
                               org-focus-hud--active-checklist-table))
                    (message "Active focus: %s (Clocked in & Timer started)" (or (plist-get cur-item :clean-text) "Item"))))
              (user-error "Could not match checkbox line"))))
        (when (buffer-file-name (buffer-base-buffer))
          (save-buffer))))
    (org-focus-hud-refresh)))

(defun org-focus-hud-edit-checklist ()
  "Edit the checklist or bullet item text and estimate at point in the Focus HUD.
Prompts with the current item text pre-filled. Updates the item line in
the underlying Org buffer while preserving checkbox state, bullet type,
and hierarchy indentation."
  (interactive)
  (let* ((pos (get-text-property (point) 'focus-check-pos))
         (check-text (get-text-property (point) 'focus-check-text))
         (m (or (get-text-property (point) 'focus-marker)
                org-focus-hud--target-marker
                (and (derived-mode-p 'org-mode)
                     (not (derived-mode-p 'org-agenda-mode))
                     (save-excursion (org-back-to-heading t) (point-marker))))))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist or bullet item"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (all-items (org-focus-hud--get-checklists m))
               (cur-item (or (cl-find-if (lambda (it) (equal (plist-get it :pos) pos)) all-items)
                             (when check-text
                               (cl-find-if (lambda (it) (string= (plist-get it :text) check-text)) all-items)))))
          (unless cur-item
            (user-error "Current item could not be located in task"))
          (setq pos (plist-get cur-item :pos))
          (let* ((cur-text (or (plist-get cur-item :text) ""))
                 (new-text (string-trim (read-string "Edit checklist item: " cur-text))))
            (when (string-empty-p new-text)
              (user-error "Checklist item text cannot be empty"))
            (goto-char pos)
            (beginning-of-line)
            (if (looking-at "^\\([ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\(?:\\[[ Xx-]\\][ \t]+\\)?\\)\\(.*\\)$")
                (replace-match (concat "\\1" new-text))
              (user-error "Failed to match list item line in buffer"))
            (when (buffer-file-name (buffer-base-buffer))
              (save-buffer))))))
    (org-focus-hud-refresh)
    (message "Updated checklist item.")))

(defun org-focus-hud--item-bounds (pos body-end)
  "Return (BEG . END) for the item block at POS, up to BODY-END.
The item block includes the bullet line and all continuation lines,
child sub-bullets, and notes indented deeper than the item's bullet."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let* ((beg (point))
           (indent (current-indentation))
           (item-end nil))
      (forward-line 1)
      (while (and (not item-end) (< (point) body-end))
        (cond
         ((looking-at "^\\*+[ \t]+")
          (setq item-end (point)))
         ((looking-at "^[ \t]*:[A-Za-z0-9_-]+:[ \t]*$")
          (setq item-end (point)))
         ((looking-at "^[ \t]*:END:?.*$")
          (setq item-end (point)))
         ((looking-at "^[ \t]*$")
          (let ((next-indent
                 (save-excursion
                   (while (and (< (point) body-end) (looking-at "^[ \t]*$"))
                     (forward-line 1))
                   (if (and (< (point) body-end)
                            (not (looking-at "^\\*+[ \t]+"))
                            (not (looking-at "^[ \t]*:[A-Za-z0-9_-]+:[ \t]*$"))
                            (not (looking-at "^[ \t]*:END:?.*$")))
                       (current-indentation)
                     -1))))
            (if (> next-indent indent)
                (forward-line 1)
              (setq item-end (point)))))
         (t
          (if (> (current-indentation) indent)
              (forward-line 1)
            (setq item-end (point))))))
      (cons beg (or item-end (point))))))

(defun org-focus-hud-add-checklist (item-text &optional as-bullet)
  "Add a checklist item or plain bullet with ITEM-TEXT to the current task.
If point is on a checklist or bullet item in the Focus HUD, inserts the
new item below the current item's block, matching its indentation.
If point is not on a list item, inserts after the last checklist item across the body.
If there are no checklist items, inserts at the start of the task body
(after all planning lines and drawers like :PROPERTIES: and :LOGBOOK:).
If ITEM-TEXT starts with a bullet marker (- , + , * ) or AS-BULLET is non-nil,
inserts as a plain bullet; otherwise inserts with a checkbox [ ]."
  (interactive
   (list (read-string (if current-prefix-arg "Plain bullet item: " "Checklist / bullet item: "))
         current-prefix-arg))
  (let* ((m (or (get-text-property (point) 'focus-marker)
                org-focus-hud--target-marker
                (and (derived-mode-p 'org-mode)
                     (not (derived-mode-p 'org-agenda-mode))
                     (save-excursion (org-back-to-heading t) (point-marker)))))
         (pos (get-text-property (point) 'focus-check-pos))
         (new-pos nil))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (when (string-empty-p (string-trim item-text))
      (user-error "Checklist item text cannot be empty"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (all-items (org-focus-hud--get-checklists m))
               (clean-text (string-trim item-text))
               (is-bullet (or as-bullet
                              (string-match-p "^[-+*][ \t]+" clean-text))))
          ;; Case 1: Cursor is on a specific list item in the Focus HUD
          (if (and pos (cl-find-if (lambda (it) (equal (plist-get it :pos) pos)) all-items))
              (let* ((cur-item (cl-find-if (lambda (it) (equal (plist-get it :pos) pos)) all-items))
                     (cur-bounds (org-focus-hud--item-bounds pos body-end))
                     (cur-indent (or (plist-get cur-item :indent)
                                     (save-excursion (goto-char (car cur-bounds)) (current-indentation))))
                     (indent-str (make-string cur-indent ?\s))
                     (new-line (if is-bullet
                                   (if (string-match-p "^[-+*][ \t]+" clean-text)
                                       (format "%s%s\n" indent-str clean-text)
                                     (format "%s- %s\n" indent-str clean-text))
                                 (format "%s- [ ] %s\n" indent-str clean-text))))
                (if (string-empty-p (string-trim (or (plist-get cur-item :text) "")))
                    (progn
                      (delete-region (car cur-bounds) (cdr cur-bounds))
                      (goto-char (car cur-bounds))
                      (setq new-pos (point))
                      (insert new-line))
                  (goto-char (cdr cur-bounds))
                  (unless (bolp) (insert "\n"))
                  (setq new-pos (point))
                  (insert new-line)))

            ;; Case 2: Cursor is NOT on a list item in the Focus HUD (e.g. pos is nil or stale)
            (if all-items
                ;; 2a. Task already has checklist items: append after the last checklist item
                (let* ((last-item (car (last all-items)))
                       (last-pos (plist-get last-item :pos))
                       (last-bounds (org-focus-hud--item-bounds last-pos body-end))
                       (last-indent (or (plist-get last-item :indent)
                                        (save-excursion (goto-char (car last-bounds)) (current-indentation))))
                       (indent-str (make-string last-indent ?\s))
                       (item-line (if is-bullet
                                      (if (string-match-p "^[-+*][ \t]+" clean-text)
                                          (format "%s%s\n" indent-str clean-text)
                                        (format "%s- %s\n" indent-str clean-text))
                                    (format "%s- [ ] %s\n" indent-str clean-text))))
                  (if (string-empty-p (string-trim (or (plist-get last-item :text) "")))
                      (progn
                        (delete-region (car last-bounds) (cdr last-bounds))
                        (goto-char (car last-bounds))
                        (setq new-pos (point))
                        (insert item-line))
                    (goto-char (cdr last-bounds))
                    (unless (bolp) (insert "\n"))
                    (setq new-pos (point))
                    (insert item-line)))

              ;; 2b. Task has NO checklist items: insert at body start (after all drawers)
              (let* ((body-start (org-focus-hud--body-start body-end))
                     (new-line (if is-bullet
                                   (if (string-match-p "^[-+*][ \t]+" clean-text)
                                       (format "  %s\n" clean-text)
                                     (format "  - %s\n" clean-text))
                                 (format "  - [ ] %s\n" clean-text))))
                (goto-char body-start)
                (unless (bolp) (insert "\n"))
                (setq new-pos (point))
                (insert new-line))))

          (ignore-errors (org-update-checkbox-count))
          (when (buffer-file-name (buffer-base-buffer)) (save-buffer)))))
    (org-focus-hud-refresh)
    (let ((hud-buf (get-buffer "*Org Focus HUD*")))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (goto-char (point-min))
          (let ((found nil))
            (when new-pos
              (while (and (not found) (not (eobp)))
                (if (equal (get-text-property (point) 'focus-check-pos) new-pos)
                    (progn (beginning-of-line) (setq found t))
                  (forward-line 1))))
            (unless found
              (goto-char (point-min))
              (when (search-forward (string-trim item-text) nil t)
                (beginning-of-line)))))))
    (message "Added checklist item: %s" item-text)))

(defun org-focus-hud-move-item-up ()
  "Move the checklist or bullet item at point UP in the Focus HUD.
Surgically swaps the item block (including its continuation lines,
child sub-bullets, and notes) with the preceding item block without
displacing or modifying any intervening node body text or images."
  (interactive)
  (let ((pos (get-text-property (point) 'focus-check-pos))
        (m (or (get-text-property (point) 'focus-marker)
               org-focus-hud--target-marker))
        (item-text (get-text-property (point) 'focus-check-text))
        (new-pos nil))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist or bullet item"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (all-items (org-focus-hud--get-checklists m))
               (cur-idx (cl-position pos all-items :key (lambda (it) (plist-get it :pos)))))
          (unless cur-idx
            (user-error "Current item could not be located in task"))
          (when (= cur-idx 0)
            (user-error "Item is already at the top of the list"))
          (let* ((cur-bounds (org-focus-hud--item-bounds pos body-end))
                 (cur-level (or (plist-get (nth cur-idx all-items) :level) 0))
                 (prev-item (or (cl-find-if (lambda (it)
                                              (and (< (plist-get it :pos) (car cur-bounds))
                                                   (<= (or (plist-get it :level) 0) cur-level)))
                                            (reverse (cl-subseq all-items 0 cur-idx)))
                                (nth (1- cur-idx) all-items)))
                 (prev-pos (plist-get prev-item :pos))
                 (prev-bounds (org-focus-hud--item-bounds prev-pos body-end))
                 (m-beg-prev (copy-marker (car prev-bounds)))
                 (block (delete-and-extract-region (car cur-bounds) (cdr cur-bounds))))
            (unless (string-suffix-p "
" block)
              (setq block (concat block "
")))
            (goto-char (marker-position m-beg-prev))
            (setq new-pos (point))
            (insert block)
            (set-marker m-beg-prev nil)
            (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))))
    (org-focus-hud-refresh)
    (let ((hud-buf (get-buffer "*Org Focus HUD*")))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (goto-char (point-min))
          (let ((found nil))
            (when new-pos
              (while (and (not found) (not (eobp)))
                (if (equal (get-text-property (point) 'focus-check-pos) new-pos)
                    (progn (beginning-of-line) (setq found t))
                  (forward-line 1))))
            (unless found
              (when item-text
                (goto-char (point-min))
                (when (search-forward item-text nil t)
                  (beginning-of-line))))))))))

(defun org-focus-hud-move-item-down ()
  "Move the checklist or bullet item at point DOWN in the Focus HUD.
Surgically swaps the item block (including its continuation lines,
child sub-bullets, and notes) with the succeeding item block without
displacing or modifying any intervening node body text or images."
  (interactive)
  (let ((pos (get-text-property (point) 'focus-check-pos))
        (m (or (get-text-property (point) 'focus-marker)
               org-focus-hud--target-marker))
        (item-text (get-text-property (point) 'focus-check-text))
        (new-pos nil))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist or bullet item"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (all-items (org-focus-hud--get-checklists m))
               (cur-bounds (org-focus-hud--item-bounds pos body-end))
               (next-item (cl-find-if (lambda (it) (>= (plist-get it :pos) (cdr cur-bounds)))
                                      all-items)))
          (unless next-item
            (user-error "Item is already at the bottom of the list"))
          (let* ((next-bounds (org-focus-hud--item-bounds (plist-get next-item :pos) body-end))
                 (m-end-next (copy-marker (cdr next-bounds) t))
                 (block (delete-and-extract-region (car cur-bounds) (cdr cur-bounds))))
            (unless (string-suffix-p "
" block)
              (setq block (concat block "
")))
            (goto-char (marker-position m-end-next))
            (unless (bolp) (insert "
"))
            (setq new-pos (point))
            (insert block)
            (set-marker m-end-next nil)
            (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))))
    (org-focus-hud-refresh)
    (let ((hud-buf (get-buffer "*Org Focus HUD*")))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (goto-char (point-min))
          (let ((found nil))
            (when new-pos
              (while (and (not found) (not (eobp)))
                (if (equal (get-text-property (point) 'focus-check-pos) new-pos)
                    (progn (beginning-of-line) (setq found t))
                  (forward-line 1))))
            (unless found
              (when item-text
                (goto-char (point-min))
                (when (search-forward item-text nil t)
                  (beginning-of-line))))))))))

(defun org-focus-hud-indent-item ()
  "Indent (demote) the checklist or bullet item block at point by 2 spaces."
  (interactive)
  (let ((pos (get-text-property (point) 'focus-check-pos))
        (m (or (get-text-property (point) 'focus-marker)
               org-focus-hud--target-marker))
        (item-text (get-text-property (point) 'focus-check-text)))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist or bullet item"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (cur-bounds (org-focus-hud--item-bounds pos body-end)))
          (indent-rigidly (car cur-bounds) (cdr cur-bounds) 2)
          (when (buffer-file-name (buffer-base-buffer)) (save-buffer)))))
    (org-focus-hud-refresh)
    (let ((hud-buf (get-buffer "*Org Focus HUD*")))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (if (equal (get-text-property (point) 'focus-check-pos) pos)
                  (progn (beginning-of-line) (setq found t))
                (forward-line 1)))
            (unless found
              (when item-text
                (goto-char (point-min))
                (when (search-forward item-text nil t)
                  (beginning-of-line))))))))))

(defun org-focus-hud-outdent-item ()
  "Outdent (promote) the checklist or bullet item block at point by 2 spaces."
  (interactive)
  (let ((pos (get-text-property (point) 'focus-check-pos))
        (m (or (get-text-property (point) 'focus-marker)
               org-focus-hud--target-marker))
        (item-text (get-text-property (point) 'focus-check-text)))
    (unless (and pos m (markerp m) (marker-buffer m))
      (user-error "Point is not on a checklist or bullet item"))
    (org-with-point-at m
      (org-back-to-heading t)
      (save-excursion
        (let* ((body-end (save-excursion
                           (or (and (org-goto-first-child) (point))
                               (and (outline-next-heading) (point))
                               (point-max))))
               (cur-bounds (org-focus-hud--item-bounds pos body-end))
               (cur-indent (save-excursion (goto-char (car cur-bounds)) (current-indentation))))
          (if (<= cur-indent 0)
              (message "Item cannot be outdented further")
            (indent-rigidly (car cur-bounds) (cdr cur-bounds) (- (min 2 cur-indent)))
            (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))))
    (org-focus-hud-refresh)
    (let ((hud-buf (get-buffer "*Org Focus HUD*")))
      (when (buffer-live-p hud-buf)
        (with-current-buffer hud-buf
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (if (equal (get-text-property (point) 'focus-check-pos) pos)
                  (progn (beginning-of-line) (setq found t))
                (forward-line 1)))
            (unless found
              (when item-text
                (goto-char (point-min))
                (when (search-forward item-text nil t)
                  (beginning-of-line))))))))))

(defun org-focus-hud-add-note (note-text)
  "Add a quick timestamped note with NOTE-TEXT to the current task."
  (interactive "sQuick note: ")
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (when (string-empty-p (string-trim note-text))
      (user-error "Note text cannot be empty"))
    (let ((ts (format-time-string "%H:%M")))
      (org-with-point-at m
        (org-back-to-heading t)
        (save-excursion
          (let* ((body-end (save-excursion
                             (or (and (org-goto-first-child) (point))
                                 (and (outline-next-heading) (point))
                                 (point-max))))
                 (meta-end (save-excursion
                             (org-back-to-heading t)
                             (org-end-of-meta-data t)
                             (min (point) body-end))))
            (goto-char (marker-position m))
            (org-back-to-heading t)
            (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" body-end t)
                (progn
                  (if (re-search-forward "^[ \t]*:END:[ \t]*$" body-end t)
                      (goto-char (match-beginning 0))
                    (goto-char body-end))
                  (insert (format "  - [%s] %s\n" ts (string-trim note-text))))
              (goto-char meta-end)
              (unless (bolp) (insert "\n"))
              (insert (format "  - [%s] %s\n" ts (string-trim note-text))))
            (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))))
    (org-focus-hud-refresh)
    (message "Note saved.")))

(defun org-focus-hud-log-work (work-text)
  "Log WORK-TEXT with an inactive timestamp under the current task's :LOGBOOK:."
  (interactive "sWork done: ")
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (when (string-empty-p (string-trim work-text))
      (user-error "Log text cannot be empty"))
    (let ((ts (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (org-with-point-at m
        (org-back-to-heading t)
        (save-excursion
          (let* ((body-end (save-excursion
                             (or (and (org-goto-first-child) (point))
                                 (and (outline-next-heading) (point))
                                 (point-max))))
                 (meta-end (save-excursion
                             (org-back-to-heading t)
                             (org-end-of-meta-data t)
                             (min (point) body-end))))
            (goto-char (marker-position m))
            (org-back-to-heading t)
            (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" body-end t)
                (progn
                  (if (re-search-forward "^[ \t]*:END:[ \t]*$" body-end t)
                      (goto-char (match-beginning 0))
                    (goto-char body-end))
                  (insert (format "  - %s %s\n" ts (string-trim work-text))))
              (goto-char meta-end)
              (unless (bolp) (insert "\n"))
              (insert "  :LOGBOOK:\n"
                      (format "  - %s %s\n" ts (string-trim work-text))
                      "  :END:\n"))
            (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))))
    (setq org-focus-hud--log-offset 0)
    (org-focus-hud-refresh)
    (message "Logged work done with inactive timestamp.")))

(defun org-focus-hud-log-scroll-up ()
  "Scroll up the Work Log section in Focus HUD to view older entries."
  (interactive)
  (setq org-focus-hud--log-offset
        (1+ (or org-focus-hud--log-offset 0)))
  (org-focus-hud-refresh))

(defun org-focus-hud-log-scroll-down ()
  "Scroll down the Work Log section in Focus HUD to view newer entries."
  (interactive)
  (setq org-focus-hud--log-offset
        (max 0 (1- (or org-focus-hud--log-offset 0))))
  (org-focus-hud-refresh))

(defun org-focus-hud-next-checklist ()
  "Move point to the next checklist item in the Focus HUD."
  (interactive)
  (let ((pos (next-single-property-change (point) 'focus-check-pos)))
    (if pos
        (goto-char pos)
      (goto-char (point-min))
      (let ((p2 (next-single-property-change (point) 'focus-check-pos)))
        (when p2 (goto-char p2))))))

(defun org-focus-hud-prev-checklist ()
  "Move point to the previous checklist item in the Focus HUD."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'focus-check-pos)))
    (if pos
        (goto-char pos)
      (goto-char (point-max))
      (let ((p2 (previous-single-property-change (point) 'focus-check-pos)))
        (when p2 (goto-char p2))))))

(defun org-focus-hud-add-subtask (title &optional effort)
  "Add a child subtask with TITLE and optional EFFORT under the current task."
  (interactive "sSubtask title: 
sEffort (e.g. 20m, optional): ")
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (when (string-empty-p (string-trim title))
      (user-error "Subtask title cannot be empty"))
    (org-with-point-at m
      (org-back-to-heading t)
      (let* ((parent-level (or (org-current-level) 1))
             (child-level (1+ parent-level))
             (stars (make-string child-level ?*)))
        ;; Jump past all existing children and direct body of the current heading
        (org-end-of-subtree t t)
        (unless (bolp) (insert "
"))
        (let ((insert-pos (point)))
          (insert (format "%s TODO %s
" stars (string-trim title)))
          (goto-char insert-pos)
          (when (and effort (not (string-empty-p (string-trim effort))))
            (org-entry-put nil "EFFORT" (string-trim effort))))
        (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))
    (org-focus-hud-refresh)
    (message "Created child subtask: %s" title)))

(defun org-focus-hud-add-sibling (title &optional effort)
  "Add a sibling task with TITLE and optional EFFORT directly after current task."
  (interactive "sSibling task title: 
sEffort (e.g. 30m, optional): ")
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (when (string-empty-p (string-trim title))
      (user-error "Sibling title cannot be empty"))
    (org-with-point-at m
      (org-back-to-heading t)
      (let* ((cur-level (or (org-current-level) 1))
             (stars (make-string cur-level ?*)))
        ;; Jump past entire subtree of current heading before inserting sibling
        (org-end-of-subtree t t)
        (unless (bolp) (insert "
"))
        (let ((insert-pos (point)))
          (insert (format "%s TODO %s
" stars (string-trim title)))
          (goto-char insert-pos)
          (when org-focus-hud-sibling-tag (org-toggle-tag org-focus-hud-sibling-tag 'on))
          (when (and effort (not (string-empty-p (string-trim effort))))
            (org-entry-put nil "EFFORT" (string-trim effort))))
        (when (buffer-file-name (buffer-base-buffer)) (save-buffer))))
    (org-focus-hud-refresh)
    (message "Sibling task '%s' created and queued after current task." title)))

(defun org-focus-hud-done ()
  "Mark current task DONE, clock out, and auto-advance to next scheduled task."
  (interactive)
  (let ((org-focus-hud--inhibit-clock-hooks t)
        (m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (org-with-point-at m
      (org-todo "DONE")
      (when (org-focus-hud--task-clocked-p m)
        (org-clock-out nil t))
      (when (buffer-file-name) (save-buffer)))
    (run-hooks 'org-focus-hud-on-done-hook)
    (when (fboundp 'org-auto-scheduler--on-todo-state-change)
      (let ((org-state "DONE"))
        (org-auto-scheduler--on-todo-state-change)))
    (if org-focus-hud-auto-clock-in-on-advance
        (let* ((today-tasks (ignore-errors (org-focus-hud--get-today-tasks)))
               (next-task nil))
          (dolist (tk today-tasks)
            (when (and (not next-task)
                       (not (plist-get tk :is-done))
                       (not (equal (plist-get tk :marker) m)))
              (setq next-task tk)))
          (if next-task
              (let ((nm (plist-get next-task :marker)))
                (org-with-point-at nm
                  (org-clock-in))
                (setq org-focus-hud--target-marker nm)
                (org-focus-hud-refresh)
                (message "Marked DONE. Advanced and clocked into: %s" (plist-get next-task :headline)))
            (setq org-focus-hud--target-marker nil)
            (org-focus-hud-refresh)
            (message "Task DONE! All scheduled tasks for today completed! 🎉")))
      (setq org-focus-hud--target-marker nil)
      (org-focus-hud-refresh)
      (message "Task marked DONE."))))


(defun org-focus-hud-wait (&optional note tickler-date)
  "Transition current task to WAITING, log an optional NOTE, and auto-advance.
Prompts for an optional reason/note and an optional follow-up TICKLER-DATE.
Clocks out of current task and auto-advances/clocks into the next scheduled task."
  (interactive
   (list (let ((n (read-string "Waiting on / Note (optional, RET to skip): ")))
           (if (string-empty-p (string-trim n)) nil n))
         (let ((t-ans (read-string "Follow-up tickler date (e.g. +3d, 2026-10-05, RET to skip): ")))
           (if (string-empty-p (string-trim t-ans))
               nil
             (condition-case nil
                 (org-read-date nil nil t-ans)
               (error t-ans))))))
  (let ((org-focus-hud--inhibit-clock-hooks t)
        (m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (let* ((waiting-state (or (car org-focus-hud-waiting-states) "WAITING")))
      (org-with-point-at m
        ;; Change TODO state to WAITING (hook clocks out and strips time)
        (org-todo waiting-state)
        ;; If tickler provided, set scheduled date
        (when (and tickler-date (not (string-empty-p (string-trim tickler-date))))
          (let ((target-day (if (>= (length tickler-date) 10) (substring tickler-date 0 10) tickler-date)))
            (org-schedule nil target-day)))
        ;; If note provided, log it
        (when (and note (not (string-empty-p (string-trim note))))
          (let ((ts (format-time-string "%H:%M")))
            (save-excursion
              (goto-char (marker-position m))
              (let ((task-end (save-excursion (or (outline-next-heading) (point-max)))))
                (if (re-search-forward ":LOGBOOK:" task-end t)
                    (progn
                      (if (re-search-forward ":END:" task-end t)
                          (goto-char (match-beginning 0))
                        (goto-char task-end))
                      (insert (format "  - [%s] WAITING: %s\n" ts (string-trim note))))
                  (org-end-of-meta-data t)
                  (unless (bolp) (insert "\n"))
                  (insert (format "  - [%s] WAITING: %s\n" ts (string-trim note))))))))
        ;; Ensure clocked out
        (when (org-focus-hud--task-clocked-p m)
          (org-clock-out nil t))
        (when (buffer-file-name) (save-buffer)))
      ;; Advance to next task if configured
      (if org-focus-hud-auto-clock-in-on-advance
          (let* ((today-tasks (ignore-errors (org-focus-hud--get-today-tasks)))
                 (next-task nil))
            (dolist (tk today-tasks)
              (when (and (not next-task)
                         (not (plist-get tk :is-done))
                         (not (member (plist-get tk :state) org-focus-hud-waiting-states))
                         (not (equal (plist-get tk :marker) m)))
                (setq next-task tk)))
            (if next-task
                (let ((nm (plist-get next-task :marker)))
                  (org-with-point-at nm
                    (org-clock-in))
                  (setq org-focus-hud--target-marker nm)
                  (org-focus-hud-refresh)
                  (message "Moved to WAITING. Advanced and clocked into: %s" (plist-get next-task :headline)))
              (setq org-focus-hud--target-marker nil)
              (org-focus-hud-refresh)
              (message "Task moved to WAITING. No more scheduled tasks for today.")))
        (setq org-focus-hud--target-marker nil)
        (org-focus-hud-refresh)
        (message "Task moved to WAITING.")))))

(defun org-focus-hud-extend (&optional minutes)
  "Extend the current task by MINUTES (default 15) and repack downstream tasks."
  (interactive (list (read-number "Extend current task by minutes: " 15)))
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (let ((mins (or minutes 15)))
      (cond
       ((bound-and-true-p org-focus-hud-extend-task-function)
        (funcall org-focus-hud-extend-task-function mins))
       ((fboundp 'org-auto-scheduler-extend-current-task)
        (org-auto-scheduler-extend-current-task mins))
       (t
        (org-with-point-at m
          (let* ((cur (or (ignore-errors (round (org-duration-to-minutes (org-entry-get nil "Effort")))) 0))
                 (new (+ cur mins))
                 (h (/ new 60))
                 (rem (% new 60))
                 (str (format "%d:%02d" h rem)))
            (org-entry-put nil "Effort" str)
            (message "Extended task effort by %dm (new effort: %s)" mins str))))))
    (org-focus-hud-refresh)))

(defun org-focus-hud-toggle-pause ()
  "Toggle pause/resume clock on the current task."
  (interactive)
  (let ((org-focus-hud--inhibit-clock-hooks t)
        (m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (if (org-focus-hud--task-clocked-p m)
        (progn
          (org-clock-out)
          (org-focus-hud-refresh)
          (message "Clock PAUSED."))
      (org-with-point-at m
        (org-clock-in))
      (org-focus-hud-refresh)
      (message "Clock RESUMED."))))

(defun org-focus-hud-toggle-compact ()
  "Toggle between compact layout and spacious layout in Focus HUD."
  (interactive)
  (setq org-focus-hud-compact (not org-focus-hud-compact))
  (setq org-focus-hud-section-spacing (if org-focus-hud-compact 0 1))
  (org-focus-hud-refresh)
  (message "Focus HUD compact mode: %s" (if org-focus-hud-compact "ON (0 blank lines)" "OFF (spacious)")))

(defun org-focus-hud-toggle-help ()
  "Toggle visibility of the shortkey legend in the Focus HUD."
  (interactive)
  (setq org-focus-hud--show-help (not org-focus-hud--show-help))
  (org-focus-hud-refresh)
  (message (if org-focus-hud--show-help
               "Shortcuts legend displayed. Press '?' again to hide."
             "Shortcuts legend hidden. Press '?' to view.")))

(defun org-focus-hud-quit ()
  "Dismiss the Focus HUD window. Clock remains running."
  (interactive)
  (quit-window t))

(defun org-focus-hud-goto-task ()
  "Jump to the original Org buffer and headline for the current task."
  (interactive)
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (switch-to-buffer (marker-buffer m))
    (goto-char (marker-position m))
    (org-reveal)
    (org-show-entry)))

(defun org-focus-hud-goto-task-other-window ()
  "Jump to the original Org buffer and headline for the current task in another window."
  (interactive)
  (let ((m org-focus-hud--target-marker))
    (unless (and m (markerp m) (marker-buffer m))
      (user-error "No active task in Focus HUD"))
    (switch-to-buffer-other-window (marker-buffer m))
    (goto-char (marker-position m))
    (org-reveal)
    (org-show-entry)))

(defun org-focus-hud-clock-in-task ()
  "Select a scheduled task from today to clock into and view in Focus HUD."
  (interactive)
  (let* ((org-focus-hud--inhibit-clock-hooks t)
         (today-tasks (ignore-errors (org-focus-hud--get-today-tasks)))
         (active-tasks (cl-remove-if (lambda (tk) (plist-get tk :is-done)) today-tasks)))
    (if (null active-tasks)
        (user-error "No pending scheduled tasks found for today")
      (let* ((choices (mapcar (lambda (tk)
                                (cons (format "%s [%s]" (plist-get tk :headline)
                                              (format-time-string "%H:%M" (plist-get tk :start)))
                                      tk))
                              active-tasks))
             (selection (completing-read "Clock into task: " (mapcar #'car choices) nil t))
             (tk (cdr (assoc selection choices))))
        (when tk
          (let ((m (plist-get tk :marker)))
            (org-with-point-at m
              (org-clock-in))
            (setq org-focus-hud--target-marker m)
            (org-focus-hud-refresh)
            (message "Clocked into: %s" (plist-get tk :headline))))))))

(defvar org-focus-hud-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "k") #'org-focus-hud-add-checklist)
    (define-key map (kbd "e") #'org-focus-hud-edit-checklist)
    (define-key map (kbd "E") #'org-focus-hud-edit-checklist)
    (define-key map (kbd "K") #'previous-line)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "TAB") #'org-focus-hud-next-checklist)
    (define-key map (kbd "<backtab>") #'org-focus-hud-prev-checklist)
    ;; RET toggles checklist; SPC left untouched for Spacemacs leader and scrolling
    (define-key map (kbd "RET") #'org-focus-hud-toggle-checklist)
    (define-key map (kbd "f") #'org-focus-hud-focus-checklist)
    (define-key map (kbd "F") #'org-focus-hud-focus-checklist)
    (define-key map (kbd "n") #'org-focus-hud-add-note)
    (define-key map (kbd "s") #'org-focus-hud-add-subtask)
    (define-key map (kbd "a") #'org-focus-hud-add-sibling)
    (define-key map (kbd "d") #'org-focus-hud-done)
    (define-key map (kbd "w") #'org-focus-hud-wait)
    (define-key map (kbd "+") #'org-focus-hud-extend)
    (define-key map (kbd "=") #'org-focus-hud-extend)
    (define-key map (kbd "p") #'org-focus-hud-toggle-pause)
    (define-key map (kbd "?") #'org-focus-hud-toggle-help)
    (define-key map (kbd "q") #'org-focus-hud-quit)
    (define-key map (kbd "g") #'org-focus-hud-refresh)
    (define-key map (kbd "o") #'org-focus-hud-goto-task-other-window)
    (define-key map (kbd "O") #'org-focus-hud-goto-task)
    (define-key map (kbd "c") #'org-focus-hud-clock-in-task)
    (define-key map (kbd "r") #'org-focus-hud-refresh)
    (define-key map (kbd "z") #'org-focus-hud-toggle-compact)
    (define-key map (kbd "l") #'org-focus-hud-log-work)
    (define-key map (kbd "[") #'org-focus-hud-log-scroll-up)
    (define-key map (kbd "]") #'org-focus-hud-log-scroll-down)
    (define-key map (kbd "M-k") #'org-focus-hud-move-item-up)
    (define-key map (kbd "M-<up>") #'org-focus-hud-move-item-up)
    (define-key map (kbd "M-j") #'org-focus-hud-move-item-down)
    (define-key map (kbd "M-<down>") #'org-focus-hud-move-item-down)
    (define-key map (kbd "M-h") #'org-focus-hud-outdent-item)
    (define-key map (kbd "M-<left>") #'org-focus-hud-outdent-item)
    (define-key map (kbd "M-l") #'org-focus-hud-indent-item)
    (define-key map (kbd "M-<right>") #'org-focus-hud-indent-item)
    map)
  "Keymap for `org-focus-hud-mode'.")

;; Ensure reload updates keymap even if defvar was previously initialized
(define-key org-focus-hud-mode-map (kbd "o") #'org-focus-hud-goto-task-other-window)
(define-key org-focus-hud-mode-map (kbd "O") #'org-focus-hud-goto-task)
(define-key org-focus-hud-mode-map (kbd "?") #'org-focus-hud-toggle-help)
(define-key org-focus-hud-mode-map (kbd "r") #'org-focus-hud-refresh)
(define-key org-focus-hud-mode-map (kbd "z") #'org-focus-hud-toggle-compact)
(define-key org-focus-hud-mode-map (kbd "g") #'org-focus-hud-refresh)
(define-key org-focus-hud-mode-map (kbd "l") #'org-focus-hud-log-work)
(define-key org-focus-hud-mode-map (kbd "[") #'org-focus-hud-log-scroll-up)
(define-key org-focus-hud-mode-map (kbd "]") #'org-focus-hud-log-scroll-down)
(define-key org-focus-hud-mode-map (kbd "M-k") #'org-focus-hud-move-item-up)
(define-key org-focus-hud-mode-map (kbd "M-<up>") #'org-focus-hud-move-item-up)
(define-key org-focus-hud-mode-map (kbd "M-j") #'org-focus-hud-move-item-down)
(define-key org-focus-hud-mode-map (kbd "M-<down>") #'org-focus-hud-move-item-down)
(define-key org-focus-hud-mode-map (kbd "M-h") #'org-focus-hud-outdent-item)
(define-key org-focus-hud-mode-map (kbd "M-<left>") #'org-focus-hud-outdent-item)
(define-key org-focus-hud-mode-map (kbd "M-l") #'org-focus-hud-indent-item)
(define-key org-focus-hud-mode-map (kbd "M-<right>") #'org-focus-hud-indent-item)

(define-derived-mode org-focus-hud-mode special-mode "Org-Focus-HUD"
  "Major mode for the Org Auto Scheduler Focus HUD cockpit.
\{org-focus-hud-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq org-focus-hud--show-help org-focus-hud-show-help)
  (add-hook 'kill-buffer-hook #'org-focus-hud--cleanup nil t)
  ;; Evil / Spacemacs compatibility: ensure HUD single-key shortcuts win in motion/normal/visual
  (when (and (featurep 'evil) (fboundp 'evil-local-set-key))
    (dolist (st '(motion normal visual))
      (evil-local-set-key st (kbd "k")         #'org-focus-hud-add-checklist)
      (evil-local-set-key st (kbd "e")         #'org-focus-hud-edit-checklist)
      (evil-local-set-key st (kbd "E")         #'org-focus-hud-edit-checklist)
      (evil-local-set-key st (kbd "K")         #'previous-line)
      (evil-local-set-key st (kbd "j")         #'next-line)
      (evil-local-set-key st (kbd "TAB")       #'org-focus-hud-next-checklist)
      (evil-local-set-key st (kbd "<backtab>") #'org-focus-hud-prev-checklist)
      (evil-local-set-key st (kbd "RET")       #'org-focus-hud-toggle-checklist)
      (evil-local-set-key st (kbd "f")         #'org-focus-hud-focus-checklist)
      (evil-local-set-key st (kbd "F")         #'org-focus-hud-focus-checklist)
      (evil-local-set-key st (kbd "n")         #'org-focus-hud-add-note)
      (evil-local-set-key st (kbd "s")         #'org-focus-hud-add-subtask)
      (evil-local-set-key st (kbd "a")         #'org-focus-hud-add-sibling)
      (evil-local-set-key st (kbd "d")         #'org-focus-hud-done)
      (evil-local-set-key st (kbd "w")         #'org-focus-hud-wait)
      (evil-local-set-key st (kbd "+")         #'org-focus-hud-extend)
      (evil-local-set-key st (kbd "=")         #'org-focus-hud-extend)
      (evil-local-set-key st (kbd "p")         #'org-focus-hud-toggle-pause)
      (evil-local-set-key st (kbd "?")         #'org-focus-hud-toggle-help)
      (evil-local-set-key st (kbd "q")         #'org-focus-hud-quit)
      (evil-local-set-key st (kbd "g")         #'org-focus-hud-refresh)
      (evil-local-set-key st (kbd "o")         #'org-focus-hud-goto-task-other-window)
      (evil-local-set-key st (kbd "O")         #'org-focus-hud-goto-task)
      (evil-local-set-key st (kbd "c")         #'org-focus-hud-clock-in-task)
      (evil-local-set-key st (kbd "r")         #'org-focus-hud-refresh)
      (evil-local-set-key st (kbd "l")         #'org-focus-hud-log-work)
      (evil-local-set-key st (kbd "[")         #'org-focus-hud-log-scroll-up)
      (evil-local-set-key st (kbd "]")         #'org-focus-hud-log-scroll-down)
      (evil-local-set-key st (kbd "M-k")       #'org-focus-hud-move-item-up)
      (evil-local-set-key st (kbd "M-<up>")     #'org-focus-hud-move-item-up)
      (evil-local-set-key st (kbd "M-j")       #'org-focus-hud-move-item-down)
      (evil-local-set-key st (kbd "M-<down>")   #'org-focus-hud-move-item-down)
      (evil-local-set-key st (kbd "M-h")       #'org-focus-hud-outdent-item)
      (evil-local-set-key st (kbd "M-<left>")   #'org-focus-hud-outdent-item)
      (evil-local-set-key st (kbd "M-l")       #'org-focus-hud-indent-item)
      (evil-local-set-key st (kbd "M-<right>")  #'org-focus-hud-indent-item))))

;; Evil/Spacemacs compatibility for Focus HUD mode map:
(with-eval-after-load 'evil
  (dolist (state '(normal motion visual))
    (evil-define-key state org-focus-hud-mode-map
      (kbd "k")         #'org-focus-hud-add-checklist
      (kbd "e")         #'org-focus-hud-edit-checklist
      (kbd "E")         #'org-focus-hud-edit-checklist
      (kbd "K")         #'previous-line
      (kbd "j")         #'next-line
      (kbd "TAB")       #'org-focus-hud-next-checklist
      (kbd "<backtab>") #'org-focus-hud-prev-checklist
      (kbd "RET")       #'org-focus-hud-toggle-checklist
      (kbd "f")         #'org-focus-hud-focus-checklist
      (kbd "F")         #'org-focus-hud-focus-checklist
      (kbd "n")         #'org-focus-hud-add-note
      (kbd "s")         #'org-focus-hud-add-subtask
      (kbd "a")         #'org-focus-hud-add-sibling
      (kbd "d")         #'org-focus-hud-done
      (kbd "w")         #'org-focus-hud-wait
      (kbd "+")         #'org-focus-hud-extend
      (kbd "=")         #'org-focus-hud-extend
      (kbd "p")         #'org-focus-hud-toggle-pause
      (kbd "?")         #'org-focus-hud-toggle-help
      (kbd "q")         #'org-focus-hud-quit
      (kbd "g")         #'org-focus-hud-refresh
      (kbd "o")         #'org-focus-hud-goto-task-other-window
      (kbd "O")         #'org-focus-hud-goto-task
      (kbd "c")         #'org-focus-hud-clock-in-task
      (kbd "r")         #'org-focus-hud-refresh
      (kbd "l")         #'org-focus-hud-log-work
      (kbd "[")         #'org-focus-hud-log-scroll-up
      (kbd "]")         #'org-focus-hud-log-scroll-down
      (kbd "M-k")       #'org-focus-hud-move-item-up
      (kbd "M-<up>")     #'org-focus-hud-move-item-up
      (kbd "M-j")       #'org-focus-hud-move-item-down
      (kbd "M-<down>")   #'org-focus-hud-move-item-down
      (kbd "M-h")       #'org-focus-hud-outdent-item
      (kbd "M-<left>")   #'org-focus-hud-outdent-item
      (kbd "M-l")       #'org-focus-hud-indent-item
      (kbd "M-<right>")  #'org-focus-hud-indent-item))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'org-focus-hud-mode 'motion)))

(defun org-focus-hud--cleanup ()
  "Cancel timer if Focus HUD buffer is killed."
  (unless (get-buffer "*Org Focus HUD*")
    (when org-focus-hud--timer
      (cancel-timer org-focus-hud--timer)
      (setq org-focus-hud--timer nil))))

(defun org-focus-hud--timer-tick ()
  "Tick function called by timer to update Focus HUD when visible."
  (let ((buf (get-buffer "*Org Focus HUD*")))
    (if (and buf (buffer-live-p buf) (get-buffer-window buf))
        (with-current-buffer buf
          (org-focus-hud-refresh))
      (unless (and buf (buffer-live-p buf))
        (when org-focus-hud--timer
          (cancel-timer org-focus-hud--timer)
          (setq org-focus-hud--timer nil))))))

(defun org-focus-hud--on-clock-in ()
  "Switch Focus HUD to newly clocked task if `org-focus-hud-follow-active-clock' is non-nil."
  (unless org-focus-hud--inhibit-clock-hooks
    (when (and (bound-and-true-p org-focus-hud-follow-active-clock)
               (fboundp 'org-clocking-p)
               (or (org-clocking-p) (and (fboundp 'org-clock-is-active) (org-clock-is-active))))
      (let ((buf (get-buffer "*Org Focus HUD*")))
        (when (and buf (buffer-live-p buf))
          (let ((m (or (and (boundp 'org-clock-hd-marker)
                            (markerp org-clock-hd-marker)
                            (marker-buffer org-clock-hd-marker)
                            (copy-marker org-clock-hd-marker))
                       (and (boundp 'org-clock-marker)
                            (markerp org-clock-marker)
                            (marker-buffer org-clock-marker)
                            (org-with-point-at org-clock-marker
                              (save-excursion
                                (org-back-to-heading t)
                                (point-marker)))))))
            (when (and m (markerp m) (marker-buffer m))
              (with-current-buffer buf
                (setq org-focus-hud--target-marker m)
                (org-focus-hud-refresh)))))))))

(defun org-focus-hud--on-clock-out ()
  "Refresh Focus HUD when clocking out so status updates immediately."
  (unless org-focus-hud--inhibit-clock-hooks
    (let ((buf (get-buffer "*Org Focus HUD*")))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (org-focus-hud-refresh))))))

(add-hook 'org-clock-in-hook #'org-focus-hud--on-clock-in)
(add-hook 'org-clock-out-hook #'org-focus-hud--on-clock-out)

;;;###autoload
(defun org-focus-hud (&optional marker)
  "Open the Org Auto Scheduler Focus HUD for MARKER (or current active task).
Brings up a dedicated, distraction-free cockpit with pacing and live capture."
  (interactive
   (list (cond
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
  (let ((buf (get-buffer-create "*Org Focus HUD*")))
    (with-current-buffer buf
      (unless (eq major-mode 'org-focus-hud-mode)
        (org-focus-hud-mode))
      (when marker
        (setq org-focus-hud--target-marker marker))
      (org-focus-hud-refresh))
    (unless org-focus-hud--timer
      (setq org-focus-hud--timer
            (run-with-timer org-focus-hud-refresh-interval
                            org-focus-hud-refresh-interval
                            #'org-focus-hud--timer-tick)))
    (pop-to-buffer buf)))

;;; ============================================================================
;;; Backwards Compatibility Aliases
;;; ============================================================================

(defalias 'org-auto-scheduler-focus 'org-focus-hud)
(defalias 'org-auto-scheduler-focus-mode 'org-focus-hud-mode)
(defvaralias 'org-auto-scheduler-focus-mode-map 'org-focus-hud-mode-map)
(defalias 'org-auto-scheduler-focus-refresh 'org-focus-hud-refresh)
(defalias 'org-auto-scheduler-focus-toggle-checklist 'org-focus-hud-toggle-checklist)
(defalias 'org-auto-scheduler-focus-add-checklist 'org-focus-hud-add-checklist)
(defalias 'org-auto-scheduler-focus-move-item-up 'org-focus-hud-move-item-up)
(defalias 'org-auto-scheduler-focus-move-item-down 'org-focus-hud-move-item-down)
(defalias 'org-auto-scheduler-focus-outdent-item 'org-focus-hud-outdent-item)
(defalias 'org-auto-scheduler-focus-indent-item 'org-focus-hud-indent-item)
(defalias 'org-auto-scheduler-focus-add-note 'org-focus-hud-add-note)
(defalias 'org-auto-scheduler-focus-add-subtask 'org-focus-hud-add-subtask)
(defalias 'org-auto-scheduler-focus-add-sibling 'org-focus-hud-add-sibling)
(defalias 'org-auto-scheduler-focus-done 'org-focus-hud-done)
(defalias 'org-auto-scheduler-focus-wait 'org-focus-hud-wait)
(defalias 'org-auto-scheduler-focus-extend 'org-focus-hud-extend)
(defalias 'org-auto-scheduler-focus-toggle-pause 'org-focus-hud-toggle-pause)
(defalias 'org-auto-scheduler-focus-toggle-help 'org-focus-hud-toggle-help)
(defalias 'org-auto-scheduler-focus-quit 'org-focus-hud-quit)
(defalias 'org-auto-scheduler-focus-goto-task 'org-focus-hud-goto-task)
(defalias 'org-auto-scheduler-focus-goto-task-other-window 'org-focus-hud-goto-task-other-window)
(defalias 'org-auto-scheduler-focus-clock-in-task 'org-focus-hud-clock-in-task)
(defvaralias 'org-auto-scheduler-focus-follow-active-clock 'org-focus-hud-follow-active-clock)
(defalias 'org-auto-scheduler-focus-log-work 'org-focus-hud-log-work)
(defalias 'org-auto-scheduler-focus-log-scroll-up 'org-focus-hud-log-scroll-up)
(defalias 'org-auto-scheduler-focus-log-scroll-down 'org-focus-hud-log-scroll-down)
(defalias 'org-auto-scheduler-focus-next-checklist 'org-focus-hud-next-checklist)
(defalias 'org-auto-scheduler-focus-prev-checklist 'org-focus-hud-prev-checklist)
(defalias 'org-auto-scheduler-focus-edit-checklist 'org-focus-hud-edit-checklist)
(defalias 'org-auto-scheduler-focus-focus-checklist 'org-focus-hud-focus-checklist)
(defalias 'org-auto-scheduler-focus--get-checklists 'org-focus-hud--get-checklists)
(defalias 'org-auto-scheduler-focus--get-notes 'org-focus-hud--get-notes)
(defalias 'org-auto-scheduler-focus--get-subtasks 'org-focus-hud--get-subtasks)
(defalias 'org-auto-scheduler-focus--get-logs 'org-focus-hud--get-logs)
(defalias 'org-auto-scheduler-focus--calculate-levels 'org-focus-hud--calculate-levels)
(defalias 'org-auto-scheduler-focus--resolve-task 'org-focus-hud--resolve-task)
(unless (fboundp 'org-auto-scheduler--task-clocked-p)
  (defalias 'org-auto-scheduler--task-clocked-p 'org-focus-hud--task-clocked-p))
(unless (fboundp 'org-auto-scheduler-get-clocked-time)
  (defalias 'org-auto-scheduler-get-clocked-time 'org-focus-hud--get-clocked-time))
(unless (fboundp 'org-auto-scheduler-get-effort)
  (defalias 'org-auto-scheduler-get-effort 'org-focus-hud--get-effort))

(defalias 'org-auto-scheduler-focus--calculate-levels 'org-focus-hud--calculate-levels)
(defalias 'org-auto-scheduler-focus--resolve-task 'org-focus-hud--resolve-task)
(defvaralias 'org-auto-scheduler-focus--target-marker 'org-focus-hud--target-marker)
(defvaralias 'org-auto-scheduler-focus--log-offset 'org-focus-hud--log-offset)
(defvaralias 'org-auto-scheduler-focus--show-help 'org-focus-hud--show-help)
(defvaralias 'org-auto-scheduler--focus-timer 'org-focus-hud--timer)

(provide 'org-focus-hud)

;;; org-focus-hud.el ends here
