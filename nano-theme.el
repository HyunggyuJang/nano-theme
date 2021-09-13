;;; nano-theme.el --- NΛNO theme -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / NΛNO theme
;; Copyright (C) 2020-2021 - NΛNO developers
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

(eval-when-compile
  (require 'cl-macs))

(deftheme nano
  "N Λ N O Theme")

(defgroup nano nil
  "N Λ N O"
  :group 'nil)

(defgroup nano-light nil
  "Light theme color palette" :group 'nano)

(defgroup nano-dark nil
  "Dark theme color palette" :group 'nano)

(defgroup nano-fonts nil
  "Dark & Light theme fonts" :group 'nano)

(defface nano-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface nano-critical-i nil
  "Critical face inversed." :group nil)

(defface nano-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface nano-popout-i nil
  "Popout face inversed." :group nil)

(defface nano-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface nano-strong-i nil
  "Strong face inversed." :group nil)

(defface nano-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface nano-salient-i nil
  "Strong face inversed." :group nil)

(defface nano-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface nano-faded-i nil
  "Faded face inversed." :group nil)

(defface nano-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface nano-subtle-i nil
  "Subtle face inversed." :group nil)

(defface nano-default nil
  "Default face." :group nil)

(defface nano-default-i nil
  "Default face inversed." :group nil)

;; (defun inherit (face &optional inherit)
;;   "Extract face properties as a property list"

;;   (let ((tags (list :family :foundry :width :height :weight :slant :underline
;; 	            :overline :strike-through :box :inverse-video :foreground
;; 		    :background :stipple :extend :inherit))
;; 	(properties))
;;     (if inherit
;; 	`(:inherit ,face)
;;       (progn
;; 	(dolist (tag tags)
;; 	  (let ((attribute (face-attribute face tag)))
;; 	    (when (not (eq attribute 'unspecified))
;; 	      (push attribute properties)
;; 	      (push tag properties)))))
;;       properties)))

(cl-macrolet
    ((nano-set-faces
      nil
      (cl-letf
          (((symbol-function
             (function nano-face-add))
            (lambda
              (face &rest attrs)
              `((,(caar face)
                 (,@(cadar face)
                  ,@attrs))
                (,(caadr face)
                 (,@(cadadr face)
                  ,@attrs)))))
           ((symbol-function
             (function nano-face-merge))
            (lambda
              (face1 face2)
              `((,(caar face1)
                 (,@(cadar face1)
                  ,@(cadar face2)))
                (,(caadr face1)
                 (,@(cadadr face1)
                  ,@(cadadr face2)))))))
        (let*
            ((nano-light-foreground "#37474F")
             (nano-light-background "#FFFFFF")
             (nano-light-highlight "#FAFAFA")
             (nano-light-subtle "#ECEFF1")
             (nano-light-faded "#B0BEC5")
             (nano-light-salient "#673AB7")
             (nano-light-strong "#000000")
             (nano-light-popout "#FFAB91")
             (nano-light-critical "#FF6F00")
             (nano-dark-foreground "#ECEFF4")
             (nano-dark-background "#2E3440")
             (nano-dark-highlight "#3B4252")
             (nano-dark-subtle "#434C5E")
             (nano-dark-faded "#677691")
             (nano-dark-salient "#81A1C1")
             (nano-dark-strong "#FFFFFF")
             (nano-dark-popout "#D08770")
             (nano-dark-critical "#EBCB8B")
             (nano-mono
              '(:family "Roboto Mono" :height 140 :weight light))
             (nano-sans
              '(:family "Roboto Slab" :height 140 :weight light))
             (light
              '((background light)))
             (dark
              '((background dark)))
             (blue
              '((t
                 (:foreground "#42A5F5" :background "#BBDEFB"))))
             (cyan
              '((t
                 (:foreground "#26C6DA" :background "#B2EBF2"))))
             (green
              '((t
                 (:foreground "#66BB6A" :background "#C8E6C9"))))
             (magenta
              '((t
                 (:foreground "#AB47BC" :background "#E1BEE7"))))
             (red
              '((t
                 (:foreground "#EF5350" :background "#FFCDD2"))))
             (yellow
              '((t
                 (:foreground "#FFEE58" :background "#FFF9C4"))))
             (default
               `((,light
                  (:foreground ,nano-light-foreground :background ,nano-light-background ,@nano-mono))
                 (,dark
                  (:foreground ,nano-dark-foreground :background ,nano-dark-background ,@nano-mono))))
             (nano-strong
              `((,light
                 (:weight normal))
                (,dark
                 (:weight normal))))
             (nano-subtle
              `((,light
                 (:background ,nano-light-subtle))
                (,dark
                 (:background ,nano-dark-subtle))))
             (nano-subtle-i
              `((,light
                 (:foreground ,nano-light-subtle))
                (,dark
                 (:foreground ,nano-dark-subtle))))
             (nano-faded
              `((,light
                 (:foreground ,nano-light-faded))
                (,dark
                 (:foreground ,nano-dark-faded))))
             (nano-faded-i
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-faded))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-faded))))
             (nano-default
              `((,light
                 (:foreground ,nano-light-foreground))
                (,dark
                 (:foreground ,nano-dark-foreground))))
             (nano-default-i
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-foreground))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-foreground))))
             (nano-salient
              `((,light
                 (:foreground ,nano-light-salient))
                (,dark
                 (:foreground ,nano-dark-salient))))
             (nano-salient-i
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-salient))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-salient))))
             (nano-strong-i
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-strong :weight normal))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-strong :weight normal))))
             (nano-popout
              `((,light
                 (:foreground ,nano-light-popout))
                (,dark
                 (:foreground ,nano-dark-popout))))
             (nano-popout-i
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-popout))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-popout))))
             (nano-critical
              `((,light
                 (:foreground ,nano-light-background :background ,nano-light-critical))
                (,dark
                 (:foreground ,nano-dark-background :background ,nano-dark-critical))))
             (nano-critical-i
              `((,light
                 (:foreground ,nano-light-critical))
                (,dark
                 (:foreground ,nano-dark-critical)))))
          `(custom-theme-set-faces
            'nano
            '(default ,default)
            '(nano-strong ,nano-strong)
            '(variable-pitch
              ((t ,nano-sans)))
            '(cursor
              ((,light
                (:foreground ,nano-light-background :background ,nano-light-foreground))
               (,dark
                (:foreground ,nano-dark-background :background ,nano-dark-foreground))))
            '(highlight
              ((,light
                (:background ,nano-light-highlight))
               (,dark
                (:background ,nano-dark-highlight))))
            '(nano-subtle ,nano-subtle)
            '(nano-subtle-i ,nano-subtle-i)
            '(nano-faded ,nano-faded)
            '(nano-faded-i ,nano-faded-i)
            '(nano-default ,nano-default)
            '(nano-default-i ,nano-default-i)
            '(nano-salient ,nano-salient)
            '(nano-salient-i ,nano-salient-i)
            '(nano-strong-i ,nano-strong-i)
            '(nano-popout ,nano-popout)
            '(nano-popout-i ,nano-popout-i)
            '(nano-critical ,nano-critical)
            '(nano-critical-i ,nano-critical-i)
            '(mode-line
              ((,light
                (:foreground ,nano-light-faded :background ,nano-light-background :overline ,nano-light-subtle))
               (,dark
                (:foreground ,nano-dark-faded :background ,nano-dark-background :overline ,nano-dark-subtle))))
            '(mode-line-highlight ,nano-popout)
            '(mode-line-buffer-id
              ((t
                (:weight regular))))
            '(mode-line-emphasis
              ((t
                (:weight regular))))
            '(mode-line-inactive
              ((,light
                (:foreground ,nano-light-faded :background ,nano-light-background :overline ,nano-light-subtle))
               (,dark
                (:foreground ,nano-dark-faded :background ,nano-dark-background :overline ,nano-dark-subtle))))
            '(header-line
              ((,light
                (:foreground ,nano-light-foreground :background ,nano-light-subtle :inherit nil :box nil))
               (,dark
                (:foreground ,nano-dark-foreground :background ,nano-dark-subtle :inherit nil :box nil))))
            '(header-line
              ((,light
                (:foreground ,nano-light-foreground :background ,nano-light-subtle :inherit nil :box nil))
               (,dark
                (:foreground ,nano-dark-foreground :background ,nano-dark-subtle :inherit nil :box nil))))
            '(bold ,nano-strong)
            '(italic ,nano-faded)
            '(bold-italic ,nano-strong)
            '(region ,nano-subtle)
            '(fringe ,nano-faded)
            '(hl-line
              ((,light
                (:background ,nano-light-highlight))
               (,dark
                (:background ,nano-dark-highlight))))
            '(link ,nano-salient)
            '(fixed-pitch ,default)
            '(fixed-pitch-serif ,default)
            '(shadow ,nano-faded)
            '(success ,nano-salient)
            '(warning ,nano-popout)
            '(error ,nano-critical)
            '(match ,nano-popout)
            '(buffer-menu-buffer ,nano-strong)
            '(minibuffer-prompt ,nano-strong)
            '(isearch ,nano-strong)
            '(isearch-fail ,nano-faded)
            '(show-paren-match ,nano-strong)
            '(show-paren-mismatch ,nano-critical)
            '(lazy-highlight ,nano-subtle)
            '(trailing-whitespace ,nano-subtle)
            '(secondary-selection ,nano-subtle)
            '(completions-annotations ,nano-faded)
            '(completions-common-part ,nano-faded)
            '(completions-first-difference ,default)
            '(tooltip ,nano-subtle)
            '(read-multiple-choice-face ,nano-strong)
            '(nobreak-hyphen ,nano-popout)
            '(nobreak-space ,nano-popout)
            '(help-argument-name ,nano-faded)
            '(tabulated-list-fake-header ,nano-strong)
            '(tool-bar ,nano-faded-i)
            '(tty-menu-disabled-face ,nano-faded-i)
            '(tty-menu-enabled-face ,nano-default-i)
            '(tty-menu-selected-face ,nano-salient-i)
            '(window-divider
              ((,light
                (:foreground ,nano-light-background))
               (,dark
                (:foreground ,nano-dark-background))))
            '(window-divider-first-pixel
              ((,light
                (:foreground ,nano-light-background))
               (,dark
                (:foreground ,nano-dark-background))))
            '(window-divider-last-pixel
              ((,light
                (:foreground ,nano-light-background))
               (,dark
                (:foreground ,nano-dark-background))))
            '(vertical-border
              ((,light
                (:foreground ,nano-light-background))
               (,dark
                (:foreground ,nano-dark-background))))
            '(tab-bar ,default)
            '(tab-bar-tab ,default)
            '(tab-bar-tab-inactive ,nano-faded)
            '(tab-line ,default)
            '(line-number ,nano-faded)
            '(line-number-current-line ,default)
            '(line-number-major-tick ,nano-faded)
            '(line-number-minor-tick ,nano-faded)
            '(font-lock-comment-face ,nano-faded)
            '(font-lock-doc-face ,nano-faded)
            '(font-lock-string-face ,nano-popout)
            '(font-lock-constant-face ,nano-salient)
            '(font-lock-warning-face ,nano-popout)
            '(font-lock-function-name-face ,nano-strong)
            '(font-lock-variable-name-face ,nano-strong)
            '(font-lock-builtin-face ,nano-salient)
            '(font-lock-type-face ,nano-salient)
            '(font-lock-keyword-face ,nano-salient)
            '(widget-field ,nano-subtle)
            '(widget-button ,nano-strong)
            '(widget-single-line-field ,nano-subtle)
            '(custom-group-subtitle ,nano-strong)
            '(custom-group-tag ,nano-strong)
            '(custom-group-tag-1 ,nano-strong)
            '(custom-comment ,nano-faded)
            '(custom-comment-tag ,nano-faded)
            '(custom-changed ,nano-salient)
            '(custom-modified ,nano-salient)
            '(custom-face-tag ,nano-strong)
            '(custom-variable-tag ,nano-strong)
            '(custom-invalid ,nano-popout)
            '(custom-visibility ,nano-salient)
            '(custom-state ,nano-salient)
            '(custom-link ,nano-salient)
            '(custom-variable-obsolete ,nano-faded)
            '(company-tooltip ,nano-subtle)
            '(company-tooltip-mouse ,nano-faded-i)
            '(company-tooltip-selection ,nano-salient-i)
            '(company-scrollbar-fg ,nano-default-i)
            '(company-scrollbar-bg ,nano-faded-i)
            '(company-tooltip-common ,nano-strong)
            '(company-tooltip-common-selection ,(nano-face-merge nano-salient-i nano-strong))
            '(company-tooltip-annotation ,nano-default)
            '(company-tooltip-annotation-selection ,nano-subtle)
            '(custom-button
              ((,light
                (:foreground ,nano-light-faded :background ,nano-light-highlight :box nil))
               (,dark
                (:foreground ,nano-dark-faded :background ,nano-dark-highlight :box nil))))
            '(custom-button-mouse
              ((,light
                (:foreground ,nano-light-foreground :background ,nano-light-subtle :box nil))
               (,dark
                (:foreground ,nano-dark-foreground :background ,nano-dark-subtle :box nil))))
            '(custom-button-pressed
              ((,light
                (:foreground ,nano-light-background :background ,nano-light-foreground :box nil))
               (,dark
                (:foreground ,nano-dark-background :background ,nano-dark-foreground :box nil))))
            '(package-description ,nano-default)
            '(package-help-section-name ,nano-default)
            '(package-name ,nano-salient)
            '(package-status-avail-obso ,nano-faded)
            '(package-status-available ,nano-default)
            '(package-status-built-in ,nano-salient)
            '(package-status-dependency ,nano-salient)
            '(package-status-disabled ,nano-faded)
            '(package-status-external ,nano-default)
            '(package-status-held ,nano-default)
            '(package-status-incompat ,nano-faded)
            '(package-status-installed ,nano-salient)
            '(package-status-new ,nano-default)
            '(package-status-unsigned ,nano-default)
            '(info-node ,nano-strong)
            '(info-menu-header ,nano-strong)
            '(info-header-node ,nano-default)
            '(info-index-match ,nano-salient)
            '(Info-quoted ,nano-faded)
            '(info-title-1 ,nano-strong)
            '(info-title-2 ,nano-strong)
            '(info-title-3 ,nano-strong)
            '(info-title-4 ,nano-strong)
            '(helpful-heading ,nano-strong)
            '(epa-field-body ,nano-default)
            '(epa-field-name ,nano-strong)
            '(epa-mark ,nano-salient)
            '(epa-string ,nano-popout)
            '(epa-validity-disabled ,nano-faded)
            '(epa-validity-high ,nano-strong)
            '(epa-validity-medium ,nano-default)
            '(epa-validity-low ,nano-faded)
            '(popup-face
              ((,light
                (:background ,nano-light-highlight))
               (,dark
                (:background ,nano-dark-highlight))))
            '(popup-isearch-match ,nano-popout)
            '(popup-menu-face ,nano-subtle)
            '(popup-menu-mouse-face ,nano-faded-i)
            '(popup-menu-selection-face ,nano-salient-i)
            '(popup-menu-summary-face ,nano-faded)
            '(popup-scroll-bar-background-face ,nano-subtle)
            '(popup-scroll-bar-foreground-face ,nano-subtle)
            '(popup-summary-face ,nano-faded)
            '(popup-tip-face ,nano-popout-i)
            '(diff-header ,nano-faded)
            '(diff-file-header ,nano-strong)
            '(diff-context ,nano-default)
            '(diff-removed ,nano-faded)
            '(diff-changed ,nano-popout)
            '(diff-added ,nano-salient)
            '(diff-refine-added ,(nano-face-merge nano-salient nano-strong))
            '(diff-refine-changed ,nano-popout)
            '(diff-refine-removed ,(nano-face-add nano-faded :strike-through t))
            '(message-cited-text-1 ,nano-faded)
            '(message-cited-text-2 ,nano-faded)
            '(message-cited-text-3 ,nano-faded)
            '(message-cited-text-4 ,nano-faded)
            '(message-cited-text ,nano-faded)
            '(message-header-cc ,nano-default)
            '(message-header-name ,nano-strong)
            '(message-header-newsgroups ,nano-default)
            '(message-header-other ,nano-default)
            '(message-header-subject ,nano-salient)
            '(message-header-to ,nano-salient)
            '(message-header-xheader ,nano-default)
            '(message-mml ,nano-popout)
            '(message-separator ,nano-faded)
            '(outline-1 ,(nano-face-merge nano-default nano-strong))
            '(outline-2 ,(nano-face-merge nano-default nano-strong))
            '(outline-3 ,(nano-face-merge nano-default nano-strong))
            '(outline-4 ,(nano-face-merge nano-default nano-strong))
            '(outline-5 ,(nano-face-merge nano-default nano-strong))
            '(outline-6 ,(nano-face-merge nano-default nano-strong))
            '(outline-7 ,(nano-face-merge nano-default nano-strong))
            '(outline-8 ,(nano-face-merge nano-default nano-strong))
            '(flyspell-duplicate ,nano-popout)
            '(flyspell-incorrect ,nano-popout)
            '(org-agenda-calendar-event ,nano-default)
            '(org-agenda-calendar-sexp ,nano-salient)
            '(org-agenda-clocking ,nano-faded)
            '(org-agenda-column-dateline ,nano-faded)
            '(org-agenda-current-time ,nano-strong)
            '(org-agenda-date ,nano-salient)
            '(org-agenda-date-today ,(nano-face-merge nano-salient nano-strong))
            '(org-agenda-date-weekend ,nano-faded)
            '(org-agenda-diary ,nano-faded)
            '(org-agenda-dimmed-todo-face ,nano-faded)
            '(org-agenda-done ,nano-faded)
            '(org-agenda-filter-category ,nano-faded)
            '(org-agenda-filter-effort ,nano-faded)
            '(org-agenda-filter-regexp ,nano-faded)
            '(org-agenda-filter-tags ,nano-faded)
            '(org-agenda-property-face ,nano-faded)
            '(org-agenda-restriction-lock ,nano-faded)
            '(org-agenda-structure ,nano-strong)
            '(org-archived ,nano-faded)
            '(org-block
              ((,light
                (:background ,nano-light-highlight))
               (,dark
                (:background ,nano-dark-highlight))))
            '(org-block-begin-line ,nano-faded)
            '(org-block-end-line ,nano-faded)
            '(org-checkbox ,nano-faded)
            '(org-checkbox-statistics-done ,nano-faded)
            '(org-checkbox-statistics-todo ,nano-faded)
            '(org-clock-overlay ,nano-faded)
            '(org-code ,nano-faded)
            '(org-column ,nano-faded)
            '(org-column-title ,nano-faded)
            '(org-date ,nano-faded)
            '(org-date-selected ,nano-faded)
            '(org-default ,nano-faded)
            '(org-document-info ,nano-faded)
            '(org-document-info-keyword ,nano-faded)
            '(org-document-title ,nano-faded)
            '(org-done ,nano-default)
            '(org-drawer ,nano-faded)
            '(org-ellipsis ,nano-faded)
            '(org-footnote ,nano-faded)
            '(org-formula ,nano-faded)
            '(org-headline-done ,nano-faded)
            '(org-hide ,nano-subtle-i)
            '(org-indent ,nano-subtle-i)
            '(org-latex-and-related ,nano-faded)
            '(org-level-1 ,(nano-face-merge nano-default nano-strong))
            '(org-level-2 ,(nano-face-merge nano-default nano-strong))
            '(org-level-3 ,(nano-face-merge nano-default nano-strong))
            '(org-level-4 ,(nano-face-merge nano-default nano-strong))
            '(org-level-5 ,(nano-face-merge nano-default nano-strong))
            '(org-level-6 ,(nano-face-merge nano-default nano-strong))
            '(org-level-7 ,(nano-face-merge nano-default nano-strong))
            '(org-level-8 ,(nano-face-merge nano-default nano-strong))
            '(org-link ,nano-salient)
            '(org-list-dt ,nano-faded)
            '(org-macro ,nano-faded)
            '(org-meta-line ,nano-faded)
            '(org-mode-line-clock ,nano-faded)
            '(org-mode-line-clock-overrun ,nano-faded)
            '(org-priority ,nano-faded)
            '(org-property-value ,nano-faded)
            '(org-quote ,nano-faded)
            '(org-scheduled ,nano-faded)
            '(org-scheduled-previously ,nano-faded)
            '(org-scheduled-today ,nano-faded)
            '(org-sexp-date ,nano-faded)
            '(org-special-keyword ,nano-faded)
            '(org-table ,nano-faded)
            '(org-tag ,nano-popout)
            '(org-tag-group ,nano-faded)
            '(org-target ,nano-faded)
            '(org-time-grid ,nano-faded)
            '(org-todo ,nano-salient)
            '(org-upcoming-deadline ,nano-default)
            '(org-verbatim ,nano-popout)
            '(org-verse ,nano-faded)
            '(org-warning ,nano-popout)
            '(org-habit-ready-face ,nano-faded)
            '(org-habit-ready-future-face ,nano-subtle-i)
            '(org-habit-clear-face ,nano-faded)
            '(org-habit-clear-future-face ,nano-subtle-i)
            '(org-habit-overdue-face ,nano-faded-i)
            '(org-habit-overdue-future-face ,nano-subtle)
            '(org-habit-alert-face ,nano-faded-i)
            '(org-habit-alert-future-face ,nano-subtle)
            ;; -- Perspective mode --------------------
            '(persp-face-lighter-buffer-not-in-persp ,nano-subtle-i)
            ;; -- Nano modeline -----------------------
            ;; '(nano-modeline-active ,nano-subtle)
            ;; '(nano-modeline-active-name ,(nano-face-merge nano-strong nano-subtle))
            ;; '(nano-modeline-active-primary ,(nano-face-merge default nano-subtle))
            ;; '(nano-modeline-active-secondary ,(nano-face-merge nano-faded nano-subtle))
            ;; '(nano-modeline-active-status-RO ,nano-popout-i)
            ;; '(nano-modeline-active-status-RW ,nano-faded-i)
            ;; '(nano-modeline-active-status-** ,nano-critical)
            ;; '(nano-modeline-inactive ,nano-subtle)
            ;; '(nano-modeline-inactive-name ,(nano-face-merge nano-faded nano-subtle))
            ;; '(nano-modeline-inactive-primary ,(nano-face-merge nano-faded nano-subtle))
            ;; '(nano-modeline-inactive-secondary ,(nano-face-merge nano-faded nano-subtle))
            ;; '(nano-modeline-inactive-status-RO ,(nano-face-merge nano-popout nano-subtle))
            ;; '(nano-modeline-inactive-status-RW ,(nano-face-merge nano-faded nano-subtle))
            ;; '(nano-modeline-inactive-status-** ,(nano-face-merge nano-critical-i nano-subtle))
            ;; -- Evil --------------------------------
            '(evil-ex-substitute-matches ,(nano-face-add nano-popout :strike-through t))
            '(evil-ex-substitute-replacement ,(nano-face-add nano-salient :underline t))
            '(notmuch-tag-face ,nano-faded)
            '(notmuch-tag-unread ,nano-faded)
            '(notmuch-search-date ,nano-faded)
            '(notmuch-tag-deleted ,(nano-face-add nano-popout :strike-through t))
            '(notmuch-tag-added ,(nano-face-add nano-salient :underline t))
            '(mu4e-attach-number-face ,nano-strong)
            '(mu4e-cited-1-face ,nano-faded)
            '(mu4e-cited-2-face ,nano-faded)
            '(mu4e-cited-3-face ,nano-faded)
            '(mu4e-cited-4-face ,nano-faded)
            '(mu4e-cited-5-face ,nano-faded)
            '(mu4e-cited-6-face ,nano-faded)
            '(mu4e-cited-7-face ,nano-faded)
            '(mu4e-compose-header-face ,nano-faded)
            '(mu4e-compose-separator-face ,nano-faded)
            '(mu4e-contact-face ,nano-salient)
            '(mu4e-context-face ,nano-faded)
            '(mu4e-draft-face ,nano-faded)
            '(mu4e-flagged-face ,nano-popout)
            '(mu4e-footer-face ,nano-faded)
            '(mu4e-forwarded-face ,nano-default)
            '(mu4e-header-face ,nano-default)
            '(mu4e-header-highlight-face
              ((,light
                (:background ,nano-light-highlight))
               (,dark
                (:background ,nano-dark-highlight))))
            '(mu4e-header-key-face ,nano-strong)
            '(mu4e-header-marks-face ,nano-faded)
            '(mu4e-header-title-face ,nano-strong)
            '(mu4e-header-value-face ,nano-default)
            '(mu4e-highlight-face ,nano-popout)
            '(mu4e-link-face ,nano-salient)
            '(mu4e-modeline-face ,nano-faded)
            '(mu4e-moved-face ,nano-faded)
            '(mu4e-ok-face ,nano-faded)
            '(mu4e-region-code ,nano-faded)
            '(mu4e-replied-face ,nano-default)
            '(mu4e-special-header-value-face ,nano-default)
            '(mu4e-system-face ,nano-faded)
            '(mu4e-title-face ,nano-strong)
            '(mu4e-trashed-face ,nano-faded)
            '(mu4e-unread-face ,nano-strong)
            '(mu4e-url-number-face ,nano-faded)
            '(mu4e-view-body-face ,nano-default)
            '(mu4e-warning-face ,nano-popout)
            '(elfeed-log-date-face ,nano-faded)
            '(elfeed-log-info-level-face ,nano-default)
            '(elfeed-log-debug-level-face ,nano-default)
            '(elfeed-log-warn-level-face ,nano-popout)
            '(elfeed-log-error-level-face ,nano-popout)
            '(elfeed-search-tag-face ,nano-faded)
            '(elfeed-search-date-face ,nano-faded)
            '(elfeed-search-feed-face ,nano-salient)
            '(elfeed-search-filter-face ,nano-faded)
            '(elfeed-search-last-update-face ,nano-salient)
            '(elfeed-search-title-face ,nano-default)
            '(elfeed-search-tag-face ,nano-faded)
            '(elfeed-search-unread-count-face ,nano-strong)
            '(elfeed-search-unread-title-face ,nano-strong)
            '(deft-filter-string-error-face ,nano-popout)
            '(deft-filter-string-face ,nano-default)
            '(deft-header-face ,nano-salient)
            '(deft-separator-face ,nano-faded)
            '(deft-summary-face ,nano-faded)
            '(deft-time-face ,nano-salient)
            '(deft-title-face ,nano-strong)
            '(rst-adornment ,nano-faded)
            '(rst-block ,nano-default)
            '(rst-comment ,nano-faded)
            '(rst-definition ,nano-salient)
            '(rst-directive ,nano-salient)
            '(rst-emphasis1 ,nano-faded)
            '(rst-emphasis2 ,nano-strong)
            '(rst-external ,nano-salient)
            '(rst-level-1 ,nano-strong)
            '(rst-level-2 ,nano-strong)
            '(rst-level-3 ,nano-strong)
            '(rst-level-4 ,nano-strong)
            '(rst-level-5 ,nano-strong)
            '(rst-level-6 ,nano-strong)
            '(rst-literal ,nano-salient)
            '(rst-reference ,nano-salient)
            '(rst-transition ,nano-default)
            '(markdown-blockquote-face ,nano-default)
            '(markdown-bold-face ,nano-strong)
            '(markdown-code-face ,nano-default)
            '(markdown-comment-face ,nano-faded)
            '(markdown-footnote-marker-face ,nano-default)
            '(markdown-footnote-text-face ,nano-default)
            '(markdown-gfm-checkbox-face ,nano-default)
            '(markdown-header-delimiter-face ,nano-faded)
            '(markdown-header-face ,nano-strong)
            '(markdown-header-face-1 ,nano-strong)
            '(markdown-header-face-2 ,nano-strong)
            '(markdown-header-face-3 ,nano-strong)
            '(markdown-header-face-4 ,nano-strong)
            '(markdown-header-face-5 ,nano-strong)
            '(markdown-header-face-6 ,nano-strong)
            '(markdown-header-rule-face ,nano-default)
            '(markdown-highlight-face ,nano-default)
            '(markdown-hr-face ,nano-default)
            '(markdown-html-attr-name-face ,nano-default)
            '(markdown-html-attr-value-face ,nano-default)
            '(markdown-html-entity-face ,nano-default)
            '(markdown-html-tag-delimiter-face ,nano-default)
            '(markdown-html-tag-name-face ,nano-default)
            '(markdown-inline-code-face ,nano-popout)
            '(markdown-italic-face ,nano-faded)
            '(markdown-language-info-face ,nano-default)
            '(markdown-language-keyword-face ,nano-default)
            '(markdown-line-break-face ,nano-default)
            '(markdown-link-face ,nano-salient)
            '(markdown-link-title-face ,nano-default)
            '(markdown-list-face ,nano-faded)
            '(markdown-markup-face ,nano-faded)
            '(markdown-math-face ,nano-default)
            '(markdown-metadata-key-face ,nano-faded)
            '(markdown-metadata-value-face ,nano-faded)
            '(markdown-missing-link-face ,nano-default)
            '(markdown-plain-url-face ,nano-default)
            '(markdown-pre-face ,nano-default)
            '(markdown-reference-face ,nano-salient)
            '(markdown-strike-through-face ,nano-faded)
            '(markdown-table-face ,nano-default)
            '(markdown-url-face ,nano-salient)
            '(term-bold ,nano-strong)
            '(term-color-black ,default)
            '(term-color-blue ,blue)
            '(term-color-cyan ,cyan)
            '(term-color-green ,green)
            '(term-color-magenta ,magenta)
            '(term-color-red ,red)
            '(term-color-yellow ,yellow))))))
  (nano-set-faces))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nano)
