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
  :group 'convenience)

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
             (function nano-face-merge-2))
            (lambda
              (face1 face2)
              `((,(caar face1)
                 (,@(cadar face1)
                  ,@(cadar face2)))
                (,(caadr face1)
                 (,@(cadadr face1)
                  ,@(cadadr face2))))))
           ((symbol-function
             (function nano-face-merge))
            (lambda (&rest faces)
              (cl-reduce #'nano-face-merge-2 faces))))
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
             (magenta-color "#AB47BC")
             (nano-mono
              '(:family "Roboto Mono" :height 140 :weight light))
             (nano-sans
              '(:family "Roboto" :weight light))
             (light
              '((background light)))
             (dark
              '((background dark)))
             (blue
              '((t
                 (:foreground "#2196f3" :background "#2196f3"))))
             (cyan
              '((t
                 (:foreground "#00796b" :background "#00796b"))))
             (green
              '((t
                 (:foreground "#8bc34a" :background "#558b2f"))))
             (magenta
              `((t
                 (:foreground "#E1BEE7" :background ,magenta-color))))
             (red
              '((t
                 (:foreground "#f36c60" :background "#f36c60"))))
             (yellow
              '((t
                 (:foreground "#FFA000" :background "#FFA000"))))
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
             (nano-faded-background
              `((,light
                 (:background ,nano-light-faded))
                (,dark
                 (:background ,nano-dark-faded))))
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
                 (:foreground ,nano-dark-critical))))
             (highlight
              `((,light
                 (:background ,nano-light-highlight))
                (,dark
                 (:background ,nano-dark-highlight)))))
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
            '(highlight ,highlight)
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
            '(pdf-isearch-match ,nano-popout)
            '(isearch-fail ,nano-faded)
            '(show-paren-match ,nano-strong)
            '(show-paren-mismatch ,nano-critical)
            '(lazy-highlight ,nano-subtle)
            '(trailing-whitespace ,nano-subtle)
            '(secondary-selection ,nano-subtle)
            '(completions-annotations ,nano-faded)
            '(completions-common-part ,nano-strong)
            '(completions-first-difference ,nano-default)
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
            ;; -- Directory and buffer lists ---------
            '(dired-directory ,nano-strong)
            '(dired-flagged ,nano-popout)
            '(dired-header ,nano-strong)
            '(dired-ignored ,nano-faded)
            '(dired-mark ,nano-salient)
            '(dired-marked ,nano-salient)
            '(dired-perm-write ,nano-default)
            '(dired-set-id ,nano-popout)
            '(dired-special ,nano-salient)
            '(dired-symlink ,nano-salient)
            '(dired-warning ,nano-popout)
            '(ibuffer-locked-buffer ,nano-faded)
            '(ibuffer-marked ,nano-salient)
            '(ibuffer-special-buffer ,nano-strong)
            '(ibuffer-title-face ,nano-strong)
            ;; -- Diagnostics and diffs --------------
            '(compilation-error ,nano-critical)
            '(compilation-info ,nano-default)
            '(compilation-warning ,nano-popout)
            '(compilation-line-number ,nano-faded)
            '(compilation-column-number ,nano-faded)
            '(compilation-mode-line-run ,nano-default-i)
            '(compilation-mode-line-exit ,nano-salient-i)
            '(compilation-mode-line-fail ,nano-critical)
            '(diff-hl-change ,nano-popout-i)
            '(diff-hl-insert ,nano-salient-i)
            '(diff-hl-delete ,nano-critical)
            '(ediff-current-diff-A ,nano-popout-i)
            '(ediff-current-diff-B ,nano-salient-i)
            '(ediff-current-diff-C ,nano-subtle)
            '(ediff-current-diff-Ancestor ,nano-subtle)
            '(ediff-even-diff-A ,highlight)
            '(ediff-even-diff-B ,highlight)
            '(ediff-even-diff-C ,highlight)
            '(ediff-even-diff-Ancestor ,highlight)
            '(ediff-fine-diff-A ,(nano-face-merge nano-popout-i nano-strong))
            '(ediff-fine-diff-B ,(nano-face-merge nano-salient-i nano-strong))
            '(ediff-fine-diff-C ,(nano-face-merge nano-default-i nano-strong))
            '(ediff-fine-diff-Ancestor ,(nano-face-merge nano-default-i nano-strong))
            '(ediff-odd-diff-A ,highlight)
            '(ediff-odd-diff-B ,highlight)
            '(ediff-odd-diff-C ,highlight)
            '(ediff-odd-diff-Ancestor ,highlight)
            '(flycheck-error ,(nano-face-add nano-critical-i :underline t))
            '(flycheck-info ,(nano-face-add nano-salient :underline t))
            '(flycheck-warning ,(nano-face-add nano-popout :underline t))
            '(flycheck-fringe-error ,nano-critical-i)
            '(flycheck-fringe-info ,nano-salient)
            '(flycheck-fringe-warning ,nano-popout)
            '(flymake-error ,(nano-face-add nano-critical-i :underline t))
            '(flymake-note ,(nano-face-add nano-salient :underline t))
            '(flymake-warning ,(nano-face-add nano-popout :underline t))
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
            '(corfu-annotations ,nano-faded)
            '(corfu-bar ,nano-default-i)
            '(corfu-border ,nano-faded-i)
            '(corfu-current ,(nano-face-merge highlight nano-strong))
            '(corfu-default ,nano-subtle)
            '(corfu-deprecated ,(nano-face-add nano-faded :strike-through t))
            '(corfu-echo ,nano-faded)
            '(orderless-match-face-0 ,(nano-face-merge nano-salient nano-strong))
            '(orderless-match-face-1 ,nano-strong)
            '(orderless-match-face-2 ,nano-salient)
            '(orderless-match-face-3 ,nano-popout)
            '(vertico-current ,(nano-face-merge highlight nano-strong))
            '(vertico-group-separator ,nano-faded)
            '(vertico-group-title ,nano-faded)
            '(vertico-multiline ,nano-faded)
            '(marginalia-archive ,nano-faded)
            '(marginalia-char ,nano-faded)
            '(marginalia-date ,nano-faded)
            '(marginalia-documentation ,nano-faded)
            '(marginalia-file-name ,nano-faded)
            '(marginalia-file-owner ,nano-faded)
            '(marginalia-file-priv-dir ,nano-faded)
            '(marginalia-file-priv-exec ,nano-faded)
            '(marginalia-file-priv-link ,nano-faded)
            '(marginalia-file-priv-no ,nano-faded)
            '(marginalia-file-priv-other ,nano-faded)
            '(marginalia-file-priv-rare ,nano-faded)
            '(marginalia-file-priv-read ,nano-faded)
            '(marginalia-file-priv-write ,nano-faded)
            '(marginalia-function ,nano-faded)
            '(marginalia-installed ,nano-faded)
            '(marginalia-key ,nano-faded)
            '(marginalia-lighter ,nano-faded)
            '(marginalia-list ,nano-faded)
            '(marginalia-mode ,nano-faded)
            '(marginalia-modified ,nano-faded)
            '(marginalia-null ,nano-faded)
            '(marginalia-number ,nano-faded)
            '(marginalia-off ,nano-faded)
            '(marginalia-on ,nano-faded)
            '(marginalia-size ,nano-faded)
            '(marginalia-string ,nano-faded)
            '(marginalia-symbol ,nano-faded)
            '(marginalia-true ,nano-faded)
            '(marginalia-type ,nano-faded)
            '(marginalia-value ,nano-faded)
            '(marginalia-version ,nano-faded)
            '(consult-async-failed ,nano-popout)
            '(consult-async-finished ,nano-salient)
            '(consult-async-running ,nano-faded)
            '(consult-async-split ,nano-faded)
            '(consult-bookmark ,nano-salient)
            '(consult-buffer ,nano-strong)
            '(consult-file ,nano-default)
            '(consult-grep-context ,nano-faded)
            '(consult-help ,nano-faded)
            '(consult-key ,nano-strong)
            '(consult-line-number ,nano-faded)
            '(consult-line-number-prefix ,nano-faded)
            '(consult-line-number-wrapped ,nano-faded)
            '(consult-narrow-indicator ,nano-popout)
            '(consult-preview-cursor ,nano-subtle)
            '(embark-keybinding ,nano-strong)
            '(which-key-command-description-face ,nano-default)
            '(which-key-docstring-face ,nano-faded)
            '(which-key-group-description-face ,nano-salient)
            '(which-key-highlighted-command-face ,nano-strong)
            '(which-key-key-face ,nano-strong)
            '(which-key-local-map-description-face ,nano-faded)
            '(which-key-note-face ,nano-faded)
            '(which-key-separator-face ,nano-faded)
            '(which-key-special-key-face ,nano-popout)
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
            '(org-done ,nano-faded)
            '(org-drawer ,nano-faded)
            '(org-ellipsis ,nano-faded)
            '(org-footnote ,nano-faded)
            '(org-formula ,nano-faded)
            '(org-headline-done ,nano-faded)
            '(org-hide ,nano-subtle-i)
            '(org-indent ,nano-subtle-i)
            '(org-latex-and-related ,nano-faded)
            '(org-level-1 ,nano-strong)
            '(org-level-2 ,nano-strong)
            '(org-level-3 ,nano-strong)
            '(org-level-4 ,nano-strong)
            '(org-level-5 ,nano-strong)
            '(org-level-6 ,nano-strong)
            '(org-level-7 ,nano-strong)
            '(org-level-8 ,nano-strong)
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
            '(nano-modeline-active ,nano-subtle)
            '(nano-modeline-active-name ,(nano-face-merge nano-strong nano-subtle))
            '(nano-modeline-active-primary ,(nano-face-merge default nano-subtle))
            '(nano-modeline-active-secondary ,(nano-face-merge nano-faded nano-subtle))
            '(nano-modeline-active-status-RO ,nano-popout-i)
            '(nano-modeline-active-status-RW ,nano-faded-i)
            '(nano-modeline-active-status-** ,nano-critical)
            '(nano-modeline-inactive ,nano-subtle)
            '(nano-modeline-inactive-name ,(nano-face-merge nano-faded nano-subtle))
            '(nano-modeline-inactive-primary ,(nano-face-merge nano-faded nano-subtle))
            '(nano-modeline-inactive-secondary ,(nano-face-merge nano-faded nano-subtle))
            '(nano-modeline-inactive-status-RO ,(nano-face-merge nano-popout nano-subtle))
            '(nano-modeline-inactive-status-RW ,(nano-face-merge nano-faded nano-subtle))
            '(nano-modeline-inactive-status-** ,(nano-face-merge nano-critical-i nano-subtle))
            ;; -- Evil --------------------------------
            '(evil-ex-substitute-matches ,(nano-face-add nano-popout :strike-through t))
            '(evil-ex-substitute-replacement ,(nano-face-add nano-salient :underline t))
            '(notmuch-tag-face ,nano-faded)
            '(notmuch-tag-unread ,nano-faded)
            '(notmuch-search-date ,nano-faded)
            '(notmuch-tag-deleted ,(nano-face-add nano-popout :strike-through t))
            '(notmuch-tag-added ,(nano-face-add nano-salient :underline t))
            '(notmuch-crypto-decryption ,nano-faded)
            '(notmuch-crypto-part-header ,nano-faded)
            '(notmuch-crypto-signature-bad ,nano-critical)
            '(notmuch-crypto-signature-good ,nano-salient)
            '(notmuch-crypto-signature-good-key ,nano-salient)
            '(notmuch-crypto-signature-unknown ,nano-popout)
            '(notmuch-message-summary-face ,nano-strong)
            '(notmuch-search-count ,nano-faded)
            '(notmuch-search-flagged-face ,nano-popout)
            '(notmuch-search-matching-authors ,nano-strong)
            '(notmuch-search-non-matching-authors ,nano-faded)
            '(notmuch-search-subject ,nano-default)
            '(notmuch-search-unread-face ,nano-strong)
            '(notmuch-tree-match-author-face ,nano-strong)
            '(notmuch-tree-match-date-face ,nano-faded)
            '(notmuch-tree-match-face ,nano-default)
            '(notmuch-tree-match-subject-face ,nano-default)
            '(notmuch-tree-no-match-author-face ,nano-faded)
            '(notmuch-tree-no-match-date-face ,nano-faded)
            '(notmuch-tree-no-match-face ,nano-faded)
            '(notmuch-tree-no-match-subject-face ,nano-faded)
            '(notmuch-wash-cited-text ,nano-faded)
            '(notmuch-wash-toggle-button ,nano-salient)
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
            '(mu4e-header-field-face ,nano-strong)
            '(mu4e-header-value-face ,nano-default)
            '(mu4e-highlight-face ,nano-popout)
            '(mu4e-link-face ,nano-salient)
            '(mu4e-modeline-face ,nano-faded)
            '(mu4e-moved-face ,nano-faded)
            '(mu4e-ok-face ,nano-faded)
            '(mu4e-region-code ,nano-faded)
            '(mu4e-replied-face ,nano-default)
            '(mu4e-related-face ,nano-faded)
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
            '(shr-abbreviation ,nano-popout)
            '(shr-h1 ,nano-strong)
            '(shr-h2 ,nano-strong)
            '(shr-h3 ,nano-strong)
            '(shr-h4 ,nano-strong)
            '(shr-h5 ,nano-strong)
            '(shr-h6 ,nano-strong)
            '(shr-link ,nano-salient)
            '(shr-selected-link ,(nano-face-merge nano-salient nano-subtle))
            '(shr-strike-through ,(nano-face-add nano-faded :strike-through t))
            '(shr-text ,nano-default)
            '(whitespace-indentation ,nano-subtle)
            '(makefile-space ,nano-salient-i)
            '(sh-heredoc ,nano-subtle)
            '(sh-quoted-exec ,nano-salient)
            ;; Hydra (WIP) ---------------------------------------------------
            '(hydra-face-red ,nano-popout)
            '(hydra-face-blue ,nano-salient)
            ;; Web, help, and side panes
            '(eldoc-highlight-function-argument ,nano-strong)
            '(eglot-highlight-symbol-face ,nano-subtle)
            '(eglot-mode-line ,nano-faded)
            '(eglot-diagnostic-tag-deprecated-face ,(nano-face-add nano-faded :strike-through t))
            '(eglot-diagnostic-tag-unnecessary-face ,nano-faded)
            '(imenu-list-entry-face ,nano-default)
            '(imenu-list-entry-face-0 ,nano-strong)
            '(imenu-list-entry-face-1 ,nano-default)
            '(imenu-list-entry-face-2 ,nano-default)
            '(imenu-list-entry-face-3 ,nano-default)
            '(imenu-list-entry-subalist-face-0 ,nano-strong)
            '(imenu-list-entry-subalist-face-1 ,nano-default)
            '(imenu-list-entry-subalist-face-2 ,nano-default)
            '(imenu-list-entry-subalist-face-3 ,nano-default)
            '(ansi-color-black                       ,nano-default)
            '(ansi-color-bold                         ,nano-strong)
            '(ansi-color-bright-black                 ,(nano-face-merge nano-faded nano-strong))
            '(vterm-color-black                      ,(nano-face-merge nano-default nano-faded-background nano-strong))
            '(ansi-color-faint                         ,nano-faded)
            '(ansi-color-fast-blink                    ,nano-faded)
            '(ansi-color-slow-blink                    ,nano-faded)
            '(ansi-color-inverse                   ,nano-default-i)
            '(ansi-color-italic                        ,nano-faded)
            '(ansi-color-underline                     ,nano-faded)
            '(ansi-color-blue           ((t (:foreground "#42A5F5")))) ;; material color blue L400
            '(ansi-color-bright-blue    ((t (:background "#BBDEFB")))) ;; material color blue L100
            '(ansi-color-cyan           ((t (:foreground "#26C6DA")))) ;; material color cyan L400
            '(ansi-color-bright-cyan    ((t (:background "#B2EBF2")))) ;; material color cyan L100
            '(ansi-color-green          ((t (:foreground "#66BB6A")))) ;; material color green L400
            '(ansi-color-bright-green   ((t (:background "#C8E6C9")))) ;; material color green L100
            '(ansi-color-magenta        ((t (:foreground ,magenta-color)))) ;; material color purple L400
            '(ansi-color-bright-magenta ((t (:background "#E1BEE7")))) ;; material color purple L100
            '(ansi-color-red            ((t (:foreground "#EF5350")))) ;; material color red L400
            '(ansi-color-bright-red     ((t (:background "#FFCDD2")))) ;; material color red L100
            '(ansi-color-white          ,nano-subtle)
            '(ansi-color-bright-white   ,default)
            '(ansi-color-yellow         ((t (:foreground "#FFEE58")))) ;; material color yellow L400
            '(ansi-color-bright-yellow  ((t (:background "#FFF9C4")))) ;; material color yellow L100
            '(term-bold ,nano-strong)
            '(term-color-black ,default)
            '(term-color-blue ,blue)
            '(term-color-cyan ,cyan)
            '(term-color-green ,green)
            '(term-color-magenta ,magenta)
            '(term-color-red ,red)
            '(term-color-yellow ,yellow)
            ;; Eat uses its own 0-15 indexed terminal faces.  Use a restrained
            ;; Tango-derived palette so light mode stays legible and dark mode
            ;; avoids raw xterm neon.
            '(eat-term-color-0
              ((,light (:foreground "#2E3436"))
               (,dark (:foreground "#2E3436"))))
            '(eat-term-color-1
              ((,light (:foreground "#CC0000"))
               (,dark (:foreground "#EF2929"))))
            '(eat-term-color-2
              ((,light (:foreground "#4E9A06"))
               (,dark (:foreground "#8AE234"))))
            '(eat-term-color-3
              ((,light (:foreground "#8B7000"))
               (,dark (:foreground "#C4A000"))))
            '(eat-term-color-4
              ((,light (:foreground "#204A87"))
               (,dark (:foreground "#729FCF"))))
            '(eat-term-color-5
              ((,light (:foreground "#5C3566"))
               (,dark (:foreground "#AD7FA8"))))
            '(eat-term-color-6
              ((,light (:foreground "#06989A"))
               (,dark (:foreground "#34E2E2"))))
            '(eat-term-color-7
              ((,light (:foreground "#888A85"))
               (,dark (:foreground "#D3D7CF"))))
            '(eat-term-color-8
              ((,light (:foreground "#555753"))
               (,dark (:foreground "#555753"))))
            '(eat-term-color-9
              ((,light (:foreground "#EF2929"))
               (,dark (:foreground "#EF2929"))))
            '(eat-term-color-10
              ((,light (:foreground "#4E9A06"))
               (,dark (:foreground "#8AE234"))))
            '(eat-term-color-11
              ((,light (:foreground "#C4A000"))
               (,dark (:foreground "#FCE94F"))))
            '(eat-term-color-12
              ((,light (:foreground "#3465A4"))
               (,dark (:foreground "#729FCF"))))
            '(eat-term-color-13
              ((,light (:foreground "#75507B"))
               (,dark (:foreground "#AD7FA8"))))
            '(eat-term-color-14
              ((,light (:foreground "#06989A"))
               (,dark (:foreground "#34E2E2"))))
            '(eat-term-color-15
              ((,light (:foreground "#888A85"))
               (,dark (:foreground "#EEEEEC"))))
            ;; --- Magit (WIP) ---------------------------------------------------
            '(magit-blame-highlight                  ,highlight)
            '(magit-diff-added-highlight             ,(nano-face-merge highlight nano-salient nano-strong))
            '(magit-diff-base-highlight              ,highlight)
            '(magit-diff-context-highlight           ,(nano-face-merge highlight nano-faded))
            '(magit-diff-file-heading-highlight      ,(nano-face-merge highlight nano-strong))
            '(magit-diff-hunk-heading-highlight      ,nano-default)
            '(magit-diff-our-highlight               ,highlight)
            '(magit-diff-removed-highlight           ,(nano-face-merge highlight nano-popout nano-strong))
            '(magit-diff-their-highlight             ,highlight)
            '(magit-section-highlight                ,highlight)

            '(magit-blame-heading                    ,(nano-face-merge nano-subtle nano-strong))
            '(magit-diff-conflict-heading            ,(nano-face-merge nano-subtle nano-strong))
            '(magit-diff-file-heading                ,nano-strong)
            '(magit-diff-hunk-heading                ,(nano-face-merge nano-subtle nano-default))
            '(magit-diff-lines-heading               ,(nano-face-merge nano-subtle nano-strong))
            '(magit-section-heading                  ,(nano-face-merge nano-salient nano-strong))

            '(magit-bisect-bad                       ,nano-default)
            '(magit-bisect-good                      ,nano-default)
            '(magit-bisect-skip                      ,nano-default)
            '(magit-blame-date                       ,nano-default)
            '(magit-blame-dimmed                     ,nano-default)
            '(magit-blame-hash                       ,nano-faded)

            '(magit-blame-margin                     ,nano-default)
            '(magit-blame-name                       ,nano-default)
            '(magit-blame-summary                    ,nano-default)

            '(magit-branch-current                   ,(nano-face-merge nano-strong nano-salient))
            '(magit-branch-local                     ,nano-salient)
            '(magit-branch-remote                    ,nano-salient)
            '(magit-branch-remote-head               ,nano-salient)
            '(magit-branch-upstream                  ,nano-salient)

            '(magit-cherry-equivalent                ,nano-default)
            '(magit-cherry-unmatched                 ,nano-default)

            '(magit-diff-added                       ,(nano-face-merge highlight nano-salient nano-strong))
            '(magit-diff-base                        ,nano-default)
            '(magit-diff-context                     ,(nano-face-merge highlight nano-faded))
            '(magit-diff-file-heading-selection      ,nano-default)
            '(magit-diff-hunk-heading-selection      ,nano-default)
            '(magit-diff-hunk-region                 ,nano-default)
            '(magit-diff-lines-boundary              ,nano-default)
            '(magit-diff-our                         ,nano-default)
            '(magit-diff-removed                     ,(nano-face-merge highlight nano-popout nano-strong))
            '(magit-diff-revision-summary            ,nano-popout)
            '(magit-diff-their                       ,nano-default)
            '(magit-diff-whitespace-warning          ,nano-default)
            '(magit-diffstat-added                   ,nano-default)
            '(magit-diffstat-removed                 ,nano-default)

            '(magit-dimmed                           ,nano-faded)
            '(magit-filename                         ,nano-default)
            '(magit-hash                             ,nano-faded)
            '(magit-head                             ,nano-default)
            '(magit-header-line                      ,nano-default)
            '(magit-header-line-key                  ,nano-default)
            '(magit-header-line-log-select           ,nano-default)

            '(magit-keyword                          ,nano-default)
            '(magit-keyword-squash                   ,nano-default)

            '(magit-log-author                       ,nano-default)
            '(magit-log-date                         ,nano-default)
            '(magit-log-graph                        ,nano-default)

            '(magit-mode-line-process                ,nano-default)
            '(magit-mode-line-process-error          ,nano-default)

            '(magit-process-ng                       ,nano-default)
            '(magit-process-ok                       ,nano-default)

            '(magit-reflog-amend                     ,nano-default)
            '(magit-reflog-checkout                  ,nano-default)
            '(magit-reflog-cherry-pick               ,nano-default)
            '(magit-reflog-commit                    ,nano-default)
            '(magit-reflog-merge                     ,nano-default)
            '(magit-reflog-other                     ,nano-default)
            '(magit-reflog-rebase                    ,nano-default)
            '(magit-reflog-remote                    ,nano-default)
            '(magit-reflog-reset                     ,nano-default)
            '(magit-refname                          ,nano-default)
            '(magit-refname-pullreq                  ,nano-default)
            '(magit-refname-stash                    ,nano-default)
            '(magit-refname-wip                      ,nano-default)

            '(magit-section-heading-selection        ,nano-default)
            '(magit-section-secondary-heading        ,nano-default)
            '(magit-sequence-done                    ,nano-default)
            '(magit-sequence-drop                    ,nano-default)
            '(magit-sequence-exec                    ,nano-default)
            '(magit-sequence-head                    ,nano-default)
            '(magit-sequence-onto                    ,nano-default)
            '(magit-sequence-part                    ,nano-default)
            '(magit-sequence-pick                    ,nano-default)
            '(magit-sequence-stop                    ,nano-default)

            '(magit-signature-bad                    ,nano-default)
            '(magit-signature-error                  ,nano-default)
            '(magit-signature-expired                ,nano-default)
            '(magit-signature-expired-key            ,nano-default)
            '(magit-signature-good                   ,nano-default)
            '(magit-signature-revoked                ,nano-default)
            '(magit-signature-untrusted              ,nano-default)

            '(magit-tag                              ,nano-default-i))))))
  (nano-set-faces))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nano)
