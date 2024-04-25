(use-package general
  :pin melpa
  :after evil
  :config
  (message "general is loaded")

  (general-create-definer +evil/leader-key
    :states '(normal visual insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-S-SPC")

  (general-create-definer +evil/local-leader-key
    :states '(normal visual insert)
    :prefix "SPC m"
    :non-normal-prefix "M-S-SPC m")

  (+evil/leader-key
    "p"   project-prefix-map
    "h"   help-map

    "'"   #'vertico-repeat
    ","   #'project-find-file
    "."   #'find-file
    "/"   #'+project/search
    "*"   #'+project/search-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "a"   #'embark-act
    "x"   #'+denote/scratch
    "X"   #'org-capture

    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bd"  #'dired-jump
    "bh"  #'browser-hist-search
    "bi"  #'ibuffer
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bn"  #'evil-buffer-new
    "bN"  #'evil-window-new
    "br"  #'revert-buffer
    "bR"  #'rename-buffer
    "bs"  #'basic-save-buffer
    "bS"  #'evil-write-all
    "bz"  #'bury-buffer

    "c"   '(:ignore t :which-key "code")
    "cc"  #'compile
    "cC"  #'recompile
    "cf"  #'apheleia-format-buffer
    "cg"  #'+github/create-gist-region-or-buffer
    "cp"  #'+sourcehut/create-paste-region-or-buffer
    "cw"  #'delete-trailing-whitespace

    "e"   '(:ignore t :which-key "eglot")
    "el"  #'eglot-list-connections
    "ek"  #'eglot-shutdown
    "eK"  #'eglot-shutdown-all
    "eR"  #'eglot-reconnect

    "f"   '(:ignore t :which-key "file")
    "fb"  #'yejun/browse-blog
    "fD"  #'+file/delete-this-file
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fp"  #'+tempel/find-private-template
    "fr"  #'recentf-open-files
    "fR"  #'+file/move-this-file
    "fs"  #'save-buffer
    "fS"  #'write-file
    "fy"  #'+buffer/yank-path
    "fY"  #'+buffer/yank-path-relative-to-project

    "g"   '(:ignore t :which-key "git")
    "gb"  #'magit-checkout
    "gB"  #'magit-blame-addition
    "gc"  '(:ignore t :which-key "create")
    "gcb" #'magit-branch-and-checkout
    "gcp" #'+github/create-pull-request
    "gf"  #'magit-file-dispatch
    "gF"  #'magit-pull
    "gg"  #'magit-status
    "gh"  #'+github/list-pull-requests
    "gl"  #'magit-log-current
    "gL"  #'magit-log-buffer-file
    "go"  '(:ignore t :which-key "open in browser")
    "goo" #'browse-at-remote
    "gop" #'+github/browse-pull-request
    "gr"  #'git-gutter:revert-hunk
    "gs"  #'git-gutter:stage-hunk
    "gt"  #'git-timemachine-toggle

    "n"   '(:ignore t :which-key "notes")
    "na"  #'consult-org-agenda
    "nb"  #'citar-open
    "nB"  #'org-babel-tangle
    "nc"  #'+org/toggle-last-clock
    "nC"  #'org-clock-goto
    "nf"  #'denote-open-or-create-with-command
    "nF"  #'+org/browse-files
    "nk"  #'denote-keywords-add
    "nK"  #'denote-keywords-remove
    "nm"  #'org-tags-view
    "nn"  #'denote
    "ns"  #'consult-denote-grep
    "nS"  #'org-search-view
    "nr"  #'denote-rename-file
    "nR"  #'denote-rename-file-using-front-matter
    "nt"  #'tmr
    "nT"  #'org-todo-list
    "nx"  #'+denote/template-with-subdirectory

    "o"   '(:ignore t :which-key "open")
    "oA"  #'org-agenda
    "oe"  #'elfeed
    "om"  #'mu4e
    "oM"  #'+mail/compose-for-service
    "oo"  #'+macos/reveal-in-finder
    "oO"  #'+macos/reveal-project-in-finder

    "op"  '(:ignore t :which-key "pass")
    "opa" #'password-store-otp-append
    "opA" #'password-store-otp-append-from-image
    "ope" #'password-store-edit
    "opi" #'password-store-insert
    "opI" #'password-store-otp-insert
    "opp" #'password-store-copy
    "opP" #'password-store-otp-token-copy
    "opr" #'password-store-rename
    "opR" #'password-store-remove
    "opu" #'+pass/create-otp-key-uri

    "q"   '(:ignore t :which-key "quit")
    "qK"  #'save-buffers-kill-emacs
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'evil-quit-all-with-error-code

    "s"   '(:ignore t :which-key "search")
    "sd"  #'deadgrep
    "sg"  #'+lookup/google-translate-guess-source-lang
    "sG"  #'+lookup/google-translate-guess-source-lang-force-select
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sk"  #'dash-at-point
    "sK"  #'dash-at-point-with-docset
    "sl"  #'ffap-menu
    "so"  #'+lookup/search-online
    "ss"  #'+buffer/search
    "sS"  #'+buffer/search-for-symbol-at-point
    "sr"  #'+buffer/search-multi
    "sR"  #'+buffer/search-multi-for-symbol-at-point
    "st"  #'osx-dictionary-search-word-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tc"  '(global-display-fill-column-indicator-mode :which-key "Fill Column Indicator")
    "tf"  '(flymake-mode :which-key "Flymake")
    "ts"  '(flyspell-mode :which-key "Spell checker")
    "tw"  '(visual-line-mode :which-key "Soft line wrapping")
    "tz"  '(+zen/toggle :which-key "Zen mode")

    "w"   '(evil-window-map :which-key "window")
    "wm"  #'switch-to-minibuffer
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

  (+evil/local-leader-key
    :keymaps 'ruby-ts-mode-map
    :major-modes t
    "b"  '(:ignore t :which-key "bundle")
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open

    "k"  '(:ignore t :which-key "rake")
    "kk" #'rake
    "kr" #'rake-rerun
    "kR" #'rake-regenerate-cache
    "kf" #'rake-find-task)

  (+evil/local-leader-key
    :keymaps '(ruby-ts-mode-map rspec-mode-map rspec-compilation-mode-map)
    :major-modes t
    "t"  '(:ignore t :which-key "test")
    "ta" #'rspec-verify-all
    "ts" #'rspec-verify-single
    "tv" #'rspec-verify
    "tr" #'rspec-rerun
    "tl" #'rspec-run-last-failed
    "te" #'rspec-toggle-example-pendingness
    "tt" #'rspec-toggle-spec-and-target)

  (+evil/local-leader-key
    :keymaps 'org-mode-map
    :major-modes t
    "."  #'consult-org-heading
    "a"  '(:ignore t :which-key "anki")
    "aa" #'org-anki-sync-entry
    "aA" #'org-anki-sync-all
    "ab" #'org-anki-browse-entry
    "ac" #'org-anki-cloze-dwim
    "A"  #'org-archive-subtree
    "C"  #'org-fold-hide-block-all
    "e"  #'org-export-dispatch
    "f"  #'org-footnote-action
    "h"  #'org-toggle-heading
    "i"  #'org-toggle-item
    "I"  #'org-id-get-create
    "k"  #'org-babel-remove-result
    "o"  #'org-set-property
    "p"  #'org-priority
    "q"  #'org-set-tags-command
    "t"  #'org-todo
    "T"  #'+org/preview-toggle
    "x"  #'org-toggle-checkbox

    "b"  '(:ignore t :which-key "table")
    "b-" #'org-table-insert-hline
    "ba" #'org-table-align
    "bb" #'org-table-blank-field
    "bc" #'org-table-create-or-convert-from-region
    "be" #'org-table-edit-field
    "bf" #'org-table-edit-formulas
    "bh" #'org-table-field-info
    "bs" #'org-table-sort-lines
    "br" #'org-table-recalculate
    "bR" #'org-table-recalculate-buffer-tables

    "bd"  '(:ignore t :which-key "delete")
    "bdc" #'org-table-delete-column
    "bdr" #'org-table-kill-row

    "bi"  '(:ignore t :which-key "insert")
    "bic" #'org-table-insert-column
    "bih" #'org-table-insert-hline
    "biH" #'org-table-hline-and-move
    "bir" #'org-table-insert-row

    "c"  '(:ignore t :which-key "clock")
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "cR" #'org-clock-report

    "d"  '(:ignore t :which-key "date/deadline")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    "g"  '(:ignore t :which-key "goto")
    "gr" #'org-refile-goto-last-stored
    "gx" #'org-capture-goto-last-stored

    "l"  '(:ignore t :which-key "links")
    "la" #'denote-add-links
    "lh" #'denote-org-extras-link-to-heading
    "ll" #'denote-link-to-existing-or-new-note
    "lt" #'org-toggle-link-display

    "r"  '(:ignore t :which-key "refile")
    "r." #'+org/refile-to-current-file
    "rf" #'+org/refile-to-file
    "rr" #'org-refile
    "rR" #'org-refile-reverse

    "s"  '(:ignore t :which-key "subtree")
    "sd" #'org-cut-subtree
    "ss" #'org-sparse-tree
    "sS" #'org-sort)

  (+evil/local-leader-key
    :keymaps 'nix-mode-map
    :major-modes t
    "t" #'+nix/formatter-toggle)

  (+evil/local-leader-key
    :keymaps '(markdown-mode-map markdown-view-mode-map)
    :major-modes t
    "T" #'+markdown/preview-toggle))

(provide 'init-evil-keybindings)
