(use-package general
  :pin melpa
  :after evil
  :config
  (message "general is loaded")

  (general-create-definer +evil/leader-key
    :states '(normal visual insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-c SPC")

  (general-create-definer +evil/local-leader-key
    :states '(normal visual insert)
    :prefix "SPC m"
    :non-normal-prefix "C-c SPC m")

  (+evil/leader-key
    "p"   project-prefix-map

    "'"   #'vertico-repeat
    ","   #'project-find-file
    "."   #'find-file
    "/"   #'+project/search
    "*"   #'+project/search-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump

    ;; buffer/bookmark
    "bb"  #'switch-to-buffer
    "bm"  #'bookmark-set
    "br"  #'revert-buffer
    "bz"  #'bury-buffer

    ;; code
    "cc"  #'compile
    "cC"  #'recompile
    "cf"  #'apheleia-format-buffer
    "cg"  #'+gh/gist-create
    "cp"  #'+hut/paste-create
    "cw"  #'delete-trailing-whitespace

    ;; file
    "fb"  #'yejun/browse-blog
    "fD"  #'+file/delete-this-file
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fp"  #'+tempel/find-private-template
    "fr"  #'recentf-open-files
    "fR"  #'+file/move-this-file
    "fy"  #'+buffer/yank-path
    "fY"  #'+buffer/yank-path-relative-to-project

    ;; git
    "gb"  #'magit-checkout
    "gB"  #'magit-blame-addition
    "gf"  #'magit-fetch
    "gF"  #'magit-pull
    "gg"  #'magit-status
    "gl"  #'magit-log-current
    "gL"  #'magit-log-buffer-file
    "gp"  #'+gh/pr-view
    "gr"  #'git-gutter:revert-hunk
    "gs"  #'git-gutter:stage-hunk
    "gt"  #'git-timemachine-toggle
    "gy"  #'git-link

    ;; git - create
    "gcb" #'magit-branch-and-checkout
    "gcB" #'+git/create-backup-commit
    "gcp" #'+gh/pr-create

    ;; git - open
    "goo" #'browse-at-remote
    "gop" #'+gh/pr-browse-at-remote

    ;; notes/agenda
    "nb"  #'citar-open
    "nB"  #'citar-create-note
    "nc"  #'+org/toggle-last-clock
    "nC"  #'org-clock-goto
    "nd"  #'denote-subdirectory
    "nf"  #'denote-open-or-create
    "nF"  #'yejun/browse-org-directory
    "nk"  #'denote-rename-file-keywords
    "nn"  #'denote
    "ns"  #'consult-denote-grep
    "nr"  #'denote-rename-file
    "nR"  #'denote-rename-file-using-front-matter
    "nt"  #'org-todo-list
    "nz"  #'denote-signature

    ;; open
    "oo"  #'+macos/reveal-in-finder
    "oO"  #'+macos/reveal-project-in-finder

    ;; search
    "sf"  #'ffap-menu
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sl"  #'consult-line
    "sL"  #'consult-line-multi

    ;; toggle
    "tf"  #'flymake-mode
    "ts"  #'flyspell-mode
    "tt"  #'+theme-buffet/toggle
    "tv"  #'visible-mode
    "tw"  #'visual-line-mode
    "tz"  #'logos-focus-mode

    ;; window
    "w"   evil-window-map
    "wm"  #'switch-to-minibuffer
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

  (+evil/local-leader-key
    :keymaps 'ruby-ts-mode-map
    :major-modes t
    ;; bundle
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open

    ;; rake
    "kk" #'rake
    "kr" #'rake-rerun
    "kR" #'rake-regenerate-cache
    "kf" #'rake-find-task

    ;; rails
    "rc" #'+inf-ruby/console-rails-in-root-dir)

  (+evil/local-leader-key
    :keymaps '(ruby-ts-mode-map rspec-mode-map rspec-compilation-mode-map)
    :major-modes t
    ;; test
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
    "a"  #'consult-org-heading
    "A"  #'org-archive-subtree
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
    "T"  #'yejun/org-preview-mode
    "x"  #'org-toggle-checkbox

    ;; table
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

    ;; table - delete
    "bdc" #'org-table-delete-column
    "bdr" #'org-table-kill-row

    ;; table - insert
    "bic" #'org-table-insert-column
    "bih" #'org-table-insert-hline
    "biH" #'org-table-hline-and-move
    "bir" #'org-table-insert-row

    ;; clock
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "cr" #'org-clock-report

    ;; date/deadline
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    ;; goto
    "gr" #'org-refile-goto-last-stored
    "gx" #'org-capture-goto-last-stored

    ;; refile
    "r." #'+org/refile-to-current-file
    "rf" #'+org/refile-to-file
    "rr" #'org-refile
    "rR" #'org-refile-reverse

    ;; subtree
    "sd" #'org-cut-subtree
    "ss" #'org-sparse-tree
    "sS" #'org-sort)

  (+evil/local-leader-key
    :keymaps 'nix-mode-map
    :major-modes t
    "t" #'+nix/formatter-toggle))

(provide 'init-evil-keybindings)
