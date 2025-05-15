.PHONY: vendor

vendor:
	cd vendor && \
	curl -O https://raw.githubusercontent.com/protesilaos/dotfiles/master/emacs/.emacs.d/prot-lisp/prot-common.el && \
	curl -O https://raw.githubusercontent.com/protesilaos/dotfiles/master/emacs/.emacs.d/prot-lisp/prot-modeline.el && \
	curl -O https://raw.githubusercontent.com/xenodium/org-block-capf/refs/heads/main/org-block-capf.el && \
	curl -O https://raw.githubusercontent.com/lizqwerscott/mcp.el/refs/heads/master/mcp.el && \
	curl -O https://raw.githubusercontent.com/lizqwerscott/mcp.el/refs/heads/master/mcp-hub.el
