# Keybindings

This configuration aims to keep familiar Visual Studio Code shortcuts while
preserving Emacs commands where a direct mapping would cause a conflict.

## Same as VS Code

| Shortcut | VS Code action | Emacs command |
|---|---|---|
| <kbd>Ctrl</kbd> + <kbd>S</kbd> | Save | `my-save` (save current buffer and refresh Magit) |
| <kbd>Ctrl</kbd> + <kbd>O</kbd> | Open File | `find-file` |
| <kbd>Ctrl</kbd> + <kbd>P</kbd> | Quick Open / Go to File | `my-projectile-find-file` (1) |
| <kbd>Ctrl</kbd> + <kbd>F</kbd> | Find | `my-consult-line-empty-or-symbol` |
| <kbd>Ctrl</kbd> + <kbd>Tab</kbd> / <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Tab</kbd> | Switch editor | `consult-buffer` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>E</kbd> | Explorer | `treemacs-select-window` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F</kbd> | Search in files | `my-project-ripgrep` (`consult-ripgrep`) (3) |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>G</kbd> | Source Control | `magit-status` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd> | Command Palette | `execute-extended-command` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>B</kbd> | Build project | `projectile-compile-project` |
| <kbd>Ctrl</kbd> + <kbd>F5</kbd> | Run without debugging | `my-run-project` (`projectile-run-project`) |
| <kbd>F5</kbd> | Start debugging | `my-debug-project` (not implemented yet) |
| <kbd>F9</kbd> | Toggle breakpoint | `my-debug-toggle-breakpoint` (not implemented yet) |
| <kbd>Ctrl</kbd> + <kbd>/</kbd> | Toggle line comment | `comment-dwim` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>K</kbd> | Delete line | `kill-whole-line` |
| <kbd>Ctrl</kbd> + <kbd>Backspace</kbd> | Delete word left | Emacs default: `backward-kill-word` |
| <kbd>Ctrl</kbd> + <kbd>Delete</kbd> | Delete word right | Emacs default: `kill-word` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Z</kbd> | Redo | `undo-redo` |
| <kbd>Ctrl</kbd> + <kbd>D</kbd> | Add next occurrence | `mc/mark-next-like-this-word` |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>↑</kbd> / <kbd>↓</kbd> | Add cursor above / below | `mc/mark-previous-lines` / `mc/mark-next-lines` |
| <kbd>Alt</kbd> + <kbd>↑</kbd> / <kbd>↓</kbd> | Move line up / down | `move-dup-move-lines-up` / `move-dup-move-lines-down` |
| <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>↑</kbd> / <kbd>↓</kbd> | Copy line up / down | `move-dup-duplicate-up` / `move-dup-duplicate-down` |
| <kbd>F2</kbd> | Rename symbol | `lsp-rename` |
| <kbd>F8</kbd> / <kbd>Shift</kbd> + <kbd>F8</kbd> | Next / previous problem | `flymake-goto-next-error` / `flymake-goto-prev-error` |
| <kbd>F12</kbd> | Go to Definition | `lsp-find-definition` |
| <kbd>Alt</kbd> + <kbd>F12</kbd> | Peek Definition | `lsp-ui-peek-find-definitions` |
| <kbd>Shift</kbd> + <kbd>F12</kbd> | Find All References | `lsp-ui-peek-find-references` |
| <kbd>Ctrl</kbd> + <kbd>F12</kbd> | Go to Implementation | `lsp-find-implementation` |
| <kbd>Esc</kbd> | Dismiss / cancel | `my-escape` (2) |

## Emacs-specific or different from VS Code

| Shortcut | Emacs action | Why it differs |
|---|---|---|
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>S</kbd> | Save all files: `my-save-all` | Dedicated binding; VS Code uses <kbd>Ctrl</kbd> + <kbd>K</kbd>, then <kbd>S</kbd>. |
| <kbd>Ctrl</kbd> + <kbd>W</kbd> | Kill current buffer and window | VS Code's close-editor action, implemented with Emacs buffer/window behavior. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>W</kbd> | Kill other file buffers: `my-kill-other-buffers` | Custom cleanup command; it does not close the entire Emacs application. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>A</kbd> | Cycle to related file: `my-cycle-other-file` | Custom switch among related source/header or Angular files. |
| <kbd>Ctrl</kbd> + <kbd>R</kbd> | Switch project: `consult-projectile-switch-project` | Reassigned from replace to project switching. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>H</kbd> | Replace: `vr/replace` | Replace command moved here to avoid the <kbd>Ctrl</kbd> + <kbd>R</kbd> conflict. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>D</kbd> | Run and Debug view | `my-debug-project` (not implemented yet) |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>X</kbd> | List packages: `list-packages` | Emacs package manager, not VS Code's Extensions marketplace. |
| <kbd>Ctrl</kbd> + <kbd>T</kbd> | Go to symbol in workspace: `my-go-to-symbol-in-workspace` | Uses LSP symbols or project indexes. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>O</kbd> | Go to symbol in current file: `consult-imenu` | Emacs/Consult implementation. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>T</kbd> | Reopen recent file: `consult-recent-file` | Reopens a recent file; it is not a full editor-history stack. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>V</kbd> | Browse clipboard history: `consult-yank-from-kill-ring` | Emacs kill-ring picker. |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>M</kbd> | Show project diagnostics: `my-flymake-focus-window` | Opens/focuses the Flymake problems panel. |
| <kbd>Ctrl</kbd> + <kbd>{</kbd> / <kbd>}</kbd> | Toggle / open code fold | `origami-recursively-toggle-node` / `origami-open-node`; uses LSP folding ranges. On an Italian layout these are entered with the corresponding shifted/AltGr keys. |
| <kbd>Ctrl</kbd> + <kbd>ò</kbd> | Open terminal: `term` | Italian keyboard layout shortcut. |
| <kbd>Ctrl</kbd> + <kbd>C</kbd>, <kbd>L</kbd>, <kbd>S</kbd> | Show workspace symbols: `lsp-treemacs-symbols` | Emacs-style multi-key sequence. |
| <kbd>Ctrl</kbd> + <kbd>L</kbd> | Open LSP command map | Emacs prefix map for language-server commands. |
| <kbd>Ctrl</kbd> + <kbd>Space</kbd> | Trigger completion: `company-complete` | Manual Company completion. |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Space</kbd> | Trigger Copilot: `copilot-complete` | Explicit Copilot completion. |
| <kbd>Ctrl</kbd> + <kbd>F9</kbd> / <kbd>Shift</kbd> + <kbd>F9</kbd> / <kbd>Alt</kbd> + <kbd>F9</kbd> | Run / configure / test project | Additional Projectile project commands. |
| <kbd>F6</kbd> | Show Git history: `magit-log-all` | Magit history view. |
| <kbd>Alt</kbd> + <kbd>F5</kbd> / <kbd>Shift</kbd> + <kbd>Alt</kbd> + <kbd>F5</kbd> | Next / previous Git hunk | `git-gutter:next-hunk` / `git-gutter:previous-hunk`. |

## Not currently bound

* <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>S</kbd> (VS Code **Save As…**): no explicit binding; use `M-x write-file`.
* <kbd>Ctrl</kbd> + <kbd>K</kbd>, then <kbd>S</kbd> (VS Code **Save All**): not bound as a sequence. Emacs keeps its standard <kbd>Ctrl</kbd> + <kbd>K</kbd> `kill-line` command; use <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>S</kbd> for Save All.

## Notes

1. `my-projectile-find-file` invokes `projectile-find-file` inside a project; outside one, <kbd>Ctrl</kbd> + <kbd>P</kbd> retains its normal previous-line behavior.
2. `my-escape` quits the minibuffer, clears the selection, or runs `keyboard-quit`; unlike `keyboard-escape-quit`, it does not close other windows. Treemacs and Magit keep local <kbd>Esc</kbd> bindings.
3. Project search requires the `rg` executable (ripgrep). Consult shows live matches in the minibuffer; selecting one visits that location. The query starts with the symbol at point when available. <kbd>Ctrl</kbd> + <kbd>S</kbd> saves the current buffer (`my-save`); <kbd>Ctrl</kbd> + <kbd>F</kbd> searches within the current buffer.
4. Debugging and breakpoints currently only display `Not implemented yet`; install and configure a debugger integration before using them. Building and running use the current Projectile project's configured commands.
