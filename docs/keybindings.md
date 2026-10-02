# Keybinding

Currently supported VSCode Keybindings.

## VSCode sidebar

| Keybinding                                        | Visual Studio Behaviour  | Bind to                      |
|---------------------------------------------------|--------------------------|------------------------------|
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>E</kbd> | Side Bar: Explorer       | `treemacs-select-window`     |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F</kbd> | Side Bar: Search         | `projectile-grep`            |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>G</kbd> | Side Bar: Source Control | `magit-status`               |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>D</kbd> | Side Bar: Run            | `projectile-compile-project` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>X</kbd> | Side Bar: Extensions     | `list-packages`              |

## File Navigation

| Keybinding                                          | Visual Studio Behaviour       | Bind to                            |
|-----------------------------------------------------|-------------------------------|------------------------------------|
| <kbd>Ctrl</kbd> + <kbd>P</kbd>                      | Go To File ...                | `my-projectile-find-file` (1)      |
| <kbd>Ctrl</kbd> + <kbd>R</kbd>                      | Open Recent (project)         | `consult-projectile-switch-project`|
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>T</kbd>   | Reopen Closed Editor          | `consult-recent-file`              |
| <kbd>Ctrl</kbd> + <kbd>Tab</kbd>                    | Move to next file             | `consult-buffer`                   |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Tab</kbd> | Move to previous file         | `consult-buffer`                   |
| <kbd>Ctrl</kbd> + <kbd>PgUp</kbd> / <kbd>PgDn</kbd> | Previous / next tab           | `centaur-tabs-backward` / `-forward` |
| <kbd>Ctrl</kbd> + <kbd>W</kbd>                      | Close Window                  | `kill-buffer-and-window`           |
| <kbd>Ctrl</kbd> + <kbd>S</kbd>                      | Save file                     | `my-save`                      (2) |

## Error management

| Keybinding                                         | Visual Studio Behaviour         | Bind to                           |
|----------------------------------------------------|---------------------------------|-----------------------------------|
| <kbd>F8</kbd>                                      | Go to next error or warning     | `flymake-goto-next-error`         |
| <kbd>Shift</kbd> + <kbd>F8</kbd>                   | Go to previous error or warning | `flymake-goto-prev-error`         |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>M</kbd>  | Show Problems panel             | `my-flymake-focus-window` (3)     |

## Editing

| Keybinding                                         | Visual Studio Behaviour      | Bind to                                |
|----------------------------------------------------|------------------------------|----------------------------------------|
| <kbd>Ctrl</kbd> + <kbd>/</kbd>                     | Toggle line comment          | `comment-dwim`                         |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>[</kbd>  | Fold innermost block         | `hs-hide-block` (4)                    |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>]</kbd>  | Unfold innermost block       | `hs-show-block` (4)                    |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Z</kbd>  | Redo                         | `undo-redo`                            |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>K</kbd>  | Delete line                  | `kill-whole-line`                      |
| <kbd>Ctrl</kbd> + <kbd>D</kbd>                     | Add next occurrence          | `mc/mark-next-like-this-word`          |
| <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>↑</kbd>/<kbd>↓</kbd>   | Add cursor above / below | `mc/mark-previous-lines` / `mc/mark-next-lines` |
| <kbd>Alt</kbd> + <kbd>Shift</kbd> + <kbd>↑</kbd>/<kbd>↓</kbd>  | Copy line up / down      | `move-dup-duplicate-up` / `-down`     |
| <kbd>Alt</kbd> + <kbd>↑</kbd>/<kbd>↓</kbd>         | Move line up / down          | `move-dup-move-lines-up` / `-down`     |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>H</kbd>  | Replace                      | `vr/replace`                           |
| <kbd>F2</kbd>                                      | Rename symbol                | `lsp-rename`                           |

## Misc

| Keybinding                                          | Visual Studio Behaviour       | Bind to                            |
|-----------------------------------------------------|-------------------------------|------------------------------------|
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd>   | List commands                 | `execute-extended-command`         |
| <kbd>Ctrl</kbd> + <kbd>T</kbd>                      | Go To Symbol in Workspace ... | `my-go-to-symbol-in-workspace`     |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>O</kbd>   | Go To Symbol in File ...      | `consult-imenu`                    |
| <kbd>F12</kbd>                                      | Go to Definition              | `lsp-find-definition`              |
| <kbd>Alt</kbd> + <kbd>F12</kbd>                     | Peek Definition               | `lsp-ui-peek-find-definitions`     |
| <kbd>Shift</kbd> + <kbd>F12</kbd>                   | Find All References           | `lsp-ui-peek-find-references`      |
| <kbd>Ctrl</kbd> + <kbd>F12</kbd>                    | Go to Implementation          | `lsp-find-implementation`          |
| <kbd>Esc</kbd>                                      | Close popup / clear selection | `my-escape` (5)                    |
| <kbd>Ctrl</kbd> + <kbd>ò</kbd>                      | Terminal (Italian layout)     | `term`                             |
| `C-c l s`                                           | Outline panel                 | `lsp-treemacs-symbols`             |

## Notes

1. This custom function that calls `projectile-find-file` when the opened file belongs to a project.
   If you press <kbd>Ctrl</kbd> + <kbd>P</kbd> outside a project the default `previous-line` is invoked.
2. This custom function is used for saving file (without confirmation) and reload git status.
3. Shows the errors of the whole project in a fixed panel at the bottom (not closed by <kbd>Esc</kbd>).
4. Emacs receives these keys as `C-{` and `C-}`. On the Italian layout `[` and `]` need AltGr,
   so the combination is <kbd>Ctrl</kbd> + <kbd>AltGr</kbd> + <kbd>Shift</kbd> + <kbd>è</kbd> / <kbd>+</kbd>.
5. Quits the minibuffer, clears the selection or runs `keyboard-quit`. Unlike `keyboard-escape-quit`
   it never closes the other windows. Treemacs and Magit keep their own local <kbd>Esc</kbd> binding.

## Changed to avoid conflicts

* <kbd>Ctrl</kbd> + <kbd>R</kbd> now switches project (was `vr/replace`, moved to <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>H</kbd>).
* <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>O</kbd> was bound twice (`projectile-switch-project` and
  `consult-projectile-switch-project`): now it is *Go To Symbol in File*.
* <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>T</kbd> was `lsp-treemacs-symbols`: moved to `C-c l s`.
* Multiple cursors and line duplication were swapped compared to VSCode: now fixed.
