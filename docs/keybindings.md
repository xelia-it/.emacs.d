# Keybinding

Currently supported VSCode Keybindings.

## VSCode sidebar

| Keybinding                                        | Visual Studio Behaviour  | Bind to                      |
|---------------------------------------------------|--------------------------|------------------------------|
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>E</kbd> | Side Bar: Explorer       | `treemacs-select-window`     |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F</kbd> | Side Bar: Search         | `helm-projectile-grep`       |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>G</kbd> | Side Bar: Source Control | `magit-status`               |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>D</kbd> | Side Bar: Run            | `projectile-compile-project` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>X</kbd> | Side Bar: Extensions     | `list-packages`              |

## File Navigation

| Keybinding                                        | Visual Studio Behaviour       | Bind to                     |
|---------------------------------------------------|-------------------------------|-----------------------------|
| <kbd>Ctrl</kbd> + <kbd>P</kbd>                    | Go To File ...                | `helm-projectile-find-file` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd> | List commands                 | `helm-M-x`                  |
| <kbd>Ctrl</kbd> + <kbd>T</kbd>                    | Go To Symbol in Workspace ... | `helm-imenu`                |
|                                                   |                               |                             |

## Error management

| Keybinding                                         | Visual Studio Behaviour         | Bind to                           |
|----------------------------------------------------|---------------------------------|-----------------------------------|
| <kbd>Ctrl</kbd> + <kbd>F8</kbd>                    | Go to next error or warning     | `flymake-goto-next-error`         |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>F8</kbd> | Go to previous error or warning | `flymake-goto-previous-error`     |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>M</kbd>  | Show Problems panel             | `flymake-show-buffer-diagnostics` |

## Editing

| Keybinding                                        | Visual Studio Behaviour | Bind to         |
|---------------------------------------------------|-------------------------|-----------------|
| <kbd>Ctrl</kbd> + <kbd>/</kbd>                    | Toggle line comment     | `comment-dwim`  |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>[</kbd> | Fold innermost block    | `hs-hide-block` |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>]</kbd> | Unfold innermost block  | `hs-show-block` |
