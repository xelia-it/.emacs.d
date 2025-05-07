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

| Keybinding                                          | Visual Studio Behaviour       | Bind to                            |
|-----------------------------------------------------|-------------------------------|------------------------------------|
| <kbd>Ctrl</kbd> + <kbd>P</kbd>                      | Go To File ...                | `my/helm-projectile-find-file` (1) |
| <kbd>Ctrl</kbd> + <kbd>Tab</kbd>                    | Move to next file             | `helm-buffer-list`                 |
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Tab</kbd> | Move to previous file         | `helm-buffer-list`                 |
| <kbd>Ctrl</kbd> + <kbd>W</kbd>                      | Close Window                  | `kill-buffer-and-window`           |
| <kbd>Ctrl</kbd> + <kbd>S</kbd>                      | Save file                     | `my/save`                      (2) |


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

## Misc

| Keybinding                                          | Visual Studio Behaviour       | Bind to                            |
|-----------------------------------------------------|-------------------------------|------------------------------------|
| <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>P</kbd>   | List commands                 | `helm-M-x`                         |
| <kbd>Ctrl</kbd> + <kbd>T</kbd>                      | Go To Symbol in Workspace ... | `helm-imenu`                       |

## Notes

1. This custom function that calls `helm-projectile-find-file` when the opened file belongs to a project.
   If you press <kbd>Ctrl</kbd> + <kbd>P</kbd> outside a project the default `previous-line` is invoked.
2. This custom function is used for saving file (without confirmation) and reload git status.
