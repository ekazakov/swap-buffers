# swap-buffers
The quickest way to swap buffers between windows.
Based on [switch-window](https://github.com/dimitri/switch-window) package.

## Install
```
(require 'swap-buffers)
```



Bind your favorite key to `'swap-buffers`:
```
(global-set-key (kbd "C-c b") 'swap-buffers)
```


By default swap-buffers move focus to second window.

Press `C-u C-c b` to keep focus in first window
