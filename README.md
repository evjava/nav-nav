# `nav-nav`
Quick path navigation based on Hydra + Helm

The `nav-nav` allows you to simplify quick navigation on paths based on
- keys (hydra)
- search (helm)

Basic config:
``` elisp
(use-package nav-nav
  :quelpa (nav-nav :fetcher git :url "https://github.com/evjava/nav-nav.git")
  :config
  (setq nav-nav-file "<path-to-nav>")
  (global-set-key (kbd "s-s") 'nav-nav))
```

`path-to-nav` is the file with list of key/path entries. It looks like this:
```
'(
 ("key-1" "path-1")
 ("key-2" "path-2")
 ...
 (nil "path-k")
 ...
 )
```

Supported switching to english layout if another one enabled before showing hydra and switching back after (if `xkblayout-state` installed):
``` elisp
(use-package nav-nav
  :quelpa (nav-nav :fetcher git :url "https://github.com/evjava/nav-nav.git")
  :config
  (setq nav-nav-file "<path-to-nav>")
  (setq nav-nav-is-switch-layout t)
  (setq nav-nav-en-layout "us")
  (global-set-key (kbd "s-s") 'nav-nav))
```

Extra commands:
- `f11` - shows missing paths
- `f12` - shows free keys
- `?`   - leads to showing helm with paths

Also:
- updates visit time for selected key/path 
- entries with `nil`-key ignored by hydra (but not helm)
