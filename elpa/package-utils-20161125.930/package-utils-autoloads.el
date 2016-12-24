;;; package-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (package-utils-install-async package-utils-remove-by-name
;;;;;;  package-utils-upgrade-by-name-no-fetch package-utils-upgrade-by-name
;;;;;;  package-utils-upgrade-all-no-fetch package-utils-upgrade-all
;;;;;;  package-utils-list-upgrades) "package-utils" "package-utils.el"
;;;;;;  (22620 51478 0 0))
;;; Generated autoloads from package-utils.el

(autoload 'package-utils-list-upgrades "package-utils" "\
List all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.

\(fn &optional NO-FETCH)" t nil)

(autoload 'package-utils-upgrade-all "package-utils" "\
Upgrade all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.

\(fn &optional NO-FETCH)" t nil)

(autoload 'package-utils-upgrade-all-no-fetch "package-utils" "\
Upgrade all packages that can be upgraded without calling `package-refresh-contents' first.

\(fn)" t nil)

(autoload 'package-utils-upgrade-by-name "package-utils" "\
Upgrade the package NAME.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.

\(fn NAME &optional NO-FETCH)" t nil)

(autoload 'package-utils-upgrade-by-name-no-fetch "package-utils" "\
Upgrade the package NAME, without calling `package-refresh-contents' first.

\(fn NAME)" t nil)

(autoload 'package-utils-remove-by-name "package-utils" "\
Uninstall the package NAME.

\(fn NAME)" t nil)

(autoload 'package-utils-install-async "package-utils" "\
Install PACKAGE asynchronously.

Contrary to `package-install', PACKAGE can only be a symbol.

\(fn PACKAGE)" t nil)

;;;***

;;;### (autoloads nil nil ("package-utils-pkg.el") (22620 51478 129562
;;;;;;  0))

;;;***

(provide 'package-utils-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; package-utils-autoloads.el ends here
