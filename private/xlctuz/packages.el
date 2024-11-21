;;; packages.el --- xlctuz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author:  <lhc@DESKTOP-P5C39GU>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `xlctuz-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `xlctuz/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `xlctuz/pre-init-PACKAGE' and/or
;;   `xlctuz/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst xlctuz-packages
  '(liberime cnfonts)
  "The list of Lisp packages required by the xlctuz layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")



(defun xlctuz/init-liberime ()
  (use-package liberime
    :init
    (when (eq system-type 'windows-nt)
      (setq liberime-module-file "~/.config/doom/bin/liberime-core.dll"))
    (setq liberime-user-data-dir (expand-file-name "private/xlctuz/rime-data" user-emacs-directory))
    :config
    (liberime-deploy)
    (require 'pyim-liberime)
    (setq pyim-default-scheme 'rime)
    (setq pyim-page-tooltip 'posframe)))

(defun xlctuz/init-cnfonts ()
  (use-package cnfonts
    :config
    (cnfonts-mode 1)
    (setq cnfonts-personal-fontnames '(
                                       ("Sarasa Gothic SC" "Sarasa Fixed Slab SC" "Sarasa Mono SC" "Sarasa Mono Slab SC" "Sarasa Term SC" "Sarasa Term Slab SC"
                                        "Sarasa Ui SC" "LXGW WenKai Mono")
                                       ("Sarasa Gothic SC" "Sarasa Fixed Slab SC" "Sarasa Mono SC" "Sarasa Mono Slab SC" "Sarasa Term SC" "Sarasa Term Slab SC"
                                        "Sarasa Ui SC" "LXGW WenKai Mono")))))
