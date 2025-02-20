;;; funcs.el --- GitHub Copilot Layer functions file for Spacemacs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Sylvain Benner & Contributors
;;
;; Author: Ferdinand Nussbaum <ferdinand.nussbaum@inf.ethz.ch>
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

(defun spacemacs//copilot-enable-predicate ()
  "Copilot is by default only enabled in `evil-insert-state', not in `evil-emacs-state',
see the default value of `copilot-enable-predicates'.
In `holy-mode', we enable `evil-emacs-state' permanently, hence this workaround."
  (or (not (bound-and-true-p evil-local-mode))
      (bound-and-true-p holy-mode)
      (evil-insert-state-p)))
