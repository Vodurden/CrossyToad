;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
 (haskell-mode
  (dante-repl-command-line . ("nix-shell" "--pure" "--run" (concat "cabal new-repl " dante-target " --builddir=dist-newstyle/dante"))))
 ("src" (haskell-mode (dante-target . "lib:crossy-toad")))
 ("executable" (haskell-mode (dante-target . "exe:crossy-toad")))
 ("test" (haskell-mode (dante-target . "spec")))
)
