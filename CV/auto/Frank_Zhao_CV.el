(TeX-add-style-hook
 "Frank_Zhao_CV"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("moderncv" "11pt" "a4paper" "sans")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "scale=0.85")))
   (TeX-run-style-hooks
    "latex2e"
    "moderncv"
    "moderncv11"
    "lipsum"
    "geometry"))
 :latex)

