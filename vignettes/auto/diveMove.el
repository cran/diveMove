(TeX-add-style-hook
 "diveMove"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("scrartcl" "letterpaper" "twocolumn")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=2cm") ("inputenc" "utf8") ("fontenc" "T1") ("hyperref" "colorlinks=true")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "scrartcl"
    "scrartcl10"
    "geometry"
    "inputenc"
    "fontenc"
    "graphicx"
    "paralist"
    "relsize"
    "boxedminipage"
    "booktabs"
    "natbib"
    "hyperref"
    "pdfpages")
   (TeX-add-symbols
    '("Rfunarg" 1)
    '("Rmethod" 1)
    '("Rclass" 1)
    '("Rpackage" 1)
    '("Rfunction" 1)
    '("Robject" 1)
    "R")
   (LaTeX-add-labels
    "sec:starting"
    "sec:data-prep"
    "sec:repr-tdr"
    "fig:1"
    "sec:ident-act"
    "sec:repr-calibr-tdr"
    "fig:2"
    "fig:3"
    "fig:4"
    "sec:dive-stats"
    "fig:summaries"
    "sec:calibr-speed"
    "fig:5"
    "sec:bouts"
    "sec:acknw")
   (LaTeX-add-bibliographies
    "biblioSPL"))
 :latex)

