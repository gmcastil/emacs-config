(require 'ox-latex)
(require 'org)

(add-to-list 'org-latex-classes
	     '("amsbook"
	       "\\documentclass{amsbook}\n[NO-DEFAULT-PACKAGES]"
	       ("\\chapter{%s}" . "\\chapter*{%s")
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
