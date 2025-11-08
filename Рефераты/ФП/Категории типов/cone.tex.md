```tikz
\usepackage{tikz-cd}
\usepackage{amsmath,amssymb,amsfonts}
\usepackage{xcolor}
\usetikzlibrary{decorations.pathmorphing}
\tikzcdset{scale cd/.style={every
    label/.append style={scale=1.7},
    cells={nodes={scale=#1}},
    arrows={ultra thick}
},
}

\begin{document}
\mathversion{bold}
\begin{tikzcd}[scale cd=2]


&&&&&&&&&& \mathcal{C} \\
&&&&&&&&&& |[alias=c]|c \\
\\
\\
\\
&&&&&&&&&&& \\
|[alias=1]|1 && |[alias=2]|2 &&&&&&& |[alias=a]|a && |[alias=b]|b

\arrow[""{name=I, anchor=center, inner sep=0}, "{\mathcal{I}}"', draw=none, from=1, to=2]

\arrow["{\small \mathrm{Cone}_{F_{ab}}(c)_1}"{pos=0.7}, color=red, from=c, to=a]
\arrow["{\tiny \mathrm{Cone}_{F_{ab}}(c)_2}"{pos=0.7}, color=red, from=c, to=b]

\arrow[""{name=Delta, inner sep=0}, "{\Delta_c}", shorten <=20mm, color=blue, Rightarrow, from=I, to=c]
\arrow[""{name=Fab, anchor=center}, "{F_{ab}}"', shift left=2, color=Green, Rightarrow, from=2, to=a]

\arrow[""{name=FabI, anchor=center, inner sep=0}, "{F_{ab}\, \mathcal{I}}"', draw=none, from=a, to=b]

\arrow["{\mathrm{Cone}_{F_{ab}}(c)}", shorten <=3mm, color=red, squiggly,  from=Delta, to=Fab]

\end{tikzcd}
\end{document}
```



```tikz
\usepackage{tikz-cd}
\begin{document} 
\begin{tikzcd}
& X \arrow[dr, "Q", line width=2] \\
Y & & Z 
\end{tikzcd} 
\end{document}
```



```tikz
\usepackage{tikz-cd}
\usetikzlibrary{arrows.meta}
\begin{document}
\begin{tikzcd} 
A \arrow[to=Z, red, line width = 1] \arrow[to=2-2, blue, ultra thick] & B \\ |[alias=Z]| C & |[alias=X]|D \arrow[from=ul, to=1-2, green, line width=1mm, -{Stealth[scale=0.8]}]\\
\end{tikzcd}
\end{document}
```
