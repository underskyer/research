```tikz
\usepackage{tikz-cd}
\usetikzlibrary{decorations.pathmorphing}
\tikzcdset{scale cd/.style={every
    label/.append style={scale=#1},
    cells={nodes={scale=#1}},
    arrows={ultra thick}
},
  every label/.append style = {
    font = \everymath\expandafter{\the\everymath\textstyle},
  },
  every label/.append style = {font = \normalsize}
}

\begin{document}

\begin{tikzcd}[scale cd=1.5]


&&&&&& \mathcal{C} \\
&&&&&& c \\
\\
\\
\\
&\mathcal{I} &&&&&& \\
1 && 2 &&& a & F_{ab}\, \mathcal{I} & b

\arrow["{\mathrm{Cone}_{F_{ab}}(c)_1}"', color=red, from=2-7, to=7-6]
\arrow["{\mathrm{Cone}_{F_{ab}}(c)_2}", color=red, from=2-7, to=7-8]
\arrow[""{name=0, anchor=center, inner sep=0}, draw=none, from=7-1, to=7-3]
\arrow[""{name=1, anchor=center}, "{\Large F_{ab}}"', shift left=2, color=green, Rightarrow, from=7-3, to=7-6]
\arrow[""{name=2, inner sep=0}, "{\Large \Delta_c}", between={0.1}{1}, color=blue, Rightarrow, from=0, to=2-7]
\arrow["{\Large \mathrm{Cone}_{F_{ab}}(c)}", shift left=5, color=red, squiggly,  from=2, to=1]

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
