```tikz
\usepackage{tikz}
\usepackage{tikz-cd}
\usepackage{xcolor}
\usetikzlibrary{decorations.pathmorphing}
\tikzcdset{scale cd/.style=
  {every
    label/.append style={scale=#1},
    cells={nodes={scale=#1}},
    arrows={ultra thick, -{Stealth[scale=1]}},
  },
}
\begin{document}

\boldmath
%\mathversion{bold}

\begin{tikzcd}[scale cd=2]

&&&&&&&&&& \mathcal{C} \\
&&&&&&&&&& |[alias=c]|c \\
\\
\\
\\
\\
|[alias=1]|1 && |[alias=2]|2 &&&&&&&

|[alias=a]|a &
\makebox[15mm]{
\begin{tikzpicture}
\filldraw[color=red!60, fill=red!5, very thick] (0,0) ellipse (1.7 and 0.7);
\end{tikzpicture}
}
& |[alias=b]|b

\arrow[""{name=I, anchor=center, inner sep=0}, "{\mathcal{I}}"', draw=none, from=1, to=2]

\arrow["{\small \mathrm{Cone}_{\bf F_{ab}}(c)_1}"{pos=0.7}, scale=0.5, color=red, from=c, to=a]
\arrow["{\tiny \mathrm{Cone}_{\bf F_{ab}}(c)_2}"{pos=0.7}, color=red, from=c, to=b]

\arrow[""{name=Delta, inner sep=0}, "{\Delta_c}", shorten <=20mm, color=blue, Rightarrow, from=I, to=c]
\arrow[""{name=Fab, anchor=center}, "{F_{ab}}"', shift left=2, color=Green, Rightarrow, from=2, to=a]

\arrow[""{name=FabI, anchor=center, inner sep=0}, "{F_{ab}\, \mathcal{I}}"', draw=none, from=a, to=b]

\arrow["{\mathrm{Cone}_{\bf F_{ab}}(c)}"'{pos=0.8}, start anchor={[yshift=7mm]}, shift left=2cm, color=red, squiggly, from=Delta, to=Fab]


\end{tikzcd}
\end{document}
```

```tikz
\usepackage{tikz}
\begin{document}

\begin{tikzpicture}
\filldraw[color=red!60, fill=red!5, very thick](-1,0) circle (1.5);
\filldraw[color=red!60, fill=red!5, very thick] (2.5,0) ellipse (1.5 and 0.5);
\draw[ultra thick, ->] (6.5,0) arc (0:220:1);
\end{tikzpicture}
\end{document}
```


