```tikz
\usepackage{tikz}
\usepackage{tikz-cd}
\usepackage{xcolor}
\usetikzlibrary{backgrounds, fit}
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

\begin{tikzcd}[
  scale cd=2,
  /tikz/execute at end picture={
	\begin{scope}[on background layer]
		% Define an invisible node that 'fits' around nodes B, D, C
		\node[fit=(a) (FabI) (b), yshift=-2mm] (FabI_ellipse) {};
		\filldraw[color=red!60, fill=red!5, very thick] (FabI_ellipse.base) ellipse (3.5 and 1);

		\node[fit=(1) (I) (2), yshift=-2mm] (I_ellipse) {};
		\filldraw[color=olive!60, fill=olive!5, very thick] (I_ellipse.base) ellipse (2 and 1);
		%\node[fill=blue!10, inner sep=5pt, rounded corners, fit=(a) (FabI) (b)] (background_rect) {};
		% Optionally draw a border around the background
		%\draw[blue!50, thick, rounded corners] (background_rect.south west) rectangle (background_rect.north east);
	\end{scope}
  }
]

&&&&&&&&&& \mathcal{C} \\
&&&&&&&&&& |[alias=c]|c \\
\\
\\
\\
\\
|[alias=1]|1 && |[alias=2]|2 &&&&&&&

|[alias=a]|a &
\makebox[15mm]{
}
& |[alias=b]|b

\arrow[""{name=I, anchor=center, inner sep=0}, "{\mathcal{I}}"', draw=none, from=1, to=2]

\arrow["{\small \mathrm{Cone}_{\bf F_{ab}}(c)_1}"{pos=0.6}, scale=0.5, color=red, from=c, to=a]
\arrow["{\tiny \mathrm{Cone}_{\bf F_{ab}}(c)_2}"{pos=0.7}, color=red, from=c, to=b]

\arrow[""{name=Delta, inner sep=0}, "{\Delta_c}", shorten <=20mm, color=blue, Rightarrow, from=I, to=c]
\arrow[""{name=Fab, anchor=center}, "{F_{ab}}"', shift left=2, color=Green, Rightarrow, from=2, to=a]

\arrow[""{name=FabI, anchor=center, inner sep=0}, "{F_{ab}\, \mathcal{I}}"', draw=none, from=a, to=b]

\arrow["{\mathrm{Cone}_{\bf F_{ab}}(c)}"'{pos=0.8}, start anchor={[yshift=6mm, xshift=20mm]}, end anchor={[xshift=25mm]}, color=red, squiggly, from=Delta, to=Fab]


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


