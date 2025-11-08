$$
\begin{tikzcd}
&&&&&& \begin{array}{c} \, \\ \mathcal{C} \end{array} \\
&&&&&& \begin{array}{c} c \\ \bullet \end{array} \\
\\
\\
\begin{array}{c} \bullet \\ 1 \end{array} & \begin{array}{c} \mathcal{I} \\ \, \\ \, \end{array} & \begin{array}{c} \bullet \\ 2 \end{array} &&& \begin{array}{c} \bullet \\ a \end{array} & \begin{array}{c} F_{ab}\, \mathcal{I} \\ \, \\ \, \end{array} & \begin{array}{c} \bullet \\ b \end{array}
\arrow["{\mathrm{Cone}_{F_{ab}}(c)_1}"', color={rgb,255:red,255;green,0;blue,4}, from=2-7, to=5-6]
\arrow["{\mathrm{Cone}_{F_{ab}}(c)_2}", color={rgb,255:red,255;green,0;blue,4}, from=2-7, to=5-8]
\arrow[""{name=0, anchor=center, inner sep=0}, draw=none, from=5-1, to=5-3]
\arrow[""{name=1, anchor=center, inner sep=0}, "{\Large F_{ab}}"', shift left=2, color={rgb,255:red,0;green,97;blue,2}, Rightarrow, from=5-3, to=5-6]
\arrow[""{name=2, anchor=center, inner sep=0}, "{\Large \Delta_c}", shift right, color={rgb,255:red,4;green,0;blue,255}, between={0.2}{1}, Rightarrow, from=0, to=2-7]
\arrow["{\Large \mathrm{Cone}_{F_{ab}}(c)}"'{pos=0.6}, shift left=5, color={rgb,255:red,255;green,0;blue,4}, squiggly, from=2, to=1]
\end{tikzcd}
$$
