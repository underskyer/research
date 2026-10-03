

- типы эквивалентности и унивалентность;
- многомерные пути в топологическом пространстве типов;
- определение действительных чисел;
- "экономичная" теория типов и проблема останова.


> **любой тип данных ведет себя в точности как группоид**.
> - **Объекты группоида** — это значения типа (например, элементы `x` и `y`).
> - **Стрелки группоида** — это доказательства равенства (тождества) между этими элементами (`p: x = y`).

## Комонада $S_b$​ и тип тождества
https://chat.deepseek.com/share/078nd29by9oifie0yt
Кратко и по существу.

В семантике теории типов контексты — объекты B, типы в контексте — расслоения p:E→B, термы — сечения. Тип тождества IdA(a,b) интерпретируется через **объект путей** (path object) расслоения: диагональ $A→A×_ΓA$ факторизуется как $A→_r Path(A)→_{(s,t )}A×_ΓA$,
где r — «рефлексивность», а (s,t) — расслоение. Тип IdA — это Path(A) как объект над $A×_ΓA$.

Связь с SbSb​ такая. Комонада Sb=pb∘RbSb​=pb​∘Rb​ на B/bB/b кодирует **данные спуска** вдоль морфизмов в bb. Коалгебры SbSb​ — это объекты, снабжённые структурой «спуска». В гомотопической интерпретации:

- морфизм g:b′→bg:b′→b — это «путь» или «отображение»;
    
- Sb(g)Sb​(g) — это «действие группы путей» или «транспорт» вдоль gg;
    
- коалгебра для SbSb​ — это объект, который «сжимается» вдоль путей.
    

Тип тождества IdAIdA​ — это **универсальная коалгебра** для комонады SbSb​ в слое над A×ΓAA×Γ​A. Точнее, правило J (элиминатор типа тождества) утверждает: чтобы задать сечение семейства PP над IdAIdA​, достаточно задать его на диагонали (на reflrefl). В категориальных терминах это означает, что r:A→Path(A)r:A→Path(A) является **коуниверсальной** стрелкой для комонады SbSb​: любой морфизм A→XA→X, совместимый с проекциями на A×ΓAA×Γ​A, единственным образом пропускается через Path(A)Path(A).

В группоидной модели HoTT это буквально так: IdA(a,b)IdA​(a,b) — это гом-группоид, а SbSb​ — это «действие фундаментального группоида», и J-правило — это универсальное свойство этого действия.

[Categorical equality](https://nrinaudo.github.io/articles/categorical_equality.html)

[Categorical homotopy theory](https://emilyriehl.github.io/files/cathtpy.pdf) (pdf) Emily Riehl

[Elements of ∞-Category Theory](https://emilyriehl.github.io/files/elements.pdf) (pdf) Emily Riehl and Dominic Verity


[Modal HoTT on the Web. On Dependent Sums and Products | by Henry Story | Medium](https://medium.com/@bblfish/modal-hott-on-the-web-2f4f7996b41f)

[A Functional Programmer’s Guide to Homotopy Type Theory (pdf)](https://dlicata.wescreates.wesleyan.edu/pubs/l16icfp/l16icfpslides.pdf)

[Programming in Homotopy Type Theory (pdf)](https://dlicata.wescreates.wesleyan.edu/pubs/lh122tttalks/lh12wg2.8.pdf)

[Introduction to Homotopy Theory - YouTube](https://www.youtube.com/playlist?list=PLR8CgI6LLTKT7WcouQpJqs5Goxxkkz39G)

YouTube
- [3 01 A Functional Programmer's Guide to Homotopy Type Theory - YouTube](https://www.youtube.com/watch?v=caSOTjr1z18&ab_channel=ICFPVideo)
- [David Jaz Myers: Homotopy type theory for doing category theory - YouTube](https://www.youtube.com/watch?v=nalC40POVLU&ab_channel=ToposInstitute)
- [Michael Shulman: "Two-dimensional semantics of homotopy type theory" - YouTube](https://www.youtube.com/watch?v=0uzk-hIuwXA&ab_channel=ToposInstitute)

[Математические модели вычислений; гомотопии (pdf)](https://maxxk.github.io/formal-models-2015/pdf/09-HomotopyTypeTheory.pdf)

[Higher inductive type - nLab](https://ncatlab.org/nlab/show/higher+inductive+type)
[Higher Inductive Types: a tour of the menagerie](https://homotopytypetheory.org/2011/04/24/higher-inductive-types-a-tour-of-the-menagerie/)

[Equivalence Versus Equality](https://typelevel.org/blog/2017/04/02/equivalence-vs-equality.html)


