
# Задача эйнштейна

```prolog
% Соседство: A и B стоят рядом (в любом порядке)
next_to(A, B, L) :- append(_, [A, B | _], L).
next_to(A, B, L) :- append(_, [B, A | _], L).

% A стоит непосредственно слева от B
left_of(A, B, L) :- append(_, [A, B | _], L).

zebra(Owner, Houses) :-
    Houses = [
        house(_, norwegian, _, _, _),          % 9. Норвежец живёт в первом доме
        house(_, _, _, _, _),
        house(_, _, milk, _, _),               % 8. В среднем доме пьют молоко
        house(_, _, _, _, _),
        house(_, _, _, _, _)
    ],

    % 1. Англичанин живёт в красном доме
    member(house(red, englishman, _, _, _), Houses),

    % 2. У шведа собака
    member(house(_, swede, _, _, dog), Houses),

    % 3. Датчанин пьёт чай
    member(house(_, dane, tea, _, _), Houses),

    % 4. Зелёный дом стоит сразу слева от белого
    left_of(house(green, _, _, _, _), house(white, _, _, _, _), Houses),

    % 5. В зелёном доме пьют кофе
    member(house(green, _, coffee, _, _), Houses),

    % 6. Курильщик Pall Mall держит птиц
    member(house(_, _, _, pall_mall, bird), Houses),

    % 7. В жёлтом доме курят Dunhill
    member(house(yellow, _, _, dunhill, _), Houses),

    % 10. Blends живёт рядом с кошкой
    next_to(house(_, _, _, blends, _), house(_, _, _, _, cat), Houses),

    % 11. Лошадь живёт рядом с Dunhill
    next_to(house(_, _, _, _, horse), house(_, _, _, dunhill, _), Houses),

    % 12. Курильщик Blue Master пьёт пиво
    member(house(_, _, beer, blue_master, _), Houses),

    % 13. Немец курит Prince
    member(house(_, german, _, prince, _), Houses),

    % 14. Норвежец живёт рядом с синим домом
    next_to(house(_, norwegian, _, _, _), house(blue, _, _, _, _), Houses),

    % 15. Сосед курильщика Blends пьёт воду
    next_to(house(_, _, _, blends, _), house(_, _, water, _, _), Houses),

    % Вопрос: кто держит рыбу?
    member(house(_, Owner, _, _, fish), Houses).

zebra(Owner) :- zebra(Owner, _).
```

```prolog
?- zebra(Owner, Houses).
```

```
Owner = german,
Houses = [
    house(yellow, norwegian, water, dunhill, cat),
    house(blue, dane, tea, blends, horse),
    house(red, englishman, milk, pall_mall, bird),
    house(green, german, coffee, prince, fish),
    house(white, swede, beer, blue_master, dog)
].
```
# Задача о ферзях

