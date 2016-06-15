Witam,

w za³¹czniku wysy³am gotowy projekt. Chcia³bym wzi¹æ udzia³ w konkursie.
Wydaje mi siê, ¿e z zewnêtrznych bibliotek u¿y³em tylko vector i hunit do testów.

Program korzysta z alpha-beta prunning oraz MTD(f) + iterative deepening + quiescence search
Reprezentacja planszy: bitboard


Kompilacja:
ghc -O2 main.hs

Uruchomienie:
main.exe [w|b]

Konfiguracja:
Maksymalny czas na jeden ruch mo¿na ustawiæ w pliku ai.hs - maxTime. Domyœlnie: 2 minuty

Gdyby by³y problemy ze zbyt du¿ym zu¿yciem pamiêci RAM to nale¿y zmniejszyæ maksymaln¹ iloœæ wpisów w transposition table w pliku table.hs. Domyœlnie 1 000 000

Uwagi:
Jeœli przeciwnik poda ruch/skok, który jest nie dozwolony to mój program wypisze na stderr "Invalid move" i zakoñczy grê.
Program na stderr wypisuje kolejne plansze. Mo¿na to wy³¹czyæ w main.hs (debuggingEnabled)