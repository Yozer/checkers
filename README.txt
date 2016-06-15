Witam,

w za��czniku wysy�am gotowy projekt. Chcia�bym wzi�� udzia� w konkursie.
Wydaje mi si�, �e z zewn�trznych bibliotek u�y�em tylko vector i hunit do test�w.

Program korzysta z alpha-beta prunning oraz MTD(f) + iterative deepening + quiescence search
Reprezentacja planszy: bitboard


Kompilacja:
ghc -O2 main.hs

Uruchomienie:
main.exe [w|b]

Konfiguracja:
Maksymalny czas na jeden ruch mo�na ustawi� w pliku ai.hs - maxTime. Domy�lnie: 2 minuty

Gdyby by�y problemy ze zbyt du�ym zu�yciem pami�ci RAM to nale�y zmniejszy� maksymaln� ilo�� wpis�w w transposition table w pliku table.hs. Domy�lnie 1 000 000

Uwagi:
Je�li przeciwnik poda ruch/skok, kt�ry jest nie dozwolony to m�j program wypisze na stderr "Invalid move" i zako�czy gr�.
Program na stderr wypisuje kolejne plansze. Mo�na to wy��czy� w main.hs (debuggingEnabled)