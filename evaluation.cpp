// dllmain.cpp : Defines the entry point for the DLL application.
#include "evaluation.h"
#include "stdio.h"

#define WHITE 1
#define BLACK 2
#define MAN 4
#define KING 8
#define MATE 10000
#define FREE 0
#define BLK_MAN (BLACK|MAN)
#define WHT_MAN (WHITE|MAN)
#define BLK_KNG  (BLACK|KING)
#define WHT_KNG (WHITE|KING)
#define OPENING 0
#define MIDGAME 1
#define ENDGAME 2
#define ull unsigned long long
#define LONG_EDGE 9241421688590303745ULL

//31,27,24,20,13,9,6,2
#define PST_king_1 45123957204583680ULL
//32,29,28,26,25,23,21,12,10,8,7,5,4,1
#define PST_king_2 9241422471801078273ULL
//22,19,18,15,14,11
#define PST_king_3 240987930624ULL

//28,27,26,20,19,18,12,11,10
#define Lattice2 18102720552763392ULL
//23,22,21,15,14,13,7,6,5
#define LatticeMinus2 35356876079616ULL

#define ROW_1 16843009ULL
#define ROW_2 8623620608ULL
#define ROW_3 17247241216ULL
#define ROW_4 8830587502592ULL
#define ROW_5 17661175005184ULL
#define ROW_6 9042521602654208ULL
#define ROW_7 18085043205308416ULL
#define ROW_8 9259542121117908992ULL

//5,13,21,29,6,14,22,30,7,15,23,31,8,16,24,32
#define VERTICAL_TOP_EDGES 9268593481931686400ULL
//1,9,17,25,2,10,18,26,3,11,19,27,4,12,20,28
#define VERTICAL_NOTTOP_EDGES 18102721644397825ULL

#define F_5 9223372036854775808ULL
#define F_6 36028797018963968ULL
#define F_7 140737488355328ULL
#define F_8 549755813888ULL
#define F_10 18014398509481984ULL
#define F_11 70368744177664ULL
#define F_12 274877906944ULL
#define F_13 1073741824ULL
#define F_14 9007199254740992ULL
#define F_15 35184372088832ULL
#define F_16 137438953472ULL
#define F_17 536870912ULL
#define F_19 17592186044416ULL
#define F_20 68719476736ULL
#define F_21 268435456ULL
#define F_22 1048576ULL
#define F_23 8796093022208ULL
#define F_24 34359738368ULL
#define F_25 134217728ULL
#define F_26 524288ULL
#define F_28 17179869184ULL
#define F_29 67108864ULL
#define F_30 262144ULL
#define F_31 1024ULL
#define F_32 8589934592ULL
#define F_33 33554432ULL
#define F_34 131072ULL
#define F_35 512ULL
#define F_37 16777216ULL
#define F_38 65536ULL
#define F_39 256ULL
#define F_40 1ULL

static __inline int get_phase(const int mat) {
	if (mat > 19) { // 20-24
		return OPENING;
	}
	else if (mat > 8) { // 9-19
		return MIDGAME;
	}
	else { // 0-8
		return ENDGAME;
	}


}

static __inline int abs(int a) {return a > 0 ? a : -a; }

int evaluation(ull wp,ull bp, ull k, int color, int alpha, int beta, int realdepth)
{
	ull whitePieces = (~k) & wp;
	ull blackPieces = (~k) & bp;
	ull whiteKings = wp & k;
	ull blackKings = bp & k;
	ull notEmpty = wp | bp;
	ull allPieces = whitePieces | blackPieces;
	ull empty = (~(notEmpty)) & 9286696203576084225ULL;
	
	int nbm = __builtin_popcountll(blackPieces);
	int nbk = __builtin_popcountll(blackKings);
	int nwm = __builtin_popcountll(whitePieces);
	int nwk = __builtin_popcountll(whiteKings);
	
	if (bp == 0)  return (color == BLACK ? (realdepth - MATE) : MATE - realdepth);
	if (wp == 0) return(color == WHITE ? (realdepth - MATE) : MATE - realdepth);

	int i, j;
	int eval;
	int v1, v2;
	unsigned int phase = get_phase(nbm + nwm + nbk + nwk); // get game phase

	if (phase == ENDGAME) {
		v1 = 100 * nbm + 300 * nbk;
		v2 = 100 * nwm + 300 * nwk;
		eval = v1 - v2;
		int White = nwm + nwk; // total number of white pieces
		int Black = nbm + nbk; // total number of black pieces

		if (nbk > 0 && (White < (2 + nbk)) && (eval < 0)) return (0);
		if (nwk > 0 && (Black < (2 + nwk)) && (eval > 0)) return (0);

		int WGL = 0; // white king on a1-h8
		int BGL = 0; // black king on a1-h8
		if(allPieces & LONG_EDGE == 0) {
			if(whiteKings & LONG_EDGE) WGL=1;
			if(blackKings & LONG_EDGE) BGL=1;
		}

		// surely winning advantage:
		if (White == 1 && nwm == 1 && Black >= 4) eval = eval + (eval >> 1);
		if (Black == 1 && nbm == 1 && White >= 4) eval = eval + (eval >> 1);

		// scaling down
		if (nbk > 0 && eval < 0) eval = eval >> 1;
		if (nwk > 0 && eval > 0) eval = eval >> 1;

		if (nbk == 1 && BGL && !WGL && White <= 3)
			if (Black <= 2 || eval < 500)
				return (0);

		if (nwk == 1 && WGL && !BGL && Black <= 3)
			if (White <= 2 || eval > -500)
				return (0);

	
		int w_lattice = 0;
		int b_lattice = 0;
		
		eval += __builtin_popcountll(PST_king_1 & blackKings);
		eval += 2*__builtin_popcountll(PST_king_2 & blackKings);
		eval += 3*__builtin_popcountll(PST_king_3 & blackKings);	
		eval -= __builtin_popcountll(PST_king_1 & whiteKings);
		eval -= 2*__builtin_popcountll(PST_king_2 & whiteKings);
		eval -= 3*__builtin_popcountll(PST_king_3 & whiteKings);
		
		eval += 2*__builtin_popcountll(ROW_7 & blackPieces);
		eval += 4*__builtin_popcountll(ROW_6 & blackPieces);
		eval += 6*__builtin_popcountll(ROW_5 & blackPieces);
		eval += 8*__builtin_popcountll(ROW_4 & blackPieces);
		eval += 10*__builtin_popcountll(ROW_3 & blackPieces);
		eval += 12*__builtin_popcountll(ROW_2 & blackPieces);
		
		eval -= 12*__builtin_popcountll(ROW_7 & whitePieces);
		eval -= 10*__builtin_popcountll(ROW_6 & whitePieces);
		eval -= 8*__builtin_popcountll(ROW_5 & whitePieces);
		eval -= 6*__builtin_popcountll(ROW_4 & whitePieces);
		eval -= 4*__builtin_popcountll(ROW_3 & whitePieces);
		eval -= 2*__builtin_popcountll(ROW_2 & whitePieces);
		
		b_lattice += 2*__builtin_popcountll(Lattice2 & blackPieces);
		b_lattice += -2*__builtin_popcountll(LatticeMinus2 & blackPieces);
		w_lattice += 2*__builtin_popcountll(Lattice2 & whitePieces);
		w_lattice += -2*__builtin_popcountll(LatticeMinus2 & whitePieces);
		
		w_lattice = abs(w_lattice);
		if (w_lattice) eval += w_lattice - 2;
		b_lattice = abs(b_lattice);
		if (b_lattice) eval -= b_lattice - 2;
		/* the move */
		if (nbk == 0 && nwk == 0 && nbm == nwm) {
			int allstones = nbm + nwm;
			int move;
			const int themoveval = 2;
			int stonesinsystem = 0;
			if (color == BLACK)
			{
				stonesinsystem = __builtin_popcountll(VERTICAL_TOP_EDGES & notEmpty);
				if (stonesinsystem % 2) // the number of stones in bp system is odd -> he has the move
					move = themoveval*(24 - allstones) / 6;
				else
					move = -themoveval*(24 - allstones) / 6;
				eval += move;
			}
			else // color is WHITE
			{
				stonesinsystem = __builtin_popcountll(VERTICAL_NOTTOP_EDGES & notEmpty);
				if (stonesinsystem % 2) // the number of stones in wp system is odd -> he has the move
					move = -themoveval*(24 - allstones) / 6;
				else
					move = themoveval*(24 - allstones) / 6;
				eval += move;
			}
		}

		// negamax formulation requires this:
		if (color == BLACK) {
			eval++; // small bonus for turn
			return (eval);
		}
		else {
			eval--; // small bonus for turn
			return (-eval);
		}
	}  // ENDGAME

	v1 = 100 * nbm + 250 * nbk;
	v2 = 100 * nwm + 250 * nwk;
	eval = v1 - v2;
	eval += (200 * (v1 - v2)) / (v1 + v2);      /*favor exchanges if in material plus*/

												// king's balance
	if (nbk != nwk) {
		if (nwk == 0 && nbm >= nwm - 2)
			eval += 200;
		else
			if (nbk == 0 && nwm >= nbm - 2)
				eval -= 200;
	}
	// scaling down
	if (nbk > 0 && eval < 0) eval = ((3 * eval) >> 2);
	if (nwk > 0 && eval > 0) eval = ((3 * eval) >> 2);

	// Lazy evaluation
	// Early exit from evaluation  if eval already is extremely low or extremely high
	int teval = (color == WHITE) ? -eval : eval;
	if (teval - 64 >= beta) return teval;
	if (teval + 64 <= alpha) return teval;
	// back rank guard:
	static int br[32] = { 0,-1,1,0,3,3,3,3,2,2,2,2,4,4,8,7,1,0,1,0,3,3,3,3,2,2,2,2,4,4,8,7 }; // back rank values
	int code;
	int backrank;
	code = 0;
	if (allPieces & F_5) code++; // field 32
	if (allPieces & F_6) code += 2; // field 31
	if (allPieces & F_7) code += 4; // Golden checker
	if (allPieces & F_8) code += 8;
	if (blackPieces & F_13) code += 16;
	backrank = br[code];
	code = 0;
	if (allPieces & F_37) code += 8;
	if (allPieces & F_38) code += 4; // Golden checker
	if (allPieces & F_39) code += 2;
	if (allPieces & F_40) code++;
	if (whitePieces * F_32) code += 16;
	backrank -= br[code];
	int brv = (phase == OPENING ? 3 : 1);  // multiplier for back rank -- back rank value
	eval += brv*backrank;

	if (nbm == nwm) {
		/* balance                */
		/* how equally the pieces are distributed on the left and right sides of the board */

		int balance;
		int nbml, nbmr; // number black men left - right
		int nwml, nwmr; // number white men left - right
						// left flank
		code = 0;
		code += __builtin_popcountll(whitePieces & 9286466472350056448ULL);
		code += 256*__builtin_popcountll(blackPieces & 9286466472350056448ULL);
		code += 16*__builtin_popcountll(whiteKings & 9286466472350056448ULL);
		code += 4096*__builtin_popcountll(blackKings & 9286466472350056448ULL);
		
		nwml = code & 15;
		nbml = (code >> 8) & 15;
		// empty left flank ?
		if (nwml == 0) eval += 10;
		if (nbml == 0) eval -= 10;
		// right flank
		code = 0;
		code += __builtin_popcountll(whitePieces & 826514605825ULL);
		code += 256*__builtin_popcountll(blackPieces & 826514605825ULL);
		code += 16*__builtin_popcountll(whiteKings & 826514605825ULL);
		code += 4096*__builtin_popcountll(blackKings & 826514605825ULL);
		nwmr = code & 15;
		nbmr = (code >> 8) & 15;
		// empty right flank ?
		if (nwmr == 0) eval += 10;
		if (nbmr == 0) eval -= 10;

		balance = abs(nbml - nbmr);
		if (balance >= 2)
			eval -= balance << 1;
		balance = abs(nwml - nwmr);
		if (balance >= 2)
			eval += balance << 1;

		if (nbml + nbmr == nbm) eval -= 4;
		if (nwml + nwmr == nwm) eval += 4;

	}

	// developed single corner
	const int devsinglecornerval = 8; // developed single corner value
	if (empty & (F_5 | F_10) == F_5 | F_10) {
		eval += devsinglecornerval;
		if (empty & F_6)
			eval -= 5;
	}

	if (empty & (F_40 | F_35) == F_40 | F_35) {
		eval -= devsinglecornerval;
		if (empty & F_39)
			eval += 5;
	}

	/* center control */
	// for black color
	// f4
	if (bp & F_21) {
		eval++;
		if (bp & F_16)
			if (bp & F_11)
				eval++;
			else
				if (empty & F_11)
					eval--;

		if (bp & F_17)
			if (bp & F_13)
				eval++;
			else
				if (empty & F_13)
					eval--;
	}

	// d4
	if (bp & F_20) {
		eval++;
		if (bp & F_15)
			if (bp & F_10)
				eval++;
			else
				if (empty & F_10)
					eval--;

		if (bp & F_16)
			if (bp & F_12)
				eval++;
			else
				if (empty & F_12)
					eval--;
	}
	
	// e3
	if (bp & F_16) {
		eval++;
		if (bp & F_11)
			if (bp & F_6)
				eval++;
			else
				if (empty & F_6)
					eval--;

		if (bp & F_12)
			if (bp & F_8)
				eval++;
			else
				if (empty & F_8)
					eval--;
	}

	// c5
	if (bp & F_24 != 0 && empty & (F_28 | F_29 | F_39) == (F_28 | F_29 | F_39)) {
		eval += 2;
		if (bp & F_19)
			if (bp & F_14)
				eval++;
			else
				if (empty & F_14)
					eval--;

		if (bp & F_20)
			if (bp & F_16)
				eval++;
			else
				if (empty & F_16)
					eval--;
	}
	

	// d6
	if (bp & F_29 != 0 && empty & F_38 != 0) {
		eval += 2;
		if (bp & F_24)
			if (bp & F_19)
				eval++;
			else
				if (empty & F_19)
					eval--;

		if (bp & F_25)
			if (bp & F_21)
				eval++;
			else
				if (empty & F_21)
					eval--;
	}
	
	// f6
	if (bp & F_30 != 0 && empty & F_39 != 0) {
		eval += 2;
		if (bp & F_25)
			if (bp & F_20)
				eval++;
			else
				if (empty & F_20)
					eval--;

		if (bp & F_26)
			if (bp & F_22)
				eval++;
			else
				if (empty & F_22)
					eval--;
	}
	
	// for white color
	// c5
	if (wp & F_24) {
		eval--;
		if (wp & F_28)
			if (wp & F_32)
				eval--;
			else
				if (empty & F_32)
					eval++;

		if (wp & F_29)
			if (wp & F_34)
				eval--;
			else
				if (empty & F_34)
					eval++;
	}

	// d6
	if (wp & F_29) {
		eval--;
		if (wp & F_33)
			if (wp & F_37)
				eval--;
			else
				if (empty & F_37)
					eval++;

		if (wp & F_34)
			if (wp & F_39)
				eval--;
			else
				if (empty & F_39)
					eval++;
	}
	// e5
	if (wp & F_25) {
		eval--;
		if (wp & F_29)
			if (wp & F_33)
				eval--;
			else
				if (empty & F_33)
					eval++;

		if (wp & F_30)
			if (wp & F_35)
				eval--;
			else
				if (empty & F_35)
					eval++;
	}
	// c3
	if ((wp & F_15 != 0) && (empty & F_6 != 0)) {
		eval -= 2;
		if (wp & F_19)
			if (wp & F_23)
				eval--;
			else
				if (empty & F_23)
					eval++;

		if (wp & F_20)
			if (wp & F_25)
				eval--;
			else
				if (empty & F_25)
					eval++;
	}
// e3
	if ((wp & F_16) != 0 && (empty & F_7 != 0)) {
		eval -= 2;
		if (wp & F_20)
			if (wp & F_24)
				eval--;
			else
				if (empty & F_24)
					eval++;

		if (wp & F_21)
			if (wp & F_26)
				eval--;
			else
				if (empty & F_26)
					eval++;
	}
	// f4
	if ((wp & F_21 != 0) && (empty & (F_16 | F_17 | F_6) == F_16 | F_17 | F_6)) {
		eval -= 2;
		if (wp & F_25)
			if (wp & F_29)
				eval--;
			else
				if (empty & F_29)
					eval++;

		if (wp & F_26)
			if (wp & F_31)
				eval--;
			else
				if (empty & F_31)
					eval++;
	}
	/*  edge squares         */

	// h2
	if (bp & F_13)
		eval--;
	// h4
	if (bp & F_22)
		eval--;
	// a3
	if (bp & F_14)
		eval--;
	// a5
	if (bp & F_23)
		eval++;
	// h6
	if (bp & F_31 != 0 && empty & F_39 != 0)
		eval++;
	// a7
	if (bp & F_32)
		eval++;
	// for white color
	// a7
	if (wp & F_32)
		eval++;
	// a5
	if (wp & F_23)
		eval++;
	// h6
	if (wp & F_31)
		eval++;
	// a3
	if (wp & F_14 != 0 && empty & F_6 != 0)
		eval--;
	// h4
	if (wp & F_22)
		eval--;
	// h2
	if (wp & F_13)
		eval--;
	// e5
	if (notEmpty & F_25) {
		if (bp & F_25)
			if (phase != OPENING) {
				eval++;
				if (bp & F_20)
					if (bp & F_15)
						eval++;
					else
						if (empty & F_15)
							eval--;

				if (bp & F_21)
					if (bp & F_17)
						eval++;
					else
						if (empty & F_17)
							eval--;
			}
			else
				eval -= 4;
	}

	if (notEmpty & F_20) {
		if (wp & F_20)
			if (phase != OPENING) {
				eval--;
				if (wp & F_24)
					if (wp & F_28)
						eval--;
					else
						if (empty & F_28)
							eval++;

				if (wp & F_25)
					if (wp & F_30)
						eval--;
					else
						if (empty & F_30)
							eval++;
			}
			else
				eval += 4;
	}

	// reward checkers that will king on the next move:
	int p_bonus = (phase == OPENING ? 8 : 16); // promote in one bonus
	// TODO, sprawdz czy moze byc faktycznie krolowka
	//eval += (color == BLACK ? p_bonus << 1 : p_bonus) * __builtin_popcountll(ROW_2 & blackPieces);
	//eval -= (color == WHITE ? p_bonus << 1 : p_bonus) * __builtin_popcountll(ROW_7 & whitePieces);


	int w_lattice = 4*__builtin_popcountll(Lattice2 & whitePieces);
	int b_lattice = 4*__builtin_popcountll(Lattice2 & blackPieces);
	w_lattice -= 4*__builtin_popcountll(LatticeMinus2 & whitePieces);
	b_lattice -= 4*__builtin_popcountll(LatticeMinus2 & blackPieces);

	w_lattice = abs(w_lattice);
	if (w_lattice) eval += w_lattice - 2;
	b_lattice = abs(b_lattice);
	if (b_lattice) eval -= b_lattice - 2;

	int turn = 2;
	if (phase == OPENING) {
		if (blackPieces & F_15)
			eval--;
		if (blackPieces & F_16)
			eval--;
		if (blackPieces & F_8)
			eval += 5;
		if (whitePieces & F_30)
			eval++;
		if (whitePieces & F_29)
			eval++;
		if (whitePieces & F_37)
			eval -= 5;
	}

	// int turn = ( phase == OPENING ? 3:2 );  // color to move gets +turn
	// negamax formulation requires this:
	if (color == BLACK) {
		eval += turn;
		return (eval);
	}
	else {
		eval -= turn;
		return (-eval);
	}

}