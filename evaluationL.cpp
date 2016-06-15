// dllmain.cpp : Defines the entry point for the DLL application.
#include "evaluationL.h"
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

int evaluationL(int *b, int color, int alpha, int beta, int nbm, int nwm, int nbk, int nwk, int realdepth)
{
	if (nbm == 0 && nbk == 0)  return (color == BLACK ? (realdepth - MATE) : MATE - realdepth);
	if (nwm == 0 && nwk == 0) return(color == WHITE ? (realdepth - MATE) : MATE - realdepth);

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
		for (i = 5; i <= 40; i += 5) {
			if (b[i]) {
				if (b[i] & MAN) {
					WGL = 0;
					BGL = 0;
					break;
				}
				if (b[i] == WHT_KNG) WGL = 1;
				else
					if (b[i] == BLK_KNG) BGL = 1;
			}
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
			
		

		static int PST_king[41] = { 0,0,0,0,0,  // 0..4
												 2,1,0,2,0, // 5..9
												 2,1,2,2, // 10..13
												 1,2,3,2,0, // 14..18
												 1,3,3,0, // 19..22
												 0,3,3,1,0, // 23..27
												 2,3,2,1, // 28..31
												 2,2,1,2,0, // 32..36
												 2,0,1,2 // 37..40
		};

		static int be_man[41] = { 0,0,0,0,0,                   // 0 .. 4
											  0,0,0,0,0,                  // 5 .. 9
											  2,2,2,2,                     // 10 .. 13
											  4,4,4,4,0,               // 14 .. 18
											  6,6,6,6,                    // 19 .. 22
											  8,8,8,8,0,              // 23 .. 27
											  10,10,10,4,                   //  28 .. 31
											   4,12,12,12,0,       // 32 .. 36
											   0,0,0,0                      // 37 .. 40
		};

		static int we_man[41] = { 0,0,0,0,0,                        // 0 .. 4
											   0,0,0,0,0,                  // 5 .. 9
											  12,12,12,4,                     // 10 .. 13
											   4,10,10,10,0,               // 14 .. 18
											   8,8,8,8,                     // 19 .. 22
											   6,6,6,6,0,              // 23 .. 27
											   4,4,4,4,                         //  28 .. 31
											   2,2,2,2,0,          // 32 .. 36
											   0,0,0,0                   // 37 .. 40
		};

		static int LatticeArray[] = { 0,0,0,0,0,  // 0 .. 4
												 0,0,0,0,0,  // 5 .. 9
												 2,2,2,0,     // 10 .. 13
												  0,-2,-2,-2,0,   // 14 .. 18
												  2,2,2,0,           // 19 .. 22
												  0,-2,-2,-2,0,    // 23 .. 27
												  2,2,2,0,            //  28 .. 31
												  0,-2,-2,-2,0,  // 32 .. 36
												  0,0,0,0                   // 37 .. 40
		};

		int w_lattice = 0;
		int b_lattice = 0;
		for (i = 5; i <= 40; i++) {
			if (b[i]) {
				switch (b[i]) {
				case BLK_MAN:
					eval = eval + be_man[i]; b_lattice += LatticeArray[i]; break;
				case WHT_MAN:
					eval = eval - we_man[i]; w_lattice += LatticeArray[i]; break;
				case BLK_KNG:
					eval = eval + PST_king[i]; break;
				case WHT_KNG:
					eval = eval - PST_king[i]; break;
				default:break;
				}
			}
		}
		return eval;
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
				for (i = 5; i <= 8; i++)
				{
					for (j = 0; j < 4; j++)
					{
						if (b[i + 9 * j] != FREE) stonesinsystem++;
					}
				}
				if (stonesinsystem % 2) // the number of stones in blacks system is odd -> he has the move
					move = themoveval*(24 - allstones) / 6;
				else
					move = -themoveval*(24 - allstones) / 6;
				eval += move;
			}
			else // color is WHITE
			{
				for (i = 10; i <= 13; i++)
				{
					for (j = 0; j < 4; j++)
					{
						if (b[i + 9 * j] != FREE) stonesinsystem++;
					}
				}
				if (stonesinsystem % 2) // the number of stones in whites system is odd -> he has the move
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

	return 0;
	v1 = 100 * nbm + 250 * nbk;
	v2 = 100 * nwm + 250 * nwk;
	eval = v1 - v2;
	eval += (200 * (v1 - v2)) / (v1 + v2);      /*favor exchanges if in material plus*/
	return nbm;
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
	if (b[5] & MAN) code++;
	if (b[6] & MAN) code += 2;
	if (b[7] & MAN) code += 4; // Golden checker
	if (b[8] & MAN) code += 8;
	if (b[13] == BLK_MAN) code += 16;
	backrank = br[code];
	code = 0;
	if (b[37] & MAN) code += 8;
	if (b[38] & MAN) code += 4; // Golden checker
	if (b[39] & MAN) code += 2;
	if (b[40] & MAN) code++;
	if (b[32] == WHT_MAN) code += 16;
	backrank -= br[code];
	int brv = (phase == OPENING ? 3 : 1);  // multiplier for back rank -- back rank value
	eval += brv*backrank;

	if (nbm == nwm) {
		/* balance                */
		/* how equally the pieces are distributed on the left and right sides of the board */

		int balance;
		static int value[17] = { 0,0,0,0,0,1,256,0,0,16,4096,0,0,0,0,0,0 };
		int nbml, nbmr; // number black men left - right
		int nwml, nwmr; // number white men left - right
		// left flank
		code = 0;
		// count file-a men ( on 5,14,23,32 )
		code += value[b[5]];
		code += value[b[14]];
		code += value[b[23]];
		code += value[b[32]];
		// count file-b men ( on 10,19,28,37 )
		code += value[b[10]];
		code += value[b[19]];
		code += value[b[28]];
		code += value[b[37]];
		// count file-c men ( on 6,15,24,33 )
		code += value[b[6]];
		code += value[b[15]];
		code += value[b[24]];
		code += value[b[33]];
		nwml = code & 15;
		nbml = (code >> 8) & 15;
		// empty left flank ?
		if (nwml == 0) eval += 10;
		if (nbml == 0) eval -= 10;
		// right flank
		code = 0;
		// count file-f men ( on 12,21,30,39 )
		code += value[b[12]];
		code += value[b[21]];
		code += value[b[30]];
		code += value[b[39]];
		// count file-g men ( on 8,17,26,35 )
		code += value[b[8]];
		code += value[b[17]];
		code += value[b[26]];
		code += value[b[35]];
		// count file-h men ( on 13,22,31,40 )
		code += value[b[13]];
		code += value[b[22]];
		code += value[b[31]];
		code += value[b[40]];
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
	if (!b[5] && !b[10]) {
		eval += devsinglecornerval;
		if (!b[6])
			eval -= 5;
	}

	if (!b[40] && !b[35]) {
		eval -= devsinglecornerval;
		if (!b[39])
			eval += 5;
	}

	/* center control */
	// for black color
						 // f4
	if (b[21]) {
		if ((b[21] & BLACK) != 0) {
			eval++;
			if ((b[16] & BLACK) != 0)
				if ((b[11] & BLACK) != 0)
					eval++;
				else
					if (!b[11])
						eval--;

			if ((b[17] & BLACK) != 0)
				if ((b[13] & BLACK) != 0)
					eval++;
				else
					if (!b[13])
						eval--;
		}
	}

	// d4
	if (b[20]) {
		if ((b[20] & BLACK) != 0) {
			eval++;
			if ((b[15] & BLACK) != 0)
				if ((b[10] & BLACK) != 0)
					eval++;
				else
					if (!b[10])
						eval--;

			if ((b[16] & BLACK) != 0)
				if ((b[12] & BLACK) != 0)
					eval++;
				else
					if (!b[12])
						eval--;
		}
	}

	// e3
	if (b[16]) {
		if ((b[16] & BLACK) != 0) {
			eval++;
			if ((b[11] & BLACK) != 0)
				if ((b[6] & BLACK) != 0)
					eval++;
				else
					if (!b[6])
						eval--;

			if ((b[12] & BLACK) != 0)
				if ((b[8] & BLACK) != 0)
					eval++;
				else
					if (!b[8])
						eval--;
		}
	}

	// c5
	if (b[24]) {
		if ((b[24] & BLACK) != 0 && b[28] == FREE && b[29] == FREE && b[39] == FREE) {
			eval += 2;
			if ((b[19] & BLACK) != 0)
				if ((b[14] & BLACK) != 0)
					eval++;
				else
					if (!b[14])
						eval--;

			if ((b[20] & BLACK) != 0)
				if ((b[16] & BLACK) != 0)
					eval++;
				else
					if (!b[16])
						eval--;
		}
	}

	// d6
	if (b[29]) {
		if ((b[29] & BLACK) != 0 && b[38] == FREE) {
			eval += 2;
			if ((b[24] & BLACK) != 0)
				if ((b[19] & BLACK) != 0)
					eval++;
				else
					if (!b[19])
						eval--;

			if ((b[25] & BLACK) != 0)
				if ((b[21] & BLACK) != 0)
					eval++;
				else
					if (!b[21])
						eval--;
		}
	}

	// f6
	if (b[30]) {
		if ((b[30] & BLACK) != 0 && b[39] == FREE) {
			eval += 2;
			if ((b[25] & BLACK) != 0)
				if ((b[20] & BLACK) != 0)
					eval++;
				else
					if (!b[20])
						eval--;

			if ((b[26] & BLACK) != 0)
				if ((b[22] & BLACK) != 0)
					eval++;
				else
					if (!b[22])
						eval--;
		}
	}
	// for white color
				   // c5
	if (b[24]) {
		if ((b[24] & WHITE) != 0) {
			eval--;
			if ((b[28] & WHITE) != 0)
				if ((b[32] & WHITE) != 0)
					eval--;
				else
					if (!b[32])
						eval++;

			if ((b[29] & WHITE) != 0)
				if ((b[34] & WHITE) != 0)
					eval--;
				else
					if (!b[34])
						eval++;
		}
	}

	// d6
	if (b[29]) {
		if ((b[29] & WHITE) != 0) {
			eval--;
			if ((b[33] & WHITE) != 0)
				if ((b[37] & WHITE) != 0)
					eval--;
				else
					if (!b[37])
						eval++;

			if ((b[34] & WHITE) != 0)
				if ((b[39] & WHITE) != 0)
					eval--;
				else
					if (!b[39])
						eval++;
		}
	}
	// e5
	if (b[25]) {
		if ((b[25] & WHITE) != 0) {
			eval--;
			if ((b[29] & WHITE) != 0)
				if ((b[33] & WHITE) != 0)
					eval--;
				else
					if (!b[33])
						eval++;

			if ((b[30] & WHITE) != 0)
				if ((b[35] & WHITE) != 0)
					eval--;
				else
					if (!b[35])
						eval++;
		}
	}
	// c3
	if (b[15]) {
		if ((b[15] & WHITE) != 0 && b[6] == FREE) {
			eval -= 2;
			if ((b[19] & WHITE) != 0)
				if ((b[23] & WHITE) != 0)
					eval--;
				else
					if (!b[23])
						eval++;

			if ((b[20] & WHITE) != 0)
				if ((b[25] & WHITE) != 0)
					eval--;
				else
					if (!b[25])
						eval++;
		}
	}
	// e3
	if (b[16]) {
		if ((b[16] & WHITE) != 0 && b[7] == FREE) {
			eval -= 2;
			if ((b[20] & WHITE) != 0)
				if ((b[24] & WHITE) != 0)
					eval--;
				else
					if (!b[24])
						eval++;

			if ((b[21] & WHITE) != 0)
				if ((b[26] & WHITE) != 0)
					eval--;
				else
					if (!b[26])
						eval++;
		}
	}
	// f4
	if (b[21]) {
		if ((b[21] & WHITE) != 0 && b[16] == FREE && b[17] == FREE && b[6] == FREE) {
			eval -= 2;
			if ((b[25] & WHITE) != 0)
				if ((b[29] & WHITE) != 0)
					eval--;
				else
					if (!b[29])
						eval++;

			if ((b[26] & WHITE) != 0)
				if ((b[31] & WHITE) != 0)
					eval--;
				else
					if (!b[31])
						eval++;
		}
	}

	/*  edge squares         */

	// h2
	if (b[13]) {
		if ((b[13] & BLACK) != 0)
			eval--;
	}
	// h4
	if (b[22]) {
		if ((b[22] & BLACK) != 0)
			eval--;
	}
	// a3
	if (b[14]) {
		if ((b[14] & BLACK) != 0)
			eval--;
	}
	// a5
	if (b[23]) {
		if ((b[23] & BLACK) != 0)
			eval++;
	}
	// h6
	if (b[31]) {
		if ((b[31] & BLACK) != 0 && b[39] == FREE)
			eval++;
	}
	// a7
	if (b[32]) {
		if ((b[32] & BLACK) != 0)
			eval++;
	}
	// for white color
				 // a7
	if (b[32]) {
		if ((b[32] & WHITE) != 0)
			eval++;
	}
	// a5
	if (b[23]) {
		if ((b[23] & WHITE) != 0)
			eval++;
	}
	// h6
	if (b[31]) {
		if ((b[31] & WHITE) != 0)
			eval++;
	}
	// a3
	if (b[14]) {
		if ((b[14] & WHITE) != 0 && b[6] == FREE)
			eval--;
	}
	// h4
	if (b[22]) {
		if ((b[22] & WHITE) != 0)
			eval--;
	}
	// h2
	if (b[13]) {
		if ((b[13] & WHITE) != 0)
			eval--;
	}
	// e5
	if (b[25]) {
		if ((b[25] & BLACK) != 0)
			if (phase != OPENING) {
				eval++;
				if ((b[20] & BLACK) != 0)
					if ((b[15] & BLACK) != 0)
						eval++;
					else
						if (!b[15])
							eval--;

				if ((b[21] & BLACK) != 0)
					if ((b[17] & BLACK) != 0)
						eval++;
					else
						if (!b[17])
							eval--;
			}
			else
				eval -= 4;
	}

	if (b[20]) {
		if ((b[20] & WHITE) != 0)
			if (phase != OPENING) {
				eval--;
				if ((b[24] & WHITE) != 0)
					if ((b[28] & WHITE) != 0)
						eval--;
					else
						if (!b[28])
							eval++;

				if ((b[25] & WHITE) != 0)
					if ((b[30] & WHITE) != 0)
						eval--;
					else
						if (!b[30])
							eval++;
			}
			else
				eval += 4;
	}

	// reward checkers that will king on the next move:
	int p_bonus = (phase == OPENING ? 8 : 16); // promote in one bonus
	for (i = 32; i <= 35; i++) {
		if ((b[i] & BLACK) != 0)
			if ((b[i] & MAN) != 0)
				if (!b[i + 5] || !b[i + 4]) eval += color == BLACK ? p_bonus << 1 : p_bonus;
	}

	for (i = 10; i <= 13; i++) {
		if ((b[i] & WHITE) != 0)
			if ((b[i] & MAN) != 0)
				if (!b[i - 5] || !b[i - 4]) eval -= color == WHITE ? p_bonus << 1 : p_bonus;
	}

	static int LatticeArray[] = { 0,0,0,0,0,  // 0 .. 4
											 0,0,0,0,0,  // 5 .. 9
											 4,4,4,0,     // 10 .. 13
											  0,-4,-4,-4,0,   // 14 .. 18
											  4,4,4,0,           // 19 .. 22
											  0,-4,-4,-4,0,    // 23 .. 27
											  4,4,4,0,            //  28 .. 31
											  0,-4,-4,-4,0  // 32 .. 36
	};

	int w_lattice = 0;
	int b_lattice = 0;
	for (i = 10; i <= 35; i++) {
		if (b[i]) {
			if (b[i] == BLK_MAN)
				b_lattice += LatticeArray[i];
			else
				if (b[i] == WHT_MAN)
					w_lattice += LatticeArray[i];
		}
	}

	w_lattice = abs(w_lattice);
	if (w_lattice) eval += w_lattice - 2;
	b_lattice = abs(b_lattice);
	if (b_lattice) eval -= b_lattice - 2;

	int turn = 2;
	if (phase == OPENING) {
		if (b[15] == BLK_MAN)
			eval--;
		if (b[16] == BLK_MAN)
			eval--;
		if (b[8] == BLK_MAN)
			eval += 5;
		if (b[30] == WHT_MAN)
			eval++;
		if (b[29] == WHT_MAN)
			eval++;
		if (b[37] == WHT_MAN)
			eval -= 5;
	}

	// int turn = ( phase == OPENING ? 3:2 );  // color to move gets +turn
	// negamax formulation requires this:
	if (color == BLACK) {
		return (eval);
	}
	else {
		return (-eval);
	}

}