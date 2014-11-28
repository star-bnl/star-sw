/****************************************************************
* This file contains the azimuthal sector to receiver board  	*
* mapping as described by the diagram				*
* http://wwwstar.mppmu.mpg.de/map/mapping.html 			*	
*								*
* This file can be used to translate from global sector		* 
* numbering (1-60) to the correct chamber (0-1), rcvboard (0-9)	*
* and rcvbsector (0-2)						*
* -or-								*
* to translate from rcvboard and rcvbsector to global sector	*
* numbering 1-30						*
* (global sectors 31-60 have the same rcvboard mapping)		*
*								*
* J.L.Klay 11-July-2000						*
*****************************************************************/

#define SECTORS 60
#define RCVBOARDS 10
#define AZIMSEC 3

//For a given sector, numbered 1 - 60, what Chamber,
// what RCVBOARD and what RcvbSector?

static const unsigned char sector_map[SECTORS][3] = {
/* Chamber 0 */
/* sector 0 */ {0,0,0},     /* sector 1 */ {0,2,0},
/* sector 2 */ {0,4,0},     /* sector 3 */ {0,5,0},
/* sector 4 */ {0,7,0},     /* sector 5 */ {0,9,0},
/* sector 6 */ {0,0,1},     /* sector 7 */ {0,2,1},
/* sector 8 */ {0,4,1},     /* sector 9 */ {0,5,1},
/* sector 10 */ {0,7,1},    /* sector 11 */ {0,9,1},
/* sector 12 */ {0,0,2},    /* sector 13 */ {0,2,2},
/* sector 14 */ {0,4,2},    /* sector 15 */ {0,5,2},
/* sector 16 */ {0,7,2},    /* sector 17 */ {0,9,2},
/* sector 18 */ {0,1,0},    /* sector 19 */ {0,3,0},
/* sector 20 */ {0,3,1},    /* sector 21 */ {0,6,0},
/* sector 22 */ {0,8,0},    /* sector 23 */ {0,8,1},
/* sector 24 */ {0,1,1},    /* sector 25 */ {0,1,2},
/* sector 26 */ {0,3,2},    /* sector 27 */ {0,6,1},
/* sector 28 */ {0,6,2},    /* sector 29 */ {0,8,2},
/* Chamber 1 */
/* sector 30 */ {1,0,0},     /* sector 31 */ {1,2,0},
/* sector 32 */ {1,4,0},     /* sector 33 */ {1,5,0},
/* sector 34 */ {1,7,0},     /* sector 35 */ {1,9,0},
/* sector 36 */ {1,0,1},     /* sector 37 */ {1,2,1},
/* sector 38 */ {1,4,1},     /* sector 39 */ {1,5,1},
/* sector 40 */ {1,7,1},    /* sector 41 */ {1,9,1},
/* sector 42 */ {1,0,2},    /* sector 43 */ {1,2,2},
/* sector 44 */ {1,4,2},    /* sector 45 */ {1,5,2},
/* sector 46 */ {1,7,2},    /* sector 47 */ {1,9,2},
/* sector 48 */ {1,1,0},    /* sector 49 */ {1,3,0},
/* sector 50 */ {1,3,1},    /* sector 51 */ {1,6,0},
/* sector 52 */ {1,8,0},    /* sector 53 */ {1,8,1},
/* sector 54 */ {1,1,1},    /* sector 55 */ {1,1,2},
/* sector 56 */ {1,3,2},    /* sector 57 */ {1,6,1},
/* sector 58 */ {1,6,2},    /* sector 59 */ {1,8,2},
};

//This is essentially the reverse - if I know the rcvb and azimsec, 
//what is the global sector?

static const unsigned char rcvb_to_sector[RCVBOARDS][AZIMSEC] = {
/* RCVBOARD 0 */
 {1,7,13},
/* RCVBOARD 1 */
 {19,25,26},
/* RCVBOARD 2 */
 {2,8,14},
/* RCVBOARD 3 */
 {20,21,27},
/* RCVBOARD 4 */
 {3,9,15},
/* RCVBOARD 5 */
 {4,10,16},
/* RCVBOARD 6 */
 {22,28,29},
/* RCVBOARD 7 */
 {5,11,17},
/* RCVBOARD 8 */
 {23,24,30},
/* RCVBOARD 9 */
 {6,12,18}
};


