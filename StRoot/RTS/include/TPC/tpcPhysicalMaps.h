
#define TPC_ROWS_PER_SECTOR	45
#define TPC_ASIC_NUM		6


static const unsigned char tpcPhysicalRowLen[TPC_ROWS_PER_SECTOR] = {
88,96,104,112,118,126,134,142,150,158,166,174,182,98,100,102,104,106,
106,108,110,112,112,114,116,118,120,122,122,124,126,128,128,130,132,134,
136,138,138,140,142,144,144,144,144} ;

/*
NOTE: the following array has been swapped to account for the L/H pairs on the
TPC RDO fiber being swapped. The proper order for non-TPC RDO boards is

                     { 3, 0, 4, 1, 5, 2}

MJL  02/18/99  

*/
static const unsigned char tpcPhysicalASIC[TPC_ASIC_NUM] = { 0, 3, 1, 4, 2, 5 } ;
