#ifndef _TRG_SPECIAL_MAP_
#define _TRG_SPECIAL_MAP_

/* 

Designates the canonical map between the special triggers [0..7] issued by the
Trigger's TCD Controller and the actual Trigger Commands fired [0..15].

*/



/* 
	TPC
	Tonko, first version, 1/4/01 
	Tonko, 1/5/01
*/
static const unsigned char trgSpecialMapTPC[8] = {
	0, 8, 9, 10, 11, 12, 0, 0 
} ;

/* 
	FTPC
	Tonko, first version, 6/7/01 
*/
static const unsigned char trgSpecialMapFTPC[8] = {
	0, 8, 9, 10, 11, 12, 0, 0 
} ;




/* 
	SVT
	Tonko, first version, 1/4/01 
*/
static const unsigned char trgSpecialMapSVT[8] = {
	0, 8, 9, 11, 12, 7, 0, 0 
} ;



/*
	EMC Towers
	Tonko, first version, 2/6/01
	Tonko, added a "9" on the position "4"
*/
static const unsigned char trgSpecialMapEMCT[8] = {
	0, 8, 7, 11, 9, 0, 0, 0 
} ;

#endif
