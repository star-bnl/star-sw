#ifndef _TRG2RTS_H_
#define _TRG2RTS_H_

/*
	Defines the static map which translates
	the RTS system ID into the TRG TCD bit.

*/

static unsigned char trg2rts_table[16] = {
	TPC_ID,
	SVT_ID,
	BSMD_ID,	// BSMD
	FTP_ID,
	TOF_ID,
	SSD_ID,
	BTOW_ID,	// BTOW
	FPD_ID,
	ETOW_ID,	// ETOW
	ESMD_ID,	// ESMD
	PMD_ID,
	255,	// spare
	255,	// spare
	255,	// spare
	255,	// CTB
	255	// BBC
} ;


static unsigned char rts2trg_table[32] = {
	TRG_TPC_BIT,
	TRG_SVT_BIT,
	TRG_TOF_BIT,
	TRG_BTOW_BIT,	// !!!
	TRG_FPD_BIT,
	TRG_FTPC_BIT,
	255,	// EXT
	255,	// TRG_RICH_BIT,
	255,	// TRG
	255,	// L3
	255,	// SC
	255,	// EXT2
	TRG_PMD_BIT,
	TRG_SSD_BIT,
	TRG_ETOW_BIT,	// !!!
	255,	// DAQ
	TRG_FP2_BIT,
	255,	// PP
	TRG_BSMD_BIT,
	TRG_ESMD_BIT
} ;


#endif
