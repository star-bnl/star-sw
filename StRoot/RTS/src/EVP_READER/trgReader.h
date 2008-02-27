#ifndef _TRG_READER_H_
#define _TRG_READER_H_

extern int trgEvtReader(char *m) ;
extern int trgReader(char *m) ;

extern int trgReader12(char *trgd) ;
extern int trgReader20(char *trgd) ;
extern int trgReader21(char *trgd) ;	// 
extern int trgReader22(char *trgd) ;    // 
extern int trgReader30(char *trgd);
extern int trgReader32(char *trgd);     // called by trgReader10   2007 updates....
extern int trgReader10(char *trgd);

struct trg {
	int mode ;
	u_int max_channels ;
	u_int channels ;

	u_short tcubits ;
	u_short detlive ;
	u_int daqbits ;
	u_char *trgc ;
	
	u_int offline_id[32] ;	// the Offline trigged ID

	void *trg_sum ;			// actual structures depend on the version of trgStructures.h, can be NULL!
	void *trgd ;			// pointer to the whole Trigger data bank but untouched (not swapped)! Can be NULL!

	u_int xing_hi, xing_lo ;
	u_short npre, npost ;
	u_short phys_word, trg_word ;

	
	u_char	CTB[240] ;

        u_char MWC[96] ;     // obsolete as of 2007  (all zeros)

        u_char MTD[32];   // starting 2007 
        u_char VPD[64];   // starting 2007 
        u_char P2P[32];   // starting 2008
        u_char TOF[16];   // starting 2007

	u_char	BEMC[2][240] ;
	u_short BEMC_l1[48] ;

	u_char EEMC[144] ;
	u_short EEMC_l1[16] ;

        u_char FPD[2][2][112] ;    // mostly obsolete (except for [0][0][i])
        u_char FPD_l1[2][2][8] ;   // mostly obsolete (except for [0][0][i])

        u_char FPDW[256];  // starting 2007

	u_char BBC[96] ;	// version 0x21 -> 0x22: extended from 80 to 96
	u_short BBC_l1[16] ;

	u_char	ZDC[16] ;
	u_short ZDC_l1[8] ;
	u_char  ZDCSMD[32] ;

        u_short QQTdataBytes;
        u_int   QQTdata[1600];
} ;

extern struct trg trg ;


#endif
