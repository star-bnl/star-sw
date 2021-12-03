#ifndef _DAQ_TRG_H_
#define _DAQ_TRG_H_

#include <stdio.h>
#include <DAQ_READER/daq_det.h>



struct trg_t {
	int mode ;
	uint32_t max_channels ;
	uint32_t channels ;

	uint16_t tcubits ;
	uint16_t detlive ;
	uint32_t daqbits ;
	uint8_t *trgc ;
	
	uint32_t offline_id[32] ;	// the Offline trigged ID

	void *trg_sum ;			// actual structures depend on the version of trgStructures.h, can be NULL!
	void *trgd ;			// pointer to the whole Trigger data bank but untouched (not swapped)! Can be NULL!

	uint32_t xing_hi, xing_lo ;
	uint16_t npre, npost ;
	uint16_t phys_word, trg_word ;

	
	uint8_t	CTB[240] ;

        uint8_t MWC[96] ;     // obsolete as of 2007  (all zeros)

        uint8_t MTD[32];   // starting 2007 
        uint8_t VPD[64];   // starting 2007 
        uint8_t P2P[32];   // starting 2008
        uint8_t TOF[16];   // starting 2007

	uint8_t	BEMC[2][240] ;
	uint16_t BEMC_l1[48] ;

	uint8_t EEMC[144] ;
	uint16_t EEMC_l1[16] ;

        uint8_t FPD[2][2][112] ;    // mostly obsolete (except for [0][0][i])
        uint8_t FPD_l1[2][2][8] ;   // mostly obsolete (except for [0][0][i])

        uint8_t FPDW[256];  // starting 2007

	uint8_t BBC[96] ;	// version 0x21 -> 0x22: extended from 80 to 96
	uint16_t BBC_l1[16] ;

	uint8_t	ZDC[16] ;
	uint16_t ZDC_l1[8] ;
	uint8_t  ZDCSMD[32] ;

        uint16_t QQTdataBytes;
        uint32_t   QQTdata[1600];
} ;



class daq_trg : public daq_det {
private:
	class daq_dta *handle_legacy() ;
	class daq_dta *handle_raw() ;

	class daq_dta *legacy ;	// "legacy" bank
	class daq_dta *raw ;	// full content of Trigger's banks, any version...

	static const char *help_string ;
protected:


public:
	daq_trg(daqReader *rts_caller=0) ;
	~daq_trg() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_TRG_H_
