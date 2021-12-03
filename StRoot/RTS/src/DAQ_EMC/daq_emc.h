#ifndef _DAQ_EMC_H_
#define _DAQ_EMC_H_


#include <DAQ_READER/daq_det.h>
#include <stdio.h>


//BARREL TOWER constants
#define BTOW_MAXFEE	30
#define BTOW_PRESIZE	4
#define BTOW_DATSIZE	160

//BARREL SMD constants
#define BSMD_FIBERS	12
#define BSMD_DATSIZE	4800

// ENDCAP Tower constants; from Piotr
#define ETOW_MAXFEE      6
#define ETOW_PRESIZE     4
#define ETOW_DATSIZE   160

// ENDACP ESMD
#define ESMD_MAXFEE     48	// used to be 30
#define ESMD_PRESIZE     4
#define ESMD_DATSIZE   192


#define EMC_PED_VERSION	0x01


struct emc_ped_t {
	uint8_t crate_id ;	//1..48
	uint8_t ch_cou ;		//160; 192
	uint8_t version ;	//for alignment
	uint8_t dummy0 ;		//ibid

	struct {
		float rms ;
		float ped ;
	} ped[0] ;
} ;

struct emc_ped_full_t {
	uint8_t rts_id ;		//ETOW, BTOW, ESMD
	uint8_t crate_cou ;	//6, 30, 48
	uint8_t version ;	
	uint8_t dummy ;	
	struct emc_ped_t crates[0] ;
} ;

struct emc_t {
	uint8_t btow_in ;
	uint16_t btow_max_ch ;
	uint16_t btow_ch ;
	uint16_t btow[4800] ;	
	uint16_t *btow_raw ;
	// added later
	uint16_t btow_new[BTOW_MAXFEE][BTOW_DATSIZE] ;
	uint16_t btow_pre[BTOW_MAXFEE][BTOW_PRESIZE] ;

	uint8_t bsmd_in ;
	uint16_t bsmd_max_ch ;
	uint16_t bsmd_ch ;
	uint16_t bsmd[12][4800] ;	// Nov 2, 2004 - extended from 8 to 12 to encompass the BPRE
#if 0
	// Both raw and zs banks will _not_ be present in the legacy. This is to observe backwards compatibility.
	// However, variable bsmd_raw_in will tell you what data it came from.
	// Use the non-legacy for BSMD readers in case you need this.
	
	uint16_t bsmd_raw[12][4800] ;	// Sep 2008 -- this bank is filled from the raw,non-zerosuppressed data
					// if the event contained both the ZS & NZS bank!
#endif
	uint8_t bsmd_raw_in ;		// flag for above!
	uint8_t  bsmd_cap[12] ;	// capacitor value...
	

	// ENDCAP TOWERS
	uint8_t etow_in ;	// in this event?
	uint16_t etow_max_ch ;	// constant ETOW_MAXFEE * ETOW_DATSIZE

	uint16_t etow_ch ;	// channels above zero
	uint16_t etow[ETOW_MAXFEE][ETOW_DATSIZE] ;	// ADC data...
	uint16_t etow_pre[ETOW_MAXFEE][ETOW_PRESIZE]; // ETOW preamble
	uint16_t *etow_raw ;	// pointer to the beginning of rawdata; raw data is little endian



	// ENDCAP Showermax & preshower(?)
	uint8_t esmd_in ;	// in this event?
	uint16_t esmd_max_ch ;	// 48 * 192
	uint16_t esmd_ch ;	// channels above 0
	uint16_t esmd_max_fee ;	// ESMD_MAXFEE changed between FY04 and FY05...
	uint16_t esmd[ESMD_MAXFEE][ESMD_DATSIZE] ;	// ADC data
	uint16_t esmd_pre[ESMD_MAXFEE][ESMD_PRESIZE]; // ESMD preamble
	uint16_t *esmd_raw ;	// pointer to the beginning of raw data; raw data is little endian


} ;

extern char *getEmcTrgData(char *input, int idx, int *bytes) ;
extern char *emc_single_reader(char *e, int *bytes, int rts_id) ;

extern int emc_reader(char *m, struct emc_t *emc, uint32_t driver, int rts_id, char *ptrs[12], int bytes[12]) ;



class daq_emc : public daq_det {
private:
	class daq_dta *handle_legacy() ;
	class daq_dta *handle_pedrms() ;

	class daq_dta *legacy ;	// "legacy" bank
	class daq_dta *pedrms ;

	static const char *help_string ;
protected:

	int Make() ;

public:
	daq_emc(daqReader *rts_caller=0) ;
	~daq_emc() ;


	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	void help() const {
		printf("%s\n%s\n",GetCVS(),help_string) ;
	}

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_EMC_H_
