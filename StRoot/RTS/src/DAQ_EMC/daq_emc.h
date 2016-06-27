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
	u_char crate_id ;	//1..48
	u_char ch_cou ;		//160; 192
	u_char version ;	//for alignment
	u_char dummy0 ;		//ibid

	struct {
		float rms ;
		float ped ;
	} ped[0] ;
} ;

struct emc_ped_full_t {
	u_char rts_id ;		//ETOW, BTOW, ESMD
	u_char crate_cou ;	//6, 30, 48
	u_char version ;	
	u_char dummy ;	
	struct emc_ped_t crates[0] ;
} ;

struct emc_t {
	u_char btow_in ;
	u_short btow_max_ch ;
	u_short btow_ch ;
	u_short btow[4800] ;	
	u_short *btow_raw ;
	// added later
	u_short btow_new[BTOW_MAXFEE][BTOW_DATSIZE] ;
	u_short btow_pre[BTOW_MAXFEE][BTOW_PRESIZE] ;

	u_char bsmd_in ;
	u_short bsmd_max_ch ;
	u_short bsmd_ch ;
	u_short bsmd[12][4800] ;	// Nov 2, 2004 - extended from 8 to 12 to encompass the BPRE
#if 0
	// Both raw and zs banks will _not_ be present in the legacy. This is to observe backwards compatibility.
	// However, variable bsmd_raw_in will tell you what data it came from.
	// Use the non-legacy for BSMD readers in case you need this.
	
	u_short bsmd_raw[12][4800] ;	// Sep 2008 -- this bank is filled from the raw,non-zerosuppressed data
					// if the event contained both the ZS & NZS bank!
#endif
	u_char bsmd_raw_in ;		// flag for above!
	u_char  bsmd_cap[12] ;	// capacitor value...
	

	// ENDCAP TOWERS
	u_char etow_in ;	// in this event?
	u_short etow_max_ch ;	// constant ETOW_MAXFEE * ETOW_DATSIZE

	u_short etow_ch ;	// channels above zero
	u_short etow[ETOW_MAXFEE][ETOW_DATSIZE] ;	// ADC data...
	u_short etow_pre[ETOW_MAXFEE][ETOW_PRESIZE]; // ETOW preamble
	u_short *etow_raw ;	// pointer to the beginning of rawdata; raw data is little endian



	// ENDCAP Showermax & preshower(?)
	u_char esmd_in ;	// in this event?
	u_short esmd_max_ch ;	// 48 * 192
	u_short esmd_ch ;	// channels above 0
	u_short esmd_max_fee ;	// ESMD_MAXFEE changed between FY04 and FY05...
	u_short esmd[ESMD_MAXFEE][ESMD_DATSIZE] ;	// ADC data
	u_short esmd_pre[ESMD_MAXFEE][ESMD_PRESIZE]; // ESMD preamble
	u_short *esmd_raw ;	// pointer to the beginning of raw data; raw data is little endian


} ;

extern char *getEmcTrgData(char *input, int idx, int *bytes) ;
extern char *emc_single_reader(char *e, int *bytes, int rts_id) ;

extern int emc_reader(char *m, struct emc_t *emc, u_int driver, int rts_id, char *ptrs[12], int bytes[12]) ;



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
