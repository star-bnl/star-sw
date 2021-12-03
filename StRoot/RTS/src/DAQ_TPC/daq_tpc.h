#ifndef _DAQ_TPC_H_
#define _DAQ_TPC_H_


#include <DAQ_READER/daq_det.h>

#define TPC_READER_MAX_CLUSTERS	5000


// cluster structures
struct tpc_cl  {
	float p ;
	float t ;
	uint16_t charge ;
	uint16_t flags ;
	uint16_t t1, t2, p1, p2 ;
//#ifdef FCF_SIM_ON
	uint16_t adc_max ;
//#endif
} ;

struct tpc_t {

	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels_all ;
	int max_channels_sector ;
	int channels_sector ;
	int has_clusters ;	// are there any clusters in the data?

	// how many valid timebins in this pad
	uint16_t counts[45][182] ;

	// up to 512 valid timebins (count is in counts)
	// timebin is overloaded with RMS data if mode==1
	uint16_t timebin[45][182][512] ;

	// up to 512 valid adcs (same count as above...)
	// overloaded with PED data if mode==1
	uint8_t adc[45][182][512] ;

	// how many valid clusters in this row
	uint16_t cl_counts[45] ;

	// cluster structures
	struct tpc_cl cl[45][TPC_READER_MAX_CLUSTERS] ;

	uint32_t *cl_p[45][3] ;	// points MZ row data

	uint8_t rdo_present[6] ;	// boolean stating the presence of an RDO
} ;

class daq_tpc : public daq_det {
private:
	class daq_dta *handle_adc(int sec, int rdo) ;
	class daq_dta *handle_cld(int sec, int rdo) ;
	class daq_dta *handle_legacy(int sec, int rdo) ;

	class daq_dta *adc ;	// "adc"
	class daq_dta *cld ;	// "cld"
	class daq_dta *legacy ;

	static const int MAX_SEC = 24 ;	// was TPC_MAXFEE; used to be 30 before FY05...
	static const int MAX_RDO = 6 ;	// not used


protected:


public:
	daq_tpc(daqReader *rts_caller=0) ;
	~daq_tpc() ;

	// special for TPC...
	// Assume that tpc raw structure is alread filled
	//
	//  t0corr   --> int   t0[46][242], for this sector
	//  gainCorr --> uint32_t gain[46][242], for this sector
	// 
	int fcfReader(int sector, int *t0c, uint32_t *gainc, tpc_t *tpc_p) ;

	daq_dta  *get(const char *bank="*",int c1=-1, int c2=-1, int c3=-1, void *p1=0, void *p2=0) ;

	int get_l2(char *buff, int buff_bytes, struct daq_trg_word *trg, int prompt) ;

	const char *GetCVS() const {	// Offline
                static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
	}

} ;


#endif	// _DAQ_TPC_H_
