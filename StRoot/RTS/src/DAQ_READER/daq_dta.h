#ifndef _DAQ_DTA_H_
#define _DAQ_DTA_H_




#include <rts.h>	// for swaps
#include <stdint.h>	// for uint32_t

#include "daq_dta_structs.h"


struct daq_store {
//	uint8_t sec ;
//	uint8_t row ;
//	uint8_t pad ;
//	char type ;		// 'r' raw; 'a' adc; 'c' clusters; 'p' pedestals; 'g' gains

	uint32_t sec ;
	uint32_t row ;
	uint32_t pad ;
	char type ;		// 'r' raw; 'a' adc; 'c' clusters; 'p' pedestals; 'g' gains

	uint32_t nitems ;		// usually 1 for raw; <512 for adc; <32 for clusters; 512 for pedestals; 1 for gains
} ;


struct daq_store_hdr {
// this MUST NOT change!
	int hdr_bytes ;
	int hdr_version ;
	uint32_t endianess ;
	int obj_bytes ;
	char obj_name[32] ;
	uint32_t bytes_used ;
	char describe[128] ;	// string for humans...
//extend after this line
} ;



#define DAQ_DTA_STRUCT(expr)	__STRING(expr),sizeof(expr)
#define DAQ_DTA_ENDIANESS	0x04030201 
#define DAQ_DTA_C_VERSION	0
#define DAQ_DTA_H_VERSION	0



class daq_dta {
private:

	daq_store *store_cur ;
	daq_store_hdr *hdr ;

	uint32_t bytes_alloced ;	// in store!
	uint32_t nitems ;

	int do_swap ;	// for endianess conversion
	char type ;	// type of structure stored; used for endianess conversion only!

	inline void sw(uint32_t &x) {
		x = swap32(x) ;
	}
	inline void sw(int &x) {
		x = swap32(x) ;
	}
	inline void sw(uint16_t &x) {
		x = swap16(x) ;
	}
	inline void sw(short &x) {
		x = swap16(x) ;
	}



	inline void psw(uint32_t &x) {
		if(do_swap) x = swap32(x) ;
	}
	inline void psw(int &x) {
		if(do_swap) x = swap32(x) ;
	}
	inline void psw(uint16_t &x) {
		if(do_swap) x = swap16(x) ;
	}
	inline void psw(short &x) {
		if(do_swap) x = swap16(x) ;
	}



	inline uint32_t tsw(uint32_t x) {
		if(do_swap) return swap32(x) ;
		return x ;
	}
	inline int tsw(int x) {
		if(do_swap) return swap32(x) ;
		return x ;
	}
	inline uint16_t tsw(uint16_t x) {
		if(do_swap) return swap16(x) ;
		return x ;
	}
	inline short tsw(short x) {
		if(do_swap) return swap16(x) ;
		return x ;
	}


	inline short l2h(short x) { return l2h16(x) ; } ;
	inline uint16_t l2h(uint16_t x) { return l2h16(x) ; } ;
	inline int l2h(int x) { return l2h32(x) ; } ;
	inline uint32_t l2h(uint32_t x) { return l2h32(x) ; } ;

	inline short b2h(short x) { return b2h16(x) ; } ;
	inline uint16_t b2h(uint16_t x) { return b2h16(x) ; } ;
	inline int b2h(int x) { return b2h32(x) ; } ;
	inline uint32_t b2h(uint32_t x) { return b2h32(x) ; } ;

	daq_store *get(uint32_t obj_cou=0) ;

	void clear() ;	// clears counters so it can be reused...

	void release() ;	// releases memory



	void commit(uint32_t bytes=0) ;

	daq_store *store ;
public:


	daq_dta() ;
	virtual ~daq_dta() ;

	// used for writing!
	daq_store *create(uint32_t bytes, const char *name, int rts_id, const char *o_name, uint32_t obj_size) ;

	void *request(uint32_t obj_cou) ;
	void finalize(uint32_t obj_cou, int s=0, int row=0, int pad=0) ;
	void rewind() ;	// rewinds at the beggining


	int is_empty() ;

	int iterate() ;

	inline size_t get_size_t() {
		return hdr->obj_bytes ;
	}

	virtual const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $: $Id: daq_dta.h,v 1.10 2018/04/10 12:13:31 tonko Exp $: built " __DATE__ " " __TIME__ ; 
		return cvs;
	}

		



	union {	// UNION of pointers!!!! ONLY!!!! ;
		// the following are system so DO NOT change foolishly
		void *Void ;
		unsigned char	*Byte ;
		unsigned short *Short ;
		unsigned int	*Int32 ;
		unsigned long long *Int64 ;

		// current specifics:
		daq_trg_word *trg_word ;

		// timebin based zero-suppressed dets:TPX, TPC, FTPC, SVT, SSD maybe
		daq_adc_tb *adc ;

		// TPC/TPX cluster data
		daq_cld *cld ;

		// TPC/TPX simulated data
		daq_sim_adc_tb *sim_adc ;
		// TPC/TPX output of simulated clusters...
		daq_sim_cld *sim_cld ;

		// TPC/TPX gain
		daq_det_gain *gain ;
		

		/* commented: to be deleted
		// older EVP_READER structures...
		struct svt_t *svt ;
		struct trg_t *trg ;
		struct ssd_t *ssd ;
		struct tof_t *tof ;
		struct pp2pp_t *pp2pp ;

		// special fixed arrays for misc dets...
		unsigned short (*etow)[160] ;		// X 6 i.e. etow[6][160]
		unsigned short (*etow_pre)[4] ;

		unsigned short (*esmd)[192] ;		// X 48 i.e. esmd[48][192]
		unsigned short (*esmd_pre)[4] ;

		unsigned short (*btow)[160] ;		// X 30 i.e. btow[30][160]
		unsigned short (*btow_pre)[4] ;

		unsigned short (*bsmd)[4800] ;		// X 12 i.e. bsmd[12][4800]
		unsigned char *bsmd_cap ;

		*/
	} ;

	



	// from subsequent data
	int sec ;
	union {
		int row ;
		int rdo ;
	} ;

	int pad ;

	void *meta ;	// detector specific info bank...

	uint32_t ncontent ;

	uint32_t mode ;	// bank/detector specific; can be error, can be something else...
} ;


#endif
