#ifndef _DAQ_DTA_H_
#define _DAQ_DTA_H_




#include <rts.h>	// for swaps
#include <sys/types.h>	// for u_int

#include <RTS_READER/rts_reader.h>



struct daq_trg_word {
	u_short t ;
	u_char daq ;
	u_char trg ;

	u_int clock ;
	u_int misc ;
#ifdef __ROOT__
ClassDef(daq_trg_word,0)
#endif
} ;

struct daq_adc_tb {
	u_short adc ;
	u_short tb ;
#ifdef __ROOT__
ClassDef(daq_adc_tb,0)
#endif
} ;

struct daq_ped_rms {
	u_short ped ;
	u_short rms ;
} ;


struct daq_cld {
	float tb ;
	float pad ;

	u_short charge ;
	u_short flags ;

	u_short t1, t2 ;
	u_short p1, p2 ;


#ifdef __ROOT__
ClassDef(daq_cld,0)
#endif

} ;

struct daq_gain {
	float g ;
	float t0 ;
} ;


struct daq_store_hdr {
// this MUST NOT change!
	int hdr_bytes ;
	int hdr_version ;
	u_int endianess ;
	int obj_bytes ;
	char obj_name[32] ;
	u_int bytes_used ;
	char describe[128] ;	// string for humans...
//extend after this line
} ;

struct daq_store {
	u_char sec ;
	u_char row ;
	u_char pad ;
	char type ;		// 'r' raw; 'a' adc; 'c' clusters; 'p' pedestals; 'g' gains
	u_int nitems ;		// usually 1 for raw; <512 for adc; <32 for clusters; 512 for pedestals; 1 for gains
} ;

#ifndef __CINT__
#define DAQ_DTA_STRUCT(expr)	__STRING(expr),sizeof(expr)
#define DAQ_DTA_ENDIANESS	0x04030201 
#define DAQ_DTA_C_VERSION	0
#define DAQ_DTA_H_VERSION	0
#endif


class daq_dta {
private:

	daq_store *store_cur ;
	daq_store_hdr *hdr ;

	u_int bytes_alloced ;	// in store!
	u_int nitems ;

	int do_swap ;	// for endianess conversion
	char type ;	// type of structure stored; used for endianess conversion only!

	inline void sw(u_int &x) {
		x = swap32(x) ;
	}
	inline void sw(int &x) {
		x = swap32(x) ;
	}
	inline void sw(u_short &x) {
		x = swap16(x) ;
	}
	inline void sw(short &x) {
		x = swap16(x) ;
	}



	inline void psw(u_int &x) {
		if(do_swap) x = swap32(x) ;
	}
	inline void psw(int &x) {
		if(do_swap) x = swap32(x) ;
	}
	inline void psw(u_short &x) {
		if(do_swap) x = swap16(x) ;
	}
	inline void psw(short &x) {
		if(do_swap) x = swap16(x) ;
	}



	inline u_int tsw(u_int x) {
		if(do_swap) return swap32(x) ;
		return x ;
	}
	inline int tsw(int x) {
		if(do_swap) return swap32(x) ;
		return x ;
	}
	inline u_short tsw(u_short x) {
		if(do_swap) return swap16(x) ;
		return x ;
	}
	inline short tsw(short x) {
		if(do_swap) return swap16(x) ;
		return x ;
	}


	inline short l2h(short x) { return l2h16(x) ; } ;
	inline u_short l2h(u_short x) { return l2h16(x) ; } ;
	inline int l2h(int x) { return l2h32(x) ; } ;
	inline u_int l2h(u_int x) { return l2h32(x) ; } ;

	inline short b2h(short x) { return b2h16(x) ; } ;
	inline u_short b2h(u_short x) { return b2h16(x) ; } ;
	inline int b2h(int x) { return b2h32(x) ; } ;
	inline u_int b2h(u_int x) { return b2h32(x) ; } ;

public:


	daq_dta() {
		bytes_alloced = 0 ;
		store = 0 ;
		store_cur = 0 ;
		do_swap = 0 ;	// obviosly
		nitems = 0 ;
	} ;

	virtual ~daq_dta() ;

	// used for writing!
	daq_store *create(u_int bytes, char *name, int rts_id, const char *o_name, u_int obj_size) ;

	daq_store *get(u_int *avail=0) ;

	void commit(u_int bytes=0) ;

	// used during reading
	void rewind() ;	// rewinds at the beggining

	void clear() ;	// clears counters so it can be reused...

	void release() ;	// releases memory

	int is_empty() ;

	int iterate() {
		if(nitems==0) return 0 ;	// done!


		sec = (store_cur->sec) ;
		row = (store_cur->row) ;
		pad = (store_cur->pad) ;
		ncontent = (store_cur->nitems) ;


		store_cur++ ;		// skip this standard header...
		Byte = (unsigned char *) store_cur  ;	// ...point to start of data!

		// and advance for next
		store_cur = (daq_store *)((char *)store_cur + ncontent * hdr->obj_bytes) ;
		nitems-- ;

		return 1 ;	// return as "have data"
	}


		
	daq_store *store ;


	union {	// UNION of pointers!!!! ONLY!!!! ;
		// the following are system so DO NOT change foolishly
		unsigned char	*Byte ;
		unsigned short *Short ;
		unsigned int	*Int32 ;
		unsigned long long *Int64 ;

		// current specifics:
		daq_trg_word *trg_word ;

		// timebin based zero-suppressed dets:TPX, TPC, FTPC, SVT, SSD maybe
		daq_adc_tb *adc ;
		daq_ped_rms *ped ;
		daq_gain *gain ;
		daq_cld *cld ;



		// special fixed arrays for misc dets...
		unsigned short (*etow)[160] ;		// X 6 i.e. etow[6][160]
		unsigned short (*etow_pre)[4] ;

		unsigned short (*esmd)[192] ;		// X 48 i.e. esmd[48][192]
		unsigned short (*esmd_pre)[4] ;

		unsigned short (*btow)[160] ;		// X 30 i.e. btow[30][160]
		unsigned short (*btow_pre)[4] ;

		unsigned short (*bsmd)[4800] ;		// X 12 i.e. bsmd[12][4800]
		unsigned char *bsmd_cap ;		
	} ;

	



	// from subsequent data
	int sec ;
	union {
		int row ;
		int rdo ;
	} ;

	int pad ;
		
	u_int ncontent ;

#ifdef __ROOT__
ClassDef(daq_dta,0)
#endif

} ;


#endif
