#ifndef _TPX_FCF_HH_
#define _TPX_FCF_HH_

#include <sys/types.h>

#include "tpxCore.h"

class tpxGain ;

//#define FCF_DEBUG1

#define FCF_DO_DOUBLE	// timebins, pads, averages are double instead of u_int

//#define FCF_DO_INT 	// p1, t1 etc. are ints instead of shorts


// flag definitions - NEVER CHANGE
#define FCF_ONEPAD              1

#define FCF_DOUBLE_PAD          2	// offline: merged
#define FCF_MERGED		2

#define FCF_DOUBLE_T            4

#define FCF_FALLING             8	// offline: charge too big!
#define FCF_BIG_CHARGE		8

#define FCF_ROW_EDGE            16      // touched end of row
#define FCF_BROKEN_EDGE         32      // touches one of the mezzanine edges
#define FCF_DEAD_EDGE           64      // touches a dead pad
#define FCF_IN_DOUBLE           128	// one should use the floating point in the union

#define FCF_MAX_CL	32		// max 1D clusters per pad


// timebins at which the gating grid opens & closes -- the gain is compromised if we cross them...
#define TPC_GG_OPEN	22
#define TPC_FF_CLOSE	380


#ifdef FCF_DO_DOUBLE
	typedef float fcf_type;
#else
	typedef int fcf_type;
#endif

#ifdef FCF_DO_INT
	typedef int fcf_short ;
#else
	typedef short fcf_short ;
#endif

#if defined(FCF_SIM_ON) || defined(FCF_ANNOTATE_CLUSTERS) || defined (FCF_DEBUG1)
#define FCF_EXTENDED
#endif

#define FCF_ADC_NOISE           4
#define FCF_ADC_MIN		4	// we additionally can kill 1d sequences where the maximum is below this
#define FCF_MIN_WIDTH           1
#define FCF_MIN_ADC_PAD_C       180


struct tpxFCF_cl {
	union {
		u_int charge ;
		fcf_type f_charge ;
	} ;
	union {
		u_int t_ave ;
		fcf_type f_t_ave ;
	} ;

	fcf_type scharge ;
	fcf_type p_ave ;



	// extents
	fcf_short t1, t_min ;
	fcf_short t2, t_max ;
	fcf_short p1 ;
	fcf_short p2 ;

	fcf_short flags ;

#ifdef FCF_EXTENDED
	fcf_short pix ;	// count of pixels in blob
	fcf_short max_adc ;	// the maximum adc
	fcf_short id ;	// simulation id
	fcf_short cl_id ;	// global cluster id
#endif

} ;




class tpxFCF {
public:
	tpxFCF() ;
	~tpxFCF() ;

	void config(u_int rb_mask) ;
	void apply_gains(int sector, tpxGain *gains) ;

	void start_evt() ;

	int do_pad(tpx_altro_struct *a) ;
	int stage2(u_int *outbuff, int max_bytes) ;


	char do_cuts ;
	int ch_min ;
private:

	u_int *loc_buff ;
	int cur_row ;
	int cur_row_clusters ;
	

	struct stage1 {
		u_short count ;
		u_short f ;	// flags?
		double g ;	// gain
		double t0 ;	// t0
		struct tpxFCF_cl cl[FCF_MAX_CL] ;		
	} ;

	struct stage1 *storage ;	// where I will allocate storage

	void dump(tpxFCF_cl *cl) ;

	inline struct stage1 *get_stage1(int row, int pad)
	{
		if(row_ix[row] < 0) return 0 ;

		return storage + row_ix[row] + (pad-1) ;

	}


	int row_ix[46] ;

	u_int rbs ;
	int sector ;
	tpxGain *gains ;
} ;

#endif
