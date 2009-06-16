#ifndef _TPX_FCF_HH_
#define _TPX_FCF_HH_


// main tunable parameters
#define FCF_ADC_NOISE           4
#define FCF_ADC_MIN		4	// we additionally can kill 1d sequences where the maximum is below this
#define FCF_MIN_WIDTH           1
#define FCF_MIN_ADC_PAD_C       180


// timebins at which the gating grid opens & closes -- the gain is compromised if we cross them...
#define TPC_GG_OPEN	22
#define TPC_FF_CLOSE	380


#define FCF_MAX_CL	32		// max 1D clusters per pad

// version/flavor reminders
#define FCF_V_FY08	0x0000		// used in the FY08 run; has the /32 "bug"
#define FCF_V_FY09	0x0001		// /32 bug fixed 


// flag definitions - NEVER CHANGE
#define FCF_ONEPAD              1

#define FCF_DOUBLE_PAD          2	// offline: merged
#define FCF_MERGED		2

#define FCF_DOUBLE_T            4

#define FCF_FALLING             8	// offline: charge too big!
#define FCF_BIG_CHARGE		8

#define FCF_ROW_EDGE            16      // 0x10 touched end of row
#define FCF_BROKEN_EDGE         32      // 0x20 touches one of the mezzanine edges
#define FCF_DEAD_EDGE           64      // 0x40 touches a dead pad
#define FCF_IN_DOUBLE           128	// 0x80 one should use the floating point in the union




//#define FCF_DEBUG

#define FCF_DO_DOUBLE	// timebins, pads, averages are double instead of u_int
//#define FCF_DO_INT 	// p1, t1 etc. are ints instead of shorts


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


// forwad decls
class tpxGain ;
struct daq_cld ;
struct daq_sim_cld ;
struct daq_sim_adc_tb ;


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
	u_short track_id ;

	short quality ;	
	short sim_length ;	// length of the corresponding sim data in *sim

	daq_sim_adc_tb *sim ;	// for simulation, keep the pointer to the cluster data...
} ;




class tpxFCF {
public:
	tpxFCF() ;
	~tpxFCF() ;

	void config(u_int rb_mask, int modes = 0) ;	// modes bitmask
	int modes ;	// bit mask: 1 run simulated; 2 run simulated with local id

	void apply_gains(int sector, tpxGain *gains) ;

	void start_evt() ;

	int do_pad(tpx_altro_struct *a, daq_sim_adc_tb *extra = 0) ;
	int stage2(u_int *outbuff, int max_bytes) ;



	char do_cuts ;
	int ch_min ;

	static int fcf_decode(u_int *p_buff, daq_cld *dc, u_short version=0) ;
	static int fcf_decode(u_int *p_buff, daq_sim_cld *sdc, u_short version=0) ;
	static int afterburner(int cou, daq_cld *store[]) ;

	const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $: $Id: tpxFCF.h,v 1.7 2009/06/16 15:19:16 tonko Exp $: built "__DATE__" "__TIME__ ; return cvs;
	}

private:

	u_int *loc_buff ;
	int cur_row ;
	int cur_row_clusters ;
	
	int cl_marker ;

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

	u_int do_version ;
	u_int read_version ;
} ;

#endif
