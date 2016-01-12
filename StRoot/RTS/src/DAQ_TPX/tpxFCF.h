#ifndef _TPX_FCF_H_
#define _TPX_FCF_H_

#include "tpxFCF_flags.h"

// main tunable parameters
#define FCF_ADC_NOISE           4
#define FCF_ADC_MIN		4	// we additionally can kill 1d sequences where the maximum is below this
#define FCF_MIN_WIDTH           1
#define FCF_MIN_ADC_PAD_C       180


// timebins at which the gating grid opens & closes -- the gain is compromised if we cross them...
#define TPC_GG_OPEN	22
#define TPC_FF_CLOSE	380


#define FCF_MAX_CL	64		// max 1D clusters per pad

// version/flavor reminders
#define FCF_V_FY08	0x0000		// used in the FY08 run; has the /32 "bug"
#define FCF_V_FY09	0x0001		// /32 bug fixed 

#define FCF_2D_V_FY13	0x1000		// first version of FCF_2D!



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
		unsigned int charge ;
		fcf_type f_charge ;
	} ;
	union {
		unsigned int t_ave ;
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
	unsigned short track_id ;

	short quality ;	
	short sim_length ;	// length of the corresponding sim data in *sim

	daq_sim_adc_tb *sim ;	// for simulation, keep the pointer to the cluster data...
} ;


struct tpx_altro_struct ;

class tpxFCF {
public:
	tpxFCF() ;
	~tpxFCF() ;

	void config(unsigned int rb_mask, int modes=0, int rows=0, unsigned char *rowlen=0) ;	// modes bitmask
	int modes ;	// bit mask: 1 run simulated; 2 run simulated with local id

	void apply_gains(int sector, tpxGain *gains) ;

	void start_evt() ;

	int do_pad(tpx_altro_struct *a, daq_sim_adc_tb *extra = 0) ;
	int stage2(unsigned int *outbuff, int max_bytes) ;



	char do_cuts ;
	char run_compatibility ;

	int ch_min ;

	static int fcf_decode(unsigned int *p_buff, daq_cld *dc, unsigned short version=0) ;
	static int fcf_decode(unsigned int *p_buff, daq_sim_cld *sdc, unsigned short version=0) ;
	static int afterburner(int cou, daq_cld *store[]) ;
	static char *fcf_flags(u_char flags) ;

	// new functions for FY13 which assist in running FCF on any sector and rdo
	void config2(int sec, int rdo, int modes=0, int rows=0, unsigned char *rowlen=0) ;
	void apply_gains2(tpxGain *gains) ;
	void start_evt2(int sec1, int rdo1) ;
	void set_id(int id) {
		my_id = id ;
	}
	int fcf_style ;	// new for FY13!


	const char *GetCVS() const {	// Offline
		static const char cvs[]="Tag $Name:  $: $Id: tpxFCF.h,v 1.18 2016/01/12 16:20:23 tonko Exp $: built " __DATE__ " " __TIME__ ; return cvs;
	}

	int sector ;	// counts from 1
	int rdo ;	// counts from 1 but can be 0 if we want the whole sector
	int row_count ;	// will default to 45 in the constructor unless overriden!

	inline int is_pad_valid(int row, int pad)
	{
		s_static_storage *ss = get_static(row,pad) ;
		if(ss==0) return 0 ;
		if(ss->f & FCF_KILLED_PAD) return 0 ;
		return 1 ;
	}

	inline void event_debug()
	{
		for(int r=1;r<=row_count;r++) {
			if(gain_storage[sector-1][r] == 0) continue ;

			for(int p=1;p<=tpx_rowlen[r];p++) {
				s_static_storage *ss = get_static(r,p) ;

				stage1 *s1 = get_working(r,p) ;

				LOG(TERR,"S %2d: RP %d:%d : gain %f, t0 %f, flags 0x%X, count %d",sector,
				    r,p,ss->g,ss->t0,ss->f,s1->count) ;
			}
		}
	}


protected:
	unsigned char *tpx_rowlen ;


	struct s_static_storage {
		double g ;
		double t0 ;

		unsigned short f ;
	} ;

	static struct s_static_storage *gain_storage[24][256] ;

	inline struct s_static_storage *get_static(int row, int pad)
	{
		int s = sector -1 ;
		if(gain_storage[s][row]==0) return 0 ;

		return gain_storage[s][row] + (pad-1) ;

	}


private:

	unsigned int *loc_buff ;
	int cur_row ;
	int cur_row_clusters ;



	int tpx_padplane ;

	int cl_marker ;

	struct stage1 {	// per pad, indexed as (row,pad)
		unsigned short count ;	// count of 1D clusters found...
		unsigned short f ;	// initial flags: dead edge, broken RDO edge, etc.
		double g ;	// gain
		double t0 ;	// t0
		struct tpxFCF_cl cl[FCF_MAX_CL] ;		
	} ;


	struct stage1 *storage ;	// where I will allocate storage

	void dump(tpxFCF_cl *cl, int row) ;

	inline struct stage1 *get_stage1(int row, int pad)
	{
		if(row_ix[row] < 0) return 0 ;

		return storage + row_ix[row] + (pad-1) ;

	}


	int row_ix[256] ;	// we exaggerate! normally was "46"

	unsigned int rbs ;
	int my_id ;


//	static const int max_tot_count = 1152 ;	// maximum pads per RDO
	static const int max_tot_count = 2000 ;	// maximum pads per run really -- used in FY16



	struct stage1 *working_storage[24][256] ;
	
	inline struct stage1  *get_working(int row, int pad)
	{

		int s = sector -1 ;

		if(working_storage[s][row]==0) {
			int bytes = tpx_rowlen[row] * sizeof(stage1) ;

			working_storage[s][row] = (stage1 *) valloc(bytes) ;
		}

		return working_storage[s][row] + (pad-1) ;
	}



	unsigned int do_version ;
	unsigned int read_version ;
} ;

#endif
