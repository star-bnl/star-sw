#ifndef _TPC23_BASE_H_
#define _TPC23_BASE_H_

struct daq_sim_cld_x ;

class itpcData ;
struct daq_dta ;

class tpc23_base {
public:
	tpc23_base() ;
	virtual ~tpc23_base() ;

	static const int ROW_MAX = 45 ;
	static const int PAD_MAX = 144 ;
	static const int SEQ_MAX = 64 ;
	static const int SIM_FIFOS = 64 ;

	int run_start() ;
	int run_stop() ;

	int evt_start() ;
	int evt_stop() ;

	virtual int rdo_scan(char *mem, int words) ;
	virtual int from22to23(char *dta, int words) ;	// rewrite the old FY22 raw data foramt to FY23
	
	u_char rts_id ;	// tpx, itpc
	u_char fmt ;	// 22: old data format, 23: FY23 data format
	u_char online ;	// 1:running online, 0:offline with quality and track_id stuff

	u_char subdet_id ;		// e.g. from 1..36

	u_char sector1 ;		// from 1
	u_char rdo1 ;		// from 1
	u_char rb_mask ;	// for iTPC really...

	u_char id ;	// of the instance e.g. number of threads

	u_char run_type ;	//1:pedestal,5:pulser,other:physcs

	u_char log_level ;

	u_char trg_cmd ;
	u_char daq_cmd ;
	u_short token ;

	u_char no_cld ;

	u_int last_ix ;
	int sequence_cou ;

	// statistics
	u_int evt ;		// processed
	u_int evt_trgd ;	// triggered event

	u_int err ;		// cleared at rdo_scan?

	u_int run_errors ;

	static itpcData *data_c ;

	// loads once, for ALL sectors...
	int gains_from_cache(const char *fname=0) ;
	virtual u_int get_token_s(char *c_addr, int words) { return 0xFFFFF ;} ;

	struct row_pad_t {
		float gain ;
		float t0 ;
		u_char flags ;
	} ;


	static short bad_fee_cou[24][6] ;
	static short bad_fee[24][6][36] ; 

	struct row_pad_t (*rp_gain)[ROW_MAX+1][PAD_MAX+1] ;	// max for both dets; all sectors

//	static struct row_pad_t (*rp_gain_tpx)[ROW_MAX+1][PAD_MAX+1] ;
//	static struct row_pad_t (*rp_gain_itpc)[ROW_MAX+1][PAD_MAX+1] ;

//	static int rowlen[ROW_MAX+1] ;
//	static int row_min ;
//	static int row_max ;
	int rowlen[ROW_MAX+1] ;
	int row_min ;
	int row_max ;

	static struct sim_dta_t {
		struct {
			char *mem ;
			int bytes ;
		} rb[6] ;
	} sim_dta[SIM_FIFOS] ;

	int load_replay(const char *fname, int sec_soft) ;
	virtual inline void set_rdo(int sec, int rdo) { return ; } ;


	// simulation
	void sim_evt_start(int sector) ;
	//void sim_do_pad(int row, int pad, short *adc, int *track_id) ;
	int do_ch_sim(int row, int pad, u_short *adc, int *track_id) ;
	static int fcf_decode(u_int *p_buff, daq_sim_cld_x *dc, u_int version) ;


	// called from daq_itpc
	int init(daq_dta *gain) ;

//private:


	// Stage 1 -- first copy of the data
	// intermediate, unpacking, storage -- allocated at start
	u_short *s1_dta ;
	int s1_bytes ;



	struct seq_t {	// just the declaration...
		short t_hi ;
		u_short t_lo ;
		u_short blob_id ;
		u_short dta_p ;	// pointer to data
	} ;

	// Stage 1 -- working storage for sequences
	struct s1_t {
		u_int ix ;		// pointer/index to data in s1_dta
		struct seq_t seq[SEQ_MAX+1] ;	// sequences
	} s1[ROW_MAX+1][PAD_MAX+1] ;
		
	// blob stuff: per row!
	struct blob_t {
		u_short t1, t2 ;
		u_short p1, p2 ;
		u_short flags ;
		u_short area ;	// here because it is useful for cuts!
	} blob[PAD_MAX*SEQ_MAX] ;	// really a lot...

	int blob_cou ;

	int blob_ix[PAD_MAX*SEQ_MAX] ;

	struct peaks_t {
		u_short t ;
		u_short p ;
	} peaks[512] ;

	int peaks_cou ;

	u_short store[PAD_MAX+1][512] ;	
	short smooth[PAD_MAX+1][512] ;	// note that it can be negative!

	
	u_int *s2_start ;	// allocated space for FCF data
	u_int *s2_dta ;		// working pointer
	int s2_max_words ;	// allocated size
	int s2_words ;	// used

	int *s1_track_id ;	// simulated data
	int *store_track_id ;
	int sim_track_id ;
	int sim_quality ;
	int sim_max_adc ;
	
	int row_stage1(int row) ;
	int row_stage2(int row) ;

//	static pthread_mutex_t peds_mutex ;
} ;


#endif

