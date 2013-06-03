#ifndef _TPX_FCF_2D_H_
#define _TPX_FCF_2D_H_


#include <stdlib.h>

#include <DAQ_TPX/tpxFCF.h>


// remove DO_SIMULATION if running in DAQ PC (aka real-time)
#ifdef TPX_REALTIME
	#ifdef DO_DBG
		#undef DO_DBG
	#endif

	#ifdef DO_SIMULATION
		#undef DO_SIMULATION
	#endif

#else
	#define DO_SIMULATION
#endif


class tpxFCF_2D : public tpxFCF
{
public:
	tpxFCF_2D() ;

	~tpxFCF_2D() { 
		if(data_raw) {
			free(data_raw) ;
			data_raw = 0 ;
		}
	}


	static const int MAX_BLOB_COUNT		= 400 ;
	static const int MAX_SEQ_PER_BLOB	= 256 ; // was 64 ;
	static const int MAX_PEAKS_PER_BLOB	= 30 ; // was 10 ;
	static const int MAX_LATE_MERGE		= 64 ;
	static const int MAX_PADS_PER_RDO	= 1152 ;

	void output(void *data, int count) ;


	int stage_2d(u_int  *outbuff, int max_bytes) ;
	int do_pad_2d(tpx_altro_struct *a, daq_sim_adc_tb *sim_adc=0) ;

	void start_evt_2d(int sec1, int rdo1) {

		sector = sec1 ;
		rdo = rdo1 ;

		data_raw_p = data_raw ;
		
		event++ ;		// for debugging
		cluster_id = 1 ;	// start of cluster ids : for track id Offline stuff

		memset(p_row_pad,0,sizeof(p_row_pad)) ;
	}

	int row ;	// for debugging mostly
	int event ;	// for debugging too
	int cluster_id ;	// for track id Offline stuff

	int do_dump(int ix, u_int *obuff) ;

	int do_print(int row) ;


	int data_raw_shorts ;

	short *data_raw ;	// row, pad, tb_cou, tb_start, adc, adc, adc...
	short *data_raw_p ;	

	short *p_row_pad[100][183] ;
	
	struct blob_seq_t {
		short *s ;

		short pad ;
		short dummy ;	// for alignment
	}  ;

	struct blob_t {
		short cou ;
		short flags ;

		struct blob_seq_t seq[MAX_SEQ_PER_BLOB] ;

	} blobs[MAX_BLOB_COUNT] ;

	struct blob_common_t {
		short p1, p2 ;
		short t1, t2 ;
		short dp, dt ;
		short dt_2 ;
		short flags ;
	} blob_c ;

	struct {
		double f_charge ;
		double f_p_ave ;
		double f_t_ave ;
			
		short p1, p2 ;
		short t1, t2 ;

		short i, j ;
		short flags ;


		short pix_cou ;

		short aux_flags ;
#ifdef DO_SIMULATION
		u_short cluster_id ;
		short quality ;
		u_short track_id ;
#endif
		
	} peaks[MAX_PEAKS_PER_BLOB] ;

	int do_peaks(int peaks_cou) ;
	void do_track_id(int peaks_cou) ;

	short dta[64*1024] ;
	short *dta_s ;	// for the filtered data...

#ifdef DO_SIMULATION
	u_short dta_t[32*1024] ;	// for the track ids
	u_short dta_id[32*1024] ;	// for the cluster ids
#endif
} ;

extern u_int *tpxFCF_2D_scan_to_next(tpxFCF_2D *fcf, u_int *end, u_int *start,tpx_altro_struct *a) ;



#endif
