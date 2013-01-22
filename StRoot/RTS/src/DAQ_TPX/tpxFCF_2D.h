#ifndef _TPX_FCF_2D_H_
#define _TPX_FCF_2D_H_


#include <stdlib.h>
//#include <string.h>

#include <DAQ_TPX/tpxFCF.h>






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
	static const int MAX_SEQ_PER_BLOB	= 128 ; // was 64 ;
	static const int MAX_PEAKS_PER_BLOB	= 30 ; // was 10 ;
	static const int MAX_LATE_MERGE		= 64 ;
	static const int MAX_PADS_PER_RDO	= 1152 ;

	void output(void *data, int count) ;


	int stage_2d(u_int  *outbuff, int max_bytes) ;
	int do_pad_2d(tpx_altro_struct *a, daq_sim_adc_tb *sim_adc) ;

	void start_evt_2d(int sec1, int rdo1) {
//		LOG(TERR,"start_evt_2d: %d %d",sec1,rdo1) ;
		sector = sec1 ;
		rdo = rdo1 ;

		data_raw_cou = 0 ;
		data_raw_p = data_raw ;

		memset(p_row_pad,0,sizeof(p_row_pad)) ;
	}

	int do_dump(int ix, u_int *obuff) ;

	int do_print(int row) ;


	u_int data_raw_cou ;		// zapped to 0 on event start...
	short *data_raw ;	// row, pad, tb_cou, tb_start, adc, adc, adc...
	short *data_raw_p ;	

	short *p_row_pad[46][183] ;
	
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

	struct {
		double f_charge ;
		double f_p_ave ;
		double f_t_ave ;
			
		short p1, p2 ;
		short t1, t2 ;
		short i, j ;

		short flags ;
		short pix_cou ;
	} peaks[MAX_PEAKS_PER_BLOB] ;

	int do_peaks(int row, int peaks_cou) ;

	short dta[64*1024] ;
	short *dta_s ;	// for the filtered data...

} ;

extern u_int *tpxFCF_2D_scan_to_next(tpxFCF_2D *fcf, u_int *end, u_int *start,tpx_altro_struct *a) ;



#endif
