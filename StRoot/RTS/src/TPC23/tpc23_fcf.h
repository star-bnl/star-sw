#ifndef _TPC23_FCF_H_
#define _TPC23_FCF_H_

#define RP_ROW_MAX	(45+1)
#define RP_PAD_MAX	(144+1)
#define RP_WC_MAX	512

class tpc23_fcf
{
public:
	tpc23_fcf() ;
	~tpc23_fcf() ;

	//
	int id ;

	// input parameters; i
	int sector1 ;
	int rdo1 ;
	int det ;

	// from the data
	int type ;
	int subtype ;
	int sector ;
	int rdo ;

	u_int *data_start ;
	u_int *data_end ;

	u_short token ;
	u_char trg_cmd ;
	u_char daq_cmd ;

	u_int *s1_store ;
	int s1_words ;

	void init(int iid, int det_type) ;
	void run_start() ;

	int stage1_rdo(void *mem, int bytes) ;
	int stage2_evt() ;

	// statics
	static struct row_pad_s_t {
		u_char gain ;
		u_char flags ;
	} row_pad_s[RP_ROW_MAX][RP_PAD_MAX] ;

private:
	struct row_pad_t {
		u_short cou ;
		u_short d[RP_WC_MAX] ;
	} row_pad[RP_ROW_MAX][RP_PAD_MAX] ;

	int rdo_start_tpx(void *mem, int bytes) ;
	int scan_tpx() ;
	int stage1_row(int row) ;

	u_char flags_row_pad(int asic, int channel, int &row, int &pad) ;
} ;



#endif
