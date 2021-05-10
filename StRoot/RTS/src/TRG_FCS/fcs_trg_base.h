#ifndef _FCS_TRG_BASE_H_
#define _FCS_TRG_BASE_H_

#include <sys/types.h>
typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;

// serial links stage_1 --> stage_2 --> stage_3
struct link_t {
	u_short d[8] ;	// make them 16 bits because we might have 12 bits eventually
} ;

// data links from ADC --> stage_0
struct adc_tick_t {
	u_short d[8] ;
} ;
	
struct geom_t {
	u_char ns ;
	u_char det ;
	u_char dep ;
	u_char ch ;
} ;

struct ped_gain_t {
	u_short ped ;
	u_short gain ;
} ;



class fcs_trg_base {
public:
//	static const int XING_COU = 20 ;	// maximum xings we will look at in simulation and verification
	static const int XING_COU = 40 ;	// maximum xings we will look at in simulation and verification
	static const int DET_COU = 4 ;		// including Trigger DEPs
	static const int ADC_DET_COU = 3 ;	// dets with ADCs (just 3)
	static const int NS_COU = 2 ;
	static const int DEP_COU = 24 ;
	static const int DEP_HCAL_COU = 9 ;
	static const int DEP_ECAL_COU = 24 ;
	static const int DEP_PRE_COU = 6 ;
	static const int DEP_HCAL_TRG_COU = 8 ;
	static const int DEP_ECAL_TRG_COU = 20 ;
	static const int DEP_PRE_TRG_COU = 6 ;

	fcs_trg_base() ;
	virtual ~fcs_trg_base() ;

	// stage_0 and stage_1 are running in DEP/ADC
	void stage_0(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *to_s1) ;
	void stage_0_201900(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *to_s1) ;
	void stage_0_202101(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *to_s1) ;
	void stage_0_202103(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *to_s1) ;

	void stage_1(u_int from_s0[], geom_t geo, link_t to_s2[]) ;
	void stage_1_201900(u_int from_s0[], geom_t geo, link_t to_s2[]) ;
	void stage_1_202201(u_int from_s0[], geom_t geo, link_t to_s2[]) ;

	void stage_2(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_201900(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_202201(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_TAMU_202202(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_202203(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_tonko_202101(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_tonko_202104(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;

	// stage_3 is running in DEP/IO (1 Main) connected to STAR Trigger RAT/DSM
	void stage_3(link_t from_s2[], u_short *to_dsm) ;
	void stage_3_201900(link_t from_s2[], u_short *to_dsm) ;
	void stage_3_202201(link_t from_s2[], u_short *to_dsm) ;
	void stage_3_202203(link_t from_s2[], u_short *to_dsm) ;
	void stage_3_tonko_202101(link_t from_s2[], u_short *to_dsm) ;

	virtual u_int get_version() ;

	void init(const char* fname) ;

	void run_start(u_int run) ;

	void start_event() ;

	void fill_event(int det, int ns, int dep, int ch, u_short *adc, int t_cou) ;

	int verify_event_io() ;

	u_short run_event_sim(int xing, int type) ;	// returns DSM bits

	int verify_event_sim(int xing) ;

	int dump_event_sim(int xing) ;

	int end_event() ;

	int run_stop() ;	// for statistics dumps et...


	u_char want_stage_2_io ;	// only if I have full events
	u_char want_stage_3_io ;	// only for eother full events or in sector 11

	// cleared at run start; logged at run stop
	struct errors_t {
		u_int sim_s1 ;	
		u_int sim_s2 ;
		u_int sim_s3 ;

		// I/O errors
		u_int io_s1_to_s2[4] ;
		u_int io_s2_to_s3 ;
		
	} errs, good ;

	// only for S3
	struct statistics_t {
		u_int self_trgs ;

		int tcd_marker ;
		int self_trg_marker ;
	} statistics ;

	u_int run_number ;	// for logging
	u_int evts ;		// for logging

	u_char got_one ;	// helper

	// log_level for various printfs
	int log_level ;

	// will change invocation to invocation when running in real-time!
	u_char realtime ;	// 1 when in STAR DAQ, 0 when running from DAQ file (default)
	u_char sim_mode ;	// 1 when running simulated data (e.g. for Akio et al)

	

//	u_char det ;
//	u_char ns ;
//	u_char dep ;

	u_char id ;	// used in realtime to identify the thread


	u_short tb_cou[NS_COU][DET_COU][DEP_COU] ;		// filled event by event; used as a "presence" marker too; det,ns,dep


	// data from DAQ file; used for replay/bitchecking or full simulation
	struct in_t {
		// DEP/ADC data for each DEP/ADC
		struct {
			// ADC data 
			adc_tick_t adc[32] ;

			// stage_1 data sent by the DEP/ADC (ns,det,dep) 
			link_t s1_to_s2 ;		
		} s1[NS_COU][ADC_DET_COU][DEP_COU] ;


		// Stage_2 DEP data for each of the 2 DEP/Stage_2
		struct {
			// stage_2 data received by the stage_2 DEP/IO from a stage_1 DEP/ADC
			link_t s2_from_s1[34] ;		// stage_1 data _received_ by stage_2; ns,ch

			// stage_2 data sent by DEP/ADC in stage_2 (ns,ch)
			link_t s2_to_s3[2] ;			// stage_2 data _sent_ to stage_3; ns,ch
		} s2[NS_COU] ;


		// Stage_3 DEP data, just 1 
		struct {
			// stage_2 data received by stage_3 DEP (ch)
			link_t s3_from_s2[NS_COU*2] ;		// stage_3 data _received_ from stage_2

			link_t dsm_out ;			// DSM output sent
		} s3 ;

	} d_in[XING_COU] ;


	// Outputs from local algorithm processing in C
	struct sim_t {
		// output from stage_1; emulation
		struct {
			link_t s1_to_s2 ;
			
		} s1[NS_COU][ADC_DET_COU][DEP_COU] ;

		//output from stage_2 emulation ;
		struct {
			link_t s2_to_s3[2] ;
		} s2[NS_COU] ;

		// output from stage_3 emulation
		struct {
			u_short dsm_out ;
		} s3 ;

	} d_out ;

	// if there's any output to DSM
	u_int dsm_any ;
	int dsm_xing ;

	// statics below
	static u_int data_format ;	// 0:pre FY21, 1=FY21


	// Stage versions for this invocation
	static u_int stage_version[4] ;



	// event markers; defaults set in init(), normally
	static struct marker_t {
		int last_xing ;	// last valid xing

		// used in DEP/ADC
		int adc_start ;
		int s1_out_start ;

		// used in stage_2 DEP/Trg
		int s2_in_start ;
		int s2_to_s3_start ;


		// used in stage_3 DEP/Trg
		int s3_in_start ;
		int dsm_out_start ;

	} marker ;

	static int fcs_trgDebug ;

	// per event 
	int event_bad ;

	// stage_x algo params (same as in firmware)
	static u_short stage_params[4][16] ;	// [stage][param_ix] ;

	// for use by stage_0; loaded in init()
	static ped_gain_t p_g[NS_COU][ADC_DET_COU][DEP_COU][32] ;		


	// for use by stage_1:
	// various thresholds indexed by det
	static u_short ht_threshold[ADC_DET_COU] ;


	// for use by stage_2:
	static unsigned long long s2_ch_mask[NS_COU] ;	// up to 34 bits
	static u_char s2_ch_phase[NS_COU][34] ;			// phase used to align data

        static int fcs_readPresMaskFromText;
        static u_int PRES_MASK[15][9][6];

	// for use by stage_3
	static u_char s3_ch_mask ;			// if '1' corresponding input masked
	static u_char s3_ch_phase[4] ;			// phasing of the 4 inputs
	static u_char s3_out_phase ;			// phasing of the 1 output to Trigger

	// various thresholds
	static u_short EM_HERATIO_THR ;
	static u_short HAD_HERATIO_THR ;
	static u_short EMTHR1 ;
	static u_short EMTHR2 ;
	static u_short EMTHR3 ;
	static u_short HADTHR1 ;
	static u_short HADTHR2 ;
	static u_short HADTHR3 ;
	static u_short JETTHR1 ;
	static u_short JETTHR2 ;       
	static u_short ETOTTHR ;       
	static u_short HTOTTHR ;       
	static u_short EHTTHR ;
	static u_short HHTTHR ;
	static u_short PHTTHR ;

	// various stuff...


        // Ecal and Hcal 4x4 sums, Ecal+nearest Hcal sum, and Pres(EPD) hit pattern at stage2
        u_int esum[2][15][9];
        u_int epdcoin[2][15][9];
        u_int hsum[2][9][5];
        u_int padc[2][6][32];
        u_int phit[2][6][32];
        u_int sum [2][15][9];
        float ratio[2][15][9];
        u_int jet[2][3];
        u_int etot[2];
        u_int htot[2];    
        u_int dsmout;
};

#endif
