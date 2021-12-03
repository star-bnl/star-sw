#ifndef _FCS_TRG_BASE_H_
#define _FCS_TRG_BASE_H_

#include <stdint.h>

// serial links stage_1 --> stage_2 --> stage_3
struct link_t {
	uint16_t d[8] ;	// make them 16 bits because we might have 12 bits eventually
} ;

// data links from ADC --> stage_0
struct adc_tick_t {
	uint16_t d[8] ;
} ;
	
struct geom_t {
	uint8_t ns ;
	uint8_t det ;
	uint8_t dep ;
	uint8_t ch ;
} ;

struct ped_gain_t {
	uint16_t ped ;
	uint16_t gain ;
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
	void stage_0(adc_tick_t adc, geom_t geo, ped_gain_t *pg, uint32_t *to_s1) ;
	void stage_0_201900(adc_tick_t adc, geom_t geo, ped_gain_t *pg, uint32_t *to_s1) ;
	void stage_0_202101(adc_tick_t adc, geom_t geo, ped_gain_t *pg, uint32_t *to_s1) ;
	void stage_0_202103(adc_tick_t adc, geom_t geo, ped_gain_t *pg, uint32_t *to_s1) ;

	void stage_1(uint32_t from_s0[], geom_t geo, link_t to_s2[]) ;
	void stage_1_201900(uint32_t from_s0[], geom_t geo, link_t to_s2[]) ;
	void stage_1_202201(uint32_t from_s0[], geom_t geo, link_t to_s2[]) ;

	void stage_2(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_201900(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_202201(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_TAMU_202202(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_202203(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_tonko_202101(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;
	void stage_2_tonko_202104(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[]) ;

	// stage_3 is running in DEP/IO (1 Main) connected to STAR Trigger RAT/DSM
	void stage_3(link_t from_s2[], uint16_t *to_dsm) ;
	void stage_3_201900(link_t from_s2[], uint16_t *to_dsm) ;
	void stage_3_202201(link_t from_s2[], uint16_t *to_dsm) ;
	void stage_3_202203(link_t from_s2[], uint16_t *to_dsm) ;
	void stage_3_tonko_202101(link_t from_s2[], uint16_t *to_dsm) ;

	virtual uint32_t get_version() ;

	void init(const char* fname) ;

	void run_start(uint32_t run) ;

	void start_event() ;

	void fill_event(int det, int ns, int dep, int ch, uint16_t *adc, int t_cou) ;

	int verify_event_io() ;

	uint16_t run_event_sim(int xing, int type) ;	// returns DSM bits

	int verify_event_sim(int xing) ;

	int dump_event_sim(int xing) ;

	int end_event() ;

	int run_stop() ;	// for statistics dumps et...


	uint8_t want_stage_2_io ;	// only if I have full events
	uint8_t want_stage_3_io ;	// only for eother full events or in sector 11

	// cleared at run start; logged at run stop
	struct errors_t {
		uint32_t sim_s1 ;	
		uint32_t sim_s2 ;
		uint32_t sim_s3 ;

		// I/O errors
		uint32_t io_s1_to_s2[4] ;
		uint32_t io_s2_to_s3 ;
		
	} errs, good ;

	// only for S3
	struct statistics_t {
		uint32_t self_trgs ;

		int tcd_marker ;
		int self_trg_marker ;
	} statistics ;

	uint32_t run_number ;	// for logging
	uint32_t evts ;		// for logging

	uint8_t got_one ;	// helper

	// log_level for various printfs
	int log_level ;

	// will change invocation to invocation when running in real-time!
	uint8_t realtime ;	// 1 when in STAR DAQ, 0 when running from DAQ file (default)
	uint8_t sim_mode ;	// 1 when running simulated data (e.g. for Akio et al)

	

//	uint8_t det ;
//	uint8_t ns ;
//	uint8_t dep ;

	uint8_t id ;	// used in realtime to identify the thread


	uint16_t tb_cou[NS_COU][DET_COU][DEP_COU] ;		// filled event by event; used as a "presence" marker too; det,ns,dep


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
			uint16_t dsm_out ;
		} s3 ;

	} d_out ;

	// if there's any output to DSM
	uint32_t dsm_any ;
	int dsm_xing ;

	// statics below
	static uint32_t data_format ;	// 0:pre FY21, 1=FY21


	// Stage versions for this invocation
	static uint32_t stage_version[4] ;



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
	static uint16_t stage_params[4][16] ;	// [stage][param_ix] ;

	// for use by stage_0; loaded in init()
	static ped_gain_t p_g[NS_COU][ADC_DET_COU][DEP_COU][32] ;		


	// for use by stage_1:
	// various thresholds indexed by det
	static uint16_t ht_threshold[ADC_DET_COU] ;


	// for use by stage_2:
	static unsigned long long s2_ch_mask[NS_COU] ;	// up to 34 bits
	static uint8_t s2_ch_phase[NS_COU][34] ;			// phase used to align data

        static int fcs_readPresMaskFromText;
        static uint32_t PRES_MASK[15][9][6];

	// for use by stage_3
	static uint8_t s3_ch_mask ;			// if '1' corresponding input masked
	static uint8_t s3_ch_phase[4] ;			// phasing of the 4 inputs
	static uint8_t s3_out_phase ;			// phasing of the 1 output to Trigger

	// various thresholds
	static uint16_t EM_HERATIO_THR ;
	static uint16_t HAD_HERATIO_THR ;
	static uint16_t EMTHR1 ;
	static uint16_t EMTHR2 ;
	static uint16_t EMTHR3 ;
	static uint16_t HADTHR1 ;
	static uint16_t HADTHR2 ;
	static uint16_t HADTHR3 ;
	static uint16_t JETTHR1 ;
	static uint16_t JETTHR2 ;       
	static uint16_t ETOTTHR ;       
	static uint16_t HTOTTHR ;       
	static uint16_t EHTTHR ;
	static uint16_t HHTTHR ;
	static uint16_t PHTTHR ;

        // Ecal and Hcal 4x4 sums, Ecal+nearest Hcal sum, and Pres(EPD) hit pattern at stage2
	uint32_t e2x2[2][16][10];
	uint32_t h2x2[2][10][6];
        uint32_t esum[2][15][9];
        uint32_t epdcoin[2][15][9];
	uint32_t hsum[2][9][5];
        uint32_t padc[2][6][32];
        uint32_t phit[2][6][32];
        uint32_t sum [2][15][9];
        float ratio[2][15][9];
        uint32_t em[2][15][9];
        uint32_t had[2][15][9];
        uint32_t jet[2][3];
        uint32_t etot[2];
        uint32_t htot[2];    
        uint32_t dsmout;
};

#endif
