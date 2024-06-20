#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <DAQ_FCS/fcs_data_c.h>

#include "fcs_trg_base.h"

// statics
fcs_trg_base::marker_t fcs_trg_base::marker ;

u_int fcs_trg_base::stage_version[4] ;

u_short fcs_trg_base::stage_params[4][32] ;

ped_gain_t fcs_trg_base::p_g[NS_COU][ADC_DET_COU][DEP_COU][32] ;
u_short fcs_trg_base::ht_threshold[ADC_DET_COU] ;


unsigned long long fcs_trg_base::s2_ch_mask[NS_COU] ;
u_char fcs_trg_base::s2_ch_phase[NS_COU][34] ;

u_char fcs_trg_base::s3_ch_mask ;
u_char fcs_trg_base::s3_ch_phase[4] ;
u_char fcs_trg_base::s3_out_phase ;

int fcs_trg_base::fcs_trgDebug ;
int fcs_trg_base::fcs_readPresMaskFromText;
u_int fcs_trg_base::PRES_MASK[15][9][6];

u_short        fcs_trg_base::EM_HERATIO_THR ;
u_short        fcs_trg_base::HAD_HERATIO_THR ;
u_short        fcs_trg_base::EMTHR0 ;
u_short        fcs_trg_base::EMTHR1 ;
u_short        fcs_trg_base::EMTHR2 ;
u_short        fcs_trg_base::EMTHR3 ;  //obsolete for 202207 
u_short        fcs_trg_base::ELETHR0 ;
u_short        fcs_trg_base::ELETHR1 ;
u_short        fcs_trg_base::ELETHR2 ;
u_short        fcs_trg_base::HADTHR0 ;
u_short        fcs_trg_base::HADTHR1 ;
u_short        fcs_trg_base::HADTHR2 ;
u_short        fcs_trg_base::HADTHR3 ; //obsolete for 202207 
u_short        fcs_trg_base::JETTHR1 ; //obsolete for 202207 
u_short        fcs_trg_base::JETTHR2 ; //obsolete for 202207 
u_short        fcs_trg_base::JPATHR2 ;      
u_short        fcs_trg_base::JPATHR1 ;      
u_short        fcs_trg_base::JPATHR0 ;      
u_short        fcs_trg_base::JPBCTHR2 ;      
u_short        fcs_trg_base::JPBCTHR1 ;      
u_short        fcs_trg_base::JPBCTHR0 ;      
u_short        fcs_trg_base::JPBCTHRD ;      
u_short        fcs_trg_base::JPDETHR2 ;      
u_short        fcs_trg_base::JPDETHR1 ;      
u_short        fcs_trg_base::JPDETHR0 ;      
u_short        fcs_trg_base::JPDETHRD ;      
u_short        fcs_trg_base::ETOTTHR ;
u_short        fcs_trg_base::HTOTTHR ;
u_short        fcs_trg_base::EHTTHR ;
u_short        fcs_trg_base::HHTTHR ;
u_short        fcs_trg_base::PHTTHR ;

u_int fcs_trg_base::data_format ;

int fcs_trg_base::run_type ;


fcs_trg_base::fcs_trg_base()
{
//	LOG(TERR,"%s (sizeof class %u)",__PRETTY_FUNCTION__,sizeof(*this)) ;

	log_level = 0 ;
	realtime = 0 ;
	id = 0 ;
	sim_mode = 0 ;
	data_format = 1 ;

	run_type = 3 ;	// assume normal

	trg_xing = 5 ;

	want_stage_2_io = 0 ;
	want_stage_3_io = 0 ;
	want_stage_1_sim = 1 ;
}


fcs_trg_base::~fcs_trg_base()
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;
}

u_int fcs_trg_base::get_version()
{
	return 0x21000000 ;	//YY,MM,DD,HH in BCD;	0 for base class
} ;


void fcs_trg_base::init(const char* fname)
{
	if(id != 0) return ;	// just one guy


	// zap input params 

	memset(stage_params,0,sizeof(stage_params)) ;
	memset(p_g,0,sizeof(p_g)) ;

	memset(s2_ch_phase,0,sizeof(s2_ch_phase)) ;
	memset(s2_ch_mask,0,sizeof(s2_ch_mask)) ;

	memset(s3_ch_phase,0,sizeof(s3_ch_phase)) ;
	s3_ch_mask = 0 ;
	s3_out_phase = 0 ;

	// and then set all gains to 1
	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<ADC_DET_COU;j++) {
	for(int k=0;k<DEP_COU;k++) {
	for(int c=0;c<32;c++) {
		p_g[i][j][k][c].gain = (1<<8) ;		// new for FY22

	}}}}


	// thresholds to defaults
	ht_threshold[0] = 90 ;	// ecal
	ht_threshold[1] = 90 ;	// hcal
	ht_threshold[2] = 45 ;	// fpRE in FY19

	// since I can use fcs_trg_base in realtime, all the stuff below is already loaded
	//	if(!realtime && !sim_mode) {
	if(!realtime) {
		LOG(INFO,"init: not realtime -- loading stuff from files") ;

		memset(fcs_data_c::ped,0,sizeof(fcs_data_c::ped)) ;

		char rdomap[128]; 
		sprintf(rdomap,"%s/fcs_daq_map.txt",fname);
		fcs_data_c::load_rdo_map(rdomap) ;
		fcs_data_c::gain_from_cache(fname) ;

		if(!sim_mode){  //no need for pedestal in sim_mode
		    for(int s=1;s<=1;s++) {
			for(int r=1;r<=6;r++) {
			    char fname[256] ;
			    sprintf(fname,"/net/fcs%02d/RTScache/fcs_pedestals_s%02d_r%d.txt",s,s,r) ;			    
			    fcs_data_c::ped_from_cache(fname) ;
			}
		    }
		}


	}
	else if(!sim_mode) {
//		LOG(TERR,"init:realtime: ht_threshold is %d",fcs_data_c::ht_threshold) ;

		for(int i=0;i<3;i++) {	// known in realtime
			ht_threshold[i] = fcs_data_c::ht_threshold ;
		}
	}

	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<ADC_DET_COU;j++) {
	for(int k=0;k<DEP_COU;k++) {

		int sec = fcs_data_c::det_map[j][i][k].sector - 1 ;
		int rdo = fcs_data_c::det_map[j][i][k].rdo - 1 ;

		if(sec<0 || rdo<0) continue ;	// protection against bad load

		for(int c=0;c<32;c++) {
			u_short p = fcs_data_c::ped[sec][rdo].i_ped[c] ;
			u_short g = fcs_data_c::ped[sec][rdo].i_gain[c] ;

			if(p && log_level>5 && c==0) {	// just a sample, to check sanity
				LOG(TERR,"S%d:%d: %d:%d:%d: ch %d = i_ped %d, i_gain %d",sec+1,rdo+1,j,i,k,c,p,g) ;
			}
			
			//u_int mask = fcs_data_c::rdo_map[sec][rdo].ch_mask & 0xFFFFFFFFll ;
		
		
			p_g[i][j][k][c].gain = fcs_data_c::ped[sec][rdo].i_gain[c] ;
			p_g[i][j][k][c].ped = fcs_data_c::ped[sec][rdo].i_ped[c] ;
		}
	}}}


	// BELOW depends on the length of processing so don't change!
	// Set by Tonko

	// concrete values at e.g. 09-Dec-2021
	marker.last_xing = 7 ;	// this depends on the post setting and should be calculable

	marker.adc_start = 7 ;				// fixed during board configuration
	marker.s1_out_start = marker.adc_start + 11 ;	// depends on stage_0/1 algo but pretty much fixed

	marker.s2_in_start = marker.s1_out_start + 2 ;		// depends on the cable length from 1-to-2 put pretty much fixed
	marker.s2_to_s3_start = marker.s2_in_start + 15 ;	// depends on stage_2 algo?


	marker.s3_in_start = marker.s2_to_s3_start + 8 ;	// depends on cable length from 2-to-3; 0 in FY19
	marker.dsm_out_start = marker.s3_in_start + 14 ;	// depends on stage_3 algo


	if(log_level>0) LOG(INFO,"init markers: last xing %d, ADC %d, s1_out %d, s2_in %d, s2_to_s3 %d, s3_in %d, dsm_out %d",
	       marker.last_xing,
	       marker.adc_start,
	       marker.s1_out_start,
	       marker.s2_in_start,
	       marker.s2_to_s3_start,
	       marker.s3_in_start,
	       marker.dsm_out_start) ;


	// for Akio's code
	fcs_trgDebug = 0 ;
	fcs_readPresMaskFromText=0;

	// stage2 params (defaults are from Akio's code)
	EM_HERATIO_THR = 32 ;  // or 128*(1/4)
	HAD_HERATIO_THR = 32 ;

	EMTHR3 = 128 ; //obsolete for 202207 
	HADTHR3 = 169 ; //obsolete for 202207   
	JETTHR2  = 128 ; //obsolete for 202207   
	JETTHR1  =  64 ; //obsolete for 202207   

	//original default
	EMTHR2 = 192 ;
	EMTHR1 = 128 ;
	EMTHR0 = 64 ; 
	
	ELETHR2 = 32 ;
	ELETHR1 = 22 ;
	ELETHR0 = 25 ; 
	
	HADTHR2 = 192 ;
	HADTHR1 = 128 ;
	HADTHR0 =  64 ; 
	
	JPATHR2  = 254 ;      
	JPATHR1  = 192 ;      
	JPATHR0  = 128 ;      
	JPBCTHR2 = 254 ;      
	JPBCTHR1 = 192 ;      
	JPBCTHR0 = 128 ;      
	JPBCTHRD = 160 ;       
	JPDETHR2 = 254 ;      
	JPDETHR1 = 192 ;      
	JPDETHR0 = 128 ;      
	JPDETHRD = 160 ;      

	ETOTTHR = 32 ;
	HTOTTHR = 32 ;
	EHTTHR = 50 ;
	HHTTHR = 50 ;
	PHTTHR = 100 ; //stage1

	if(sim_mode){
	    ht_threshold[0]=EHTTHR;
	    ht_threshold[1]=HHTTHR;
	    ht_threshold[2]=PHTTHR;
	}	 

	// IMPORTANT: Requested Stage_x versions defaults
	// Either set by the user to her/his wishes or picked up from the DAQ file
	stage_version[0] = 2 ;
	stage_version[1] = 1 ;
	stage_version[2] = 7 ;
	stage_version[3] = 7 ;

	// DEP/Trigger masks
	//s3_ch_mask = (1<<2) ;	        // South 0 
	//s2_ch_mask[0] = 0 ;		// Nothing in North
	//s2_ch_mask[1] = (1<<3) ;	// just 1 fPRE channel in South for early FY19
}


void fcs_trg_base::run_start(u_int run)
{
	run_number = run ;
	evts = 0 ;

	memset(&errs,0,sizeof(errs)) ;
	memset(&good,0,sizeof(good)) ;
	memset(&statistics,0,sizeof(statistics)) ;

	// cleanup, just in case
	memset(d_in,0,sizeof(d_in)); 
	memset(&d_out,0,sizeof(d_out)); 

	start_event() ;	// just in case

	if(sim_mode) {
		marker.adc_start = 0 ;
		marker.last_xing = 1 ;
	}

	LOG(INFO,"%d: starting run %08u, realtime %d, sim_mode %d",id,run_number,realtime,sim_mode) ;

	return ;
}


void fcs_trg_base::start_event() 
{
	got_one = 0 ;		

	memset(tb_cou,0,sizeof(tb_cou)) ;

	if(sim_mode) {	// only for simulated data
		memset(d_in,0,sizeof(d_in)); //akio?
	}
} ;


void fcs_trg_base::fill_event(int det, int ns, int dep, int c, u_short *d16, int t_cou) 
{

	if(t_cou) {
		if(got_one==0) {
			evts++ ;
			got_one = 1 ;
		}
	}
	else return ;

	//printf("... tb_cou %d %d %d: %d\n",ns,det,dep,t_cou) ;

	tb_cou[ns][det][dep] = t_cou ;

	// various markers
	int is_self = -1 ;
	int is_tcd = -1 ;

	for(int t=0;t<t_cou;t++) {
		int dta = d16[t] & 0xFFF ;
		int fla = d16[t] >> 12 ;
				
		int xing ;
		int xou ;
		int tix ;

		if(log_level>100) printf("%d:%d:%d ch %d: tb %d: ADC %d, fla %d\n",
		       ns,det,dep,c,t,dta,fla) ;

		if(det != 3) {	// DEP/ADC
			if(c==32) {	// stage_1 out data
				tix = t - marker.s1_out_start ;
				xing = tix/8 ;
				xou = tix%8 ;

				if(dta && log_level>10) {
					printf("s1 out: %d:%d:%d -- at xing %d:%d(%d) = %d\n",ns,det,dep,xing,xou,t,dta) ;
				}

				if(tix>=0 && xing<XING_COU) {
					// data stage_1 out
					d_in[xing].s1[ns][det][dep].s1_to_s2.d[xou] = dta ;
				}
			}
			else {		// raw ADC data
				tix = t - marker.adc_start ;
				xing = tix/8 ;
				xou = tix%8 ;

				if(tix>=0 && xing<XING_COU) {
					// actual ADC data
					d_in[xing].s1[ns][det][dep].adc[c].d[xou] = dta ;
				}

				switch(data_format) {
				case 0 :	// some old shit -- ignore
					if(c==0 && (fla & 0x1)) {
					

						if(is_tcd < 0) is_tcd = t ;

						if(log_level > 101) {
							printf("ADC tcd_marker -- at xing  %d:%d(%d)\n",xing,xou,t) ;
						}
					
					}

					if(c==0 && (fla & 0x2)) {
					
						if(is_self < 0) is_self = t ;

						if(log_level>101) {
							printf("ADC self_trg -- at xing  %d:%d(%d)\n",xing,xou,t) ;
						}
					}

					break ;
				case 1 :	//FY21

					if(c==3 && (fla&4)) {

						if(is_tcd < 0) is_tcd = t ;

						if(log_level > 101) {
							printf("ADC tcd_marker -- at xing  %d:%d(%d)\n",xing,xou,t) ;
						}
					
					}

					if(c==1 && (fla&4)) {
						if(is_self < 0) is_self = t ;

						if(log_level>101) {
							printf("ADC self_trg -- at xing  %d:%d(%d)\n",xing,xou,t) ;
						}
					}


					break ;
				}

			}
		}
		else if(dep==0) {	// FY21 stage 3
			switch(c) {
			case 4 :	// dsm out lo 8 bits
				tix = t - marker.dsm_out_start ;

				xing = tix/8 ;
				xou = tix%8 ;

				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s3.dsm_out.d[xou] = dta & 0xFF ;	// link
				}
				break ;
			case 5 :	// DSM out hi 8 bits
				tix = t - marker.dsm_out_start ;

				xing = tix/8 ;
				xou = tix%8 ;


				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s3.dsm_out.d[xou] |= (dta & 0xFF)<<8 ;
				}
				break ;
			default :	
				tix = t - marker.s3_in_start ;

				xing = tix/8 ;
				xou = tix%8 ;

				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s3.s3_from_s2[c].d[xou] = dta & 0xFF ;
				}
				

				break ;
			}
		}
		else {			// Stage 2
			switch(c) {
			case 34 :	// s2_to_s3
			case 35 :
				tix = t - marker.s2_to_s3_start ;

				xing = tix/8 ;
				xou = tix%8 ;

				if(tix>=0 && xing<XING_COU) {
					// I had to remap this because the stage3 input is _swapped_
					// S6:34 is S5:1
					// S6:35 is S5:0
					// S7:34 is S5:3
					// S7:35 is S5:2
					//int cc = (c==34?1:0) ;
					//d_in[xing].s2[ns].s2_to_s3[cc].d[xou] = dta & 0xFF ;

					d_in[xing].s2[ns].s2_to_s3[c-34].d[xou] = dta & 0xFF ;
				}
				break ;
			case 36 :	// new in FY22
				//tix = t - (marker.s2_to_s3_start - 1) ;	// adjusted by hand!
				tix = t - marker.dsm_out_start ;		// same as S3 to DSM!

				xing = tix/8 ;
				xou = tix%8 ;

				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s2[ns].s2_to_dsm.d[xou] = dta & 0xFF ;
				}
				break ;
				
				break ;	// for now
			default :
				tix = t - marker.s2_in_start ;	
				
				xing = tix/8 ;
				xou = tix%8 ;
				
				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s2[ns].s2_from_s1[c].d[xou] = dta & 0xFF ;
				}

				break ;
			}
		}
	}

	// this generally is nonsense because we have many boards...
	if(is_self>0) {
		statistics.self_trgs++ ;
		statistics.self_trg_marker = is_self ;
	}
	
	if(is_tcd > 0) {
		statistics.tcd_marker = is_tcd ;
	}

} ;

int fcs_trg_base::end_event()
{
	event_bad = 0 ;

	s2_io_ns_bad = 0 ;
	s2_io_ch_bad = 0 ;

	s3_io_ch_bad = 0 ;

	s1_dep_bad = 0 ;
	s1_det_bad = 0 ;
	s1_ns_bad = 0 ;

	s2_ns_bad = 0 ;
	s2_ch_bad = 0 ;

	s3_to_dsm = s2_to_dsm[0] = s2_to_dsm[1] = 0 ;

	if(!got_one) return 0 ;	// nothing to do; let's not waste time

	verify_event_io() ;	// verify interconnectivity 

	int dsmout = 0;

	self_trigger = 0 ;

	s3_to_dsm = d_in[trg_xing].s3.dsm_out.d[0] ;
	s2_to_dsm[0] = d_in[trg_xing].s2[0].s2_to_dsm.d[0] ;
	s2_to_dsm[1] = d_in[trg_xing].s2[1].s2_to_dsm.d[0] ;
			  
	for(int xing=0;xing<marker.last_xing;xing++) {
    		if(log_level>1) {
			LOG(NOTE,"run_event_sim: xing %d",xing) ;
		}

		dsmout = run_event_sim(xing,sim_mode) ;		

		if(sim_mode) {	// when running offline
			dump_event_sim(xing) ;
		}
		else {
			verify_event_sim(xing) ;
		}
	}

	return dsmout;
}

int fcs_trg_base::run_stop()
{
	int err = 0 ;

	for(int i=0;i<4;i++) {
		if(errs.io_s1_to_s2[i]) err |= 1 ;
	}

	if(errs.io_s2_to_s3) err |= 1 ;

	if(errs.sim_s1 || errs.sim_s2 || errs.sim_s3) {
		err |= 2 ;
	}

	LOG(INFO,"thread %d: self_trg_marker %d, tcd_marker %d",id,statistics.self_trg_marker,statistics.tcd_marker) ;

	if(err) {
		if(run_type==3 || (err&1)) {	// normal run or IO
			LOG(ERR,"thread %d: %d/%d events in run %d: errs sim %u %u %u; io [%u %u %u %u] %u",id,
			    statistics.self_trgs,
			    evts,run_number,
			    errs.sim_s1,
			    errs.sim_s2,
			    errs.sim_s3,
			    errs.io_s1_to_s2[0],errs.io_s1_to_s2[1],errs.io_s1_to_s2[2],errs.io_s1_to_s2[3],
			    errs.io_s2_to_s3) ;
		}
		else {
			LOG(WARN,"thread %d: %d/%d events in run %d: errs sim %u %u %u; io [%u %u %u %u] %u",id,
			    statistics.self_trgs,
			    evts,run_number,
			    errs.sim_s1,
			    errs.sim_s2,
			    errs.sim_s3,
			    errs.io_s1_to_s2[0],errs.io_s1_to_s2[1],errs.io_s1_to_s2[2],errs.io_s1_to_s2[3],
			    errs.io_s2_to_s3) ;

		}

	}

	LOG(INFO,"thread %d: %d/%d events in run %d: good sim %u %u %u; io [%u %u %u %u] %u",id,
	    statistics.self_trgs,
	    evts,run_number,
	    good.sim_s1,
	    good.sim_s2,
	    good.sim_s3,
	    good.io_s1_to_s2[0],good.io_s1_to_s2[1],good.io_s1_to_s2[2],good.io_s1_to_s2[3],
	    good.io_s2_to_s3) ;


	return 0 ;
}

// verify the IO parts of the event, various links
int fcs_trg_base::verify_event_io() 
{
	int printed = 0 ;
	int bad = 0 ;

	// Stage 1 to Stage 2 DEPs: but ONLY if I have the whole event!!!
	if(!want_stage_2_io) goto stage_3 ;


	for(int x=0;x<marker.last_xing;x++) {
		for(int t=0;t<8;t++) {
			for(int ns=0;ns<2;ns++) {
				int s2_from_s1[34] ;
				int s1_to_s2[34] ;
				int ix = 0 ;

				long mask = 0 ;
				
				int cns ;

				if(ns==0) cns='N' ;
				else cns = 'S' ;

				//LOG(TERR,"xing %d, t %d, ns %d = %d",x,t,ns,tb_cou[ns][3][1]) ;

				//watch it!
				//if(tb_cou[ns][3][1]==0) continue ;	// no stage_2

				for(int i=0;i<34;i++) {
					s2_from_s1[i] = d_in[x].s2[ns].s2_from_s1[i].d[t] ;
				}


				// ECAL
				//LOG(TERR," %d %d %d %d",
				//    tb_cou[ns][0][0],
				//    tb_cou[ns][0][0],
				//    tb_cou[ns][0][0],
				//    tb_cou[ns][0][0]) ;

				for(int d=0;d<20;d++) {
					if(tb_cou[ns][0][d]) mask |= (1ll<<ix) ;
					s1_to_s2[ix] = d_in[x].s1[ns][0][d].s1_to_s2.d[t] ;
					ix++ ;
				}

				// HCAL
				for(int d=0;d<8;d++) {
					if(tb_cou[ns][1][d]) mask |= (1ll<<ix) ;
					s1_to_s2[ix] = d_in[x].s1[ns][1][d].s1_to_s2.d[t] ;
					ix++ ;
				}

				// FPRE
				for(int d=0;d<6;d++) {
					if(tb_cou[ns][2][d]) mask |= (1ll<<ix) ;
					s1_to_s2[ix] = d_in[x].s1[ns][2][d].s1_to_s2.d[t] ;
					ix++ ;
				}

			
				

				if(mask != 0 && log_level>1) LOG(WARN,"xing %d:%d, ns %d: mask 0x%llX, ix %d",x,t,ns,mask,ix) ;

				for(int i=0;i<34;i++) {
					// watch it!
					//if(mask & (1ll<<i)) ;
					//else continue ;

					// separate mask
					if(s2_ch_mask[ns] & (1ll<<i)) continue ;

					if(s2_from_s1[i] || s1_to_s2[i]) {
						//printf("... xing %d:%d, ns %d: %d: s2_in 0x%02X, s1_out 0x%02X\n",x,t,ns,i,s2_from_s1[i],s1_to_s2[i]) ;
					}

					if(s2_from_s1[i] != s1_to_s2[i]) {
						event_bad |= 0x10 ;

						s2_io_ns_bad = ns ;
						s2_io_ch_bad = i ;

						if(log_level>0) LOG(ERR,"evt %d: S1_to_S2 IO: NS %c: ch %d: xing %d:%d: out 0x%02X, in 0x%02X",
						    evts,cns,i,x,t,s1_to_s2[i],s2_from_s1[i]) ;

						if(ns==0 && i<17) errs.io_s1_to_s2[0]++ ;
						else if(ns==0) errs.io_s1_to_s2[1]++ ;
						else if(ns==1 && i<17) errs.io_s1_to_s2[2]++ ;
						else errs.io_s1_to_s2[3]++ ;

					}
					else {
						if(ns==0 && i<17) good.io_s1_to_s2[0]++ ;
						else if(ns==0) good.io_s1_to_s2[1]++ ;
						else if(ns==1 && i<17) good.io_s1_to_s2[2]++ ;
						else good.io_s1_to_s2[3]++ ;

					}
				}
				
			}
		}
	}

	stage_3:

	if(!want_stage_3_io) goto end ;

	// Stage 2 to Stage 3: but only if I have Stage_2 and Stage_3 DEPs in the data
	for(int x=0;x<marker.last_xing;x++) {

		if(tb_cou[0][3][0]==0) continue ;	// no stage_3 at all
		
		for(int c=0;c<4;c++) {
			int cc = c%2 ;

			if(tb_cou[c/2][3][1]==0) continue ;
			int err = 0 ;

#if 0			// need to remap
			switch(c) {
			case 0 :
			case 2 :
				cc = 1 ;
				break ;
			case 1 :
			case 3 :
				cc = 0 ;
				break ;
			}
#endif
			for(int t=0;t<8;t++) {
				int s3_from_s2 = d_in[x].s3.s3_from_s2[c].d[t] ;
				int s2_to_s3 = d_in[x].s2[c/2].s2_to_s3[cc].d[t] ;

				if(s2_to_s3 != s3_from_s2) {
					event_bad |= 0x20 ;

					s3_io_ch_bad = c ;

					errs.io_s2_to_s3++ ;
					err = 1 ;
				}
				else {
					good.io_s2_to_s3++ ;
				}
			}

			if(err==1 && log_level>0) {
				for(int t=0;t<8;t++) {
					char ctmp ;

					int s3_from_s2 = d_in[x].s3.s3_from_s2[c].d[t] ;
					int s2_to_s3 = d_in[x].s2[c/2].s2_to_s3[cc].d[t] ;

					if(s2_to_s3 != s3_from_s2) ctmp = '*' ;
					else ctmp = ' ' ;

					LOG(ERR,"evt %d: S3 IO: ch %d: xing %d:%d: in S3 0x%02X, out of S2 0x%02X%c",
					    evts,c,x,t,
					    s3_from_s2,s2_to_s3,ctmp) ;
				}

			}
		}
	}

	end:;
		

	if(printed) fflush(stdout) ;



	return bad ;
}

int fcs_trg_base::dump_event_sim(int xing)
{
	// verify stage_1 data

	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<ADC_DET_COU;j++) {
	for(int k=0;k<DEP_COU;k++) {

		//if(tb_cou[i][j][k]==0) continue ;	// not in the daat...
		
		for(int t=0;t<8;t++) {
			int d_sim = d_out.s1[i][j][k].s1_to_s2.d[t] ;

			if(d_sim && fcs_trgDebug>0) 
			  printf("S1 sim: %d:%d:%d - xing %d:%d, dta %d\n",
				 i,j,k,xing,t,d_sim) ;
		}
	}
	}
	}


	// verify stage_2 data locally to stage_2 DEP
	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<2;j++) {


		for(int t=0;t<8;t++) {
			int d_sim = d_out.s2[i].s2_to_s3[j].d[t] ;

			if(fcs_trgDebug>0)
			  printf("S2 sim: %d:%d - xing %d:%d, dta 0x%03X\n",
				 i,j,xing,t,d_sim) ;
		}
	}
	}

	//printf("S3 sim: to DSM 0x%04X\n",d_out.s3.dsm_out) ;

	return 0 ;
}


int fcs_trg_base::verify_event_sim(int xing) 
{
	int bad = 0 ;
	
	// class vars
	
	// verify stage_1 data

	int s1_failed = 0 ;
	int s2_failed = 0 ;
	int s3_failed = 0 ;

	if(!want_stage_1_sim) goto skip_stage1 ;
	
	for(int i=0;i<NS_COU;i++) {
	int cns ;

	if(i==0) cns='N' ;
	else cns='S' ;

	for(int j=0;j<ADC_DET_COU;j++) {
	int cdet ;

	switch(j) {
	case 0 :
		cdet='E' ;
		break ;
	case 1 :
		cdet='H' ;
		break ;
	case 2 :
		cdet='F' ;
		break ;
	}

	for(int k=0;k<DEP_COU;k++) {
		int want_print = 0 ;
		int want_log = 0 ;

		if(tb_cou[i][j][k]==0) continue ;	// not in the daat...


		for(int t=0;t<8;t++) {
			int d_sim = d_out.s1[i][j][k].s1_to_s2.d[t] ;
			int d_i = d_in[xing].s1[i][j][k].s1_to_s2.d[t] ;

			if(d_sim != d_i) {
				s1_failed = 1 ;
				errs.sim_s1++ ;

				s1_dep_bad = k ;
				s1_det_bad = j ;
				s1_ns_bad = i ;

				want_log = 1 ;
				bad++ ;
			}
			else {
				good.sim_s1++ ;
			}

			if(d_i || d_sim) {
				want_print = 1 ;
			}

		}

		
		for(int t=0;t<8;t++) {
			int d_sim = d_out.s1[i][j][k].s1_to_s2.d[t] ;
			int d_i = d_in[xing].s1[i][j][k].s1_to_s2.d[t] ;

			if(want_log && log_level>0) {
				LOG(ERR,"evt %d: S1 sim: %c:%c:%d - xing %d:%d: sim 0x%02X, dta 0x%02X %c",evts,cns,cdet,k,
				    xing,t,
				    d_sim,
				    d_i,d_sim!=d_i?'*':' ') ;
			}

			if(want_print && log_level>3) {
				printf("S1: %d:%d:%d - xing %d:%d: s1 sim %d, dta %d %s\n",i,j,k,
				       xing,t,
				       d_sim,
				       d_i,want_log?"ERROR":"") ;
			}
		}

		if(want_log && log_level>4) {
			u_int s1_bits = 0 ;
			for(int c=0;c<32;c++) {
				int sum = 0 ;
				for(int t=0;t<8;t++) {
					sum += d_in[xing].s1[i][j][k].adc[c].d[t] ;
					LOG(ERR,"ch %2d: t %d: dta %d",c,t,d_in[xing].s1[i][j][k].adc[c].d[t]) ;
				}

				//LOG(ERR,"  sum %d, ped %d, sum-ped %d, ht thresh %d",sum,p_g[i][j][k][c].ped,sum-p_g[i][j][k][c].ped,ht_threshold[j]) ;
				sum -= p_g[i][j][k][c].ped ;
				if(sum > ht_threshold[j]) s1_bits |= (1<<c) ;
			}
			LOG(ERR,"  s1_bits 0x%08X",s1_bits) ;
		}

	}}}
	
	skip_stage1: ;

	if(s1_failed) {
		event_bad |= 1 ;
	}

	// verify stage_2 data locally to stage_2 DEP
	for(int i=0;i<NS_COU;i++) {

	int cns ;

	if(i==0) cns = 'N' ;
	else cns = 'S' ;

#if 1
	// check S2-to-DSM
	for(int j=0;j<1;j++) {
		int want_print = 0 ;
		int want_log = 0 ;

		if(tb_cou[i][3][1]==0) continue ;	// no stage_2 in data

		for(int t=0;t<4;t++) {		// just the first 4
			int d_sim = d_out.s2[i].s2_to_dsm ;
			int d_i = d_in[xing].s2[i].s2_to_dsm.d[t] ;

			if(d_sim != d_i) {
				errs.sim_s2++ ;
				s2_failed = 1 ;
				event_bad |= 8 ;

				s2_ns_bad = i ;
				s2_ch_bad = j ;

				want_log = 1 ;
				bad++ ;
			}
			else {
				good.sim_s2++ ;
			}

			if(d_i || d_sim) {
				want_print = 1 ;
			}
		}

		for(int t=0;t<4;t++) {
			char ctmp = ' ' ;

			int d_sim = d_out.s2[i].s2_to_dsm ;
			int d_i = d_in[xing].s2[i].s2_to_dsm.d[t] ;

			if(d_sim != d_i) ctmp = '*' ;

			if(want_log && log_level>0) {
				LOG(ERR,"evt %d: S2_to_DSM sim: %c - xing %d:%d: sim 0x%02X, dta 0x%02X%c",evts,cns,
				    xing,t,
				    d_sim,
				    d_i,ctmp) ;
			}

			if(want_print && log_level>3) {
				printf("evt %d: S2_to_DSM sim: %c: - xing %d:%d: sim %d, dta %d %s\n",evts,cns,
				       xing,t,
				       d_sim,
				       d_i,want_log?"ERROR":"") ;
			}
		}


	}
#endif		

	for(int j=0;j<2;j++) {
		int want_print = 0 ;
		int want_log = 0 ;

		if(tb_cou[i][3][1]==0) continue ;	// no stage_2 in data

		for(int t=0;t<7;t++) {	// skip 0xABCD
			int d_sim = d_out.s2[i].s2_to_s3[j].d[t] ;
			int d_i = d_in[xing].s2[i].s2_to_s3[j].d[t] ;

			if(d_sim != d_i) {
				errs.sim_s2++ ;
				s2_failed = 1 ;
				event_bad |= 2 ;

				s2_ns_bad = i ;
				s2_ch_bad = j ;

				want_log = 1 ;
				bad++ ;
			}
			else {
				good.sim_s2++ ;
			}

			if(d_i || d_sim) {
				want_print = 1 ;
			}
		}

		for(int t=0;t<8;t++) {
			char ctmp = ' ' ;

			int d_sim = d_out.s2[i].s2_to_s3[j].d[t] ;
			int d_i = d_in[xing].s2[i].s2_to_s3[j].d[t] ;

			if(d_sim != d_i) ctmp = '*' ;

			if(want_log && log_level>0) {
				LOG(ERR,"evt %d: S2 sim: %c:%d - xing %d:%d: sim 0x%02X, dta 0x%02X%c",evts,cns,j,
				    xing,t,
				    d_sim,
				    d_i,ctmp) ;
			}

			if(want_print && log_level>3) {
				printf("evt %d: S2 sim: %c:%d: - xing %d:%d: sim %d, dta %d %s\n",evts,cns,j,
				       xing,t,
				       d_sim,
				       d_i,want_log?"ERROR":"") ;
			}
		}


	}}


//	if(s2_failed) {
//		event_bad |= 2 ;
//	}

	// verify stage_3 locally to stage_2 DEP
	if(tb_cou[0][3][0]==0) return bad ;	// no stage_3 in data



	int want_log = 0 ;
	int want_print = 0 ;

	for(int t=0;t<4;t++) {	// not that we look at only the first 4 timebins because the other 4 are 0 in FPGA
		int d_sim = d_out.s3.dsm_out ;
		int d_i = d_in[xing].s3.dsm_out.d[t] ;


		if(d_sim || d_i) want_print = 1 ;

		// 24-Feb-21 hack to remove the free-running bit
		//d_i &= 0xFFFB ;

		if(d_sim != d_i) {
			errs.sim_s3++ ;
			s3_failed = 1 ;
			bad++ ;
			want_log = 1 ;
		}
		else {
			good.sim_s3++ ;
		}
	}

	// in case I want printouts
	for(int t=0;t<4;t++) {
		int d_sim = d_out.s3.dsm_out ;
		int d_i = d_in[xing].s3.dsm_out.d[t] ;

		if(want_log && log_level>0) {
			LOG(ERR,"evt %d: S3 sim: xing %d:%d: sim 0x%04X, dta 0x%04X %s",evts,xing,t,
			       d_sim,d_i,want_log?"ERROR":"") ;

		}
		if(want_print && log_level > 3) {
			printf("evt %d: S3 sim: xing %d:%d: sim 0x%04X, dta 0x%04X %s\n",evts,xing,t,
			       d_sim,d_i,want_log?"ERROR":"") ;
		}
	}
			    

	if(s3_failed) {
		event_bad |= 4 ;
	}

	return bad ;
}

// Main routine to run the simulations
// type==0 if we only want to compare data from actual DAQ files
// type==1 if this is a GEANT simulation and there are no actual DEP boards

//u_short fcs_trg_base::run_event_sim(int xing, int type) 
u_int fcs_trg_base::run_event_sim(int xing, int type) 
{
	geom_t geo ;

	if(type) {
		memset(&d_out,0,sizeof(d_out)) ;
	}

	dbg_xing = xing ;

	for(int i=0;i<NS_COU;i++) {			// NS
		geo.ns = i ;

		// for stage_2
		link_t ecal_in[DEP_ECAL_COU] ;
		link_t hcal_in[DEP_HCAL_COU] ;
		link_t fpre_in[DEP_PRE_COU] ;

		for(int j=0;j<ADC_DET_COU;j++) {		// DET
			geo.det = j ;

			for(int k=0;k<DEP_COU;k++) {	// DEP/ADC
				u_int s0_to_s1[32] ;

				if(tb_cou[i][j][k]==0) continue ;	// this DEP/ADC wasn't filled

				geo.dep = k ;

				//if(realtime && (dep != k)) continue ;

				for(int c=0;c<32;c++) {	// channel
					geo.ch = c ;

					u_int res ;

					stage_0(d_in[xing].s1[i][j][k].adc[c],geo,&(p_g[i][j][k][c]),&res) ;

					s0_to_s1[c] = res ;

					if(log_level>100) printf("... S0: xing %d: %d:%d:%d: ch %d = %d (ADC %d) (ped %d, gain %d) %s\n",xing,i,j,k,c,res,
								 d_in[xing].s1[i][j][k].adc[c].d[0],
								p_g[i][j][k][c].ped,p_g[i][j][k][c].gain,
								 res?"S1_HIT":"") ;

					if(log_level>101) {
						int sum = 0 ;
						for(int t=0;t<8;t++) {
							sum += d_in[xing].s1[i][j][k].adc[c].d[t] ;
							printf("       ADC %d = %d [sum %d, delta %d]\n",t,d_in[xing].s1[i][j][k].adc[c].d[t],sum,sum-p_g[i][j][k][c].ped) ;
						}

						printf("SSS0: %d %d %d %d %d\n",j,i,k,c,sum-p_g[i][j][k][c].ped) ;
					}
				}

				// so that we compare d_out.s1_to_s2 and d_in.s1_to_s2
				stage_1(s0_to_s1, geo, &d_out.s1[i][j][k].s1_to_s2) ;
				
			}
		}

		geo.det = 3 ;
		geo.dep = 1 ;	// stage_2's
		geo.ch = 0 ;	// ignored

		if(type==0) {	// running in realtime or through DAQ file
			if(tb_cou[i][3][1]==0) continue ;	// DEP/Stage_2 wasn't filled

			memset(ecal_in,0,sizeof(ecal_in)) ; //akio
			memset(hcal_in,0,sizeof(hcal_in)) ; //akio	 		
			memset(fpre_in,0,sizeof(fpre_in)) ;
			
			for(int j=0;j<20;j++) {
				ecal_in[j] = d_in[xing].s2[i].s2_from_s1[j] ;
			}
			for(int j=0;j<8;j++) {
				hcal_in[j] = d_in[xing].s2[i].s2_from_s1[20+j] ;
			}
			for(int j=0;j<6;j++) {
				fpre_in[j] = d_in[xing].s2[i].s2_from_s1[28+j] ;
			}

			stage_2(ecal_in, hcal_in, fpre_in, geo, d_out.s2[i].s2_to_s3, &d_out.s2[i].s2_to_dsm) ;
		}
		else {	// GEANT-like simulation

			for(int c=0;c<DEP_ECAL_COU;c++) {
				ecal_in[c] = d_out.s1[i][0][c].s1_to_s2 ;
			}
			for(int c=0;c<DEP_HCAL_COU;c++) {
				hcal_in[c] = d_out.s1[i][1][c].s1_to_s2 ;	// FY19 fPRE
			}
			for(int c=0;c<DEP_PRE_COU;c++) {
				fpre_in[c] = d_out.s1[i][2][c].s1_to_s2 ;	// FY19 fPRE
			}
			
			stage_2(ecal_in, hcal_in, fpre_in, geo, d_out.s2[i].s2_to_s3, &d_out.s2[i].s2_to_dsm) ;
		}
	}


	// stage_3
	geo.det = 3 ;
	geo.ns = 0 ;
	geo.dep = 0 ;
	geo.ch = 0 ;

	link_t l_in[4] ;

	if(type==0) {	// from actual SFS file
	        if(tb_cou[0][3][0]==0) return 0 ;	// no stage_3 data present

		l_in[0] = d_in[xing].s3.s3_from_s2[0] ;
		l_in[1] = d_in[xing].s3.s3_from_s2[1] ;
		l_in[2] = d_in[xing].s3.s3_from_s2[2] ;
		l_in[3] = d_in[xing].s3.s3_from_s2[3] ;

		stage_3(l_in,&d_out.s3.dsm_out) ;

	}
	else {

		
		l_in[0] = d_out.s2[0].s2_to_s3[0] ;
		l_in[1] = d_out.s2[0].s2_to_s3[1] ;
		l_in[2] = d_out.s2[1].s2_to_s3[0] ;
		l_in[3] = d_out.s2[1].s2_to_s3[1] ;
	
		stage_3(l_in,&d_out.s3.dsm_out) ;
	}

	//	return d_out.s3.dsm_out ;	// not that the return is the _simulated_ DSM
	return d_out.s3.dsm_out
	    + ((int)(d_out.s2[0].s2_to_dsm & 0xFF) << 16)
	    + ((int)(d_out.s2[1].s2_to_dsm & 0xFF) << 24);
	
}




void fcs_trg_base::stage_0(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	switch(stage_version[0]) {
	case 0 :
		stage_0_201900(adc, geo, pg, dta_out) ;
		break ;
	case 1 :
		stage_0_202101(adc, geo, pg, dta_out) ;
		break ;
	case 2 :
		stage_0_202103(adc, geo, pg, dta_out) ;
		break ;
	case 3 :	// current at start of FY22
		stage_0_202109(adc, geo, pg, dta_out) ;
		break ;
	default :
		*dta_out = 0 ;
		LOG(ERR,"stage_0: unknown version %d",stage_version[0]) ;
	}
}




void fcs_trg_base::stage_1(u_int s0[], geom_t geo, link_t *output) 
{

	switch(stage_version[1]) {
	case 0 :
		stage_1_201900(s0,geo,output) ;
		break ;
	case 1 :	// at start of FY22
		stage_1_202201(s0,geo,output) ;
		break ;
	default :
		LOG(ERR,"Unknown stage_1 version %d",stage_version[1]) ;
		break ;
	}
}


// 2 links are output: lo & hi
void fcs_trg_base::stage_2(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[2], u_short* s2_to_dsm) 
{
	switch(stage_version[2]) {
	case 0 :
		stage_2_201900(ecal,hcal,pres,geo,output) ;
		break ;
	case 1 :
		stage_2_202201(ecal,hcal,pres,geo,output) ;
		break ;
	case 2 :
		stage_2_TAMU_202202(ecal,hcal,pres,geo,output) ;
		break ;
	case 3 :	// at start of FY22
		stage_2_202203(ecal,hcal,pres,geo,output) ;
		break ;
	case 4 :
		stage_2_JP6_202204(ecal,hcal,pres,geo,output) ;
		break ;
	case 5 :
	        stage_2_JP6Carl_202205(ecal,hcal,pres,geo,output) ;
		break ;
	case 6 :
		stage_2_JP5_202206(ecal,hcal,pres,geo,output) ;
		break ;
	case 7 :	// for FY22
	        stage_2_202207(ecal,hcal,pres,geo,output,s2_to_dsm) ;
		break ;

	// debugging versions below
	case 0xFF210201 :
		stage_2_tonko_202101(ecal,hcal,pres,geo,output) ;
		break ;
	case 0xFF210204 :
		stage_2_tonko_202104(ecal,hcal,pres,geo,output) ;
		break ;
	default :
		LOG(ERR,"Unknown stage_2 version %d",stage_version[2]) ;
		break ;
	}
	
}


// VERSION 0x0
void fcs_trg_base::stage_3(link_t link[4], u_short *dsm_out) 
{

	switch(stage_version[3]) {
	case 0 :
		stage_3_201900(link,dsm_out) ;
		break ;
	case 1 :
		stage_3_202201(link,dsm_out) ;
		break ;
	case 3 :	// at start of FY22
		stage_3_202203(link,dsm_out) ;
		break ;
	case 7 :	// for FY22
		stage_3_202207(link,dsm_out) ;
		break ;
	// debugging versions below
	case 0xFF210201 :
		stage_3_tonko_202101(link, dsm_out) ;
		break ;

	default :
		LOG(ERR,"Unknown stage_3 version %d",stage_version[3]) ;
		break ;
	}

}

