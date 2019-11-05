#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include <rtsLog.h>
#include <DAQ_FCS/fcs_data_c.h>

#include "fcs_trg_base.h"



// statics
fcs_trg_base::marker_t fcs_trg_base::marker ;
ped_gain_t fcs_trg_base::p_g[2][3][24][32] ;
u_int fcs_trg_base::ht_threshold[3] ;
u_int fcs_trg_base::stage_version[4] ;

u_int fcs_trg_base::s2_ch_mask[NS_COU] ;
u_int fcs_trg_base::s3_ch_mask ;


int fcs_trg_base::fcs_trgDebug ;
u_short        fcs_trg_base::EM_HERATIO_THR ;
u_short        fcs_trg_base::HAD_HERATIO_THR ;
u_short        fcs_trg_base::EMTHR1 ;
u_short        fcs_trg_base::EMTHR2 ;
u_short        fcs_trg_base::HADTHR1 ;
u_short        fcs_trg_base::HADTHR2 ;
u_short        fcs_trg_base::JETTHR1 ;
u_short        fcs_trg_base::JETTHR2 ;

fcs_trg_base::fcs_trg_base()
{
//	LOG(TERR,"%s (sizeof class %u)",__PRETTY_FUNCTION__,sizeof(*this)) ;

	log_level = 0 ;
	realtime = 0 ;
	id = 0 ;
	sim_mode = 0 ;
}


fcs_trg_base::~fcs_trg_base()
{
	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;
}

u_int fcs_trg_base::get_version()
{
	return 0x19000000 ;	//YY,MM,DD,HH in BCD;	0 for base class
} ;


void fcs_trg_base::init()
{
	if(id != 0) return ;	// just one guy


	// zap peds to 0
	memset(p_g,0,sizeof(p_g)) ;


	// and then set all gains to 1
	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<ADC_DET_COU;j++) {
	for(int k=0;k<DEP_COU;k++) {
	for(int c=0;c<32;c++) {
		p_g[i][j][k][c].gain = (1<<6) ;	// set gains to 1
	}}}}


	// thresholds to defaults
	ht_threshold[0] = 90 ;	// ecal
	ht_threshold[1] = 90 ;	// hcal
	ht_threshold[2] = 90 ;	// fpRE in FY19

	// since I can use fcs_trg_base in realtime, all the stuff below is already loaded
	if(!realtime && !sim_mode) {
		LOG(INFO,"init: not realtime -- loading stuff from files") ;

		memset(fcs_data_c::ped,0,sizeof(fcs_data_c::ped)) ;

		fcs_data_c::load_rdo_map() ;
		fcs_data_c::gain_from_cache() ;

		for(int s=1;s<=1;s++) {
		for(int r=1;r<=6;r++) {
			char fname[256] ;
			sprintf(fname,"/net/fcs%02d/RTScache/fcs_pedestals_s%02d_r%d.txt",s,s,r) ;

			fcs_data_c::ped_from_cache(fname) ;
		}
		}
		


	}
	else if(!sim_mode) {
		LOG(INFO,"init:realtime: threshold is %d",fcs_data_c::ht_threshold) ;

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

			if(p) {
				LOG(TERR,"S%d:%d = ns %d, det %d, dep %d, ch %d = ped %d, gain %d",sec+1,rdo+1,i,j,k,c,p,g) ;
			}
			
			//u_int mask = fcs_data_c::rdo_map[sec][rdo].ch_mask & 0xFFFFFFFFll ;
		
		
			p_g[i][j][k][c].gain = fcs_data_c::ped[sec][rdo].i_gain[c] ;
			p_g[i][j][k][c].ped = fcs_data_c::ped[sec][rdo].i_ped[c] ;
		}
	}}}


	// BELOW depends on the length of processing so don't change!
	// Set by Tonko

	marker.last_xing = 14 ;	// this depends on the post setting and should be calculable


	
	marker.adc_start = 1 ;				// fixed during board configuration
	marker.s1_out_start = marker.adc_start + 11 ;	// depends on stage_0/1 algo but pretty much fixed

	marker.s2_in_start = marker.s1_out_start + 4 ;		// depends on the cable length from 1-to-2 put pretty much fixed
	marker.s2_to_s3_start = marker.s2_in_start + 13 ;	// depends on stage_2 algo?


	marker.s3_in_start = marker.s2_to_s3_start + 4 ;	// depends on cable length from 2-to-3; 0 in FY19
	marker.dsm_out_start = marker.s3_in_start + 10 ;	// depends on stage_3 algo


	if(log_level>0) LOG(INFO,"Markers: last xing %d, ADC %d, s1_out %d, s2_in %d, s2_to_s3 %d, s3_in %d, dsm_out %d",
	       marker.last_xing,
	       marker.adc_start,
	       marker.s1_out_start,
	       marker.s2_in_start,
	       marker.s2_to_s3_start,
	       marker.s3_in_start,
	       marker.dsm_out_start) ;


	// for Akio's code
	fcs_trgDebug = 0 ;

	// stage2 params (defaults are from Akio's code)
	EM_HERATIO_THR = 32 ;	// or 128*(1/4)
	HAD_HERATIO_THR = 32 ;
	EMTHR1 = 16 ;
	EMTHR2 = 32 ;
	HADTHR1 = 16 ;
	HADTHR2 = 32 ;
	JETTHR1 = 32 ;
	JETTHR2 = 64 ;
	

	

	// IMPORTANT: Requested Stage_x versions defaults
	// Either set by the user to her/his wishes or picked up from the DAQ file
	stage_version[0] = 0 ;
	stage_version[1] = 0 ;
	stage_version[2] = 0 ;
	stage_version[3] = 0 ;



	// DEP/Trigger masks
	s3_ch_mask = (1<<2) ;	// South 0 

	s2_ch_mask[0] = 0 ;		// Nothing in North
	s2_ch_mask[1] = (1<<3) ;	// just 1 fPRE channel in South for early FY19
}


void fcs_trg_base::run_start(u_int run)
{
	run_number = run ;
	evts = 0 ;

	memset(&errs,0,sizeof(errs)) ;
	memset(&statistics,0,sizeof(statistics)) ;

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
} ;


void fcs_trg_base::fill_event(int det, int ns, int dep, int c, u_short *d16, int t_cou) 
{

	if(t_cou) {
		if(got_one==0) evts++ ;
		got_one = 1 ;
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

				if(c==0 && (fla & 0x1)) {

					if(is_tcd < 0) is_tcd = t ;

					if(log_level > 10) {
						printf("ADC tcd_marker -- at xing  %d:%d(%d)\n",xing,xou,t) ;
					}
					
				}

				if(c==0 && (fla & 0x2)) {
					if(is_self < 0) is_self = t ;

					if(log_level>101) {
						printf("ADC self_trg -- at xing  %d:%d(%d)\n",xing,xou,t) ;
					}
				}

			}
		}
		else {			// DEP/Trigger in FY19
			switch(c) {
			case 4 :	// data from stage_2 to stage_3
				tix = t - marker.s2_to_s3_start ;

				xing = tix/8 ;
				xou = tix%8 ;

				if(fla & 0x4) {
					if(is_tcd < 0) is_tcd = t ;

					if(log_level > 10) {
						int xing = (t - marker.adc_start)/8 ;
						int xou = (t - marker.adc_start)%8 ;
						printf("S2 tcd_marker -- at xing  %d:%d(%d)\n",xing,xou,t) ;
					}
				}

				if(dta && log_level>10) {
					printf("s2_to_s3: in: %d:%d:%d:%d -- at xing %d:%d(%d) = %d\n",ns,det,dep,c,xing,xou,t,dta) ;
				}

				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s2[ns].s2_to_s3[0].d[xou] = dta & 0xFF ;	// link=0 only for now
					d_in[xing].s3.s3_from_s2[2*ns].d[xou] = dta & 0xFF ;
				}
				break ;
			case 5 :	// data from stage_3
				tix = t - marker.dsm_out_start ;
				xing = tix/8 ;
				xou = tix%8 ;

				if(fla & 0x4) {
					if(is_self < 0) is_self = t ;

					if(log_level>10) {
						printf("s3: in: self_trg -- at xing  %d:%d(%d)\n",xing,xou,t) ;
					}
				}

				if(dta && log_level>10) {
					printf("s3: in: %d:%d:%d:%d -- at xing %d:%d(%d) = 0x%03X\n",ns,det,dep,c,xing,xou,t,dta) ;
				}

				if(tix>=0 && xing<XING_COU) {
					d_in[xing].s3.dsm_out.d[xou] = dta ;
				}
				break ;
			default :	
				tix = t - marker.s2_in_start ;
				xing = tix/8 ;
				xou = tix%8 ;
				
				if(dta && log_level>10) {
					printf("s2_from_s1: in: %d:%d:%d:%d -- at xing %d:%d(%d) = %d\n",ns,det,dep,c,xing,xou,t,dta) ;
				}
			
				if(1) {
				//if(s2_ch_mask[ns] & (1<<c)) {
					if(tix>=0 && xing<XING_COU) {
						// data received from stage_1
						//printf("NS %d, ch %d, xing %d:%d = %d\n",f.hdr_ns,c,xing,xou,dta) ;
						d_in[xing].s2[ns].s2_from_s1[c].d[xou] = dta & 0xFFF;
					}
				}
				break ;
			}
						
		}
	}

	if(is_self>0) {
//		LOG(TERR,"self at %d",is_self) ;
		statistics.self_trgs++ ;

		statistics.self_trg_marker = is_self ;
	}
	
	if(is_tcd > 0) {
		statistics.tcd_marker = is_tcd ;
	}

} ;

int fcs_trg_base::end_event()
{
	if(!got_one) return 0 ;	// nothing to do; let's not waste time

	verify_event_io() ;

	for(int xing=0;xing<marker.last_xing;xing++) {

		run_event_sim(xing,sim_mode) ;
		
		if(sim_mode) {
			dump_event_sim(xing) ;
		}
		else {
			verify_event_sim(xing) ;
		}
	}

	return 0 ;
}

int fcs_trg_base::run_stop()
{
	int err = 0 ;

	for(int i=0;i<4;i++) {
		if(errs.io_s1_to_s2[i]) err = 1 ;
	}

	if(errs.sim_s1 || errs.sim_s2 || errs.sim_s3 || errs.io_s2_to_s3) {
		err = 1 ;
	}

	LOG(TERR,"thread %d: self_trg_marker %d, tcd_marker %d",id,statistics.self_trg_marker,statistics.tcd_marker) ;

	if(err) {
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
	LOG(INFO,"thread %d: %d/%d events in run %d: errs sim %u %u %u; io [%u %u %u %u] %u",id,
	    statistics.self_trgs,
	    evts,run_number,
	    errs.sim_s1,
	    errs.sim_s2,
	    errs.sim_s3,
	    errs.io_s1_to_s2[0],errs.io_s1_to_s2[1],errs.io_s1_to_s2[2],errs.io_s1_to_s2[3],
	    errs.io_s2_to_s3) ;
	
	}

	return 0 ;
}

// verify the IO parts of the event, various links
int fcs_trg_base::verify_event_io() 
{
	int printed = 0 ;
	int bad = 0 ;


	// verify data passing from DEP/ADC to DEP/Stage_2
	if(tb_cou[1][3][0]) ;	// special for FY10 (South,stage_2)
	else return 0 ;

	int ns,det,dep ;

	ns = det = dep = 0 ;

	for(int c=0;c<4;c++) {
		switch(c) {
		case 0 :
			ns = 1 ;
			det = 0 ;
			dep = 0 ;
			break ;
		case 1 :
			ns = 1 ;
			det = 0 ;
			dep = 1 ;
			break ;
		case 2 :
			ns = 1 ;
			det = 1 ;
			dep = 0 ;
			break ;
		case 3 :
			ns = 1 ;
			det = 2 ;
			dep =0 ;
			break ;
		}

		if(tb_cou[ns][det][dep]) ;
		else continue ;

		if(c==2) continue ;	// HACK: broken HCAL links in Jul 2019

		for(int x=0;x<marker.last_xing;x++) {
			int want_print = 0 ;
			int want_log = 0 ;

			for(int t=0;t<8;t++) {
				int s1 = d_in[x].s1[ns][det][dep].s1_to_s2.d[t] ;	// outgoing

				int s2 = d_in[x].s2[ns].s2_from_s1[c].d[t] ;	// incoming; on ch 4

				if(s1 || s2) want_print = 1 ;

				//if(t==1 && (s1 & 1)) {
				//	printf("Got bit for ch 0\n") ;
				//}

				if(s1 != s2) {
					want_log = 1 ;

					errs.io_s1_to_s2[c]++ ;

					bad++ ;
				}


			}

			for(int t=0;t<8;t++) {
				int s1 = d_in[x].s1[ns][det][dep].s1_to_s2.d[t] ;	// outgoing
				int s2 = d_in[x].s2[ns].s2_from_s1[c].d[t] ;	// incoming; on ch 0


				if(want_log && log_level>0) {
					LOG(ERR,"evt %d: S1 IO: %d:%d:%d at ch %d: xing %d:%d: out %d, in %d",evts,
					    ns,det,dep,c,
					    x,t,
					    s1,
					    s2) ;
				}

				if(want_print && log_level>3) {
					printed = 1 ;
					printf("S1 IO: %d:%d:%d at ch %d: xing %d:%d: out %d, in %d %s\n",
					       ns,det,dep,c,
					       x,t,
					       s1,
					       s2,want_log?"ERROR":"") ;
				}


			}
		}
	}

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

			if(d_sim) printf("S1 out: %d:%d:%d - xing %d:%d, dta %d\n",
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

			printf("S2 out: %d:%d - xing %d:%d, dta 0x%03X\n",
			       i,j,xing,t,d_sim) ;
		}
	}
	}

	printf("S3 out: to DSM 0x%04X\n",d_out.s3.dsm_out) ;

	return 0 ;
}


int fcs_trg_base::verify_event_sim(int xing) 
{
	int bad = 0 ;
	
	// verify stage_1 data

	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<ADC_DET_COU;j++) {
	for(int k=0;k<DEP_COU;k++) {
		int want_print = 0 ;
		int want_log = 0 ;

		if(tb_cou[i][j][k]==0) continue ;	// not in the daat...

		for(int t=0;t<8;t++) {
			int d_sim = d_out.s1[i][j][k].s1_to_s2.d[t] ;
			int d_i = d_in[xing].s1[i][j][k].s1_to_s2.d[t] ;

			if(d_sim != d_i) {
				errs.sim_s1++ ;

				want_log = 1 ;
				bad++ ;
			}

			if(d_i || d_sim) {
				want_print = 1 ;
			}
		}

		for(int t=0;t<8;t++) {
			int d_sim = d_out.s1[i][j][k].s1_to_s2.d[t] ;
			int d_i = d_in[xing].s1[i][j][k].s1_to_s2.d[t] ;

			if(want_log && log_level>0) {
				LOG(ERR,"evt %d: S1 sim: %d:%d:%d - xing %d:%d: sim %d, dta %d",evts,i,j,k,
				    xing,t,
				    d_sim,
				    d_i) ;
			}

			if(want_print && log_level>3) {
				printf("S1: %d:%d:%d - xing %d:%d: s1 sim %d, dta %d %s\n",i,j,k,
				       xing,t,
				       d_sim,
				       d_i,want_log?"ERROR":"") ;
			}
		}

	}}}
				       

	// verify stage_2 data locally to stage_2 DEP
	for(int i=0;i<NS_COU;i++) {
	for(int j=0;j<2;j++) {
		int want_print = 0 ;
		int want_log = 0 ;

		if(tb_cou[i][3][0]==0) continue ;	// no stage_2 in data

		for(int t=0;t<8;t++) {
			int d_sim = d_out.s2[i].s2_to_s3[j].d[t] ;
			int d_i = d_in[xing].s2[i].s2_to_s3[j].d[t] ;

			if(d_sim != d_i) {
				errs.sim_s2++ ;

				want_log = 1 ;
				bad++ ;
			}

			if(d_i || d_sim) {
				want_print = 1 ;
			}
		}

		for(int t=0;t<8;t++) {
			int d_sim = d_out.s2[i].s2_to_s3[j].d[t] ;
			int d_i = d_in[xing].s2[i].s2_to_s3[j].d[t] ;

			if(want_log && log_level>0) {
				LOG(ERR,"evt %d: S2 sim: %d:%d - xing %d:%d: sim %d, dta %d",evts,i,j,
				    xing,t,
				    d_sim,
				    d_i) ;
			}

			if(want_print && log_level>2) {
				printf("evt %d: S2 sim: %d:%d: - xing %d:%d: sim %d, dta %d %s\n",evts,i,j,
				       xing,t,
				       d_sim,
				       d_i,want_log?"ERROR":"") ;
			}
		}


	}}


	// verify stage_3 locally to stage_2 DEP
	if(tb_cou[1][3][0]==0) return bad ;



	int want_log = 0 ;
	int want_print = 0 ;
	for(int t=0;t<8;t++) {
		int d_sim = d_out.s3.dsm_out ;
		int d_i = d_in[xing].s3.dsm_out.d[t] ;

		if(d_sim || d_i) want_print = 1 ;

		if(d_sim != d_i) {
			errs.sim_s3++ ;

			bad++ ;
			want_log = 1 ;
		}
	}

	for(int t=0;t<8;t++) {
		int d_sim = d_out.s3.dsm_out ;
		int d_i = d_in[xing].s3.dsm_out.d[t] ;

		if(want_log && log_level>0) {
			LOG(ERR,"evt %d: S3 sim: xing %d:%d: sim 0x%03X, dta 0x%03X %s",evts,xing,t,
			       d_sim,d_i,want_log?"ERROR":"") ;

		}
		if(want_print && log_level > 1) {
			printf("evt %d: S3 sim: xing %d:%d: sim 0x%03X, dta 0x%03X %s\n",evts,xing,t,
			       d_sim,d_i,want_log?"ERROR":"") ;
		}
	}
			    
	return bad ;
}

// Main routine to run the simulations
// type==0 if we only want to compare data from actual DAQ files
// type==1 if this is a GEANT simulation and there are no actual DEP boards

u_short fcs_trg_base::run_event_sim(int xing, int type) 
{
	geom_t geo ;

	if(type) {
		memset(&d_out,0,sizeof(d_out)) ;
	}

	for(int i=0;i<NS_COU;i++) {			// NS
		geo.ns = i ;

		// for stage_2
		link_t ecal_in[DEP_ECAL_COU] ;
		link_t hcal_in[DEP_HCAL_COU] ;
		link_t fpre_in[DEP_PRE_COU] ;


		for(int j=0;j<ADC_DET_COU;j++) {		// DET
			geo.det = j ;

			for(int k=0;k<DEP_COU;k++) {	// DEP/ADC

				//if(type==0) {	// only for non-GEANT
					if(tb_cou[i][j][k]==0) continue ;	// this DEP/ADC wasn't filled
				//}

				u_int s0_to_s1[32] ;

				geo.dep = k ;

				//if(realtime && (dep != k)) continue ;

				for(int c=0;c<32;c++) {	// channel
					geo.ch = c ;

					u_int res ;

					stage_0(d_in[xing].s1[i][j][k].adc[c],geo,&(p_g[i][j][k][c]),&res) ;

					s0_to_s1[c] = res ;
				}


				// so that we compare d_out.s1_to_s2 and d_in.s1_to_s2
				stage_1(s0_to_s1, geo, &d_out.s1[i][j][k].s1_to_s2) ;
			}
		}

		geo.det = 3 ;
		geo.dep = 0 ;	// stage_2's
		geo.ch = 0 ;	// ignored


		if(type==0) {	// running in realtime or through DAQ file
			if(tb_cou[i][3][0]==0) continue ;	// DEP/Stage_2 wasn't filled

			memset(ecal_in,0,sizeof(fpre_in)) ;
			memset(hcal_in,0,sizeof(fpre_in)) ;			
			memset(fpre_in,0,sizeof(fpre_in)) ;
			
			// FY19 setup
			fpre_in[0] = d_in[xing].s2[i].s2_from_s1[3] ;

			ecal_in[0] = d_in[xing].s2[i].s2_from_s1[0] ;
			ecal_in[1] = d_in[xing].s2[i].s2_from_s1[1] ;
			hcal_in[0] = d_in[xing].s2[i].s2_from_s1[2] ;
		
			//printf("Sizeofs %d %d \n",sizeof(fpre_in[0]),sizeof(fpre_in)) ;



			stage_2(ecal_in, hcal_in, fpre_in, geo, d_out.s2[i].s2_to_s3) ;
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
			
			stage_2(ecal_in, hcal_in, fpre_in, geo, d_out.s2[i].s2_to_s3) ;

		}
	}


	// stage_3
	geo.det = 3 ;
	geo.ns = 0 ;
	geo.dep = 1 ;
	geo.ch = 0 ;

	if(type==0) {
		// special for FY19 which acts also as stage_3
		if(tb_cou[1][3][0]==0) return 0 ;

		// otherwise it will be
		//if(tb_cou[0][3][1]==0) return 0 ;

		stage_3(d_in[xing].s3.s3_from_s2,&d_out.s3.dsm_out) ;
	}
	else {
		link_t l_in[4] ;
		
		l_in[0] = d_out.s2[0].s2_to_s3[0] ;
		l_in[1] = d_out.s2[0].s2_to_s3[1] ;
		l_in[2] = d_out.s2[1].s2_to_s3[0] ;	//FY19
		l_in[3] = d_out.s2[1].s2_to_s3[1] ;
	
		stage_3(l_in,&d_out.s3.dsm_out) ;
	}

	return d_out.s3.dsm_out ;
}




void fcs_trg_base::stage_0(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	if(stage_version[0] != 0) {
		LOG(ERR,"stage_0: unknown version %d",stage_version[0]) ;
		// but continue
	}

	switch(stage_version[0]) {
	case 0 :
		stage_0_201900(adc, geo, pg, dta_out) ;
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
	default :
		LOG(ERR,"Unknown stage_1 version %d",stage_version[1]) ;
		break ;
	}

}


// 2 links are output: lo & hi
void fcs_trg_base::stage_2(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[2]) 
{

	switch(stage_version[2]) {
	case 0 :
		stage_2_201900(ecal,hcal,pres,geo,output) ;
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
	default :
		LOG(ERR,"Unknown stage_3 version %d",stage_version[3]) ;
		break ;
	}

}

