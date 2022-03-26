#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <rtsLog.h>

#include "fcs_data_c.h"

static inline u_int sw16(u_int d)
{
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;

        return d ;
}

// this shouldn't really be static when running offline!
//unsigned long long fcs_data_c::ch_mask[8] ;		// channel masks for the 8 RDOs; where are they? map file!
struct fcs_data_c::fcs_ped_t fcs_data_c::ped[FCS_SECTOR_COU][8] ;	// 8 RDO

struct fcs_data_c::rdo_map_t fcs_data_c::rdo_map[FCS_SECTOR_COU][8] ;	// FCS_SECTOR_COU sectors, 8 RDOs each --> det,ns,dep
struct fcs_data_c::det_map_t fcs_data_c::det_map[4][2][24] ;	// det,ns,dep --> sector RDO
u_char fcs_data_c::rdo_map_loaded ;

u_char fcs_data_c::fcs_bad_ch[8][34] ;
u_char fcs_data_c::fcs_bad_ch_all[FCS_SECTOR_COU][8][34] ;

u_int fcs_data_c::run_number ;
u_int fcs_data_c::run_type ;

// for ZS
float fcs_data_c::n_sigma ;
short fcs_data_c::n_pre ;
short fcs_data_c::n_post ;
short fcs_data_c::n_cou ;
char fcs_data_c::n_mode ;

// set in send_config, for shared access during data-checking
u_short fcs_data_c::ht_threshold ;
u_short fcs_data_c::tb_pre ;
u_short fcs_data_c::tb_all ;

u_char fcs_data_c::ascii_no ;

pthread_mutex_t fcs_data_c::ped_mutex ;
	
fcs_data_c::statistics_t fcs_data_c::statistics[8] ;

int fcs_data_c::stage_params_txt[32] ;


long fcs_data_c::dep_to_char(int det, int ns, int dep)
{
        long ret ;
        char *ctmp = (char *)&ret ;

        switch(det) {
        case 0 :
                ctmp[0] = 'E' ;
                break ;
        case 1 :
                ctmp[0] = 'H' ;
                break ;
        case 2 :
                ctmp[0] = 'P' ;
                break ;
        case 3 :
                ctmp[0] = 'M' ;
                break ;
        default :
                ctmp[0] = 'X' ;
                break ;
	}


        if(ns==0) ctmp[1] = 'N' ;
        else ctmp[1] = 'S' ;

        sprintf(ctmp+2,"%02d",dep) ;

        return ret ;


}

const char *fcs_data_c::stage_labels[] = {
	"FCS_HAD-HERATIO-THR", //
        "FCS_EM-HERATIO-THR", //
        "FCS_HADTHR1", //
        "FCS_HADTHR2", //
        "FCS_HADTHR3",
        "FCS_EMTHR1", //
        "FCS_EMTHR2", //
        "FCS_EMTHR3",
        "FCS_JETTHR1",
        "FCS_JETTHR2",
        "FCS_ETOTTHR", //
        "FCS_HTOTTHR",	// 11 //

        "FCS_EHTTHR",	// 12 //
        "FCS_HHTTHR",	// 13 //
        "FCS_PHTTHR"	// 14 //
} ;

int fcs_data_c::load_stage_params(int sec1, const char *fname)
{
	if(fname==0) {
		fname="/RTS/conf/fcs/stage_params.txt" ;
	}

	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"sector %2d: %s: [%s]",sec1,fname,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"sector %2d: stage_params %s opened",sec1,fname) ;

	int max_i = 0 ;

	while(!feof(f)) {
		char buff[128] ;
		char name[64] ;
		int val ;
		int ix ;
		int dummy ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;

		name[0] = '?' ;
		name[1] = 0 ;
		val = -1 ;

		int ret = sscanf(buff,"%d %d %d %s %d",&dummy,&dummy,&ix,name,&val) ;
		if(ret != 5) continue ;

//		LOG(TERR,"ret %d: [%s]=%d",ret,name,val) ;

		char got_it = -1 ;
		for(u_int i=0;i<sizeof(stage_labels)/sizeof(stage_labels[0]);i++) {
			if(stage_labels[i]==0) continue ;

			if(strcasecmp(stage_labels[i],name)==0) {
				stage_params_txt[i] = val ;
				got_it = i ;
				max_i = i ;
				break ;
			}
		}

		if(sec1==11) {	// LOG only from this one
			if(got_it<0) {
				LOG(ERR,"stage_param_txt [%s]=%d NOT coded locally",name,val) ;
			}
			else {
				LOG(INFO,"stage_param_txt [%s]=%d, index %d, ix %d",name,val,got_it,ix) ;
			}
		}
	}

	if(sec1==11) {	// LOG from this one only
		for(int i=0;i<max_i;i++) {
			LOG(TERR,"stage_params_txt: %d/%d = %d",i,max_i,stage_params_txt[i]) ;
		}
	}

	fclose(f) ;
	return 0 ;
}

	

int fcs_data_c::zs_start(u_short *buff)
{
	int thr ;
	int l_cou ;
	int l_pre, l_post ;
	int is_trg = 0 ;
	int i_ped ;

	// trigger channels are special so figure this out
	if(ch >= 32) is_trg = 1 ;
	if(hdr_det >= 3) is_trg = 1;

	if(is_trg) {	// this is the trigger data channel, no need to go pre/post
		thr = 0 ;
		l_cou = 1 ;
		l_pre = 0 ;
		l_post = 0 ;
		i_ped = 0 ;
	}
	else {
		i_ped = (int)(ped[sector-1][rdo-1].mean[ch]+0.5) ;

		LOG(DBG,"S%d:%d:%d mean %f, n_sigma %f, rms %f",
		    sector,rdo,ch,
		    (float)ped[sector-1][rdo-1].mean[ch],
		    (float)n_sigma,
		    (float)ped[sector-1][rdo-1].rms[ch]) ;

		// I don't think that a threshold as a function of RMS is a good idea.
		// I should do what the ASICs do and have a fixed digital threshold
		if(n_mode==0) {
			thr = (int)(ped[sector-1][rdo-1].mean[ch] + n_sigma * ped[sector-1][rdo-1].rms[ch] + 0.5) ;
		}
		else {
			thr = (int)(ped[sector-1][rdo-1].mean[ch] + n_sigma) ;
		}

		l_cou = n_cou ;
		l_pre = n_pre ;
		l_post = n_post ;
	}

	int t_cou = 0 ;
	int t_start = 0 ;
	int t_stop ;
	int got_one = 0 ;

//	int q_ped = 0 ;
	
	// form sequences including the l_pre and l_post bits
	for(int i=0;i<tb_cou;i++) {
		short i_adc = adc[i] & 0xFFF ;

//		if(i<4) {
//			q_ped += d ;
//		}

//		printf("CH %d: %d = %d < thr %d: t_start %d, t_cou %d\n",ch,i,d,thr,t_start,t_cou) ;

		if(i_adc <= thr) {	// datum needs to be greater than the threshold
			if(t_cou >= l_cou) {
				t_stop = t_start + t_cou ;

				t_start -= l_pre ;
				if(t_start < 0) t_start = 0 ;

				t_stop += l_post ;
				if(t_stop > tb_cou) t_stop = tb_cou ;
				
				if(got_one==0) {	// first one
					memset(mark,0,tb_cou) ;
				}

				got_one = 1;
				for(;t_start<t_stop;t_start++) {
					mark[t_start] = 1 ;
				}
			}
			t_cou = 0 ;
		}
		else {
			if(t_cou==0) {
				t_start = i ;				
			}
			t_cou++ ;
		}

	}


#if 0
	if(!is_trg) {
		q_ped /= 4 ;	// quick ped
		LOG(DBG,"RDO %d, ch %d, q_ped %d, i_ped %d",rdo,ch,q_ped,i_ped) ;

		q_ped -= i_ped ;
		if(abs(q_ped)>3) {
			statistics[rdo-1].odd_ped[ch]++ ;
		}
	}
#endif

	//finalize the last sequence
	if(t_cou >= l_cou) {
		t_stop = t_start + t_cou ;

		t_start -= l_pre ;
		if(t_start < 0) t_start = 0 ;

		t_stop += l_post ;
		if(t_stop > tb_cou) t_stop = tb_cou ;

		if(got_one==0) {
			memset(mark,0,tb_cou) ;
		}

		got_one = 1 ;
				
		for(;t_start<t_stop;t_start++) {
			mark[t_start] = 1 ;
		}

	}


	LOG(DBG,"RDO %d, ch %d: thr %d: got %d",rdo,ch,thr,got_one) ;

	if(got_one==0) return 0 ;	// nothing found

	u_short *dp ;

	dp = (u_short *)buff ;


	int seq_cou = 0 ;

	// and now go through the "mark"
	u_short *dstart = dp ;

	dstart[0] = ch ;
	dstart[1] = 0 ;	// count of sequences

	dp += 2 ;	// skip the header

	u_short *t_cou_p = dp + 1 ;
	t_cou = 0 ;

	for(int i=0;i<tb_cou;i++) {
		if(mark[i]) {
//			printf("Mark at %d\n",i) ;

			if(t_cou==0) {
//				printf("t_start %d\n",i) ;
				*dp++ = i ;
				t_cou_p = dp++ ;
				t_start = i ;
			}

			short i_adc = adc[i] & 0xFFF ;
			short fla = adc[i] >> 12 ;

			// Akio's request to see if the ADC pegged
			if(i_adc==4095) ;	// leave it!
			else {
				i_adc -= i_ped ;
				if(i_adc < 0) i_adc = 0 ;	// no stinkin' negative numbers
			}

			i_adc |= (fla<<12) ;			// put the flags back

//			printf("adc[%d] = %d\n",i,i_adc&0xFFF) ;
			*dp++ = i_adc ;

			t_cou++ ;
		}
		else {
			if(t_cou) {				
				*t_cou_p = t_cou ;
				seq_cou++ ;
//				printf("ZS: Ch %d:%d: seq %d: t_start %d, t_cou %d\n",rdo,ch,seq_cou,t_start,t_cou) ;
			}
			t_cou = 0 ;
		}
	}

	if(t_cou) {
		*t_cou_p = t_cou ;
		seq_cou++ ;
//		printf("ZS: Ch %d:%d: seq %d(last): t_start %d, t_cou %d\n",rdo,ch,seq_cou,t_start,t_cou) ;
	}

	dstart[1] = seq_cou ;

//	printf("... ZS is now %d shorts (seq_cou %d)\n",(int)(dp-dstart),seq_cou) ;

	// I probably want to return 0 if nothing is founf
	if(seq_cou == 0) return 0 ;

	return dp-dstart ;	// shorts


}


/*******************************/
int fcs_data_c::start(u_short *d16, int shorts)
{
	u_int *d ;

	//class members
	events++ ;

	bad_error = 0 ;

	dta_start = dta_p = d16 ;
	dta_stop = d16 + shorts ;
	dta_shorts = shorts ;

	d = (u_int *)d16 ;

	rhic_start = 0;
	ch_count = 0 ;
	ch_mask_seen = 0 ;
	want_saved = 0 ;
	
	
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"...start: %d = 0x%04X",i,d16[i]) ;
//	}


	//version = 0 ;	// unknown...
//	LOG(TERR,"VERSION 0x%X",d[0]) ;

	//check version
	if(d[0]==0xDDDDDDDD) {	// new FY18 data!
		d += 4 ;	// skip GTP header
		d16 += 8 ;

		// d16[0] is start comma
		// d16[1] is cccc ;
		// d16[2] is 0x9801
		// ... and then trigger data

		version = sw16(d[2]) ;

		switch(version) {
		case 0x12340000 :	// pre-May-15-2018
			version = 0x18040000 ;	//Apr 2018
			d += 12 ;	// skip event header to go to ADC data
			break ;
		default :		// nre
			switch(d16[2]) {
			case 0x9801 :
				version = 0x18050000 ;	// 15-May-2018

				dta_p = ((u_short *)d)+6 ;	// this is for May18-Dec18


				for(int i=0;i<16;i++) {
					LOG(TERR,"...data9801: %d = 0x%04X",i,dta_p[i]) ;
				}


				return 1 ;
			case 0x9802 :
			case 0x9803 :
				version = 0x18110000 ;

				dta_p = d16 ;

				return hdr_event() ;

			case 0x9810 :	// experimental streaming, May 2020
				version = 0x30050000 ;

				dta_p = d16 ;

				return hdr_event() ;

			}

			LOG(ERR,"uknown version 0x%04X",d16[2]) ;
			bad_error |= 1 ;
			return 0 ;

			break ;
		}

		// pre-May-15-2018
		dta_p = (u_short *) d ;


		for(int i=0;i<8;i++) {
			LOG(TERR,"...data: %d = 0x%04X",i,dta_p[i]) ;
		}

		return 1 ;
	}

	// old 2017 format here
	//LOG(TERR,"start: 0x%08X 0x%08X",d[0],d[1]) ;

	//move to start-of-ADC marker
	while(dta_p < dta_stop) {
		if(*dta_p++ == 0xFD06) {
			//for(int i=0;i<16;i++) {
			//	LOG(TERR,"...%d = 0x%04X",i,dta_p[i]) ;
			//}


			return 1 ;
		}
	}


	return -1 ;
}

// at entry dta_p points to the start-comma of the event
// at exit, dta_p must point to start of ADC data
// returns
//	>0 if all OK and triggered event
//	0 is all OK and not a triggered event
//	<0 is not all OK
int fcs_data_c::hdr_event()
{
	u_short *start_p = dta_p ;


//	for(int i=0;i<32;i++) {
//		LOG(TERR,"... %d 0x%04X",i,dta_p[i]) ;
//	}

//	has_ascii = 0 ;
	first_tb_cou = 0 ;

	//I will need the board id as a sector/id combo
	hdr_board_id = dta_p[3] ;


	hdr_sector = ((hdr_board_id >> 11) & 0x1F)+1 ;
	hdr_rdo = ((hdr_board_id >> 8) & 0x7)+1 ;

	hdr_det = (hdr_board_id >> 6) & 0x3 ;
	hdr_ns = (hdr_board_id >> 5) & 1 ;
	hdr_dep = hdr_board_id & 0x1F ;


//	LOG(TERR,"... 0x%X S%d:%d %d %d %d",hdr_board_id,hdr_sector,hdr_rdo,hdr_det,hdr_ns,hdr_dep) ;


	if((sector != hdr_sector) || (rdo != hdr_rdo)) {
		bad_error |= 2 ;
		LOG(ERR,"%d: sector %d:%d expected, received %d:%d [0x%X]",id,sector,rdo,hdr_sector,hdr_rdo,hdr_board_id) ;
	}

	// this won't work Offline because I don't have the real board id...
	if(realtime && (hdr_board_id != board_id)) {
		bad_error |= 2 ;
		LOG(ERR,"%d: evt %d: board_id: expected 0x%04X, received 0x%04X",id,events,board_id,hdr_board_id) ;
	}

	//extract trigger_word and rhic_counter
	hdr_trg_word = ((dta_p[5]&0xF)<<16) | dta_p[4] ;
	hdr_rhic_counter = (dta_p[7]<<16)|dta_p[6] ;


	LOG(NOTE,"HDR S%d:%d: trg_word 0x%05X, RHIC %u, upper 0x%03X",hdr_sector,hdr_rdo, hdr_trg_word,hdr_rhic_counter,dta_p[5]>>4) ;

	trg_cmd = hdr_trg_word & 0xF ;
	daq_cmd = (hdr_trg_word>>4) & 0xF ;
	token = ((hdr_trg_word>>8)&0xF)<<8 ;
	token |= ((hdr_trg_word>>12)&0xF)<<4 ;
	token |= ((hdr_trg_word>>16)&0xF) ;

	LOG(NOTE,"HDR: token %d, trg_cmd %d, daq_cmd %d",token,trg_cmd,daq_cmd) ;

	if(version == 0x30050000) {
		// leave dta_p

		trgd_event = 0 ;	// need to check what this really means?
		return 0 ;
	}		


	// skip to first datum
	dta_p += 8 ;

//	has_ascii = 0 ;
	ascii_p = 0 ;
	ascii_words = 0 ;


	if(dta_p[0]==0xEEEE && dta_p[1]==0xEEEE) {	// start of ASCII
		char ctmp[1024] ;

		dta_p += 2 ;	// adjust
		u_int *d32 = (u_int *)dta_p ;

		int words = (dta_shorts - 8 - 2)/2 ;	// adjust

//		has_ascii = 1 ;
		ascii_p = (char *)dta_p ;
		ascii_words = words ;

		if(ascii_no==0) LOG(TERR,"ASCII contribution - words %d[%d]: sector %d, rdo %d, hdr_trg_word 0x%X, hdr_board 0x%X",words,dta_shorts,sector,rdo,hdr_trg_word,hdr_board_id) ;

		int end_marker = 0 ;
		u_int cou = 0 ;
		for(int i=0;i<words;i++) {
			u_int asc = d32[i] ;

			if((asc&0xFF00FFFF)==0xF5009800) {
				char c = (asc>>16)&0xFF ;

				if(cou>sizeof(ctmp)) ;
				else {
					if(c=='\n') {
						float f_val = 0.0 ;
						u_int i_val = 0 ;
						char *c ;
						//int ret ;

						ctmp[cou] = 0 ;
						if(ascii_no==0) LOG(TERR,"S%d:%d:%d: \"%s\"",sector,rdo,events,ctmp) ;
						cou = 0 ;

					
						if((c=strstr(ctmp,"r0 7"))) {
							sscanf(c,"r0 7 %f",&f_val) ;

							ped_lock() ;
							statistics[rdo-1].ht_rate = (int) f_val ;
							ped_unlock() ;


						}
						else if((c=strstr(ctmp,"t W "))) {
							sscanf(c,"t W %f",&f_val) ;

							ped_lock() ;
							statistics[rdo-1].temperature = f_val ;
							ped_unlock() ;
						}
						else if((c=strstr(ctmp,"tb "))) {
							sscanf(c,"tb 0x%X",&i_val) ;

							f_val = 100.0*(double)(i_val & 0x3FF)/1023.0 ;

							ped_lock() ;
							statistics[rdo-1].deadtime = f_val ;
							ped_unlock() ;

							//if(f_val > 50.0) {
							//	LOG(WARN,"S%d:%d: deadtime %.1f",sector,rdo,f_val) ;
							//}
							//else {
							//	LOG(TERR,"S%d:%d: deadtime %.1f",sector,rdo,f_val) ;
							//}
						}
						else if((c=strstr(ctmp,"b "))) {
							sscanf(c,"b %f",&f_val) ;

							ped_lock() ;
							statistics[rdo-1].deadtime = f_val ;
							ped_unlock() ;
						}
						else if((c=strstr(ctmp,"rg 7 "))) {
							sscanf(c,"rg 7 0x%X",&i_val) ;

							f_val = 100.0*(double)(i_val & 0x3ff)/1023.0 ;

							
							ped_lock() ;
							statistics[rdo-1].rx_throttle = f_val ;
							ped_unlock() ;

							//if(f_val > 50.0) {
							//	LOG(WARN,"S%d:%d: RX-throttle %.1f",sector,rdo,f_val) ;
							//}
							//else {
							//	LOG(TERR,"S%d:%d: RX-throttle %.1f",sector,rdo,f_val) ;
							//}
						}
											   

					}
					else {
						ctmp[cou] = c ;
						cou++ ;
					}
				}
			}
			else if(asc != 0xFFFFFFFF) {
				bad_error |= 4 ;
				LOG(ERR,"ASCII wha %d: 0x%08X",i,asc) ;
			}

			dta_p += 2 ;

			if(asc==0xFFFFFFFF) {
				end_marker = 1 ;
				break ;
			}

		}

		ctmp[cou] = 0 ;
		if(!end_marker) {
			bad_error |= 8 ;
			LOG(ERR,"S%d:%d:%d: ASCII[%d] but no end-marker \"%s\"",sector,rdo,events,cou,ctmp) ;
		}
		else if(cou) {
			bad_error |= 8 ;
			LOG(ERR,"S%d:%d:%d: ASCII[%d] \"%s\"",sector,rdo,events,cou,ctmp) ;
		}

	}
	else if(dta_p[0]==0xFFFF && dta_p[1]==0xFFFF) {	// bug: end-of-ascii without ascii
		bad_error |= 8 ;
		LOG(ERR,"S%d:%d:%d: ASCII bug: 0x%X, 0x%X",sector,rdo,events,hdr_trg_word,dta_p[2]) ;
		for(int i=0;i<32;i++) {
			LOG(TERR,"... %d = 0x%04X",i,start_p[i]) ;
		}
		dta_p += 2 ;
	}
#if 1
	else if(dta_p[0]==0xFFFF) {
		bad_error |= 8 ;

		LOG(ERR,"BAD 0xFFFF bug -- 0x%08X",*((u_int *)dta_p)) ;

		u_int *d32 = (u_int *)start_p ;

		for(int i=0;i<32;i++) {
			LOG(ERR,"... %d = 0x%04X",i,d32[i]) ;
		}

		
		dta_p++ ;	// this is super bad for streaming!@@
	}
#endif

//	LOG(TERR,"... 0x%X 0x%X",dta_p[0],dta_p[1]) ;

	if(dta_p[0]==0xE800) {
		trgd_event = 0 ;
		return 0 ;	// no triggered
	}
	else {
		trgd_event = 1 ;
		return 1 ;
	}


} 


// how==0 : OK
// else: some error
int fcs_data_c::event_end(int how)
{
	if(!trgd_event) return 0 ;

	if(rdo_map_loaded && (ch_mask_seen != rdo_map[sector-1][rdo-1].ch_mask)) {
		bad_error |= 0x10 ;
		LOG(ERR,"%d: event_end: %d: S%02d:%d: mask not-complete 0x%llX (T %d)",id,events,sector,rdo,ch_mask_seen,token) ;
	}


	return 0 ;
}

int fcs_data_c::event_stream()
{
	u_int *d = (u_int *)dta_p ;
	u_int *d_stop = (u_int *)dta_stop ;
	u_int xings = 0 ;
	u_int deadtime = 0 ;
	u_int trgs = 0 ;
	u_char want_log = 0 ;
	u_char end_seen = 0 ;
	
	ch = -1 ;
	tb_cou = 0 ;
	d += 1 ;	// d[0] is now at version

//	LOG(TERR,"start packet 0x%08X, end 0x%08X",d[0],d_stop[0]) ;


	if(d_stop[0]==0x5C || d_stop[0]==0xAAAABBBB) ;
	else {
		want_log |= 1 ;

		for(int i=-8;i<=8;i++) {
			LOG(ERR,"%d: d_stop 0x%08X",i,d_stop[i]) ;
		}
	}


	u_int slice = d[1] ;
	u_int old_slice = d[2] ;
	u_int pkt_counter = d[5] ;

	u_int end_slice = 0xdeadbeef ;
	u_int pkt_status = 0xdeadbeef ;


	double mean[32] ;
	double rms[32] ;
	u_int cou[32] ;

	memset(mean,0,sizeof(mean)) ;
	memset(rms,0,sizeof(rms)) ;
	memset(cou,0,sizeof(cou)) ;


	u_int ch_dead[32] ;
	memset(ch_dead,0,sizeof(ch_dead)) ;

	// skip header
	d += 7 ;


//	for(int i=0;i<16;i++) {
//		LOG(TERR,"%d: 0x%08X",i,d[i]) ;
//	}


	u_int *adc32 = (u_int *)adc ;	// re-use instance storage 
	int adc_cou = 0 ;
	int adc_ch = 0 ;


	u_char sync = 0xFF ;

	u_int got_adc_end = 0 ;

	u_int bad_sync = 0 ;

	while(d < d_stop) {
		u_int dta = *d ;
		u_int t = dta>>28 ;	// type


		switch(t) {
		case 0xE :	// trg last
			//LOG(WARN,"TRG last 0x%08X at RHIC %u(%u), sync %d",dta,d[1],d[1]&0xFFFF,(dta>>20)&3) ;
			d++ ;
			break ;
		case 0xA :	// trg
			//LOG(WARN,"TRG norm 0x%08X at RHIC %u(%u), sync %d",dta,d[1],d[1]&0xFFFF,(dta>>20)&3) ;
			if((dta & 0xF)==0xC) {
				sync = (dta>>20) & 3 ;
			}
			trgs++ ;
			d++ ;
			break ;
		case 0xC :	// ADC slice-end
			adc_ch = dta & 0x1F ;

			if(got_adc_end & (1<<adc_ch)) {
				LOG(ERR,"Ch %d -- adc-slice_end already received",adc_ch) ;
			}
			got_adc_end |= (1<<adc_ch) ;

			LOG(WARN,"ADC slice-end 0x%08X at RHIC %u, sync %d, ch %d",dta,(dta>>8)&0xFFFF,(dta>>24)&3,
				dta&0x1F) ; 
			break ;
		case 0x8 :	// ADC ch end
			adc_ch = dta & 0x1F ;

			if(((dta>>24)&0x3)!= sync) {
				bad_sync++ ;
				//LOG(ERR,"Bad sync 0x%08X: ch %d: expect %d, have %d",dta,adc_ch,sync,(dta>>24)&3) ;
			}
			
			//LOG(TERR,"CH %d: adc_cou %d",adc_ch,adc_cou) ;

			// check for error: can happen if there's deadtime...
			if(adc_cou%4 || adc_cou==0) {
				LOG(ERR,"ADC ch %d, adc_cou %d??",adc_ch,adc_cou) ;
				for(int i=2;i>=-20;i--) {
					LOG(ERR,"   prev %d 0x%08X",i,d[i]) ;
				}
				adc_cou = 0 ;
				break ;
			}

			xings += adc_cou / 4 ;	// 2adcs per entry, 

			for(int i=0;i<adc_cou;i++) {
				u_int d_lo = adc32[i] & 0xFFF ;
				u_int d_hi = (adc32[i]>>16) & 0xFFF ;

				mean[adc_ch] += d_lo ;
				rms[adc_ch] += d_lo * d_lo ;
				cou[adc_ch]++ ;

				mean[adc_ch] += d_hi ;
				rms[adc_ch] += d_hi * d_hi ;
				cou[adc_ch]++ ;

#if 0
				if(((adc32[i]>>13)&3)!=sync) {
					LOG(ERR,"ADC ch %d: 0x%08X: bad sync: expect %d",adc_ch,adc32[i],sync) ;
				}
				if(((adc32[i]>>(13+16))&3)!=sync) {
					LOG(ERR,"ADC ch %d: 0x%08X: bad sync: expect %d",adc_ch,adc32[i],sync) ;
				}
#endif
			}

			adc_cou = 0 ;

			break ;
		case 0x9 :	// deadtime
			// can happen that the high value is already on the next slice!
			//
			if(adc_cou != 1) {	// must be 1 because I counted the 0x7....
				LOG(WARN,"DEAD but unfinished ADCs %d on ch %d",adc_cou,adc_ch) ;
			}

			adc_cou = 0 ;

			//for(int i=2;i>-8;i--) {
			//	LOG(ERR,"   dead %d 0x%08X",i,d[i]) ;
			//}

			{
				want_log |= 2 ;
				//u_int d_start = (d[-1]>>8)&0xFFFF ;
				u_int d_end = (dta>>8) & 0xFFFF ;
				u_int ch = dta & 0x1F ;

				//LOG(ERR,"DEAD 0x%08X 0x%08X - %u %u",dta,d[-1],d_end,d_start) ;

				deadtime += d_end ;
				ch_dead[ch] += d_end ;

			}
			break ;
		case 0xF :	// end
			end_slice = d[1] ;
			pkt_status = d[5] ;
			end_seen = 1 ;
			goto event_end ;
			break ;
		default :
			if(t&8) {
				LOG(ERR,"Unknown packet 0x%X",t) ;
			}
			else {
				if(adc_cou>=128) {
					LOG(ERR,"Too many ADCs %d after ch %d",adc_cou,adc_ch) ;
				}
				else {
					adc32[adc_cou] = dta ;
					adc_cou++ ;
				}
			}
			break ;
		}
		
		d++ ;
	}

	event_end: ;

	if(got_adc_end != 0xFFFFFFFF) {
		LOG(ERR,"ADCs got end 0x%08X",got_adc_end) ;
		want_log |= 4 ;
	}

	if(bad_sync) {
		LOG(ERR,"ADCs with bad sync %d",bad_sync) ;
		want_log |= 4 ;
	}

	if(want_log || !end_seen || deadtime || pkt_status || (slice==old_slice) || (slice != end_slice)) {

		LOG(ERR,"Packet 0x%08X:0x%08X:0x%08X:%d, shorts %d - status 0x%08X: xings %d, trgs %d, deadtime %d",
			slice,old_slice,end_slice,pkt_counter,dta_shorts,
			pkt_status,
			xings,trgs,deadtime) ;

		if(deadtime) {
			for(int i=0;i<32;i++) {
				if(ch_dead[i]) {
					LOG(ERR,"\t deadtime %d=%d",i,ch_dead[i]) ;
				}
			}
		}
	}


	for(int i=0;i<32;i++) {
		if(cou[i]==0) continue ;

		mean[i] /= cou[i] ;		
		rms[i] /= cou[i] ;

		rms[i] = sqrt(rms[i]-mean[i]*mean[i]) ;
	
		if(cou[i]>10000) LOG(TERR,"Ch %d: %f +- %f, cou %d -- %d",i,mean[i],rms[i],cou[i],(int)(mean[i]*8.0+0.5)) ;

		if(run_type==1) printf("PED %d %f %f %d %d\n",i,mean[i],rms[i],cou[i],(int)(mean[i]*8.0+0.5)) ;
	}



	return 0 ;
}

// this gets called over and over again for each channel!
int fcs_data_c::event()
{

	if(version != 0x18110000) {
		if(version == 0x30050000) {
			return event_stream() ;
		}
		return event_pre_fy19() ;
	}

	if(!trgd_event) {
		event_end(0) ;
		return 0 ;
	}

	if(dta_p[0]==0xE800) {
		event_end(0) ;
		return 0 ;	// end of event
	}

	// this is pretty critical...
//	if(*dta_p == 0xFFFF) {
	while(*dta_p == 0xFFFF) {
		want_saved = 1 ;

		bad_error |= 0x20 ;

		LOG(ERR,"S%d:%d: events %d: BUG 0xFFFF: ch %d, bytes left %d",sector,rdo,events,ch_count,dta_stop-dta_p) ;
		LOG(ERR,"   0x%X 0x%X 0x%X",dta_p[1],dta_p[2],dta_p[3]) ;

//		u_short *dta_use = dta_p - 1000 ;
//		while(dta_use<dta_stop) {
//			printf("%d = 0x%04X\n",dta_stop-dta_use,*dta_use++) ;
//		}

		event_end(1) ;
		return 0 ;
		
		dta_p++ ;
	}

	// from class
	tb_cou = 0 ;
	ch = -1 ;

	ch_count++ ;

	u_int rhic_cou_xpect = hdr_rhic_counter & 0x7F ;
	u_int board_id_xpect = board_id & 0xFF ;

//	for(int i=-16;i<16;i++) {
//		LOG(TERR,"in event %d = 0x%04X",i,dta_p[i]) ;
//	}




	while(dta_p<dta_stop) {
		u_short h[0] ;
		u_int trg_word ;
		u_int rhic_cou ;
		u_int board ;
		u_char complain =  0 ;
		u_short *dbg_h = dta_p ;

		h[0] = *dta_p++ ;	// board,channel
		h[1] = *dta_p++ ;	// trigger cmd
		h[2] = *dta_p++ ;	// rhic_cou, token lo

		ch = h[0]&0x3F ;
		board = (h[0] >> 6) ;

		trg_word = ((h[2]&0xFF)<<12)|(h[1]) ;
		rhic_cou = h[2]>>8 ;

//		LOG(TERR,"ch %d: trg_word 0x%X, rhic_cou %d",ch,trg_word,rhic_cou) ;
//		LOG(TERR,"ch %d: 0x%X 0x%X 0x%X",ch,h[0],h[1],h[2]) ;

		//complain = 1 ;
		if(board_id_xpect != board) complain = 1 ;

		if(ch>36) complain = 1 ;	// Sep21: stage2 can have 37 chs
		else {
			if(ch_mask_seen & (1LL<<ch)) {
				if(realtime) LOG(ERR,"event %d: ch duplicate %d",events,ch) ;
				complain = 1 ;
			}
			ch_mask_seen |= (1LL<<ch) ;
		}

		if((hdr_trg_word!=trg_word)|(rhic_cou_xpect!=rhic_cou)) {
			complain = 1 ;
		}

		if(complain) {
			bad_error |= 0x40 ;

			if(realtime && err_count<10) {
				LOG(ERR,"%d: S%d:%d: Evt %d, ch %d[%d]: 0x%X 0x%05X %d expected: 0x%X 0x%05X %d seen",id,sector,rdo,
				    events,ch,ch_count,
				    board_id_xpect,hdr_trg_word,rhic_cou_xpect,
				    board,trg_word,rhic_cou) ;

				LOG(ERR,"%d:   0x%04X 0x%04X 0x%04X 0x%04X",id,dbg_h[-1],dbg_h[0],dbg_h[1],dbg_h[2]) ;
			}

			err_count++ ;
		}
	

		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;

			//if(ch==32 && tb_cou==2) LOG(TERR,".... ch %d = %d = 0x%X",ch,tb_cou,d) ;

			if(d==0xFFFF) {		// last item of adc_single
				//LOG(TERR,"... tb_cou %d: 0x%04X",tb_cou,d) ;
				break ;
			}
			else if(d & 0x8000) {
				bad_error |= 0x80 ;
				if(realtime) LOG(ERR,"... ch %d: tb_cou %d: 0x%04X",ch,tb_cou,d) ;
			}

			//protect structures
			if((u_int)tb_cou>=(sizeof(adc)/sizeof(adc[0]))) {
				bad_error |= 0x80 ;
				LOG(ERR,"Event too big, ch %d, tb %d",ch,tb_cou) ;
				event_end(1) ;
				return 0 ;
			}

			//if(tb_cou < 5) {
			//	LOG(TERR,"%d: %d = 0x%04X",ch,tb_cou,d) ;
			//}

			adc[tb_cou] = d ;	//but store the full data, with flags


			tb_cou++ ;
		}

		if(first_tb_cou==0) {
			first_tb_cou = tb_cou ;
		}
		else if(tb_cou != first_tb_cou) {
			bad_error |= 0x80 ;
			if(complain) LOG(ERR,"%d: ch length mismatch: expect %d, is %d: ch %d(%d)",rdo,first_tb_cou,tb_cou,ch,ch_count) ;
		}

		ana_ch() ;

		//LOG(TERR,"0x%08X 0x%08X 0x%08X",dta_p[0],dta_p[1],dta_p[2]) ;

		//LOG(TERR,"Ch %d, %d ADCs, trg 0x%05X",ch,tb_cou,trg_word) ;
		return 1 ;
	}

//	u_int rhic_end = (dta_p[1]<<16)|dta_p[2] ;
//	LOG(TERR,"RHIC ticks %u",rhic_end-rhic_start) ;

	//LOG(TERR,"0x%08X 0x%08X 0x%08X: 0x%08X",dta_p[0],dta_p[1],dta_p[2],rhic_end) ;	

	event_end(0) ;
	return 0 ;
}


int fcs_data_c::ana_ch()
{


	switch(run_type) {
	case 1 :
	case 2 :
//	case 5 :
		break ;
	default:
		return 0 ;
	}

	if(run_type==2 && sector==11 && trg_cmd==4) {
		static int first ;

		
		static u_char expect[3][37] ;
		
		if(first==0) {
		expect[0][0]=0xC1 ;
		expect[0][1]=0xC1 ;
		expect[0][2]=0xE1 ;
		expect[0][3]=0xE1 ;

		expect[1][34]=0xC1 ;
		expect[1][35]=0xC1 ;
		expect[1][36]=0x66 ;

		expect[2][34]=0xE1 ;
		expect[2][35]=0xE1 ;
		expect[2][36]=0x77 ;

		for(int i=0;i<20;i++) {
			expect[1][i]= i ;
			expect[2][i]=(1<<5)|i ;
		}

		for(int i=20;i<28;i++) {
			expect[1][i] = (1<<6)|(i-20) ;
			expect[2][i] = (1<<6)|(1<<5)|(i-20) ;
		}

		for(int i=28;i<34;i++) {
			expect[1][i] = (2<<6)|(i-28) ;
			expect[2][i] = (2<<6)|(1<<5)|(i-28) ;
		}

		first = 1 ;
		}

		int errs = 0 ;
		for(int tb=0;tb<tb_cou;tb++) {
			int r=rdo-5 ;
			int d = adc[tb] & 0xFF ;

			if(ch==36) {
				if(d && (expect[r][ch] != d)) {
					errs++ ;
				}
			}
			else if(expect[r][ch] != d) {
				errs++ ;
			}
		}
		

//		if(errs) {
			ped_lock() ;
			ped[sector-1][rdo-1].cou[ch]++ ;
			ped[sector-1][rdo-1].bad_4[ch] += errs ;
			ped_unlock() ;
//		}

		return 0 ;

	}

	if(ch>=32 || sector==11) return 0 ;

	ped_lock() ;

	u_int aaa[2] ;

	aaa[0] = 0xAAA ;
	aaa[1] = 0x555 ;

	if((adc[0] & 0xFFF)==0x555) {
		aaa[0] = 0x555 ;
		aaa[1] = 0xAAA ;
	}

	for(int tb=0;tb<tb_cou;tb++) {
		u_int iadc = adc[tb] & 0xFFF ;

		if(run_type==2) {	
			if(aaa[tb%2] != iadc) {
				ped[sector-1][rdo-1].bad_4[ch]++ ;
			}
		}

		double sadc = (double)iadc ;

		ped[sector-1][rdo-1].mean[ch] += sadc ;
		ped[sector-1][rdo-1].rms[ch] += sadc * sadc ;
		ped[sector-1][rdo-1].cou[ch]++ ;


		ped[sector-1][rdo-1].tmp_val_8[ch] += sadc ;
		ped[sector-1][rdo-1].tmp_cou_8[ch]++ ;

		
		if(ped[sector-1][rdo-1].tmp_cou_8[ch]==8) {
			double d = ped[sector-1][rdo-1].tmp_val_8[ch] ;

			ped[sector-1][rdo-1].mean_8[ch] += d ;
			ped[sector-1][rdo-1].rms_8[ch] += d*d ;
			ped[sector-1][rdo-1].cou_8[ch]++ ;

			ped[sector-1][rdo-1].tmp_val_8[ch] = 0.0 ;
			ped[sector-1][rdo-1].tmp_cou_8[ch] = 0 ; 
		}
	}

	ped_unlock() ;

	return 0 ;
}



int fcs_data_c::accum_pre_fy19(u_int ch, u_int tb, u_short sadc)
{
//	int fla ;

	//protect structures
	if(tb>=(sizeof(adc)/sizeof(adc[0]))) {
		return -1 ;
	}

	adc[tb] = sadc ;	//but store the full data, with flags


	if(ch>=32) return 0 ;	// skip non-ADC channels


//	fla = sadc >> 12 ;	// flags
	sadc &= 0xFFF ;	//zap the flags to get to raw ADC


	switch(run_type) {
	case 1 :
	case 2 :
//	case 5 :
		ped[sector-1][rdo-1].mean[ch] += (double)sadc ;
		ped[sector-1][rdo-1].rms[ch] += (double)sadc * (double)sadc ;
		ped[sector-1][rdo-1].cou[ch]++ ;


		ped[sector-1][rdo-1].tmp_val_8[ch] += (double)sadc ;
		ped[sector-1][rdo-1].tmp_cou_8[ch]++ ;

		if(ped[sector-1][rdo-1].tmp_cou_8[ch]==8) {
			double d = ped[sector-1][rdo-1].tmp_val_8[ch] ;

			ped[sector-1][rdo-1].mean_8[ch] += d ;
			ped[sector-1][rdo-1].rms_8[ch] += d*d ;
			ped[sector-1][rdo-1].cou_8[ch]++ ;

			ped[sector-1][rdo-1].tmp_val_8[ch] = 0.0 ;
			ped[sector-1][rdo-1].tmp_cou_8[ch] = 0 ; 
		}



		break ;
	}

	return 0 ;

}



void fcs_data_c::run_start(u_int run, int type)
{

	events = 0 ;
	err_count = 0 ;

	switch(run_type) {
	case 1 :
	case 2 :
//	case 5 :
		ped_start() ;
		break ;
	}

	if(id==0) {
		memset(&statistics,0,sizeof(statistics)) ;

		run_number = run ;
		run_type = type ;

		ped_mutex_init() ;
	}
	
}

void fcs_data_c::run_stop(int bad_ped)
{
	switch(run_type) {
	case 1 :
	case 2 :
//	case 5 :
		ped_stop(bad_ped) ;
		break ;
	}

}

// RDO-per-RDO
void fcs_data_c::ped_start()
{
	int r = rdo - 1 ;
	int s = sector -1 ;

	memset(ped[s][r].mean,0,sizeof(ped[s][r].mean)) ;
	memset(ped[s][r].rms,0,sizeof(ped[s][r].rms)) ;
	memset(ped[s][r].cou,0,sizeof(ped[s][r].cou)) ;

	memset(ped[s][r].bad_4,0,sizeof(ped[s][r].bad_4)) ;

	memset(ped[s][r].mean_8,0,sizeof(ped[s][r].mean_8)) ;
	memset(ped[s][r].rms_8,0,sizeof(ped[s][r].rms_8)) ;
	memset(ped[s][r].cou_8,0,sizeof(ped[s][r].cou_8)) ;

	memset(ped[s][r].tmp_val_8,0,sizeof(ped[s][r].tmp_val_8)) ;
	memset(ped[s][r].tmp_cou_8,0,sizeof(ped[s][r].tmp_cou_8)) ;

}

// RDO per RDO
void fcs_data_c::ped_stop(int bad_ped)
{
	char status[64] ;

	int s = sector - 1 ;
	int r = rdo - 1 ;
	u_int max_c = 0 ;


//	if(rdo_map[s][r].det >= 3) {	// trigger DEPs
//		LOG(WARN,"S%d:%d is a DEP/IO -- skipping ped_stop",sector,rdo) ;
//		return ;
//	}


	// check for bad pedestals... since we can have masked channels just find the max
	for(int c=0;c<32;c++) {
		if(ped[s][r].cou[c] > max_c) {
			max_c = ped[s][r].cou[c] ;
		}
	}

	if(max_c < 500) bad_ped |= 4 ;

	for(int c=0;c<32;c++) {

		if(ped[s][r].cou[c]) {
			ped[s][r].mean[c] /= ped[s][r].cou[c] ;
			ped[s][r].rms[c] /= ped[s][r].cou[c] ;

			ped[s][r].rms[c] -= ped[s][r].mean[c] * ped[s][r].mean[c] ;

			if(ped[s][r].rms[c] < 0.0) ped[s][r].rms[c] = 0.0 ;

			ped[s][r].rms[c] = sqrt(ped[s][r].rms[c]) ;
		}
		else {
			ped[s][r].mean[c] = -1.0 ;
			ped[s][r].rms[c] = 0.0 ;

		}

		if(ped[s][r].cou_8[c]) {
			ped[s][r].mean_8[c] /= ped[s][r].cou_8[c] ;
			ped[s][r].rms_8[c] /= ped[s][r].cou_8[c] ;

			ped[s][r].rms_8[c] -= ped[s][r].mean_8[c] * ped[s][r].mean_8[c] ;

			if(ped[s][r].rms_8[c] < 0.0) ped[s][r].rms_8[c] = 0.0 ;

			ped[s][r].rms_8[c] = sqrt(ped[s][r].rms_8[c]) ;


		}
		else {
			ped[s][r].mean_8[c] = -1.0 ;
			ped[s][r].rms_8[c] = 0.0 ;
		}


	}

	if(bad_ped) {
		strcpy(status," -- Bad: ") ;
		if(bad_ped & 1) strcat(status,"wrong_run_type,") ;
		if(bad_ped & 2) strcat(status,"beam_in_rhic,") ;
		if(bad_ped & 4) strcat(status,"not_enough_events") ;

		LOG(ERR,"S%d:%d pedestals %s",sector,rdo,status) ;
	}
	else {
		strcpy(status," -- Status OK") ;
	}

	//pedestal dump...
	FILE *pedf ;

	time_t now = time(0) ;
	struct tm *tm = localtime(&now) ;

	char fname[128] ;

	if(run_number) {
		sprintf(fname,"/RTScache/fcs_pedestals_s%02d_r%d_t%d_%08u_f%u.txt",sector,rdo,run_type,run_number,rhic_freq) ;
	}
	else {
		sprintf(fname,"/RTScache/fcs_pedestals_%d_%d_%d_%d_%d.txt",
			tm->tm_year+1900,
			tm->tm_mon+1,
			tm->tm_mday,
			tm->tm_hour,
			tm->tm_min) ;
	}

	pedf = fopen(fname,"w") ;
	if(pedf==0) {
		LOG(ERR,"Can't open %s [%s]",fname,strerror(errno)) ;
		return ;
	}

	int d = rdo_map[s][r].det ;
	int n = rdo_map[s][r].ns ;
	int p = rdo_map[s][r].dep ;

	fprintf(pedf,"# Sector %2d, RDO %d\n",sector,rdo) ;
	fprintf(pedf,"# Det %d, NS %d, DEP %d\n",d,n,p) ;
	fprintf(pedf,"# RUN %08u, type %d %s\n",run_number,run_type,status) ;
	fprintf(pedf,"# TIME %u\n",(unsigned int)now) ;
	char *ctm = ctime(&now) ;
	fprintf(pedf,"# DATE %s",ctm) ;
	fprintf(pedf,"# RHIC %u, FEE state %d\n",rhic_freq,fee_state) ;

	fprintf(pedf,"\n") ;

	int c_max ;

	if((s+1)==11) {
		if((r+1)==5) c_max = 4 ;
		else c_max = 37 ;
	}
	else c_max = 32 ;
	
	for(int c=0;c<c_max;c++) {
		int err = 0 ;
		double m = ped[s][r].mean[c] ;
		double rms = ped[s][r].rms[c] ;

		switch(run_type) {
		case 1 :
			if((m<6.0)||(m>200.0)||(rms<0.3)||(rms>1.0)) err = 1 ;
			break ;
		case 2 :
			if(ped[s][r].bad_4[c]) {
//			if((m != 2047.5)||(rms != 682.5)) {
				err = 1 ;
			}
			break ;
		}
		
		if(err) {
			LOG(ERR,"S%02d:%d ch %02d: ped %.1f, rms %.1f: bad cou %u",sector,rdo,c,m,rms,ped[s][r].bad_4[c]) ;
		}

		//LOG(TERR,"PEDs: S%02d:%d: %d: %.1f [0x%03X] %.2f - %.1f %.1f [cou %d]",sector,rdo,c,
		//    ped[s][r].mean[c],(int)ped[s][r].mean[c],
		//    ped[s][r].rms[c],
		//    ped[s][r].mean_8[c],ped[s][r].rms_8[c],ped[s][r].cou[c]) ;

		double rms8 = ped[s][r].rms_8[c] ;

		if(run_type==2) rms8 = ped[s][r].bad_4[c] ;
		
		fprintf(pedf,"%d %d %d %d %d %d %f %f %f %f\n",sector,rdo,d,n,p,c,
			ped[s][r].mean[c],ped[s][r].rms[c],
			ped[s][r].mean_8[c],rms8) ;

	}

	fclose(pedf) ;

}

// load_map MUST be called before!
int fcs_data_c::gain_from_cache(const char *fname)
{
	int ret ;
	const char *file_name ;
	struct stat sstat ;
	int is_dir ;

	if(!rdo_map_loaded) {
		LOG(ERR,"You must load the rdo map before!") ;
	}

	// set defaults!
	for(int s=0;s<FCS_SECTOR_COU;s++) {
		for(int i=0;i<8;i++) {
			for(int c=0;c<32;c++) {
				ped[s][i].el_gain[c] = 1.0 ;
				ped[s][i].et_gain[c] = 1.0 ;
			}
		}
	}

	if(fname==0) {
		file_name = "/RTS/conf/fcs" ;
	}
	else {
		file_name = fname ;
	}

	ret = stat(file_name,&sstat) ;
	if(ret<0) {
		LOG(ERR,"gain_from_cache: %s: [%s]",file_name,strerror(errno)) ;
		return -1 ;
	}

	if(sstat.st_mode & S_IFDIR) {
		is_dir = 1 ;
	}
	else if(sstat.st_mode & S_IFREG) {
		is_dir = 0 ;
	}
	else {
		LOG(ERR,"gain_from_cache: %s: incorrect file type",file_name) ;
		return -1 ;
	}
	
	for(int det_ix=0;det_ix<3;det_ix++) {	// ECAL, HCAL, FPRE
	for(int v=0;v<2;v++) {		// 0=electronics, 1=et
		char ff[128] ;

		if(!is_dir) {
			strncpy(ff,file_name,sizeof(ff)-1) ;
		}
		else {
			const char *c_det, *c_typ ;

			switch(det_ix) {
			case 0 :
				c_det = "ecal" ;
				break ;
			case 1 :
				c_det = "hcal" ;
				break ;
			default :
				c_det = "fpre" ;
				break ;
			}

			if(v==0) c_typ = "electronics" ;		// was "electronics"
			else c_typ = "et" ;			// was "et"

			sprintf(ff,"%s/fcs_%s_%s_gains.txt",file_name,c_det,c_typ) ;
		}

		FILE *f = fopen(ff,"r") ;
		if(f==0) {
			LOG(ERR,"gain_from_cache: %s [%s] (perhaps not an error)",ff,strerror(errno)) ;
			if(!is_dir) goto read_done ;
			continue ;
		}

		LOG(INFO,"gain_from_cache: Opened gains[%s] %s",v==0?"electronics":"Et",ff) ;


		while(!feof(f)) {
			char buff[128] ;

			if(fgets(buff,sizeof(buff),f)==0) continue ;

			if(buff[0]=='#') continue ;
			if(buff[0]==0) continue ;

			int ch ;
			int det, ns, dep ;
			float gain ;

			int ret = sscanf(buff,"%d %d %d %d %f",&det,&ns,&dep,&ch,&gain) ;

			if(ret!=5) continue ;

			if(is_dir && (det_ix != det)) {
				LOG(WARN,"det expect %d, det in file %d",det_ix,det) ;
				continue ;
			}


				
			int s = det_map[det][ns][dep].sector - 1 ;
			int r = det_map[det][ns][dep].rdo - 1 ;

			if(s<0 || r<0) continue ;	// bad map?
			if(ch < 0) continue ;		// really bad!

			if(v==0) {
				ped[s][r].el_gain[ch] = gain ;
			}
			else {
				ped[s][r].et_gain[ch] = gain ;
			}
		}

		fclose(f) ;
		if(!is_dir) goto read_done ;
	}}

	read_done: ;

	for(int s=0;s<FCS_SECTOR_COU;s++) {
	for(int i=0;i<8;i++) {
		for(int c=0;c<32;c++) {

			double d = ped[s][i].el_gain[c] * ped[s][i].et_gain[c] ;


			// pre FY20: 
			//ped[s][i].i_gain[c] = (u_int)(d*64.0+0.5) ;
			ped[s][i].i_gain[c] = (u_int)(d*256.0+0.5) ; // Akio changing to 4.8 fixed

			if(ped[s][i].i_gain[c]>4095) {	// 12 bit max!
				LOG(NOTE,"S%d:%d: ch %d -- gain correction too big",s+1,i+1,c,ped[s][i].i_gain[c]) ;
			}
			else {

			}
		}
	}
	}

	return 0 ;

}


// static; expect to have it called for each and every file so 
// don't clear stuff etc.

int fcs_data_c::ped_from_cache(const char *ff)
{
	FILE *f = fopen(ff,"r") ;
	if(f==0) {
		LOG(ERR,"ped_from_cache: can't open %s [%s]",ff,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"ped_from_cache: opened  %s",ff) ;

	while(!feof(f)) {
		char buff[256] ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]==0) continue ;

		int c ;
		float p,r,pp,rr ;
		int ss,rrd,dd,nn,ppp ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %f %f %f %f",&ss,&rrd,&dd,&nn,&ppp,&c,&p,&r,&pp,&rr) ;

		if(ret!=10) continue ;


		// single timebin: for ZS
		ped[ss-1][rrd-1].mean[c] =  p ;
		ped[ss-1][rrd-1].rms[c] = r ;
		ped[ss-1][rrd-1].cou[c] = 0 ;	// irrelevant when loading from file

		// 8xtimebin: for trigger
		ped[ss-1][rrd-1].mean_8[c] =  pp ;
		ped[ss-1][rrd-1].rms_8[c] = rr ;
		ped[ss-1][rrd-1].cou_8[c] = 0 ;	// irrelevant when loading from file

		u_short pppp = (u_short)(pp+0.5) ;

		//if(pppp) {
		//	LOG(TERR,"S%d:%d: %d",ss,rrd,pppp) ;
		//}

		ped[ss-1][rrd-1].i_ped[c] = pppp ;	// also for trigger

	}

	fclose(f) ;

	return 0 ;
}


int fcs_data_c::event_pre_fy19()
{
	tb_cou = 0 ;
	ch = -1 ;

//	trigger_tick = -1 ;
//	first_rhic_strobe_tick = -1 ;

	LOG(TERR,"event() version 0x%08X",version) ;
	return 0 ;

	while(dta_p<dta_stop) {

#if 0
		u_short h[3] ;


		for(int i=0;i<128;i++) printf("%d 0x%04X\n",i,dta_p[i]) ;

		

		h[0] = *dta_p++ ;	// adc_single ID

		if(h[0]==0xFD07 || h[0]==0x5800) {	//end of adc_single stream at 0x580000007
			break ;
		}

		if(version==0x28010518) dta_p++ ;	// the ID is doubled always...

		h[1] = *dta_p++ ;	// adc_single token
		h[2] = *dta_p++ ;	// adc_single rhic

		ch = h[0] & 0xF ;
#else
		//printf("+++ 0x%04X 0x%04X\n",dta_p[0],dta_p[1]) ;
		if((dta_p[0]==0xFD07) || (dta_p[0]==0x5800)) break ;
		if((dta_p[0]==0x0066) && (dta_p[1]==0x7788)) break ;


		ch = *dta_p & 0xF ;
		dta_p += 1 ;

		// and also skip the token for now
		dta_p += 2 ;
#endif
		//LOG(TERR,"H 0x%X 0x%X 0x%X (ch %2d)",h[0],h[1],h[2],ch) ;

		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;

			//printf("... %d = 0x%04X [%u]\n",tb_cou,d,d) ;

			//LOG(TERR,".... %d = 0x%X",tb_cou,d) ;

			if(d==0xFFFF) {		// last item of adc_single
				//LOG(TERR,"... tb_cou %d",tb_cou) ;
				break ;
			}

#if 0
			if(d & 0x2000) {
				if(first_rhic_strobe_tick < 0) {
					first_rhic_strobe_tick = tb_cou ;
					//LOG(TERR,"... first rhic strobe at %d",tb_cou) ;
				}
			}
			if(d & 0x8000) {
				if(trigger_tick < 0) {
					trigger_tick = tb_cou ;
					//LOG(TERR,"... trigger tick at %d",tb_cou) ;
				}
			}
#endif

//			accum(ch,tb_cou,d&0xFFF) ;
			if(accum_pre_fy19(ch,tb_cou,d)<0) {
				LOG(ERR,"Event too big, ch %d, tb %d",ch,tb_cou) ;
				return 0 ;
			}
			tb_cou++ ;
		}

		//LOG(TERR,"0x%08X 0x%08X 0x%08X",dta_p[0],dta_p[1],dta_p[2]) ;

		//LOG(TERR,"Ch %d, %d ADCs",ch,tb_cou) ;
		return 1 ;
	}

//	u_int rhic_end = (dta_p[1]<<16)|dta_p[2] ;
//	LOG(TERR,"RHIC ticks %u",rhic_end-rhic_start) ;

	//LOG(TERR,"0x%08X 0x%08X 0x%08X: 0x%08X",dta_p[0],dta_p[1],dta_p[2],rhic_end) ;	

	return 0 ;
}

int fcs_data_c::load_readout_map(const char *fname)
{
	char buff[256] ;
	const char *fn ;

	if(rdo_map_loaded==0) {
		LOG(ERR,"rdo_map not loaded!") ;
		return -1 ;
	}

	for(u_int dd=0;dd<4;dd++) {


	switch(dd) {
	case 0 :
		fn = "/RTS/conf/fcs/fcs_ecal_readout_map.csv" ;
		break ;
	case 1 :
		fn = "/RTS/conf/fcs/fcs_hcal_readout_map.csv" ;
		break ;
	case 2 :
		fn = "/RTS/conf/fcs/fcs_fpre_readout_map.csv" ;
		break ;
	case 3 :
		fn = "/RTS/conf/fcs/fcs_main_readout_map.csv" ;
		break ;
	}
		

	

	FILE *f = fopen(fn,"r") ;

	if(f) LOG(INFO,"load_readout_map: opened %s",fn) ;
	else {
		LOG(ERR,"load_readout_map: %s [%s]",fn,strerror(errno)) ;
		return -1 ;
	}

	while(!feof(f)) {
		u_int adet, id ;
		u_int row, col ;
		u_int det, ns, dep, ch ;
		u_int crt, slt ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;
		if(buff[0]==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %d %d %d %d",
				 &adet,&id,&row,&col,
				 &det,&ns,&crt,&slt,&dep,&ch) ;

		if(ret!=10) continue ;


		if(det != dd) {
			LOG(ERR,"expect det %d, not %d",dd,det) ;
			continue ;
		}

		if(ns>=2) {
			LOG(ERR,"bad ns %d",ns) ;
			continue ;
		}

		if(dep>=24) {
			LOG(ERR,"bad dep %d",dep) ;
			continue ;
		}

		if(ch>=32) {
			LOG(ERR,"bad ch %d",ch) ;
			continue ;
		}

		int sec = det_map[det][ns][dep].sector ;
		int rdo = det_map[det][ns][dep].rdo ;

		rdo_map[sec-1][rdo-1].ch[ch].id = id ;
		rdo_map[sec-1][rdo-1].ch[ch].row = row ;
		rdo_map[sec-1][rdo-1].ch[ch].col = col ;
		rdo_map[sec-1][rdo-1].crt = crt ;
		rdo_map[sec-1][rdo-1].slt = slt ;
	}

	fclose(f) ;

	}

	return 0 ;
}


int fcs_data_c::load_sc_map(const char *fname)
{
	char buff[256] ;
	const char *fn ;

	if(rdo_map_loaded==0) {
		LOG(ERR,"rdo_map not loaded!") ;
		return -1 ;
	}

	for(int s=0;s<10;s++) {
	for(int r=0;r<8;r++) {
	for(int c=0;c<32;c++) {
		rdo_map[s][r].ch[c].sc_sipm = 0xFF ;
	}}}

	for(u_int dd=0;dd<3;dd++) {


	switch(dd) {
	case 0 :
		fn = "/RTS/conf/fcs/fcs_ecal_sc_map.csv" ;
		break ;
	case 1 :
		fn = "/RTS/conf/fcs/fcs_hcal_sc_map.csv" ;
		break ;
	case 2 :
		fn = "/RTS/conf/fcs/fcs_pres_sc_map.csv" ;
		break ;
	case 3 :
		fn = "/RTS/conf/fcs/fcs_main_sc_map.csv" ;
		break ;
	}
		
#if 0
	if(dd==2) {	// SPECIAL HACKS FOR EPD Splitter!
		LOG(INFO,"load_sc_map: constructing FPRE map") ;

		for(int s=0;s<10;s++) {
		for(int r=0;r<8;r++) {
			if(rdo_map[s][r].det != 2) continue ;	// skip non preshower

			int dep = rdo_map[s][r].dep ;

			for(int c=0;c<32;c++) {
				rdo_map[s][r].ch[c].sc_sipm = 0 ;	// make them ALL active
				rdo_map[s][r].ch[c].sc_dep = dep ;
				rdo_map[s][r].ch[c].sc_add = 0 ;
				rdo_map[s][r].ch[c].sc_bra = 0 ;
			}

			int ns = rdo_map[s][r].ns ;


			// and now kill some of them according to Akio's Aug-2021 recipe

			if(ns==0) {	// North
				rdo_map[s][r].ch[16].sc_sipm= 0xFF ;
			}
			else {
				rdo_map[s][r].ch[0].sc_sipm = 0xFF ;
			}

			if(dep==0) {
				for(int c=0;c<16;c++) {
					rdo_map[s][r].ch[c].sc_sipm = 0xFF ;
				}
			}
			else if(dep==5) {
				for(int c=16;c<32;c++) {
					rdo_map[s][r].ch[c].sc_sipm = 0xFF ;
				}

			}

		}}

		return 0 ;
	}

#endif	

	FILE *f = fopen(fn,"r") ;

	if(f) LOG(INFO,"load_sc_map: opened %s",fn) ;
	else {
		LOG(ERR,"load_sc_map: %s [%s]",fn,strerror(errno)) ;
		return -1 ;
	}

	while(!feof(f)) {
		u_int adet, id ;
		u_int row, col ;
		u_int det, ns, dep ;	//NOTE: this is the DEP where the FEE is connected!
		u_int bra, add, sipm ;


		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;
		if(buff[0]==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %d %d %d %d",
				 &adet,&id,&row,&col,
				 &det,&ns,&dep,&bra,&add,&sipm) ;

		if(ret!=10) continue ;


		if(det != dd) {
			LOG(ERR,"expect det %d, not %d",dd,det) ;
			continue ;
		}

		if(ns>=2) {
			LOG(ERR,"bad ns %d",ns) ;
			continue ;
		}

		if(dep>=24) {
			LOG(ERR,"bad dep %d",dep) ;
			continue ;
		}

		if(bra>=2) {
			LOG(ERR,"bad bra %d",bra) ;
			continue ;
		}

		int found_it = 0 ;

		for(int s=0;s<10;s++) {
		for(int r=0;r<8;r++) {
			if(rdo_map[s][r].det != dd) continue ;
			if(rdo_map[s][r].ns != ns) continue ;

			for(int c=0;c<32;c++) {
				if(rdo_map[s][r].ch[c].id != id) continue ;

				rdo_map[s][r].ch[c].sc_dep = dep ;
				rdo_map[s][r].ch[c].sc_bra = bra ;
				rdo_map[s][r].ch[c].sc_add = add ;


				// FPRE is special: Akio marked unused channels with non-0
				if(dd==2) {
					if(sipm!=0) {
						//LOG(TERR,"%d %d %d = %d",s,r,c,sipm) ;
						sipm = 0xFF ;
					}
				}

				rdo_map[s][r].ch[c].sc_sipm = sipm ;

				found_it = 1 ;
				goto done ;
			}
		}}

		done: ;

		if(!found_it) {
			LOG(ERR,"DET %d: can't find id %d",dd,id) ;
		}
	}

	fclose(f) ;

	}

	return 0 ;
}

int fcs_data_c::load_rdo_map(const char *fname)
{
//	if(id != 0) return 0 ;



	rdo_map_loaded = 0 ;
	memset(rdo_map,0,sizeof(rdo_map)) ;
	memset(det_map,0,sizeof(det_map)) ;


	// set up defaults so that non-existing entries in
	// the map file map to non-existing nodes
	for(int s=0;s<12;s++) {
	for(int r=0;r<8;r++) {
		rdo_map[s][r].det = 3 ;		//pseudo-main
		rdo_map[s][r].ns = 0 ;		//pseudo-side
		rdo_map[s][r].dep = 10 ;	// dummy!!!
	}}

	// physical crate map: done by hand
	for(int r=0;r<8;r++) {
		// Crate 0 

		rdo_map[0][r].crate = 0 ;
		rdo_map[0][r].slot = r ;

		rdo_map[1][r].crate = 0 ;
		rdo_map[1][r].slot = r+8 ;

		if(r < 4) {
			rdo_map[2][r].crate = 0 ;
			rdo_map[2][r].slot = r+16 ;
		}
		else {
			rdo_map[2][r].crate = 1 ;
			rdo_map[2][r].slot = r+16-4 ;
		}


		// Crate 1
		rdo_map[3][r].crate = 1 ;
		rdo_map[3][r].slot = r ;

		rdo_map[4][r].crate = 1 ;
		rdo_map[4][r].slot = r+8 ;


		// crate 2 aka Main
#if 0
	
		if(r < 3) {
			rdo_map[10][r].crate = 2 ;
			rdo_map[10][r].slot = r ;
		}
#else
		// moved fibers from 1,2,3 to 5,6,7

		if(r>=4 && r<=7) {
			rdo_map[10][r].crate = 2 ;
			rdo_map[10][r].slot = r - 4 ;
		}
#endif
		rdo_map[5][r].crate = 4 ;
		rdo_map[5][r].slot = r ;

		rdo_map[6][r].crate = 4 ;
		rdo_map[6][r].slot = r+8 ;

		if(r < 4) {
			rdo_map[7][r].crate = 4 ;
			rdo_map[7][r].slot = r+16 ;
		}
		else {
			rdo_map[7][r].crate = 3 ;
			rdo_map[7][r].slot = r+16-4 ;
		}


		// Crate 3
		rdo_map[8][r].crate = 3 ;
		rdo_map[8][r].slot = r ;

		rdo_map[9][r].crate = 3 ;
		rdo_map[9][r].slot = r+8 ;


	}

	//special cases and remaps
	rdo_map[5-1][2-1] = rdo_map[7-1][8-1] ;	// moved 7-8 to 5-2
	rdo_map[10-1][2-1] = rdo_map[10-1][8-1] ;	// moved 10-8 to 10-2
			

	if(fname==0) {
		fname = "/RTS/conf/fcs/fcs_daq_map.txt" ;
	}

	FILE *f = fopen(fname,"r") ;
	if(f == 0) {
		LOG(ERR,"Can't open map file %s [%s]",fname,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"load_rdo_map: opened %s",fname) ;


	while(!feof(f)) {
		char buff[128] ;

		int s,r,d,n,b ;
		unsigned long long mask ;

		buff[0] =0 ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;
		if(buff[0]==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d 0x%llX",&s,&r,&d,&n,&b,&mask) ;

		if(ret != 6) continue ;

//		if(sector != s) continue ;

		r-- ;
		s-- ;


		if((s<0) || (r<0)) {
			
			LOG(ERR,"Mapping S%d:%d --> %d,%d,%d, mask 0x%llX",s+1,r+1,d,n,b,mask) ;
			continue ;
		}

		// forward and reverse
		rdo_map[s][r].det = d ;
		rdo_map[s][r].ns = n ;
		rdo_map[s][r].dep = b ;
		rdo_map[s][r].ch_mask = mask ;

		det_map[d][n][b].sector = s+1 ;
		det_map[d][n][b].rdo = r+1 ;
	}

	rdo_map_loaded = 1 ;

	fclose(f) ;

	return 0 ;
}

u_short fcs_data_c::set_rdo(int rdo1)
{
	rdo = rdo1 ;

	return set_board_id() ;
} ;


u_short fcs_data_c::set_board_id()
{
	int sec = sector - 1 ;
	int r = rdo - 1 ;


	if((sec<0)||(r<0)) {
		LOG(ERR,"bad %d %d",sec,r) ;
	}

	int det = rdo_map[sec][r].det ;
	int ns = rdo_map[sec][r].ns ;
	int dep = rdo_map[sec][r].dep ;
	

	board_id = (sec<<11)|(r<<8)|(det<<6)|(ns<<5)|dep ;

//	LOG(TERR,"set_board_id: %d %d --> %d %d %d --> 0x%X",sector,rdo,det,ns,dep,board_id) ;

	return board_id ;
}

int fcs_data_c::load_bad_ch(const char *fname, int sector)
{
	memset(fcs_bad_ch,0,sizeof(fcs_bad_ch)) ;
	memset(fcs_bad_ch_all,0,sizeof(fcs_bad_ch_all)) ;

	if(fname==0) {
		fname = "/RTS/conf/fcs/bad_channels.txt" ;
	}

	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"fcs_load_bad_ch: %s [%s]",fname,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"fcs_load_bad_ch: %s",fname) ;

	while(!feof(f)) {
		char buff[1024] ;

//		LOG(ERR,"asd") ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

//		LOG(ERR,"--- %s",buff) ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;

		int s,r,det,ns,dep,ch,flag ;
		float ped, rms ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %f %f 0x%X",&s,&r,&det,&ns,&dep,&ch,&ped,&rms,&flag) ;

//		LOG(WARN,"ret [%s]",buff) ;

		if(ret != 9) continue ;

		if(sector==0) LOG(WARN,"Bad ch: S%02d:%d:%02d",s,r,ch) ;

		if((r<1)||(r>8)) continue ;
		if((ch<0)||(ch>33)) continue ;
		if((s<1)||(s>10)) continue ;

		fcs_bad_ch_all[s-1][r-1][ch] = 1 ;

		if(s != sector) continue ;

		LOG(WARN,"Bad ch: S%02d:%d:%02d",s,r,ch) ;

		fcs_bad_ch[r-1][ch] = 1 ;
	}

	fclose(f) ;

	return 0 ;

}
