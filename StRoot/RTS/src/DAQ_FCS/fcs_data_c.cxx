#include <assert.h>
#include <sys/types.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdio.h>


#include <rtsLog.h>

#include "fcs_data_c.h"

static inline u_int sw16(u_int d)
{
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;

        return d ;
}

double fcs_data_c::fee_currents[8][32][3] ;	// 8 RDOs, 32 channel
struct fcs_data_c::fcs_ped_t fcs_data_c::ped[8] ;	// 8 RDO
u_int fcs_data_c::run_number ;
u_int fcs_data_c::run_type ;
struct fcs_data_c::rdo_map_t fcs_data_c::rdo_map[8] ;


	
int fcs_data_c::zs_start(u_short *buff)
{
	int thr ;
	int l_cou ;
	int l_pre, l_post ;

	if(ch==32) {	// this is the trigger data channel, no need to go pre/post
		thr = 0 ;
		l_cou = 1 ;
		l_pre = 0 ;
		l_post = 0 ;
	}
	else {
		thr = (int)(ped[rdo-1].mean[ch] + n_sigma * ped[rdo-1].rms[ch] + 0.5) ;
		l_cou = n_cou ;
		l_pre = n_pre ;
		l_post = n_post ;
	}

	int t_cou = 0 ;
	int t_start = 0 ;
	int t_stop ;
	int got_one = 0 ;

	
	for(int i=0;i<tb_cou;i++) {
		short d = adc[i] & 0xFFF ;

//		printf("CH %d: %d = %d < thr %d: t_start %d, t_cou %d\n",ch,i,d,thr,t_start,t_cou) ;

		if(d <= thr) {	// datum needs to be greater than the threshold
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



	//finalize
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


	if(got_one==0) return 0 ;	// nothing found

	u_short *dp ;

	dp = (u_short *)buff ;


	
	int i_ped ;

	if(ch==32) i_ped = 0 ;
	else i_ped = (int)(ped[rdo-1].mean[ch]+0.5) ;

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
				*dp++ = i ;
				t_cou_p = dp++ ;
				t_start = i ;
			}

			short i_adc = adc[i] & 0xFFF ;
			short fla = adc[i] >> 12 ;

			i_adc -= i_ped ;
			if(i_adc < 0) i_adc = 0 ;

			i_adc |= (fla<<12) ;

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

//	printf("... ZS is now %d shorts\n",(int)(dp-dstart)) ;

	return dp-dstart ;	// shorts


}


/*******************************/
int fcs_data_c::start(u_short *d16, int shorts)
{
	u_int *d ;

	//class members
	events++ ;

	dta_start = dta_p = d16 ;
	dta_stop = d16 + shorts ;
	dta_shorts = shorts ;

	d = (u_int *)d16 ;

	rhic_start = 0;
	ch_count = 0 ;
	ch_mask_seen = 0 ;

	
//	for(int i=0;i<16;i++) {
//		LOG(TERR,"...start: %d = 0x%04X",i,d16[i]) ;
//	}


	//version = 0 ;	// unknown...

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
			if(d16[2]==0x9801) {	// May-2018 to Dec-2018
				version = 0x18050000 ;	// 15-May-2018

				dta_p = ((u_short *)d)+6 ;	// this is for May18-Dec18


				for(int i=0;i<16;i++) {
					LOG(TERR,"...data9801: %d = 0x%04X",i,dta_p[i]) ;
				}


				return 1 ;
			}
			else if(d16[2]==0x9802) { 	// Nov 2018
				version = 0x18110000 ;

				dta_p = d16 ;

				return hdr_event() ;

			}
			LOG(ERR,"uknown version 0x%04X",d16[2]) ;
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

//	u_short hdr_board_id ;


//	for(int i=0;i<32;i++) {
//		LOG(TERR,"... %d 0x%04X",i,dta_p[i]) ;
//	}


	//I will need the board id as a sector/id combo
	hdr_board_id = dta_p[3] ;

	sector = ((hdr_board_id >> 11) & 0x1F)+1 ;
	rdo = ((hdr_board_id >> 8) & 0x7)+1 ;

	hdr_det = (hdr_board_id >> 6) & 0x3 ;
	hdr_ns = (hdr_board_id >> 5) & 1 ;
	hdr_dep = hdr_board_id & 0x1F ;


//	LOG(TERR,"... 0x%X S%d:%d %d %d %d",hdr_board_id,sector,rdo,hdr_det,hdr_ns,hdr_dep) ;


	// this won't work Offline
	if(realtime && (hdr_board_id != board_id)) {
		LOG(ERR,"evt %d: board_id: expected 0x%04X, received 0x%04X",events,board_id,hdr_board_id) ;
	}

	//extract trigger_word and rhic_counter
	hdr_trg_word = ((dta_p[5]&0xF)<<16) | dta_p[4] ;
	hdr_rhic_counter = (dta_p[7]<<16)|dta_p[6] ;


	LOG(DBG,"HDR: trg_word 0x%05X, %d",hdr_trg_word,hdr_rhic_counter) ;


	// skip to first datum
	dta_p += 8 ;

	if(dta_p[0]==0xEEEE && dta_p[1]==0xEEEE) {	// start of ASCII
		char ctmp[64] ;

		dta_p += 2 ;	// adjust
		u_int *d32 = (u_int *)dta_p ;

		int words = (dta_shorts - 8 - 2)/2 ;	// adjust

		LOG(TERR,"ASCII contribution - words %d[%d]: sector %d, rdo %d, hdr_trg_word 0x%X",words,dta_shorts,sector,rdo,hdr_trg_word) ;

		int end_marker = 0 ;
		u_int cou = 0 ;
		for(int i=0;i<words;i++) {
			u_int asc = d32[i] ;

			if((asc&0xFF00FFFF)==0xF5009800) {
				char c = (asc>>16)&0xFF ;

				if(cou>sizeof(ctmp)) ;
				else {
					if(c=='\n') {
						ctmp[cou] = 0 ;
						LOG(TERR,"S%d:%d:%d: \"%s\"",sector,rdo,events,ctmp) ;
						cou = 0 ;
					}
					else {
						ctmp[cou] = c ;
						cou++ ;
					}
				}
			}
			else if(asc != 0xFFFFFFFF) {
				LOG(WARN,"ASCII wha %d: 0x%08X",i,asc) ;
			}

			dta_p += 2 ;

			if(asc==0xFFFFFFFF) {
				end_marker = 1 ;
				break ;
			}

		}

		ctmp[cou] = 0 ;
		if(!end_marker) {
			LOG(WARN,"S%d:%d:%d: ASCII[%d] but no end-marker \"%s\"",sector,rdo,events,cou,ctmp) ;
		}
		else if(cou) {
			LOG(WARN,"S%d:%d:%d: ASCII[%d] \"%s\"",sector,rdo,events,cou,ctmp) ;
		}

	}
	else if(dta_p[0]==0xFFFF && dta_p[1]==0xFFFF) {	// bug: end-of-ascii without ascii
		LOG(WARN,"S%d:%d:%d: ASCII bug: 0x%X, 0x%X",sector,rdo,events,hdr_trg_word,dta_p[2]) ;
		for(int i=0;i<32;i++) {
			LOG(TERR,"... %d = 0x%04X",i,start_p[i]) ;
		}
		dta_p += 2 ;
	}
#if 0
	else if(dta_p[0]==0xFFFF) {
		LOG(ERR,"BAD 0xFFFF bug") ;
		dta_p++ ;
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

	if(ch_mask_seen != 0x1FFFFFFFFLL) {
		LOG(ERR,"event_end: %d: mask not-complete 0x%llX",events,ch_mask_seen) ;
	}

	if(ch_count != 33) {
		LOG(ERR,"event_end: %d: chs found %d",events,ch_count) ;
	}

	return 0 ;
}

// this gets called over and over again for each channel!
int fcs_data_c::event()
{

	if(version != 0x18110000) {
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
		LOG(ERR,"S%d:%d: events %d: BUG 0xFFFF",sector,rdo,events) ;
		//event_end(1) ;
		//return 0 ;
		dta_p++ ;
	}

	// from class
	tb_cou = 0 ;
	ch = -1 ;

	ch_count++ ;

	u_int rhic_cou_xpect = hdr_rhic_counter & 0x7F ;
	u_int board_id_xpect = board_id & 0xFF ;

//	for(int i=0;i<16;i++) {
//		LOG(TERR,"in event %d = 0x%04X",i,dta_p[i]) ;
//	}




	while(dta_p<dta_stop) {
		u_short h[0] ;
		u_int trg_word ;
		u_int rhic_cou ;
		u_int board ;
		u_char complain =  0 ;
		u_short *dbg_h = dta_p ;

		h[0] = *dta_p++ ;	// 0x20cc
		h[1] = *dta_p++ ;
		h[2] = *dta_p++ ;

		ch = h[0]&0x3F ;
		board = (h[0] >> 6) ;

		trg_word = ((h[2]&0xFF)<<12)|(h[1]) ;
		rhic_cou = h[2]>>8 ;

		//complain = 1 ;
		if(realtime && (board_id_xpect != board)) complain = 1 ;

		if(ch>32) complain = 1 ;
		else {
			if(ch_mask_seen & (1LL<<ch)) {
				LOG(ERR,"event %d: ch duplicate %d",events,ch) ;
				complain = 1 ;
			}
			ch_mask_seen |= (1LL<<ch) ;
		}

		if((hdr_trg_word!=trg_word)|(rhic_cou_xpect!=rhic_cou)) {
			complain = 1 ;
		}

		if(complain) {
			LOG(ERR,"S%d:%d: Evt %d, ch %d[%d]: 0x%X 0x%05X %d expected: 0x%X 0x%05X %d seen",sector,rdo,
			    events,ch,ch_count,
			    board_id_xpect,hdr_trg_word,rhic_cou_xpect,
			    board,trg_word,rhic_cou) ;

			LOG(ERR,"   0x%04X 0x%04X 0x%04X 0x%04X",dbg_h[-1],dbg_h[0],dbg_h[1],dbg_h[2]) ;

		}
	
		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;

			//LOG(TERR,".... ch %d = %d = 0x%X",ch,tb_cou,d) ;

			if((d & 0x8000) && (d != 0xFFFF)) {
				LOG(ERR,"... ch %d: tb_cou %d: 0x%04X",ch,tb_cou,d) ;
			}

			if(d==0xFFFF) {		// last item of adc_single
				//LOG(TERR,"... tb_cou %d: 0x%04X",tb_cou,d) ;
				break ;
			}

			if(d & 0x2000) {
				if(first_rhic_strobe_tick < 0) {
					first_rhic_strobe_tick = tb_cou ;
					//LOG(TERR,"... first rhic strobe at %d",tb_cou) ;
				}
			}
			if(d & 0x4000) {
				if(trigger_tick < 0) {
					trigger_tick = tb_cou ;
					//LOG(TERR,"... trigger tick at %d",tb_cou) ;
				}
			}

			if(accum(ch,tb_cou,d)<0) {
				LOG(ERR,"Event too big, ch %d, tb %d",ch,tb_cou) ;
				event_end(1) ;
				return 0 ;
			}

			tb_cou++ ;
		}

		//LOG(TERR,"0x%08X 0x%08X 0x%08X",dta_p[0],dta_p[1],dta_p[2]) ;

		LOG(DBG,"Ch %d, %d ADCs, trg 0x%05X",ch,tb_cou,trg_word) ;
		return 1 ;
	}

//	u_int rhic_end = (dta_p[1]<<16)|dta_p[2] ;
//	LOG(TERR,"RHIC ticks %u",rhic_end-rhic_start) ;

	//LOG(TERR,"0x%08X 0x%08X 0x%08X: 0x%08X",dta_p[0],dta_p[1],dta_p[2],rhic_end) ;	

	event_end(0) ;
	return 0 ;
}


int fcs_data_c::accum(u_int ch, u_int tb, u_short sadc)
{
	//protect structures
	if(tb>=(sizeof(adc)/sizeof(adc[0]))) {
		return -1 ;
	}

	adc[tb] = sadc ;	//but store the full data, with flags

	sadc &= 0xFFF ;	//zap the flags

	if(ch>=32) return 0 ;	// skip non-ADC channels

	switch(run_type) {
	case 1 :
	case 5 :
		ped[rdo-1].mean[ch] += (double)sadc ;
		ped[rdo-1].rms[ch] += (double)sadc * (double)sadc ;
		ped[rdo-1].cou[ch]++ ;
		break ;
	}

	return 0 ;

}


void fcs_data_c::run_start(u_int run, int type)
{
	run_number = run ;
	run_type = type ;

	events = 0 ;

	switch(run_type) {
	case 1 :
	case 5 :
		ped_start() ;
		break ;
	}
}

void fcs_data_c::run_stop()
{
	switch(run_type) {
	case 1 :
	case 5 :
		ped_stop() ;
		break ;
	}

}

void fcs_data_c::ped_start()
{
	int i = rdo - 1 ;

	memset(ped[i].mean,0,sizeof(ped[i].mean)) ;
	memset(ped[i].rms,0,sizeof(ped[i].rms)) ;
	memset(ped[i].cou,0,sizeof(ped[i].cou)) ;


}


void fcs_data_c::ped_stop()
{

	for(int c=0;c<32;c++) {
		if(ped[rdo-1].cou[c]) {
			ped[rdo-1].mean[c] /= ped[rdo-1].cou[c] ;
			ped[rdo-1].rms[c] /= ped[rdo-1].cou[c] ;

			ped[rdo-1].rms[c] -= ped[rdo-1].mean[c] * ped[rdo-1].mean[c] ;

			if(ped[rdo-1].rms[c] < 0.0) ped[rdo-1].rms[c] = 0.0 ;

			ped[rdo-1].rms[c] = sqrt(ped[rdo-1].rms[c]) ;
		}
		else {
			ped[rdo-1].mean[c] = -1.0 ;
			ped[rdo-1].rms[c] = 0.0 ;
		}

	}

	//pedestal dump...
	FILE *pedf ;

	time_t now = time(0) ;
	struct tm *tm = localtime(&now) ;

	char fname[128] ;

	if(run_number) {
		sprintf(fname,"/RTScache/fcs_pedestals_s%02d_r%d_%08u.txt",sector,rdo,run_number) ;
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

	fprintf(pedf,"#Sector %2d, RDO %d\n",sector,rdo) ;
	fprintf(pedf,"#RUN %08u, type %d\n",run_number,run_type) ;
	fprintf(pedf,"#TIME %u\n",(unsigned int)now) ;
	char *ctm = ctime(&now) ;
	fprintf(pedf,"#DATE %s",ctm) ;
	
	fprintf(pedf,"\n") ;

	for(int c=0;c<32;c++) {
		LOG(TERR,"PEDs: S%02d:%d: %2d %.3f %.3f %.3f %.3f %.3f",sector,rdo,c,ped[rdo-1].mean[c],ped[rdo-1].rms[c],
			fee_currents[rdo-1][c][0],fee_currents[rdo-1][c][1],fee_currents[rdo-1][c][2]) ;

		
		fprintf(pedf,"%2d %f %f %.3f %.3f %.3f\n",c,ped[rdo-1].mean[c],ped[rdo-1].rms[c],
			fee_currents[rdo-1][c][0],fee_currents[rdo-1][c][1],fee_currents[rdo-1][c][2]) ;

	}

	fclose(pedf) ;

}

int fcs_data_c::gain_from_cache(const char *fname)
{
	char ff[128] ;

	if(id != 0) return 0 ;	// just ID0


	// set defaults!
	for(int i=0;i<8;i++) {
		for(int c=0;c<32;c++) {
			ped[i].gain[c] = 1.0 ;
			ped[i].i_gain[c] = (1<<6) ;
		}
	}


	if(fname==0) {
		sprintf(ff,"/RTS/conf/fcs/fcs_gains.txt") ;
	}
	else {
		strcpy(ff,fname) ;
	}

	FILE *f = fopen(ff,"r") ;
	if(f==0) {
		LOG(ERR,"Can't open %s [%s]",ff,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"S%d: opened gains %s",sector,ff) ;


	while(!feof(f)) {
		char buff[128] ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]==0) continue ;

		int sec, rdo, ch ;
		float gain ;

		int ret = sscanf(buff,"%d %d %d %f",&sec,&rdo,&ch,&gain) ;

		if(ret!=4) continue ;

		if(sec != sector) continue ;

		ped[rdo-1].gain[ch] = gain ;
		ped[rdo-1].i_gain[ch] = (int)(64.0 * gain + 0.5) ;
			
		LOG(NOTE,"Gains S%d:%d:%d = %d",sector,rdo,ch,ped[rdo-1].i_gain[ch]) ;
		
	}

	fclose(f) ;

	return 0 ;

}

int fcs_data_c::ped_from_cache(const char *fname)
{
	char ff[128] ;



	if(fname==0) {
		sprintf(ff,"/RTScache/fcs_pedestals_s%02d_r%d.txt",sector,rdo) ;
	}
	else {
		strcpy(ff,fname) ;
	}

	FILE *f = fopen(ff,"r") ;
	if(f==0) {
		LOG(ERR,"Can't open %s [%s]",ff,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"S%d:%d: opened pedestals %s",sector,rdo,ff) ;

	while(!feof(f)) {
		char buff[128] ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]==0) continue ;

		int c ;
		float p,r,d ;

		int ret = sscanf(buff,"%d %f %f %f %f %f",&c,&p,&r,&d,&d,&d) ;

		if(ret!=6) continue ;

		ped[rdo-1].mean[c] =  p ;
		ped[rdo-1].rms[c] = r ;
		ped[rdo-1].cou[c] = 0 ;

		ped[rdo-1].i_ped[c] = (u_short)(p*8.0+0.5) ;

//		ped[rdo-1].f_gain[c] = 1.0 ;
//		ped[rdo-1].gain[c] = (1<<6) ;
		
	}

	fclose(f) ;

	return 0 ;
}


int fcs_data_c::event_pre_fy19()
{
	tb_cou = 0 ;
	ch = -1 ;

	trigger_tick = -1 ;
	first_rhic_strobe_tick = -1 ;

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

//			accum(ch,tb_cou,d&0xFFF) ;
			if(accum(ch,tb_cou,d)<0) {
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

int fcs_data_c::load_rdo_map(const char *fname)
{
	if(id != 0) return 0 ;

	memset(rdo_map,0,sizeof(rdo_map)) ;

	FILE *f = fopen(fname,"r") ;
	if(f == 0) {
		LOG(ERR,"Can't open map file %s [%s]",fname,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"Opened %s",fname) ;

	while(!feof(f)) {
		char buff[128] ;

		int s,r,d,n,b ;

		buff[0] =0 ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(buff[0]=='\n') continue ;
		if(buff[0]==0) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d",&s,&r,&d,&n,&b) ;

		if(ret != 5) continue ;

		if(sector != s) continue ;

		r-- ;

		LOG(TERR,"Mapping S%d:%d --> %d,%d,%d",sector,r+1,d,n,b) ;

		rdo_map[r].det = d ;
		rdo_map[r].ns = n ;
		rdo_map[r].dep = b ;
	}

	fclose(f) ;

	return 0 ;
}

void fcs_data_c::set_rdo(int rdo1)
{
	rdo = rdo1 ;

	set_board_id() ;
} ;


u_short fcs_data_c::set_board_id()
{
	int sec = sector - 1 ;
	int r = rdo - 1 ;

	int det = rdo_map[r].det ;
	int ns = rdo_map[r].ns ;
	int dep = rdo_map[r].dep ;
	
	board_id = (sec<<11)|(rdo<<8)|(det<<6)|(ns<<5)|dep ;

	return board_id ;
}


		
