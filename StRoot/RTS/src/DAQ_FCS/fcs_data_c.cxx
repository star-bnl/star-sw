#include <assert.h>
#include <sys/types.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <stdio.h>


#include <rtsLog.h>

#if 0
#include <rtsSystems.h>
#include <daqFormats.h>

#include <SFS/sfs_index.h>

#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#endif

#include "fcs_data_c.h"

static inline u_int sw16(u_int d)
{
        u_int tmp = d ;

        d >>= 16 ;

        d |= (tmp & 0xFFFF)<<16 ;

        return d ;
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

				hdr_event() ;

				return 1 ;
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
int fcs_data_c::hdr_event()
{
	u_short hdr_board_id ;

#if 0
	int cou ;	
	cou = dta_shorts - 8 + 4 ;
	if(cou > 10000) cou = 10000 ;
	for(int i=0;i<cou;i++) {
		LOG(TERR,"...data9802: %d = 0x%04X",i,dta_p[i]) ;
	}
#endif 

	//I will need the board id as a sector/id combo
	hdr_board_id = dta_p[3] ;

	// this won't work Offline
	if(realtime && (hdr_board_id != board_id)) {
		LOG(ERR,"evt %d: board_id: expected 0x%04X, received 0x%04X",events,board_id,hdr_board_id) ;
	}

	//extract trigger_word and rhic_counter
	hdr_trg_word = ((dta_p[5]&0xF)<<16) | dta_p[4] ;
	hdr_rhic_counter = (dta_p[7]<<16)|dta_p[6] ;


	sector = (hdr_board_id >> 11)+1 ;
	rdo = ((hdr_board_id >> 8) & 0x7)+1 ;

	LOG(DBG,"HDR: trg_word 0x%05X, %d",hdr_trg_word,hdr_rhic_counter) ;


	// skip to first datum
	dta_p += 8 ;

	if(dta_p[0]==0xEEEE && dta_p[1]==0xEEEE) {	// start of ASCII
		char ctmp[64] ;

		dta_p += 2 ;	// adjust
		u_int *d32 = (u_int *)dta_p ;

		int words = (dta_shorts - 8 - 2)/2 ;	// adjust

		LOG(NOTE,"ASCII contribution - words %d",words) ;

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
						LOG(TERR,"0x%X: \"%s\"",board_id,ctmp) ;
						cou = 0 ;
					}
					else {
						ctmp[cou] = c ;
						cou++ ;
					}
				}
			}

			dta_p += 2 ;

			if(asc==0xFFFFFFFF) {
				end_marker = 1 ;
				break ;
			}

		}

		ctmp[cou] = 0 ;
		if(!end_marker) {
			LOG(WARN,"%d: ASCII[%d] but no end-marker \"%s\"",0,cou,ctmp) ;
		}
		else {
			LOG(NOTE,"%d: ASCII[%d] \"%s\"",0,cou,ctmp) ;
		}

	}

//	LOG(TERR,"... 0x%X 0x%X",dta_p[0],dta_p[1]) ;

	if(dta_p[0]==0xE800) trgd_event = 0 ;
	else trgd_event = 1 ;

	return 0 ;	
} 


// this gets called over and over again for each channel!
int fcs_data_c::event()
{

	if(version != 0x18110000) {
		return event_pre_fy19() ;
	}

	if(!trgd_event) return 0 ;
	
	if(dta_p[0]==0xE800) return 0 ;	// end of event





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

		h[0] = *dta_p++ ;
		h[1] = *dta_p++ ;
		h[2] = *dta_p++ ;

		ch = h[0]&0x3F ;
		board = (h[0] >> 6) ;

		trg_word = ((h[2]&0xFF)<<12)|(h[1]) ;
		rhic_cou = h[2]>>8 ;

		if(realtime && (board_id_xpect != board)) complain = 1 ;
		
		if((hdr_trg_word!=trg_word)|(rhic_cou_xpect!=rhic_cou)) {
			complain = 1 ;
		}

		if(complain) {
			LOG(ERR,"Evt %d, ch %d[%d]: 0x%X 0x%05X %d expected: 0x%X 0x%05X %d seen",events,ch,ch_count,
			    board_id_xpect,hdr_trg_word,rhic_cou_xpect,
			    board,trg_word,rhic_cou) ;

		}
	
		while(dta_p<dta_stop) {
			u_short d = *dta_p++ ;

			//printf("... %d = 0x%04X [%u]\n",tb_cou,d,d) ;

			//if(tb_cou==0) LOG(TERR,".... ch %d = %d = 0x%X",ch,tb_cou,d) ;

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

		LOG(DBG,"Ch %d, %d ADCs, trg 0x%05X",ch,tb_cou,trg_word) ;
		return 1 ;
	}

//	u_int rhic_end = (dta_p[1]<<16)|dta_p[2] ;
//	LOG(TERR,"RHIC ticks %u",rhic_end-rhic_start) ;

	//LOG(TERR,"0x%08X 0x%08X 0x%08X: 0x%08X",dta_p[0],dta_p[1],dta_p[2],rhic_end) ;	

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


int fcs_data_c::accum(int ch, int tb, u_short sadc)
{
	if((u_int)tb>=sizeof(adc)/sizeof(adc[0])) {
		return -1 ;
	}

	adc[tb] = sadc ;	//but store the full data, with flags

	sadc &= 0xFFF ;	//zap the flags

	if((run_type==1) & (ch<32)) {
		//if(tb==0) LOG(TERR,"Accum: ch %d = %d",ch,sadc) ;

		ped.mean[ch] += (double)sadc ;
		ped.rms[ch] += (double)sadc * (double)sadc ;
		ped.cou[ch]++ ;
	}

	return 0 ;

}


void fcs_data_c::run_start(u_int run, int type)
{
	run_number = run ;
	run_type = type ;

	events = 0 ;
	ped_start() ;
}

void fcs_data_c::run_stop()
{
	if(run_type==1) ped_stop() ;

}

void fcs_data_c::ped_start()
{
	memset(&ped,0,sizeof(ped)) ;
}


void fcs_data_c::ped_stop()
{

	for(int c=0;c<32;c++) {
		if(ped.cou[c]) {
			ped.mean[c] /= ped.cou[c] ;
			ped.rms[c] /= ped.cou[c] ;

			ped.rms[c] = sqrt(ped.rms[c]-ped.mean[c]*ped.mean[c]) ;
		}
		else {
			ped.mean[c] = -1.0 ;
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
		fprintf(pedf,"#RUN %u\n",run_number) ;
		fprintf(pedf,"#TIME %u\n",(unsigned int)now) ;
		char *ctm = ctime(&now) ;
		fprintf(pedf,"#DATE %s",ctm) ;
		
		fprintf(pedf,"\n") ;

		for(int c=0;c<32;c++) {
			LOG(TERR,"PEDs: S%02d:%d: %2d %.3f %.3f %.3f %.3f %.3f",sector,rdo,c,ped.mean[c],ped.rms[c],
				fee_currents[c][0],fee_currents[c][1],fee_currents[c][2]) ;

		
			fprintf(pedf,"%2d %f %f %.3f %.3f %.3f\n",c,ped.mean[c],ped.rms[c],
				fee_currents[c][0],fee_currents[c][1],fee_currents[c][2]) ;

		}

		fclose(pedf) ;


}
