#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <math.h>

#include <rtsLog.h>
#include <DAQ_READER/daq_dta.h>


#include "itpcInterpreter.h"
#include "itpcPed.h"

static void hammingdecode(unsigned int buffer[2], bool& error, bool& uncorrectable, bool fix_data) ;

itpcInterpreter::itpcInterpreter()
{
	evt_ix = 0 ;
	realtime = 0 ;
	dbg_level = 0 ;

	fee_evt_cou = 0 ;
	run_number = 0 ;

	fout = 0 ;

	memset(fee,0,sizeof(fee)) ;
	ped_c = 0 ;
}

void itpcInterpreter::run_start(u_int run)
{
	run_number = run ;
	evt_ix = 0 ;
	fee_evt_cou = 0 ;

	if(fout) {
		fprintf(fout,"***** RUN_START: %08u\n",run_number) ;
		fflush(fout) ;
	}

	LOG(DBG,"Starting run %08u",run_number) ;
}

void itpcInterpreter::run_stop()
{

	if(fout) {
		fprintf(fout,"***** RUN_STOP: %08u\n",run_number) ;
		fflush(fout) ;
		fclose(fout) ;
		fout = 0 ;
	}

	LOG(INFO,"Stopping run %08u after %d/%d events",run_number,fee_evt_cou,evt_ix) ;
	
}


void itpcInterpreter::start_event(u_int bytes)
{
	evt_ix++ ;	//events will start from 1

	evt_bytes = bytes ;	//store length

	//zap stuff
	word_ix = 0 ;

	status = 0 ;

	state = S_IDLE ;

	fee_port = -1 ;
	
	d_cou = -1 ;

	sampa_bx = -1 ;

	ascii_cou = 0 ;

	memset(evt_err,0,sizeof(evt_err)) ;
}

void itpcInterpreter::stop_event()
{
	for(int i=0;i<8;i++) {
		if(evt_err[i]) LOG(ERR,"%d: event errors[%d] = %u",rdo_id,i,evt_err[i]) ;
	}
	
}


static inline u_int sw16(u_int d)
{
	u_int tmp = d ;

	d >>= 16 ;

	d |= (tmp & 0xFFFF)<<16 ;

	return d ;
}


#if 0
int itpcInterpreter::get_l2(char *addr, int words, struct daq_trg_word *trg, int do_log)
{
	u_int err = 0 ;
	u_int trg_fired ;
	u_int v_fired ;
	int trl_ix = -1 ;
	int trg_cou ;
	int t_cou = 0 ;
	u_int evt_status ;

	u_int *d = (u_int *)addr + 4 ;	// skip header

	// NOTE that since Dec 2017 the 16 bit words are swapped!!!

	if(sw16(d[0]) != 0x001CCCCC) {	// expect start-comma
		err |= 1 ;
		goto err_end ;
	}

//	LOG(TERR,"   0x%08X 0x%08X 0x%08X", sw16(d[1]),sw16(d[2]),sw16(d[words-1])) ;
	
	if(sw16(d[1]) != 0x98000004) { // not a triggered event
		trg[0].t = 4096 ;	// a "log" event
		trg[0].daq = 0 ;
		trg[0].trg = 0 ;
	
		return 1 ;	
	}

	if(sw16(d[2]) != 0x12340000) {	// wrong version
		err |= 2 ;
		goto err_end ;
	}

	trg_fired = sw16(d[3]) ;
	v_fired = sw16(d[4]) ;	// if 0, no prompt trigger	



	// this gets messy so we won't check
	/*
	if(sw16(d[words-1]) != 0xFFFF005C) {	// expect stop-comma
		err |= 0x10 ;
		goto err_end ;
	}
	*/

	//find trailer

	for(int i=(words-1);i>=0;i--) {
		if(sw16(d[i]) == 0x98001000) {
			trl_ix = i ;
			break ;
		}
	}


	if(trl_ix < 0) {
		err |= 0x20 ;
		goto err_end ;
	}

	trl_ix++ ;

	if(sw16(d[trl_ix++]) != 0xABCD0000) {
		err |= 0x40 ;
		goto err_end ;
	}

	evt_status = sw16(d[trl_ix++]) ;
	trg_cou = sw16(d[trl_ix++]) ;

	trg[t_cou].reserved[0] = trg_fired ;
	t_cou++ ;
	for(int i=0;i<trg_cou;i++) {
		trg[t_cou].reserved[0] = sw16(d[trl_ix++]) ;
		t_cou++ ;
	}
	

	if(evt_status) {
		LOG(ERR,"%d: %d/%d -- evt status 0x%08X",rdo_id,d[6],d[5],evt_status) ;

	}

	for(int i=0;i<t_cou;i++) {
		u_int v = trg[i].reserved[0] ;
		u_int t ;

		t = ((v>>8)&0xF)<<8 ;
		t |= ((v>>12)&0xF)<<4 ;
		t |= ((v>>16)&0xF) ;

		trg[i].trg = v & 0xF ;
		trg[i].daq = (v>>4) & 0xF ;
		trg[i].t = t ; 

		if(trg[i].reserved[0]==0) {
			trg[i].t = 4097 ;	// event without L0
		}


		if(trg[i].trg>=4 && trg[i].trg<=13) {
			if((v&0xFFF00000) != 0x04300000) {
				LOG(WARN,"... %d/%d = 0x%08X: %d %d %d",i,t_cou,trg[i].reserved[0],trg[i].t,trg[i].trg,trg[i].daq) ;
			}
		}

#if 0
		if(i==0) {
			LOG(OPER,"... %d/%d [%d/%d] = 0x%08X: %d %d %d [%d words]",d[6],d[5],i,t_cou,trg[i].reserved[0],trg[i].t,trg[i].trg,trg[i].daq,words) ;
		}
		else {
			LOG(TERR,"... %d/%d [%d/%d] = 0x%08X: %d %d %d [%d words]",d[6],d[5],i,t_cou,trg[i].reserved[0],trg[i].t,trg[i].trg,trg[i].daq,words) ;
		}
#endif
	}

	

	return t_cou ;

	err_end:;

	LOG(ERR,"%d: Error in get_l2 %d",rdo_id,err) ;

	return 0 ;
	
}
#endif


/* We start with 0x980000008 */
u_int *itpcInterpreter::fee_scan(u_int *start, u_int *end)
{
	u_int *d = start ;
	u_int dd ;
	int hdr ;
	int trg ;
	int cfg ;

	d++ ;	// skip over 0x98000008 ;

	// next is FEE port (from 0)
	fee_port = *d++ ;	// fee port
	fee_port++ ;		// start from 1

	fee_id = -1 ;
	trg = 0 ;	// event is triggered
	hdr = 0 ;
	cfg = 0 ;
	ascii_cou = 0 ;

	// We are at the start of FEE data:
	u_int dd_x = d[0] & 0xFFC000FF ;	// command

	// I should now point at 0x80xx0001
	if(dd_x != 0x80000001) {
		// HAPPENS
		//LOG(WARN,"Missing start 0x%08X",d[0]) ;

		//for(int i=0;i<32;i++) {
		//	LOG(TERR,"%d = 0x%08X",i-8,d[i-8]) ;
		//}

		fee_bx = 0 ;	// dummy

		cfg++ ;
		trg++ ;
	}

	while(d<end) {
		dd = *d++ ;
		dd_x = dd & 0xFFC000FF ;	// just commands
		u_int dd_a ;

		LOG(NOTE,"FEE #%d: %u 0x%08X",fee_port,d-start,dd) ;

		// let's get non-FEE headers out of the way first
		switch(dd_x) {
		case 0x980000F8 :	// End-of-FEE status (from RDO)
			if(d[1]) {
				LOG(ERR,"%d: FEE #%d[%d] END: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,fee_id,
				    d[0],d[1],d[2],d[3],d[4],d[5],d[6]) ;
			}
			else {
				LOG(NOTE,"FEE #%d[%d] END: 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X 0x%08X",fee_port,fee_id,
				    d[0],d[1],d[2],d[3],d[4],d[5],d[6]) ;
			}

			if((d[-2]&0xFFC00000) != 0x40000000) {
				// might fire due to a bug in readout
				if((d[-2]&0xFFC00000) != 0x80000000) {				
					LOG(ERR,"%d: FEE #%d: Before END 0x%08X",rdo_id,fee_port,d[-2]) ;
				}
				else {
					//OFTEN
					//LOG(WARN,"%d: FEE #%d: Before END 0x%08X",rdo_id,fee_port,d[-2]) ;
				}
			}

			// search for RDO-END-FEE marker
			for(int i=0;i<16;i++) {
				dd = *d++  ;
				if(dd == 0x58000009) {
					dd-- ;	// go back one
					goto stop_loop ;
				}
			}

			LOG(ERR,"%d: FEE #%d: can't find RDO-END",rdo_id,fee_port) ;
			goto stop_loop ;

		}


		
		int t_fee_id = (dd >> 16) & 0x3F ;
		if(fee_id<0) {
			if((dd_x & 0xFF000000)==0x80000000) fee_id = t_fee_id ;
		}
		else {
			if(fee_id != t_fee_id) {
				evt_err[ERR_DIFFERENT_FEE_IDS]++ ;
				LOG(NOTE,"FEE #%d: IDs differ: expect %d, is %d, 0x%08X",fee_port,fee_id,t_fee_id,dd) ;
			}
		}

		switch(dd_x) {
		case 0xA00000A0 :	// ASCII start
			break ;
		case 0x600000A0 :	// ASCII end
			break ;
		case 0xA00000B0 :	// binary start
			LOG(ERR,"%d: FEE #%d: Unexpected 0x%08X",rdo_id,fee_port,dd) ;
			break ;
		case 0x600000B0 :	// binary end
			LOG(ERR,"%d: FEE #%d: Unexpected 0x%08X",rdo_id,fee_port,dd) ;
			break ;
		case 0x80000001 :	// START EVENT (also hdr)
			// this can be missing! and can also show up at the very end!
			{
				u_int bx_lo = d[4] & 0xFFFF ;
				u_int bx_hi = d[5] & 0xFFFF ;

				bx_lo |= (bx_hi<<16) ;

				fee_bx = bx_lo ;
				//LOG(TERR,"BX %u 0x%08X",bx_lo&0xFFFFF,bx_lo) ;
			}

			//for(int jj=-2;jj<6;jj++) {
			//	LOG(TERR,"... %d = 0x%08X",jj,d[jj]) ;
			//}
			cfg++ ;
			trg++ ;	
			break ;
		case 0x60000001 :	// end event hdr
			//LOG(TERR,"END event %d %d",cfg,trg) ;
			cfg = 0 ;
			if(trg==1) {
				fee_id = (dd>>16)&0x3F ;
				//LOG(TERR,"FEE #%d(%d) start SAMPA: FEE BX %u (0x%08X)",fee_port,fee_id,fee_bx&0xFFFFF,fee_bx) ;

				for(int i=0;i<4;i++) {
					u_int expect_mask ;
					u_int *cur_d = d ;

					switch(i) {
					case 0 :
					case 2 :
						expect_mask = 0x0000FFFF ;
						break ;
					default :
						expect_mask = 0xFFFF0000 ;
						break ;
					}

					//if(i==0 || i==2) found_ch_mask = 0 ;
					found_ch_mask = 0 ;

					d = sampa_lane_scan(d,end) ;
					//LOG(TERR,"fee_port %d: lane %d was %d words",fee_port,i,d-cur_d) ;
					
					//if(i==1 || i==3) {
						if(found_ch_mask != expect_mask) {
							//LOG(ERR,"expect 0x%08X",expect_mask) ;
							LOG(ERR,"%d: fee_port %d: missing ch after lane %d: 0x%08X",
							    rdo_id,fee_port,i,found_ch_mask) ;
						}
					//}


				}
				break ;
			}
			LOG(ERR,"%d: FEE #%d: %u 0x%08X -- odd",rdo_id,fee_port,d-start,dd) ;
			break ;
		case 0xA0000001 :	// start event trailer 
			cfg++ ;
			break ;
		case 0x40000001 :	// END EVENT (also trailer)
			if(dd & 0xFF00) {
				LOG(ERR,"%d: FEE #%d: EVENT error 0x%02X",rdo_id,fee_port,(dd>>8)&0xFF) ;
				fee[fee_port].event_errs++ ;
			}
			cfg = 0 ;
			break ;
		case 0x80000002 :	// START SEND_CONFIG (also header)
			hdr++ ;
			cfg++ ;
			break ;
		case 0x60000002 :	// end send_config header
			cfg = 0 ;
			break ;
		case 0xA00000FA :	// start config data
			cfg++ ;
			break ;
		case 0x600000FA :	// end config data
			cfg = 0 ;
			break ;
		case 0xA00000EC :	// start monitoring header
			break ;
		case 0x600000EC :	// end monitoring header
			break ;
		case 0xA00000ED :	// start monitoring trailer
			break ;
		case 0x600000ED :	// end monitoring trailer
			break ;
		case 0xA0000002 :	// start send_config trailier
			cfg++ ;
			break ;
		case 0x40000002 :	// END SEND_CONFIG event/trailer
			cfg = 0 ;
			if(dd & 0xFF00) {
				fee[fee_port].send_config_errs++ ;
				LOG(ERR,"FEE #%d: SEND_CONFIG error 0x%02X",fee_port,(dd>>8)&0xFF) ;
			}
			break ;
		case 0x80000003 :	// START of pedestal confirm
			//LOG(TERR,"Start Ped, fee %d",fee_port) ;
			{
				u_int delta = ((d[3]&0xFFFF)<<16)|(d[2]&0xFFFF) ;
				int for_me = (d[0] & 0x8000)?1:0 ;
				int timebins = d[0] & 0x3FF ;
				int ch = (d[0]>>10)&0x3F ;
				LOG(TERR,"FEE #%d: ped: for me %d, ch %d, timebins %d, chsum %u, delta %u",fee_port,for_me,ch,timebins,d[1]&0xFFFF,delta) ;
			}
			d += 5 ;

			break ;
		case 0x40000003 :	// END of pedestal confirm event
			if(dd & 0xFF00) {
				fee[fee_port].pedestal_errs++ ;
				LOG(ERR,"FEE #%d: PEDESTAL error 0x%02X",fee_port,(dd>>8)&0xFF) ;
			}

			LOG(ERR,"Shouldn't be 0x%X",dd) ;
			break ;
		case 0x80000004 :
			break ;
		case 0x40000004 :
			break ;
		default :	// none of the above...

			dd_a = dd & 0xFFC0FF00 ;	// for ASCII
			if(dd_a == 0x0000F500 || dd_a==0x00800000) {	// ASCII char
				u_char c = dd & 0xFF ;
				//LOG(TERR,"... [0x%08X]",dd) ;

				if(isprint(c) || isspace(c)) ;
				else c = '?' ;

				ascii_dta[ascii_cou++] = c ;
				if(c=='\n') {
					ascii_dta[ascii_cou++] = 0 ;
					if(fout) {
						fprintf(fout,"#%02d: %s",fee_port,ascii_dta) ;
						fflush(fout) ;
					}


					if(fee_port > 0) {	// this must be!
						u_int id1 = 0 ;
						if(strncmp(ascii_dta,"1Wire:",6)==0) {
							char *id = strstr(ascii_dta,"ID") ;
							if(id) {
								if(sscanf(id,"ID 0x%X",&id1)==1) {
									//printf("********** port %d, WIRE 0x%08X\n",fee_port,id1) ;
									fee[fee_port].wire1_id = id1 ;
								}
							}
						}
						else if(strncmp(ascii_dta,"Padplane ",9)==0) {
							if(sscanf(ascii_dta,"Padplane %d",&id1)==1) {
								//printf("********** port %d, PADPLANE %d\n",fee_port,id1) ;
								fee[fee_port].padplane_id = id1 ;
							}
						}
						   
						if(strstr(ascii_dta,"ERROR")) {
							int last = strlen(ascii_dta) - 1 ;
							if(ascii_dta[last]=='\n') ascii_dta[last] = 0 ;
							LOG(ERR,"%d: FEE #%d: [%s]",rdo_id,fee_port,ascii_dta) ;
						}

					}


					ascii_cou = 0 ;
				}
				break ;
			}
			if(cfg) break ;

			evt_err[ERR_UNKNOWN]++ ;
			LOG(NOTE,"FEE #%d: %u 0x%08X -- unkown",fee_port,d-start,dd) ;
			break ;
		}

	}

	stop_loop: ; 


	if(fee_id < 0) {
		LOG(ERR,"%d: fee_id %d, FEE #%d [0x%08]: format error [%u 0x%08X]",rdo_id,fee_id,fee_port,(u_int)fee_port,d-start,*d) ;
		return d ;
	}


	return d ;
}

int itpcInterpreter::sampa_ch_scan()
{
	int err = 0 ;
	int t_stop_last = -1 ;
	int s,r,p ;
	
	if(ped_c) {
		ped_c->sector = s = sector_id - 1 ;
		ped_c->rdo = r = rdo_id - 1 ;
		ped_c->port = p = fee_port - 1 ;
		ped_c->fee_id = fee_id ;

		ped_c->ch_start(fee_ch) ;
	}


	for(int i=0;i<tb_cou;) {
//		LOG(TERR,"i   %d",i) ;

		int t_cou = tb_buff[i++] ;
		int t_start = tb_buff[i++] ;



		int t_stop = t_start + t_cou - 1 ;

//		LOG(TERR,"...%d %d %d",t_start,t_cou,t_stop) ;

		if(t_start <= t_stop_last) {
                        LOG(ERR,"%d: t_start %d, t_cou %d, t_stop %d, t_stop_last %d",rdo_id,t_start,t_cou,t_stop,t_stop_last) ;
			err = -1 ;
			break ;
		}

		if(t_stop > 512) {
                        LOG(ERR,"%d: t_start %d, t_cou %d, t_stop %d, t_stop_last %d",rdo_id,t_start,t_cou,t_stop,t_stop_last) ;
			err = -2 ;
			break ;
		}

		t_stop_last = t_stop ;

		if(ped_c) {
			for(int t=t_start;t<=t_stop;t++) {
				u_short adc = tb_buff[i++] ;

				ped_c->accum(s,r,p,fee_id,fee_ch,t,adc) ;
				//ped_c->accum(t,adc) ;
			}
		}
		else {
			i += t_stop - t_start + 1 ;
		}
	}


	if(ped_c) ped_c->ch_done(err) ;

	if(err) {
		for(int i=0;i<tb_cou;i++) {
			LOG(NOTE,"%d/%d = %u",i,tb_cou,tb_buff[i]) ;
		}
	}

	return err ;
}


u_int *itpcInterpreter::sampa_lane_scan(u_int *start, u_int *end)
{
	u_int d ;
	u_int h[6] ;
	u_int type, words ;
	u_int word32 ;
	u_char lane ;
	u_int err = 0 ;
	u_int *data ;
	int ch_loop_cou = 0 ;
	int l_sampa_bx ;
	u_int lane_hdr ;
	bool parity_err ;
	bool hamming_err ;

	data = start ;
	// first datum is the 0xB....
	d = *data++ ;
	lane_hdr = d ;

	fee_id = (d >> 16) & 0x3F ;
	lane = (d>>24) & 0x3 ;

	//LOG(TERR,"SAMPA lane: FEE %3d, lane %d [0x%08X]",fee_id,lane,d) ;

	new_ch:;		// start of channel data

	ch_loop_cou++ ;		// count how many channels we found

	d = *data++ ;

	if(d & 0xC0000000) {
		err |= 0x100 ;
		LOG(ERR,"%d: Bad Hdr 1",rdo_id) ;
		d = *data++ ;
	}

	h[0] = (d >> 20) & 0x3FF ;
	h[1] = (d >> 10) & 0x3FF ;
	h[2] = d & 0x3FF ;
	
	d = *data++ ;

	if(d & 0xC0000000) {
		err |= 0x200 ;
		LOG(ERR,"%d: Bad Hdr 2",rdo_id) ;
		d = *data++ ;
	}

	h[3] = (d >> 20) & 0x3FF ;
	h[4] = (d >> 10) & 0x3FF ;

	h[5] = d & 0x3FF ;

//	if(h[5] != 0xAB) {
//		err |= 1 ;
//		goto err_ret ;
//	}


	type = h[0] >> 7 ;
	words = h[1] ;
	sampa_id = h[2] & 0xF ;
	sampa_ch = (h[2]>>4) & 0x1F ;

	if(sampa_id & 1) fee_ch = sampa_ch + 32 ;
	else fee_ch = sampa_ch ;

	l_sampa_bx = (h[2]&0x200) >> 9 ;
	l_sampa_bx |= (h[3]<<1) ;
	l_sampa_bx |= (h[4]&0x1FF)<<11 ;


	// check parity
	int p_cou = 0 ;
	for(int i=0;i<=4;i++) {
		for(int j=0;j<10;j++) {
			if(h[i] & (1<<j)) p_cou++ ;
		}
	}

	if(p_cou&1) {
		parity_err = 1 ;
	}
	else {
		parity_err = 0 ;
	}


	unsigned long long hc = ((long long)h[4]<<40)|((long long)h[3]<<30)|(h[2]<<20)|(h[1]<<10)|h[0];
	u_int hh[2] ;
	hh[0] = hc & 0x3FFFFFFF ;
	hh[1] = (hc>>30) ;

	bool uncorrectable ;
	hammingdecode(hh, hamming_err,uncorrectable,0) ;


	if(parity_err || hamming_err) {
		LOG(ERR,"%d:%d: Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d",rdo_id,fee_port,type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err) ;
		// and I should do something here!
		err |= 1 ;
		goto err_ret ;
	}
	else {
		//LOG(TERR,"Type %d, words %d, SAMPA %d:%d, BX %u, errors %d:%d, fee_port %d",type,words,sampa_id,sampa_ch,l_sampa_bx,parity_err,hamming_err,fee_port) ;
	}


	switch(type) {
	case 0 :	// heartbeat
		if(words != 0) {
			err |= 2 ;
			goto err_ret ;
		}
		if(sampa_ch != 21) {
			err |= 4 ;
			goto err_ret ;
		}
		break ;
	case 4 :	// physics
//		LOG(WARN,"Type %d, words %d, SAMPA %d:%d, BX %u",type,words,sampa_id,sampa_ch,l_sampa_bx) ;
		break ;
	case 1 :	// trigger overrun
		LOG(ERR,"%d: Type %d, words %d, SAMPA %d:%d, BX %u [lane_hdr 0x%08X],fee_port %d",rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx,lane_hdr,fee_port) ;
		break ;
	default :
		LOG(ERR,"%d: Type %d, words %d, SAMPA %d:%d, BX %u [lane_hdr 0x%08X]",rdo_id,type,words,sampa_id,sampa_ch,l_sampa_bx,lane_hdr) ;
		err |= 8 ;
		goto err_ret ;
	}


	if(sampa_bx < 0) {
		sampa_bx = l_sampa_bx ;
	}
	else if(sampa_bx != l_sampa_bx) {
		LOG(NOTE,"Expect %u, got %u",sampa_bx,l_sampa_bx) ;
	}


	//now go for the data...
	word32 = words / 3 + (words%3?1:0) ;	

	//LOG(TERR,"words %d, word32 %d",words,word32) ;
	tb_cou = words ;

	if(ped_c && ped_c->want_data) {	// I will handle my own data
		ped_c->sector = sector_id ;
		ped_c->rdo = rdo_id ;
		ped_c->port = fee_port ;

		if(ped_c->do_ch(fee_id, fee_ch, data, words)<0) {
			err |= 0x200 ;
		}

		data += word32 ;
	}
	else {
		int t_cou = 0 ;	// local
		for(u_int i=0;i<word32;i++) {
			d = *data++ ;

			if((d&0xC0000000)) {
				LOG(ERR,"%d:%d: %d:%d sampa data word %d/%d = 0x%08X",rdo_id,fee_port,sampa_id,sampa_ch,i,word32,d) ;
				err |= 0x10 ;
				i-- ;

				if(d==0x980000F8) {	//end of event!!!
					return data-1 ;
				}
				continue ;
			}

			//LOG(TERR,"... 0x%08X",d) ;

			tb_buff[t_cou++] = (d>>20) & 0x3FF ;
			tb_buff[t_cou++] = (d>>10) & 0x3FF ;
			tb_buff[t_cou++] = d & 0x3FF ;
		}

		if(sampa_ch_scan()<0) {
			err |= 0x200 ;
		}
	}

	//note this hack
	//tb_cou = words ;

	

	if(err) {
		LOG(ERR,"%d: Last SAMPA: FEE #%d: %d:%d = 0x%08X [err 0x%0x]",rdo_id,fee_port,sampa_id,sampa_ch,*data,err) ;
	}
	else {
		found_ch_mask |= (1<<sampa_ch) ;
		LOG(NOTE,"Last SAMPA %d:%d = 0x%08X [err 0x%0x]",sampa_id,sampa_ch,*data,err) ;
	}
		
	err = 0 ;	// clear error before we go into a new channel

	if(*data & 0xC0000000) {
		data++ ;
		return data ;	//keep the signature
	}
	else {
		goto new_ch ;
	}

	err_ret:

	LOG(ERR,"%d: ERR 0x%X: 0x%03X 0x%03X 0x%03X 0x%03X 0x%03X 0x%03X",rdo_id,err,
	    h[0],h[1],h[2],h[3],h[4],h[5]) ;

	return data ;
}


void itpcInterpreter::fee_dbase(const char *fname) 
{
	FILE *db ;
	char hname[128] ;
	char uname[128] ;

	//date,time
	
	//hostname
	strcpy(hname,"host-unknown") ;
	gethostname(hname,sizeof(hname)) ;

	//user
	strcpy(uname,"user-unknown") ;
	getlogin_r(uname,sizeof(uname)) ;


	if(fname==0) {
		fname = "/RTScache/fee_dbase.csv" ;

	}

	db = fopen(fname,"a") ;

	if(db==0) {
		LOG(ERR,"%s: %s [%s]",__PRETTY_FUNCTION__,fname,strerror(errno)) ;
		return ;
	}

	time_t tm = time(0) ;

	char *ct = ctime(&tm) ;
	ct[strlen(ct)-1] = 0 ;

	for(int i=1;i<=16;i++) {
		if(fee[i].wire1_id == 0) continue ;	//skip non-FEEs

		//1wire1, padplane, port, RDO 1wire, RDO #, host, uname,date, comment
		fprintf(db,"0x%08X,%2d,%2d,0x%08X,%d,%s,%s,%s,%s\n",fee[i].wire1_id,fee[i].padplane_id,i,
			rdo_wire1_id,rdo_id,uname,hname,ct,"NC") ;
	}
	fprintf(db,"\n") ;	//one more NL

	fclose(db) ;

}


int itpcInterpreter::rdo_scan(u_int *data, int words)
{
//	int status = 0 ;
	int log_start = 0 ;

	u_int *data_end = data + words ;
	u_int *data_start = data ;
	u_int rh_xing_start = 0 ;
	u_int flags = 0 ;

	u_int d  ;

	char mon_string[512] ;
	int mon_cou = 0 ;


	// the data is already SWAPPED if processed in the sector brokers!!!
	for(int i=0;i<16;i++) {
		LOG(NOTE,"...%d/%d = 0x%08X",i,words,data[i]) ;

		if((data[i] == 0xCCCC001C)||(data[i] == 0x001CCCCC)) {
			data = data + i ;
			break ;
		}
	}

	if(data[0]==0xCCCC001C) {	// need swapping!!!!
		LOG(NOTE,"swapping") ;
		for(int i=0;i<words;i++) {
			data[i] = sw16(data[i]) ;
		}
	}

	d = *data++ ;

	switch(d) {
	case 0xFFFF001C	:	//old style
	case 0x001CCCCC :	// new style
		flags |= 1 ;
		break ;
	default:
		LOG(ERR,"%d: First word is not a START comma!? [0x%08X 0x%08X 0x%08X]",rdo_id,data[-1],data[0],data[1]) ;
	}

	while(data<data_end) {
		u_int hdr ;

		word_ix = data - data_start ;

		d = *data++ ;


		switch(d) {
		//case 0x5800FD71 :	// end of send_config 
		//case 0x5800FD81 :	// end of run
		case 0x5800FD61 :	// end of manual "Fd"
			LOG(INFO,"END of something [0x%08X]",d) ;
			return 0 ;
		}

		if(log_start) {
			LOG(TERR,"... %d = 0x%08X",word_ix,d) ;
		}

		if(d==0x5800FD01) {
			LOG(WARN,"End of run (but still %d words)",data_end-data) ;
			return -1 ;
			//goto re_loop ;
		}

#if 0
		if((d&0xE0000000)==0x80000000) {
			LOG(WARN,"START HDR %d 0x%08X",word_ix,d) ;
		}

		if((d&0xE0000000)==0x40000000) {
			LOG(WARN,"END HDR %d 0x%08X",word_ix,d) ;
		}

#endif
		switch(state) {
		case S_FEE_ASCII :
			if((d&0xFFFF0000)==0x00AA0000) {
				u_char c = d & 0xFF ;

				if(isprint(c) || isspace(c)) ;
				else c = '?' ;

				ascii_dta[ascii_cou++] = c ;
				if(c=='\n') {
					ascii_dta[ascii_cou++] = 0 ;
					LOG(ERR,"WTF?") ;
					printf("HERE #%02d: %s",fee_port,ascii_dta) ;

					if(strncmp(ascii_dta,"1Wire ",6)) {
						u_int id1 ;
						char *id = strstr(ascii_dta,"ID") ;
						sscanf(id,"ID 0x%X",&id1) ;
						printf("=====> 0x%08X",id1) ;
					}
					fflush(stdout) ;
					ascii_cou = 0 ;
				}
			}
			else {
				LOG(ERR,"Bad: %d = 0x%08X",word_ix,d) ;
			}
			state = S_FEE_ASCII_END ;
			goto re_loop ;

		case S_FEE_ASCII_END :
			if((d&0xF80000FF)!=0x600000A1) {
				LOG(ERR,"Bad %d = 0x%08X",word_ix,d) ;
			}
			state = S_IDLE ;
			goto re_loop ;
		case S_FEE_PORT :
			fee_port = (d&0xFF)+1 ;
			LOG(NOTE,"FEE port #%02d",fee_port) ;
			state = S_IDLE ;
			goto re_loop ;
		case S_TRIGGER :
			LOG(TERR,"Trg %2d = 0x%08X",d_cou,d) ;
			d_cou++ ;
			if((d&0xF80000FF)==0x58000005) {
				state = S_IDLE ;
			}
			goto re_loop ;
		case S_IDLE :
		default :
			break ;
		}

		if((d & 0xF80000FF)==0xA00000A0) {
			state = S_FEE_ASCII ;
			goto re_loop ;
		}


		hdr = d & 0xF800FFFF ;

		switch(hdr) {
		case 0x9800FD70 :
			LOG(INFO,"SEND_CONFIG: start") ;
			break ;
		case 0x5800FD71 :
			if(fout) {
				fprintf(fout,"%d: 1-wire Id 0x%08X\n",rdo_id,rdo_wire1_id) ;
			}

			LOG(INFO,"SEND_CONFIG: end for RDO ID 0x%08X",rdo_wire1_id) ;
			for(int i=1;i<=16;i++) {
				LOG(INFO,"   FEE #%02d: Padplane %02d, 1Wire 0x%08X",i,fee[i].padplane_id,fee[i].wire1_id) ;
				if(fout) {
					fprintf(fout,"   FEE #%02d: Padplane %02d, 1Wire 0x%08X\n",i,fee[i].padplane_id,fee[i].wire1_id) ;
				}
			}

			if(fout) fflush(fout) ;

			//fee_dbase() ;

			return 2 ;
			break ;
		case 0x9800FD80 :
			LOG(INFO,"RUN_START: start") ;
			break ;
		case 0x5800FD81 :
			LOG(INFO,"RUN_START: stop, events %u",evt_ix) ;
			return 0 ;
			break ;
		case 0x98000008 :
			// sometimes I get ASCII immediatelly
			if((data[1]&0xFFC00000)!=0x80000000) {
				if((data[1]&0xFFC0FFFF)!=0xA00000A0) {
					// I read e.g. 0x002E0000 instread of 0x802E0001
					//LOG(ERR,"%d: evt %d: After start 0x%08X, FEE #%d",rdo_id,evt_ix,data[1],data[0]+1) ;
				}
			}


			data = fee_scan(data-1, data_end) ;
#if 0
			fee_port = *data++ ;
			fee_port &= 0xFF ;
			fee_port++ ;
			LOG(NOTE,"RDO FEE data: START: fee_port %d",fee_port) ;
#endif
			break ;
		case 0x58000009 :
			LOG(NOTE,"RDO FEE data: END: fee_port %d",fee_port) ;
			break ;
		case 0x98000004 :
			if(flags & 0x6) {
				LOG(ERR,"Duplicate hdr") ;
			}

			flags |= 2 ;
			LOG(NOTE,"RDO Event Header: START") ;
			{
				u_int *d_now = data ;

				for(int i=0;i<10;i++) {
					
					LOG(NOTE,"Event Header %2d = 0x%08X",i,d) ;

					if(d==0x58000005) {
						flags |= 4 ;
						break ;
					}
					else {
						d = *data++ ;
					}
				}

				if((data-d_now)!=8) {
					LOG(ERR,"RDO Event Header corrupt %d",data-d_now) ;
				}

			}
			break ;
		case 0x58000005 :
			LOG(ERR,"RDO Event Header: END (could be spurious)") ;
			break ;
		case 0x98000006 :
			fee_evt_cou++ ;
			LOG(NOTE,"RDO FEE complement: START") ;
			break ;
		case 0x58000007 :
			LOG(NOTE,"RDO FEE complement:   END") ;
			break ;
#if 0
		case 0x98000044 :
			d = *data++ ;
			if(d) {
				LOG(ERR,"RDO FEE Readout Status: START: status 0x%08X",d) ;
			}
			else {
				LOG(NOTE,"RDO FEE Readout Status: START: status 0x%08X",d) ;
			}
			break ;
		case 0x58000045 :
			LOG(NOTE,"RDO FEE Readout Status:   END") ;
			break ;
#endif

		case 0x98000066 :
			LOG(NOTE,"RDO Boottext: START: %d = 0x%08X",word_ix,d) ;
			for(;;) {
				d = *data++ ;
				if((d & 0xFFFFFF00)==0x9800F500) {
					int c = d & 0xFF ;
					if(c=='\n') {
						mon_string[mon_cou++] = 0 ;

						if(fout) {
							fprintf(fout,"%d: %s\n",rdo_id,mon_string) ;
							fflush(fout) ;
						}
						LOG(INFO,"%d: \"%s\"",rdo_id,mon_string) ;
						mon_cou = 0 ;
					}
					else {
						mon_string[mon_cou++] = c ;
					}
					//printf("%c",d&0xFF) ;
				}
				else {
					LOG(NOTE,"RDO Boottext: END: %d = 0x%08X",word_ix,d) ;	
					mon_cou = 0 ;
					break ;
				}
			}
			fflush(stdout) ;
			break ;
		case 0x58000067 :	// should not see it here...
			LOG(ERR,"RDO Boottext:  END: %d = 0x%08X",word_ix,d) ;
			break ;

		case 0x980000FC :
			LOG(NOTE,"RDO Monitoring: START: %d = 0x%08X",word_ix,d) ;
			for(;;) {
				d = *data++ ;
				if((d & 0xFFFFFF00)==0x9800F500) {
					int c = d & 0xFF ;
					if(c=='\n') {
						mon_string[mon_cou++] = 0 ;
						LOG(INFO,"%d: \"%s\"",rdo_id,mon_string) ;
						if(fout) {
							fprintf(fout,"%d: \"%s\"\n",rdo_id,mon_string) ;
							fflush(fout) ;
						}
						mon_cou = 0 ;
					}
					else {
						mon_string[mon_cou++] = c ;
					}
					//printf("%c",d&0xFF) ;
				}
				else {
					LOG(NOTE,"RDO Monitoring: END: %d = 0x%08X",word_ix,d) ;
					mon_cou =0 ;
					break ;
				}
			}
			fflush(stdout) ;
			break ;
		case 0x580000FD :	// should not see it here...
			LOG(ERR,"RDO Monitoring:  END: %d = 0x%08X",word_ix,d) ;
			break ;
		case 0x98001000 :
			if(flags & 0x18) {
				LOG(ERR,"RDO %d: duplicate trailer",rdo_id) ;
			}
			flags |= 8 ;
			LOG(NOTE,"RDO: Event Trailer Start") ;
			{
				u_int trg_cou ;
				u_int *d_now = data ;
				int suspect = 0 ;

				if(data[0] != 0xABCD0000) {
					suspect = 1 ;
					LOG(ERR,"RDO %d: Event Trailer %u/%u = ABCD 0x%08X",rdo_id,word_ix,words,data[0]) ;
				}

				if(data[1] != 0) {
					suspect = 1 ;
					//often!
					//LOG(ERR,"RDO %d: Event Trailer %u/%u = status 0x%08X",rdo_id,word_ix,words,data[1]) ;
				}

				trg_cou=data[2] ;
				if(trg_cou>100) {
					suspect = 1 ;
					//often
					//LOG(ERR,"RDO %d: Event Trailer %u/%u = trg_cou 0x%08X",rdo_id,word_ix,words,data[2]) ;
					trg_cou = 0 ;
				}

				if(suspect) {
					// almost always 0xF which is correct.
					if(flags != 0xF) LOG(ERR,"flags 0x%X",flags) ;
				}

				if((data[3+trg_cou+2]&0xF800FFFF)!=0x58001001) suspect = 1 ;	// very often


				//LOG(TERR,"END 0x%08X",data[3+trg_cou+2]) ;

				// data[0] = 0xabcd0000 ;
				// data[1] = status (must be 0)
				// data[2] = trigger count
				// data[3..] = triggers
				// data[x] = yada ;
				// data[x+1] = yada
				// data[x+2] = 0x58001001 ;

				for(int i=0;i<100;i++) {
					//if(i==2 && d) {	//status
					//	LOG(ERR,"FEE #%d: Event Trailer %u = 0x%08X",fee_port,word_ix,d) ;
					//}

					LOG(NOTE,"Event Trailer %2d = 0x%08X",i,d) ;

					if((d&0xF800FFFF)==0x58001001) {
						flags |= 0x10 ;
						break ;
					}
					else {
						d = *data++ ;
						if(data>data_end) {
							flags |= 0x1000 ;
							suspect = 2 ;
							break ;
						}
					}
				}

				// unfortuntatelly this is not a fixed value... THINK!
				// ALSO -- it seems to be 100 easily which means that I am missing the end of event!
				int t_len = data - d_now ;
				if(0) {
				//if(suspect) {
					LOG(ERR,"RDO %d: Event Trailer Suspect %d - %d",rdo_id,suspect,t_len) ;
					for(int i=0;i<16;i++) {
						LOG(TERR,"%d/%d = 0x%08X",d_now-data_start,words,*d_now) ;
						d_now++ ;
					}
					data = data_end + 1 ;	// to make sure it's over
				}
			}

			break ;
		case 0x58001001 :	// should not see it here
			LOG(ERR,"RDO: Event Trailer End") ;
			break ;

		case 0x980000FA :
			LOG(NOTE,"RDO Configuration: START: %d = 0x%08X",word_ix,d) ;
			//LOG(TERR,"RDO wire1_id 0x%08X",data[7]) ;
			rdo_wire1_id = data[7] ;
			data += 10 ;
			break ;
		case 0x580000FB :
			LOG(NOTE,"RDO Configuration:   END: %d = 0x%08X",word_ix,d) ;
			break ;
		case 0x9800FD60:
			LOG(NOTE,"RDO FEE DUMP : START") ;
			break ;
		case 0x980000F8 :
			if(data[1]) {
				LOG(ERR,"%d: FEE #%d: stat 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,data[1],data[2],data[3],data[4]) ;
			}
			else {
				LOG(TERR,"%d: FEE #%d: stat 0x%08X 0x%08X 0x%08X 0x%08X",rdo_id,fee_port,data[1],data[2],data[3],data[4]) ;
			}
			//log_start = 1 ;
			break ;


		// FEE headers
		// 0x8 is start triggered
		//  0x4 is end triggred
		// 0xA is start header
		//  0x6 is end header

		case 0x80000010 :
			{
				
				u_int h, l ;
				u_int glo_status ;
				
				l = (*data++) & 0xFFFF ;
				h = (*data++) & 0xFFFF ;
			
				glo_status = (h<<16) | l ;

				if(glo_status) {
					LOG(ERR,"FEE SEND_CONFIG: START 0: status 0x%08X",glo_status) ;
				}
				else {
					LOG(NOTE,"FEE SEND_CONFIG: START 0: status 0x%08X",glo_status) ;
				}
			}
			break ;
		case 0x60000011 :
			LOG(NOTE,"FEE SEND_CONFIG:   END 0") ;
			break ;

		case 0xA00000FA :
			LOG(NOTE,"FEE Configuration: START") ;
			break ;
		case 0x600000FB :
			LOG(NOTE,"FEE Configuration:   END") ;
			break ;

		case 0xA00000EC :
			LOG(NOTE,"FEE Monitoring: START 0") ;
			break ;
		case 0x600000ED :
			LOG(NOTE,"FEE Monitoring:   END 0") ;
			break ;
		case 0xA00000FC :
			LOG(NOTE,"FEE Monitoring: START 1") ;
			break ;
		case 0x600000FD :
			LOG(NOTE,"FEE Monitoring:   END 1") ;
			break ;

		case 0xA0000020 :
			{
				
				u_int h, l ;
				u_int glo_status ;
				
				l = (*data++) & 0xFFFF ;
				h = (*data++) & 0xFFFF ;
			
				glo_status = (h<<16) | l ;

				if(glo_status) {
					LOG(ERR,"FEE SEND_CONFIG: START 1: status 0x%08X",glo_status) ;
				}
				else {
					LOG(NOTE,"FEE SEND_CONFIG: START 1: status 0x%08X",glo_status) ;
				}
			}

			break ;
		case 0x40000021 :
			LOG(NOTE,"FEE SEND_CONFIG:   END 1 (all)") ;
			break ;


		case 0x80000200 :
			LOG(NOTE,"FEE Trigger: START: TRIGGERED") ;
			{
			u_int h, l ;
			u_int bx_xing ;
			u_int glo_status ;
			u_int fee_evt ;
			u_int type ;

			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			glo_status = (h<<16) | l ;


			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			rh_xing_start = (h<<16) | l ;


			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			bx_xing = (h<<16) | l ;

			
			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			type = (h<<16) | l ;

			l = (*data++) & 0xFFFF ;
			h = (*data++) & 0xFFFF ;
			
			fee_evt = (h<<16) | l ;


			int fee_id = (d>>16)&0x3F ;

			if(glo_status) {
				LOG(ERR,"FEE %d(#%d) START Event %d: type 0x%08X, xing %u, 20bit %u, RHIC %u, glo 0x%08X",fee_id,fee_port,
				    fee_evt,type,bx_xing,bx_xing&0xFFFFF,rh_xing_start,glo_status) ;
			}
			else {
				LOG(TERR,"FEE %d(#%d) START Event %d: type 0x%08X, xing %u, 20bit %u, RHIC %u, glo 0x%08X",fee_id,fee_port,
				    fee_evt,type,bx_xing,bx_xing&0xFFFFF,rh_xing_start,glo_status) ;
			}

			}
			break ;
		case 0x60000201 :
			LOG(NOTE,"FEE Trigger:   END: hdr") ;
			break ;
		case 0xA0000300:
			{
				u_int a, b ;
				u_int status ;
				u_int xing ;
				u_int glo_status ;
				u_int fee_evt ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji

				glo_status = (b<<16) | a ;


				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji

				status = (b<<16) | a ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji
				
				xing = (b<<16) | a ;

				a = (*data++) & 0xFFFF ;	//lo
				b = (*data++) & 0xFFFF ;	//ji
				
				fee_evt = (b<<16) | a ;

				if(glo_status || status) {
					LOG(ERR,"FEE END Event %d: glo 0x%08X, status 0x%08X, xing %u, delta %u",
					    fee_evt,glo_status,status, xing, xing-rh_xing_start) ;
				}
				else {
					LOG(TERR,"FEE END Event %d: glo 0x%08X, status 0x%08X, xing %u, delta %u",
					    fee_evt,glo_status,status, xing, xing-rh_xing_start) ;
				}
			}
			break ;
		case 0x40000301:
			LOG(WARN,"FEE Event:   END") ;
			break ;
		default :	// all other cases

			if(words==(data-data_start)) {	// last word
				if((d & 0xFFFF0000)==0x005C0000) break ;	// last word and it's a end-comma -- normal

				//last word but it's some gibberish -- it can happen occassionally, don't know why
				//LOG(WARN,"%d: last huh 0x%08X at %d/%d",rdo_id,d,word_ix,words) ;
				break ;
			}	
				

			//LOG(WARN,"%d: huh 0x%08X at %d/%d",rdo_id,d,word_ix,words) ;

#if 0
			if((d & 0xFFFF0000)==0x005C0000) ;	// stop-comma is OK here
			else {
				if(words==(data-data_start)) ;	// OK for the last word
				else {
					// junk at the end of event
					//LOG(WARN,"%d: datum huh 0x%08X at %d/%d",rdo_id,d,data-data_start,words) ;
				}
			}
#endif

			break ;
		}


		re_loop: ;

	}



	if((flags != 0x1F) || ((data_end-data) != 0)) {
		//LOG(WARN,"At end: %d, flags 0x%X",data_end-data,flags) ;
		for(int i=0;i<32;i++) {
			//LOG(WARN,"... %2d = 0x%08X",i,data_end[16-i]) ;
		}
	}

	if((data[-1] & 0xFFFF0000)==0x005C0000) {
		log_start = 0 ;
		//LOG(WARN,"Stop Comma at the very end") ;
		return 1 ;
	}
	else LOG(NOTE,"%d: no end-comma %d %d = 0x%08X!",rdo_id,data_end-data,words,data[-1]) ;

	return 1 ;



}



/* Tonko: received from Arild on 16 Nov 2017

buffer[0] -- first 30 bits of 50 bit word
buffer[1] -- last 20 bits of 50 bit word

*/

typedef unsigned char uint8_t ;

static void hammingdecode(unsigned int buffer[2], bool& error, bool& uncorrectable, bool fix_data) // Least efficient hamming decoder ever
{

  // header split
  bool parityreceived[6];
  bool data_in[43];
  bool overallparity;

  for (int i = 0; i < 6; i++)
    parityreceived[i] = (buffer[0] >> i) & 0x1;

  overallparity = (buffer[0] >> 6) & 0x1;

  //for (int i = 0; i < 43; i++)
  //  data_in[i] = (header_in >> (i + 7)) & 0x1;

  for (int i = 7; i < 30; i++)
    data_in[i-7] = (buffer[0] >> i) & 0x1;

  for (int i = 30; i < 50; i++)
    data_in[i-7] = (buffer[1] >> (i - 30)) & 0x1;

  //calculated values
  bool corrected_out[43];
  bool overallparitycalc = 0;
  bool overallparity_out = 0;
  bool paritycalc[6];
  bool paritycorreced_out[6];

  ////////////////////////////////////////////////////////////////////////////////////////////////
  // calculate parity
  paritycalc[0]   =   data_in[0]  ^ data_in[1]  ^ data_in[3]  ^ data_in[4]  ^ data_in[6]  ^ 
                      data_in[8]  ^ data_in[10] ^ data_in[11] ^ data_in[13] ^ data_in[15] ^ 
                      data_in[17] ^ data_in[19] ^ data_in[21] ^ data_in[23] ^ data_in[25] ^ 
                      data_in[26] ^ data_in[28] ^ data_in[30] ^ data_in[32] ^ data_in[34] ^ 
                      data_in[36] ^ data_in[38] ^ data_in[40] ^ data_in[42];

  paritycalc[1]   =   data_in[0]  ^ data_in[2]  ^ data_in[3]  ^ data_in[5]  ^ data_in[6]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[12] ^ data_in[13] ^ data_in[16] ^ 
                      data_in[17] ^ data_in[20] ^ data_in[21] ^ data_in[24] ^ data_in[25] ^ 
                      data_in[27] ^ data_in[28] ^ data_in[31] ^ data_in[32] ^ data_in[35] ^ 
                      data_in[36] ^ data_in[39] ^ data_in[40] ;

  paritycalc[2]   =   data_in[1]  ^ data_in[2]  ^ data_in[3]  ^ data_in[7]  ^ data_in[8]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[14] ^ data_in[15] ^ data_in[16] ^ 
                      data_in[17] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^ 
                      data_in[29] ^ data_in[30] ^ data_in[31] ^ data_in[32] ^ data_in[37] ^
                      data_in[38] ^ data_in[39] ^ data_in[40] ;

  paritycalc[3]   =   data_in[4]  ^ data_in[5]  ^ data_in[6]  ^ data_in[7]  ^ data_in[8]  ^ 
                      data_in[9]  ^ data_in[10] ^ data_in[18] ^ data_in[19] ^ data_in[20] ^ 
                      data_in[21] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^
                      data_in[33] ^ data_in[34] ^ data_in[35] ^ data_in[36] ^ data_in[37] ^
                      data_in[38] ^ data_in[39] ^ data_in[40] ;

  paritycalc[4]   =   data_in[11] ^ data_in[12] ^ data_in[13] ^ data_in[14] ^ data_in[15] ^ 
                      data_in[16] ^ data_in[17] ^ data_in[18] ^ data_in[19] ^ data_in[20] ^ 
                      data_in[21] ^ data_in[22] ^ data_in[23] ^ data_in[24] ^ data_in[25] ^
                      data_in[41] ^ data_in[42] ;

  paritycalc[5]   =   data_in[26] ^ data_in[27] ^ data_in[28] ^ data_in[29] ^ data_in[30] ^
                      data_in[31] ^ data_in[32] ^ data_in[33] ^ data_in[34] ^ data_in[35] ^
                      data_in[36] ^ data_in[37] ^ data_in[38] ^ data_in[39] ^ data_in[40] ^
                      data_in[41] ^ data_in[42] ;
  ////////////////////////////////////////////////////////////////////////////////////////////////

  uint8_t syndrome = 0;

  for (int i = 0; i < 6; i++)
   syndrome |= (paritycalc[i]^parityreceived[i]) << i;

  bool data_parity_interleaved[64];
  bool syndromeerror;

  //data_parity_interleaved[0]          =  0;
  data_parity_interleaved[1]          =  parityreceived[0];
  data_parity_interleaved[2]          =  parityreceived[1];
  data_parity_interleaved[3]          =  data_in[0];
  data_parity_interleaved[4]          =  parityreceived[2];
  for (int i = 1; i <= 3; i++)
    data_parity_interleaved[i+5-1]    =  data_in[i];
  data_parity_interleaved[8]          =  parityreceived[3];
  for (int i = 4; i <= 10; i++)
    data_parity_interleaved[i+9-4]    =  data_in[i];
  data_parity_interleaved[16]         =  parityreceived[4];
  for (int i = 11; i <= 25; i++)
    data_parity_interleaved[i+17-11]  =  data_in[i];
  data_parity_interleaved[32]         =  parityreceived[5];
  for (int i = 26; i <= 42; i++)
    data_parity_interleaved[i+33-26]  =  data_in[i];
  //for (int i = 50; i <= 63; i++)
  //  data_parity_interleaved[i]        =  0;

  data_parity_interleaved[syndrome] =  !data_parity_interleaved[syndrome]; // correct the interleaved

  paritycorreced_out[0] = data_parity_interleaved[1];
  paritycorreced_out[1] = data_parity_interleaved[2];
  corrected_out[0]        = data_parity_interleaved[3];
  paritycorreced_out[2]   = data_parity_interleaved[4];
  for (int i = 1; i <= 3; i++)
    corrected_out[i]      = data_parity_interleaved[i+5-1];
  paritycorreced_out[3]   = data_parity_interleaved[8];
  for (int i = 4; i <= 10; i++)
    corrected_out[i]     = data_parity_interleaved[i+9-4];
  paritycorreced_out[4]   = data_parity_interleaved[16];
  for (int i = 11; i <= 25; i++)
    corrected_out[i]    = data_parity_interleaved[i+17-11];
  paritycorreced_out[5]   = data_parity_interleaved[32];
  for (int i = 26; i <= 42; i++)
    corrected_out[i]    = data_parity_interleaved[i+33-26];

    // now we have the "corrected" data -> update the flags

  bool wrongparity;
  for (int i = 0; i < 43; i++)
    overallparitycalc ^=data_in[i];
  for (int i = 0; i < 6; i++)
    overallparitycalc ^= parityreceived[i];
  syndromeerror = (syndrome > 0) ? 1 : 0; // error if syndrome larger than 0
  wrongparity = (overallparitycalc != overallparity);
  overallparity_out = !syndromeerror &&  wrongparity ? overallparitycalc : overallparity; // If error was in parity fix parity
  error = syndromeerror |  wrongparity;
  uncorrectable = (syndromeerror && (!wrongparity));

  
  //header_out = 0;
  //for (int i = 0; i < 43; i++)
  //  header_out |= corrected_out[i] << (i + 7);
  //header_out |= overallparity_out << 6;
  //for (int i = 0; i < 6; i++)
  //  header_out |= paritycorreced_out[i] << i;
  if (fix_data)
  {
    for (int i = 0; i < 6; i++)
      buffer[0] = (buffer[0] & ~(1 << i)) | (paritycorreced_out[i] << i);
    buffer[0] = (buffer[0] & ~(1 << 6)) | (overallparity_out << 6);
    for (int i = 7; i < 30; i++)
      buffer[0] = (buffer[0] & ~(1 << i)) | (corrected_out[i - 7] << i);
    for (int i = 30; i < 50; i++)
      buffer[1] = (buffer[1] & ~(1 << (i - 30))) | (corrected_out[i - 7] << (i - 30));
  }
}
