#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>

#include <rtsLog.h>
#include <rts.h>
#include <rtsSystems.h>

#include <DAQ_READER/daq_dta.h>

#include <DAQ_ITPC/itpcCore.h>
#include <DAQ_ITPC/itpc_rowlen.h>
#include <DAQ_ITPC/itpcPed.h>

#include "itpc23.h"

//================================================================================
//============= iTPC Data Support Routines for the NEW (FY23) version!!! =========
//================================================================================

tpc23_base::row_pad_t (*itpc23::rp_gain_itpc)[ROW_MAX+1][PAD_MAX+1] ;


static const char *hwicap_version(u_int v) ;
static u_int get_ifee_mask(int sec1, int rdo1) ;


static inline u_int sw16(u_int d)
{
        d = ((d>>16)&0xFFFF)|(d<<16) ;

        return d ;
}

inline void itpc23::set_rdo(int sec, int rdo)
{
//	LOG(TERR,"set_rdo %d: S%02d:%d",id,sec,rdo) ;

	sector1 = sec ;
	rdo1 = rdo ;

	fee_mask = get_ifee_mask(sector1,rdo1) ;
} ;

// Change the data format from the old FY22 version to the new
// FY23 version. Store data back into the original storage.
int itpc23::from22to23(char *c_dta, int words)
{
	int n_words = words ;
	u_int *data = (u_int *)c_dta ;

	u_int *d_use = (u_int *)malloc(words*4) ;	// allocate sotrage

	u_int *data_end = data + words ;
	u_int *data_start = data ;

	// this is wrong! I need to get it from the data!!
	fee_mask = get_ifee_mask(sector1,rdo1) ;

//	LOG(TERR,"iS%02d:%d fee_mask 0x%04X",sector1,rdo1,fee_mask) ;

	int w_cou = (words<16)?words:16 ;

	for(int i=0;i<w_cou;i++) {
		if((data[i]==0xCCCC001C)||(data[i]==0x001CCCCC)) {
			data = data + i ;
			break ;
		}
	}

	w_cou = data_end - data ;

	int need_swapping = 0 ;
	if(data[0]==0xCCCC001C) {
//		LOG(WARN,"Need swapping!") ;
		need_swapping = 1 ;
		for(int i=0;i<w_cou;i++) {
			data[i] = sw16(data[i]) ;
		}
	}
	data += 1 ;	// skip start comma
	
//	data += 5 ;	// at start comma

	// data[0] = 0x9800.... ;
	// data[1] = trigger word


	if((data[0]&0xFFFF000F) != 0x98000004) {
		LOG(ERR,"start 0 0x98 = 0x%08X",data[0]) ;
	}

//	LOG(TERR,"wds 0x%08X 0x%08X; data end 0x%08X",data[0],data[1],data_end[0]) ;
//	for(int i=0;i>-15;i--) {
//		LOG(TERR,"  end %d = 0x%08X",i,data_end[i]) ;
//	}

	// data[3] is 0x80310010...???


	// go to the end...


	int found ;

#if 0
	data_end = data_end - 1 - 2 - 1 - 6 - 1 ;

	// hm, live data seems to need -10 and not -11???
	if(need_swapping) {
//		LOG(WARN,"adjusting data end") ;
		data_end++ ;
	}

	//data_end[0] is 0x5800...
	if((data_end[0]&0xFF000000) != 0x58000000) {
		LOG(ERR,"data_end 0 0x58 = 0x%08X",data_end[0]) ;
	}
#else	
	// find it going backwwards

	found = 0 ;
	for(int i=0;i<16;i++) {
//		LOG(TERR,"%d: 0x%08X",-i,data_end[-1]) ;

		if((data_end[-i]&0xFF000000)==0x58000000) {
			data_end = data_end - i ;
			found=1 ;
			break ;
		}
	}

	if(!found) {
		LOG(ERR,"%d: can't find data_end!",rdo1) ;
	}

#endif

//	LOG(TERR,"data_end 0x%08X",data_end[0]) ;

	found = 0 ;
	while(data_end>data) {
		if((*data_end & 0xFF000000)==0x98000000) {
			found = 1 ;
			break ;
		}
		data_end-- ;
	}
	
	// data_end[0] is 0x9800.... : trigger header

	if(!found) {
		LOG(ERR,"%d: data_end 0x98 not found = 0x%08X",rdo1,data_end[0]) ;
	}

	n_words = data_end - data ;

	u_int *d = data + 2 ;	// now at 0x8.....



	int fee_cou = 0 ;
	u_int *fee_p[16] ;
	int l_fee_mask = 0 ;

	memset(fee_p,0,sizeof(fee_p)) ;

	while(d<data_end) {
//		if((d-data)<16) LOG(TERR,"... %d 0x%08X",d-data,d[0]) ;

		if((d[0]&0xFF000000)==0x80000000) {	// FEE start
			int port = d[4]&0xF ;
			fee_cou++ ;

			l_fee_mask |= (1<<port) ;
			fee_p[port] = d ;

//			LOG(TERR,"FEE cou %d: port %d: fee_mask 0x%04X at %d",fee_cou,port,l_fee_mask,d-data) ;
			
			
		}
		d++ ;
	}

	fee_mask = l_fee_mask ;

//	fee_mask = get_ifee_mask(sector1,rdo1) ;

	for(int i=0;i<5;i++) d_use[i] = data_start[i] ;

	d_use[5] = 0x20000000 ;			// 1:
	d_use[6] = data[1] ;		// 2:trigger
	d_use[7] = 0 ;			// 3: MHZ start
	d_use[8] = fee_mask<<16 ;	// 4: fee_synced,overrun
	d_use[9] = 0 ;			// 5: fee_xoff,rdo_stuff
	d_use[10] = 0 ;			// 6: fee_emtpy
	d_use[11] = 0xF0000000 | fee_mask ;	// 7: sig

	u_int *d_fee = d_use + 12 ;

	int ix = 0 ;
	for(int i=0;i<16;i++) {
		u_int *p = fee_p[i] ;

		if(p==0) continue ;

		d_fee[ix] = p[0] & 0xFFFF0000 ;
		d_fee[ix] |= 0xF0 ;	// even-type

		ix++ ;

		// find 0xB....
		while((p[0]&0xF0000000)!=0xB0000000) {
			p++ ;
		}

		d_fee[ix] = p[0] ;
		ix++ ;
		p++ ;
		
		int no_cpy = 0 ;
		while((p[0]&0xF0000000)!=0x40000000) {
			if((p[0]&0xF0000000)==0xA0000000) {
				no_cpy = 1 ;
			}

			if(no_cpy==0) {
				d_fee[ix] = p[0] ;
				ix++ ;
			}
			p++ ;
		}
		
		d_fee[ix] = p[0] ;
		ix++ ;
	}



	// trailer
	d_use[n_words-1] = 0xDEADC0DE ;
	d_use[n_words-2] = 0xFEEDC0FE ;
	d_use[n_words-3] = 0x0 ;	// trl1: mhz_end
	d_use[n_words-4] = 0xFFFFFFFF ;	// trl0: evt_status ;

//	LOG(TERR,"words %d, n_words %d",words,n_words) ;	// n_words is 20 less than words

	// and now FEE data???
	
	// finalize

	memcpy(c_dta,d_use,n_words*4) ;

	free(d_use) ;

//	LOG(TERR,"from22to23: done") ;

	return n_words ;
}




int itpc23::init(daq_dta *gain)
{
	if(gain==0) return -1 ;

	while(gain->iterate()) {
		int s = gain->sec ;
		int r = gain->row ;

		daq_det_gain *gp = (daq_det_gain *) gain->Void ;

		for(u_int p=0;p<gain->ncontent;p++) {
			if(p==0) continue ;

			//gain row,pad is gp[p].gain, gp[p].t0 ;
			LOG(TERR,"gains: S%02d, rp %d:%d = %.1f",s,r,p,gp[p].gain) ;
		}
			
	}

	return 9 ;
}


u_int *itpc23::ch_scan(u_int *start)
{
	u_short w[6] ;
	u_int *d = start ;
	int row, pad ;

	// we are at the SAMPA header
	w[0] = (d[0]>>20)&0x3FF ;
	w[1] = (d[0]>>10)&0x3FF ;
	w[2] = (d[0]>>00)&0x3FF ;

	w[3] = (d[1]>>20)&0x3FF ;
	w[4] = (d[1]>>10)&0x3FF ;
	w[5] = (d[1]>>00)&0x3FF ;	// what do I have here?

	int pkt = (w[0]>>7)&0x7 ;
	int sampa_ch = (w[2]>>4)&0x1F ;
	int sampa_id = w[2]&0xF ;

	int words10 = w[1] ;

	// I need to get to row,pad here!
	if(log_level>=1) LOG(TERR,"%d: words10 %d:%d (SAMPA %d,%d): words10 %d",rdo1,fee_ix,ch_ix,sampa_id,sampa_ch,words10) ;

	if(unlikely(words10==1023)) {	// channel skipped because of prog-full!
		prog_fulls++ ;
		LOG(ERR,"%d: ch_scan %d:%d: SAMPA%d:%d -- prog-full",rdo1,fee_ix,ch_ix,sampa_id,sampa_ch) ;
		words10 = 0 ;
	}

//	LOG(WARN,"pkt %d: sampa_id %d, sampa_ch %d, words10 %d",pkt,sampa_id,sampa_ch,words10) ;

	if(unlikely(pkt!=4 || sampa_ch>31 || words10>512)) {
		err |= 0x1000000 ;
		fee_errs++ ;
		LOG(ERR,"%d: ch_scan %d:%d: pkt %d, sampa_ch %2d, words10 %d [0x%08X]",rdo1,fee_ix,ch_ix,
		    pkt,sampa_ch,words10,
		    d[0]) ;
	}

	int bx = ((w[4]&0x1FF)<<17)|(w[3]<<1)|((w[2]>>9)&1) ;

	if(unlikely(bx_count<0)) {	// first channel of the FEE
		bx_count = bx ;
	}
	else {
		if(unlikely(bx != bx_count)) {
			err |= 0x2000000 ;
			fee_errs++ ;
			if(fee_errs<10) LOG(ERR,"%d: ch_scan %d:%d: bx %d, expect %d",rdo1,fee_ix,ch_ix,bx,bx_count) ;
		}
	}


	int words10_start = words10 ;	// remember

	while(words10%3) words10++ ;	// align on 32bit word 

	int words = words10/3 ;		// and then count them...

	d += 2 ;		// move from channel header...

	int ix = 0 ;
	int a_ix = 0 ;
	int tb_cou = 0 ;
	int tb_start = 0 ;
	int tb_last = -1 ;
	int w10 = 0 ;

	u_short *d_start = s1_dta + last_ix ;
	struct seq_t *seq = 0 ;
	u_short *dd = s1_dta + last_ix ;
	u_short *dd_save = dd ;

	int seq_ix = 0 ;

	// craft id
	int id = (fee_pp<<1)|(sampa_id&1) ;
	int flags = flags_row_pad(id, sampa_ch, row, pad) ;	// also gets row,pad from id sampa_ch

	if(log_level>=2) {
		LOG(TERR,"... %d: fee_ix %d, fee_pp %d: sampa_id %d, sampa_ch %d: row %d, pad %d, flags 0x%X",rdo1,fee_ix,fee_pp,
		    sampa_id,sampa_ch,row,pad,flags) ;
	}

	
//	printf("... evt %d: DDDD %d: fee_ix %d, fee_pp %d, ch_ix %d: sampa_id %d, sampa_ch %d: row %d, pad %d, flags 0x%X\n",evt,rdo1,fee_ix,fee_pp,ch_ix,
//	    sampa_id,sampa_ch,row,pad,flags) ;


	// skip non-wanted rows and pads
	if(flags==0xFF) {
		goto done_ch ;
	}

	seq = s1[row][pad].seq ;

	if(log_level>=2) LOG(TERR,"Here %p, words %d, %p",seq,words,dd) ;

	// now dump the data out...

	for(int i=0;likely(i<words);i++) {
		w[0] = (d[i]>>20)&0x3FF ;
		w[1] = (d[i]>>10)&0x3FF ;
		w[2] = (d[i]>>00)&0x3FF ;

		// it goes
		// tb_cou, tb_start, adc, adc, adc x tb_cou times
		// from low tb_start to high
		if(unlikely(d[i]&0xC0000000)) {
			LOG(ERR,"ch_scan %d:%d: bad word 0x%08X",fee_ix,ch_ix,d[i]) ;
		}

		if(log_level>=2) LOG(TERR,"FEE %d:%d -- %d = 0x%08X",fee_ix,ch_ix,i,d[i]) ;


		for(int j=0;likely(j<3);j++) {
			w10++ ;

			switch(ix) {
			case 0 :
				tb_cou = w[j] ;		// actual count of ADCs

				if(log_level>=100) LOG(TERR,"  tb_cou %d %p",tb_cou,dd) ;

				*dd++ = tb_cou ;
				
				if(log_level>=100) LOG(TERR,"  tb_cou %d",tb_cou) ;

				if(unlikely(tb_cou>500)) {
					LOG(ERR,"tb_cou %d [0x%08X,%d]",tb_cou,d[i],i) ;
				}
				ix = 1 ;
				break ;
			case 1 :
				tb_start = w[j] ;	// which is tb_lo
				*dd++ = tb_start ;

				if(seq_ix>=(SEQ_MAX-1)) {
					LOG(ERR,"too many seqs %d",seq_ix) ;
					goto done_ch ;
				}

				//printf("seq_ix %d: %d %d = %d %d\n",seq_ix,row,pad,tb_start,tb_cou) ;


				//LOG(TERR,"DDD %d %d %d",tb_start,tb_cou,tb_start+tb_cou-1) ;

				seq[seq_ix].t_lo = tb_start ;
				seq[seq_ix].t_hi = tb_start + tb_cou - 1 ;
				seq[seq_ix].dta_p = (dd-d_start) ;	// where is this sequence...
				seq[seq_ix].blob_id = 0 ;
				seq_ix++ ;

				//dd += tb_cou ;	// this doesn't sound correct!!!

				if(unlikely(log_level>=100)) LOG(TERR,"  tb_start %d",tb_start) ;

				if(unlikely(tb_start<=tb_last)) {
					LOG(ERR,"tb_start %d, tb_last %d",tb_start,tb_last) ;
				}



				tb_last = tb_start + tb_cou ;
				if(unlikely(tb_last>500)) {
					LOG(ERR,"tb_last %d [0x%08X,%d]",tb_last,d[i],i) ;
				}



				ix = 2 ;
				break ;
			default :
				//adc[a_ix] = w[j] ;
				if(log_level>=100) LOG(TERR,"  adc %d, %d",w[j],a_ix) ;
				a_ix++ ;
				*dd++ = w[j] ;	// store ADC


				//printf("    a_ix %d, adc %d\n",a_ix,w[j]) ;

				if(unlikely(a_ix==tb_cou)) {	// sequence done!
					tb_last = -1 ;
					ix = 0 ;
					a_ix = 0 ;
					if(w10==words10_start) goto done_ch ;
				}
				break ;
			}
		}

		if(log_level>=2) LOG(TERR,"Here %d",__LINE__) ;

//		printf("%d:%d %d: %d %d %d\n",fee_ix,ch_ix,i,w[0],w[1],w[2]) ;
		
	}

	done_ch :;

	if(seq) {
		sequence_cou += seq_ix  ;
		seq[seq_ix].t_hi = -1 ;
		s1[row][pad].ix = last_ix ;


		// data_c will exist in either: pedestal/pulser runs OR in the daqReader

		if(data_c) {
			u_short *dd = dd_save ;

			int ii = lane_ix/2 ;
			int ch = ii*32 + sampa_ch ;

			if(log_level>=2) {
				LOG(TERR,"fee_pp %d, fee_ix %d, lane_ix %d, sampa_ch %d, ch %d",
				       fee_pp,fee_ix,lane_ix,sampa_ch,ch) ;
			}



			data_c->sector = sector1 -1 ;
			data_c->rdo = rdo1-1 ;
			data_c->port = fee_ix  ;
			data_c->fee_id = fee_pp ;

			data_c->ch_start(ch) ;
			

			for(int i=0;i<seq_ix;i++) {
				dd += 2 ;
				int t_len = seq[i].t_hi - seq[i].t_lo + 1 ;

//				printf("   t_start %d, len %d\n",seq[i].t_lo,t_len) ;

				int ii = 0 ;
				for(int j=0;j<t_len;j++) {
					int adc = *dd++ ;
					int tb = seq[i].t_lo + ii ;
					//adc = (fee_ix+1)+100 ;
					data_c->accum(sector1-1,rdo1-1,fee_ix,fee_pp,ch,tb,adc) ;
					ii++ ;
				}
			}

			data_c->ch_done(0) ;
		}
#if 0
		{
		u_short *dd = dd_save ;
		for(int i=0;i<seq_ix;i++) {
			dd += 2 ;
			int t_len = seq[i].t_hi - seq[i].t_lo + 1 ;

			printf("RP %d:%d, seq %d: t_lo %d, t_hi %d\n",row,pad,i,seq[i].t_lo,seq[i].t_hi) ;

			int ii = 0 ;
			for(int j=0;j<t_len;j++) {
				int adc = *dd++ ;
				int tb = seq[i].t_lo + ii ;
				
				printf("    tb %3d = %3d ADC\n",tb,adc);

				ii++ ;
			}
		}
		}
#endif

	}

	last_ix += dd-d_start ;

	d += words ;


	
	return d ;
}

u_int *itpc23::lane_scan(u_int *start)
{
	u_int *d = start ;

	if(log_level>=1) LOG(TERR,"%d: lane scan %d: 0x%08X",rdo1,lane_ix,d[0]) ;

	// should be at start of lane 0xB....
	if((d[0]&0xF0000000)!=0xB0000000) {	// start of lane
		err |= 0x100000 ;
		LOG(ERR,"%d: lane_scan %d:%d: unknown start 0x%08X",rdo1,fee_ix,lane_ix,d[0]) ;
	}
	else if((d[0]>>26)&0x3) {	// SAMPA FIFOs overwritten!
		err |= 0x200000 ;
		LOG(ERR,"%d: lane_scan %d:%d: SAMPA FIFO overwritten 0x%08X",rdo1,fee_ix,lane_ix,d[0]) ;
	}

	d++ ;	// skip 0xB....

	if(log_level>=1) LOG(TERR,"%d: lane scan %d: 0x%08X",rdo1,lane_ix,d[0]) ;

	for(int i=0;i<16;i++) {
		ch_ix = i ;
		d = ch_scan(d) ;
	}

	// should be at end of lane 0x7....
	if((d[0]&0xF0000000)!=0x70000000) {	// end of lane
		err |= 0x400000 ;
		LOG(ERR,"%d: lane_scan %d:%d: unknown end 0x%08X",rdo1,fee_ix,lane_ix,d[0]) ;
	}

	d++ ;	// skip 0x7...

	return d ;
}

u_int *itpc23::fee_non_trgd(u_int *start)
{
	u_int *d = start ;

	int fee_words = 0 ;

	if(fee_evt_type != 0x02) {	// no clue
		LOG(ERR,"%d: fee_non_trgd %d: evt_type 0x%02X",rdo1,fee_ix,fee_evt_type) ;


		while(d<trl) {
			if((d[0]&0xF0000000)==0x40000000) {
				break ;
			}
			d++ ;
			fee_words++ ;
		}
	}
	else {
		LOG(NOTE,"fee_scan %d: evt_type 0x%02X, words %d",fee_ix,fee_evt_type,fee_words) ;

		// I am now at 0x6...
		for(int i=0;i<16;i++) {
			LOG(NOTE,"%d = 0x%08X",i,d[i]) ;
		}

		d++ ;	// I am now at 0xA...

		
		d++ ;	// I am now at data

		//0: shorts config version
		//1: shorts rdo_port
		//2: shorts fee_id
		//3: shorts for_me(WTF)
		int rdo_port = d[1]&0xFFFF ;

		d += 4 ;	// skip blabbler

		u_short sampa_stat[2] ;

		sampa_stat[0] = d[0]&0xFFFF ;
		sampa_stat[1] = d[1]&0xFFFF ;

		d += 2 ;

		d += 16 ;	// zeros

		d += 2 ;	// something

		u_int v_all = d[0]&0xFFFF ;
		v_all |= (d[1]&0xFFFF)<<16 ;

		u_int v_bit = d[2]&0xFFFF ;
		v_bit |= (d[3]&0xFFFF)<<16 ;

		unsigned long wire1 = d[4]&0xFFFF ;
		wire1 |= (long)(d[5]&0xFFFF)<<16 ;
		wire1 |= (long)(d[6]&0xFFFF)<<32 ;
		wire1 |= (long)(d[7]&0xFFFF)<<48 ;

		wire1 >>= 8 ;
		wire1 &= 0xFFFFFFFF ;

		u_int temp = d[8]&0xFFFF ;
		temp |= (d[9]&0xFFFF)<<16 ;

		u_int reg[2] ;

		reg[0] = d[10]&0xFFFF ;
		reg[0] |= (d[11]&0xFFFF)<<16 ;
		reg[1] = d[12]&0xFFFF ;
		reg[1] |= (d[13]&0xFFFF)<<16 ;

		char s_all[64] ;
		strcpy(s_all,hwicap_version(v_all)) ;

		LOG(INFO,"FEE %2d[%02d]: v_all 0x%08X[%s], v_bit 0x%08X[%s], wire1 0x%08llX, padplane %02d",fee_ix,rdo_port,
		    v_all,s_all,v_bit,hwicap_version(v_bit),wire1,fee_pp) ;

		LOG(NOTE,"   regs 0x%08X 0x%08X",reg[0],reg[1]) ;	// don't care that much

		if(sampa_stat[0]||sampa_stat[1]) {
			LOG(ERR,"   SAMPA stat: 0x%04X 0x%04X",sampa_stat[0],sampa_stat[1]) ;
		}

		// advance to the end, ignore ASCII stuff, who cares...
		while(d<trl) {
			if((d[0]&0xF0000000)==0x40000000) break ;
			d++ ;
		}

	}

	return d ;	// leave at 0x4.....
}


u_int *itpc23::fee_scan(u_int *start)
{
	u_int *d = start ;
	u_int *d_save = start ;

	bx_count = -1 ;
	fee_errs = 0 ;
	fee_evt_type = 0 ;
	fee_pp = 0 ;

	if(d_save>=trl) return start ;	// we are done -- don't move the return pointer
		
	// we must be at 0x8....
	if((d[0]&0xF0000000)!=0x80000000) {	// start of fee
		err |= 0x10000 ;	// oopsy -- what now!?
		LOG(ERR,"%d: fee_scan %d: not start-of-FEE 0x%08X",rdo1,fee_ix,d[0]) ;
	}
	else {
		if(d[0]&0x00800000) {	// from real FEE
			err |= 0x20000 ;
			LOG(ERR,"%d: fee_scan %d: SAMPA overrun 0x%08X",rdo1,fee_ix,d[0]) ;
		}
		if(d[0]&0x00400000) {	// from real FEE: xoff was on
			//LOG(WARN,"fee_scan %d: XOFF on 0x%08X",fee_ix,d[0]) ;
		}
	}

	fee_evt_type = d[0] & 0xFF ;
	fee_pp = (d[0]>>16)&0xFF ;	// padplane id; aka fee_id

	LOG(DBG,"FEE %d = 0x%08X",fee_ix,d[0]) ;

	if(log_level>=2) LOG(TERR,"%d: FEE %d (0x%08X) fee_padplane %d, fee_evt_type 0x%02X",rdo1,fee_ix,d[0],fee_pp,fee_evt_type) ;

	d++ ;				// done with header

		
	if(fee_evt_type==0xF0) {				// physics trigger, have sampa
		for(int i=0;i<4;i++) {
			lane_ix = i ;
			d = lane_scan(d) ;
		}
	}
	else {	// non-physics trigger... typically send_config stuff
		LOG(WARN,"%d: non-physics",rdo1) ;
		d = fee_non_trgd(d) ;
	}
	
		
	if((d[0]&0xF0000000)!=0x40000000) {
		err |= 0x40000 ;	// oopsy -- what now!?
		LOG(ERR,"%d: fee_scan %d: not end-of-FEE 0x%08X",rdo1,fee_ix,d[0]) ;
	}
	else {
		if(d[0]&0x00800000) {
			err |= 0x80000 ;
			LOG(ERR,"fee_scan %d: SAMPA overrun 0x%08X",fee_ix,d[0]) ;
		}
		if(d[0]&0x00400000) {
			//LOG(WARN,"fee_scan %d: XOFF on 0x%08X",fee_ix,d[0]) ;
		}
	}

	d++ ;

#if 0
	if(fee_errs) {
		for(int i=0;i<(d-d_save);i++) {
			printf("fee_ix %2d: %d = 0x%08X\n",fee_ix,i,d_save[i]) ;
		}
	}
#endif

	return d ;
}

int itpc23::rdo_scan(char *c_addr, int iwords)
{
	words = iwords ;

	u_int *d = (u_int *)c_addr ;
	
	trl = d + words ;

	// skip TEF header
	d += 4 ;
	d_start = d ;	// remember

	err = 0 ;
	//evt++ ;

	fee_ix = 0 ;
	prog_fulls = 0 ;

	// now at 0xCCCC001C
	// if 0xCCCC001C -- FY23 format
	// if 0x001CCCCC -- FY22 format
	if(d[0]==0xCCCC001C || d[0]==0x001CCCCC) ;	// ALL ok
	else {
		LOG(ERR,"%d: evt %d: bad header 0x%08X",rdo1,evt,d[0]) ;
		for(int i=-2;i<=2;i++) {
			LOG(ERR,"   %d = 0x%08X",i,d[i]) ;
		}

		err |= 0x1 ;
	}

	//1: event type, version, sector-id
	//2: trigger
	//3: event start 1 MHz counter
	//4: FEE status: synced|overrun
	//5: some status: prog_full|misc_busy_stuff
	//6: another status: 0|fee_empty

	trg_cmd = (d[2]>>0)&0xF ;
	daq_cmd = (d[2]>>4)&0xF ;


	// unwrap token
	{
		u_short t_hi = (d[2]>>8)&0xF ;
		u_short t_mid = (d[2]>>12)&0xF ;
		u_short t_lo = (d[2]>>16)&0xF ;

		token = (t_hi<<8)|(t_mid<<4)|t_lo ;
	}
	

	if(log_level>=1) LOG(TERR,"%d: T %d(%d,%d)",rdo1,token,trg_cmd,daq_cmd) ;

	if(log_level>=10) {
	for(int i=0;i<8;i++) {
		LOG(TERR,"rdo_scan %d/%d = 0x%08X",i,words,d[i]) ;
	}
	}

	u_int mhz_start = d[3] ;
	u_int fee_synced = d[4]>>16 ;
	u_int fee_overrun = d[4]&0xFFFF	;		
	u_int fee_xoff = d[5]>>16 ;			// actually prog_full
	u_int rdo_stuff = d[5]&0xFFFF ;
	u_int fee_empty = d[6]&0xFFFF ;
//	u_int sig = d[7] ;

	// I need a special hack here when running in Offline (from a file)
	// because I might have a FEE masked in online...
	if(!online) {
		fee_mask = fee_synced ;
	}

//	LOG(TERR,"fee_mask 0x%X, fee_synced 0x%X, fee_overrun 0x%X, fee_xoff 0x%X, rdo_stuff 0x%X, fee_empty 0x%X, sig 0x%X",
//	    fee_mask,fee_synced,fee_overrun,fee_xoff,rdo_stuff,fee_empty,sig) ;

	if((fee_synced&fee_mask)!=fee_mask) {
		LOG(ERR,"%d: evt %d: fee sync error 0x%04X, expect 0x%04X",rdo1,evt,fee_synced,fee_mask) ;
		// STOP: auto-recovery
		err |= 0x10 ;
	}

	if(fee_overrun&fee_mask) {
		LOG(ERR,"%d: %d: RDOs fee FIFO overrun 0x%04X",rdo1,evt,fee_overrun&fee_mask) ;
		// STOP: auto-recovery
		err |= 0x10 ;
	}

	if((fee_xoff&fee_mask) != 0) {
		//LOG(WARN,"fee_xoff 0x%04X",fee_xoff&fee_mask) ;
		// note as interesting
	}

	if(rdo_stuff & 0xF000) {	// revisit this... what is it?
		LOG(NOTE,"%d: rdo_stuff 0x%04X",rdo1,rdo_stuff) ;
	}


	// revisit this...
	if((fee_empty&fee_mask)==fee_mask) ;	// I expect it to be empty
	else {
		//if(!fee_words) LOG(WARN,"fee_empty 0x%04X",fee_empty&fee_mask) ;
	}


	// go to the end
	int got_it = 0 ;
	for(int i=0;i>-35;i--) {
//		LOG(TERR,"    %d = 0x%08X",i,trl[i]) ;

		if(trl[i]==0xDEADC0DE) got_it |=1 ;
		else if(got_it && (trl[i]==0xFEEDC0FE)) {
			got_it |= 2 ;
			trl += i - 1 ;	// at the last datum
			break ;
		}
	}

	if(got_it != 3) {
		LOG(ERR,"%d: %d: no trailer (0x%08X), %d",rdo1,evt,trl[0],got_it) ;
		// STOP: auto-recovery
		err |= 0x2 ;
//		for(int i=0;i<words;i++) {
//			LOG(ERR,"   %d = 0x%08X",i,d_start[i]) ;
//		}
	}

	trl -= 1 ;
	u_int evt_status = trl[0] ;
	u_int mhz_end = trl[1] ;


//	LOG(TERR,"trl0 0x%08X, trl1 0x%08X",trl[0],trl[1]) ;

	u_int evt_type = (d[1]>>28)&0xF ;


	int trg_cou = 0 ;

	
	switch(evt_type) {
	case 1 :	// timer
		token = 4096 ;
		trg_cmd = 0 ;
		daq_cmd = 0 ;
		goto done ;
	case 2 :	// trigger!
		evt_trgd++ ;
		break ;
	default :
		LOG(ERR,"%d: %d: unknown event type %d: 0x%08X",rdo1,evt,evt_type,d[1]) ;
		err |= 0x4 ;
		goto done ;
		break ;
	}



	for(int i=0;i<16;i++) {
		if(fee_mask & (1<<i)) ;
		else continue ;

		u_int st = (evt_status>>(i*2)) & 0x3 ;

		if(st != 3) {
			err |= 0x1000 ;
			LOG(ERR,"%d: %d: FEE %2d: timeout 0x%X [0x%08X]",rdo1,evt,i,st,evt_status) ;
		}
	}

	// continue with data
	d += 7 ;		// should be at the trigger FIFO
	while((*d>>28)==0x4) {	// skip data from Trigger FIFO because we don't care much
		trg_cou++ ;
		d++ ;
	}
	
	if(trg_cou>8) {
		LOG(WARN,"Lots of triggers %d",trg_cou) ;
	}

	if(log_level>=1) LOG(TERR,"%d: evt %d: fee_mask expected 0x%04X",rdo1,evt,fee_mask) ;

	// should be at the FEE start: 0xF000_mmmm	
	if(((*d>>28)!=0xF)||((*d&0xFFFF)!=fee_mask)) {
		LOG(ERR,"%d: evt %d: Bad FEE_START 0x%08X",rdo1,evt,*d) ;
		err |= 0x20 ;
		goto done ;
	}


	d++ ;	// move to start-of-FEE bank


	if(fee_words) goto done ;	// emulation: skip scans

#if 0
	printf("==== trg_evts %d\n",trgd_evt) ;
	for(int i=0;i<(trl-d);i++) {
		printf("%d = 0x%08X\n",i,d[i]) ;
	}
	fflush(stdout) ;
#endif

//	if(run_type==1 || run_type==5) {
//		pthread_mutex_lock(&peds_mutex) ;
//	}

	if(log_level>=2) LOG(TERR,"here") ;

//	LOG(TERR,"%d: fee_mask 0x%08X",rdo1,fee_mask) ;

	for(int i=0;i<16;i++) {
		if(fee_mask & (1<<i)) ;
		else continue ;

		fee_ix = i ;
		d = fee_scan(d) ;
		if(d>=trl) break ;
	}

	if(log_level>=2) LOG(TERR,"here2") ;

//	if(run_type==1 || run_type==5) {
//		pthread_mutex_unlock(&peds_mutex) ;
//	}


	
	done:;



	if(err||prog_fulls) {
		LOG(ERR,"%d: evt %d/%d: T %d,%d,%d: error 0x%06X, prog_fulls %d: words %d, %d us",rdo1,evt_trgd,evt,
		    token,trg_cmd,daq_cmd,
		    err,
		    prog_fulls,
		    words,mhz_end-mhz_start) ;
	}
	else if(token==4096) {
		LOG(DBG,"%d/%d: T %d,%d,%d: error 0x%06X, prog_fulls %d, rdo_stuff 0x%04X: words %d, %d us",evt_trgd,evt,
		    token,trg_cmd,daq_cmd,
		    err,
		    prog_fulls,rdo_stuff,
		    words,mhz_end-mhz_start) ;
	}
	else if((evt_trgd%1000)==1) {
		if(log_level>=1) {
			LOG(TERR,"%d/%d: T %d,%d,%d: error 0x%06X, prog_fulls %d, rdo_stuff 0x%04X: words %d, %d us",evt_trgd,evt,
			    token,trg_cmd,daq_cmd,
			    err,	
			    prog_fulls,rdo_stuff,
			    words,mhz_end-mhz_start) ;
		}

	}
	else {
		LOG(NOTE,"%d/%d: T %d,%d,%d: error 0x%06X, prog_fulls %d, rdo_stuff 0x%04X: words %d, %d us",evt_trgd,evt,
		    token,trg_cmd,daq_cmd,
		    err,
		    prog_fulls,rdo_stuff,
		    words,mhz_end-mhz_start) ;

	}

	return 0 ;	// 0 is no error
}




u_int itpc23::get_token_s(char *c_addr, int words) 
{
	u_int *d = (u_int *)c_addr ;
	int t, trg, daq ;

	t = 4097 ;
	trg = 0 ;
	daq = 0 ;

//	for(int i=0;i<32;i++) {
//		LOG(TERR,"get_token_s: %d/%d = 0x%08X",i,words,d[i]) ;
//	}
	
	
	u_int sig, trg_w,sub ;
	u_int fmt23 ;
//	if(d[4]==0xCCCC001C) {
	if(d[4]==0xCCCC001C && fmt<23) {
		sig = sw16(d[5]) ;
		trg_w = sw16(d[6]) ;
		sub = sw16(d[7]) ;
	}
	else {
		sig = d[5] ;
		trg_w = d[6] ;
		sub = d[7] ;
	}

//	LOG(TERR,"0x%08X: 0x%08X 0x%08X 0x%08X",d[4],sig,trg_w,sub) ;

	if((sig&0xFF000000)==0x98000000) {
		fmt23 = 0 ;
	}
	else {
		fmt23 = 23 ;
	}

	if(fmt23>22) {
		int evt_type ;

		// unwrap token
		t = 0xFFF ;
		{
			u_short t_hi = (trg_w>>8)&0xF ;
			u_short t_mid = (trg_w>>12)&0xF ;
			u_short t_lo = (trg_w>>16)&0xF ;

			t = (t_hi<<8)|(t_mid<<4)|t_lo ;
		}

		trg = (trg_w>>0)&0xF ;
		daq = (trg_w>>4)&0xF ;

		evt_type = (sig>>28)&0xF ;

		if(evt_type==1) {	// timer
			t = 4096 ;
			trg = 0 ;
			daq = 1 ;
		}

	}
	else {

#if 0
		d += 4 ;	// skip header
		words -= 4 ;

		for(int i=0;i<16;i++) {
			if(d[i] == 0xCCCC001C || d[i]==0x001CCCCC) {
				d = d + i ;
				words-- ;
				break ;
			}
		}
#endif


//		for(int i=0;i<32;i++) {
//			LOG(TERR,"%d/%d = 0x%08X",i,words,d[i]) ;
//		}


		int rdo_version = 0 ;

		if(sig==0x98000004) rdo_version = 0 ;
		else if((sig&0xFF00000F)==0x98000004) rdo_version = (sig>>4)&0xFF ;
		else {
			LOG(ERR,"%d: fmt 22: not triggered: ds 0x%08X, words",rdo1,sig,words) ;
			t = 4096 ;
			goto done ;
		}




		if(trg_w==0) {
			if(rdo_version==1) {
				if(sub==0x980000FC) {
					LOG(WARN,"%d: RDO_mon, words %d",rdo1,words) ;
				}
			}
		}


		t = ((trg_w>>8)&0xF)<<8 ;
		t |= ((trg_w>>12)&0xF) << 4 ;
		t |= ((trg_w>>16)&0xF) ;

		trg = trg_w & 0xF ;
		daq = (trg_w>>4) & 0xF ;
 
//		LOG(TERR,"%d: rdo_version %d, trg_fired 0x%08X: T %d, trg %d:%d",rdo1,rdo_version,trg_fired,t,trg,daq) ;

		if(trg_w==0) {
			t = 4097 ;
			trg = 0 ;
			daq = 0 ;
			goto done ;
		}


	}

	done:;

//	LOG(TERR,"T %d, trg %d:%d",t,trg,daq) ;



	return (trg<<16)|(daq<<12)|t ;
}


itpc23::itpc23()
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

	rts_id = ITPC_ID ;

	if(rp_gain_itpc==0) {
		rp_gain_itpc = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;

		// inititalize here!
		// initialize here!
		for(int s=0;s<24;s++) {
		for(int r=0;r<=ROW_MAX;r++) {
		for(int p=0;p<=PAD_MAX;p++) {
			rp_gain_itpc[s][r][p].gain = 1.0 ;
			rp_gain_itpc[s][r][p].t0 = 0.0 ;
			rp_gain_itpc[s][r][p].flags = 0 ;
		}}}


	}

	rp_gain = rp_gain_itpc ;

//	if(rp_gain==0) {
//		rp_gain = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;
//	}
	
	row_min = 1 ;
	row_max = 40 ;

        for(int row=1;row<=40;row++) rowlen[row] = itpc_rowlen[row] ;

	data_c = 0 ;


	fmt = 0 ;

}


itpc23::~itpc23()
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

	return ;
}

u_char itpc23::flags_row_pad(int asic, int channel, int &row, int &pad)
{
	int id, sampa ;

	id = asic>>1 ;	// FEE padplane id
	sampa = asic & 1 ;

	itpc_sampa_to_rowpad(id,sampa,channel,row,pad) ;
	if(row==0 || row==255) return 0xFF ;
	if(pad==0 || pad==255) return 0xFF ;


	return rp_gain[sector1-1][row][pad].flags ;
}


static const char *hwicap_version(u_int v) 
{	
	static char ascii[64] ;

        int s = v & 0x3F ;
        int m = (v>>6)&0x3F ;
        int h = (v>>12)&0x1F ;
        int y = ((v>>17)&0x3F) ;
        int mo = (v>>23)&0xF ;
        int d = (v>>27)&0x1F ;


        sprintf(ascii,"%02d-%02d-%02d %02d:%02d:%02d",
                mo,d,y,h,m,s) ;

	return ascii ;
}


//static const int itpc_fee_map[24][4][16] = {
static int itpc_fee_map[24][4][16] = {
{//S1 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},   //ok 
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}  
},
{//S2 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
//usual	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
//	{ 7, 1,17,12,24, 0,13, 8,28, 2,19,20,29,25,21, 3}, // moved #6 to #11 
	{ 7, 1, 0,12,24,17,13, 8,28, 2,19,20,29,25,21, 3}, // moved #6 to #11; and #3 to #6
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S3 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S4 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S5 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S6 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S7 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S8 mess with mapping
	{49,52,46, 0, 0, 54,0,47, 0,50,48,55, 0, 0,51,53}, 	//FY21: bad port #13 on RDO ID 0x0052ED7C moved to #11
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S9 mess with mapping
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S10 checked
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S11
//	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, // usual
	{49,52,46,47, 0, 54,0, 0, 0,50, 0,55,48, 0,51,53}, // new: bad port #8 on RDO ID 0x0052F5EA, moved to #4
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S12
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    

},
{//S13
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S14
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S15
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S16
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S17
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S18
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S19
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S20
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S21
//	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{49,52,46, 0, 0,54,47, 0, 0,50, 0,55,48, 0,51,53},  // moved #8 to #7
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S22
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S23
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
{//S24
	{49,52,46, 0, 0, 54,0,47, 0,50, 0,55,48, 0,51,53}, 
	{36,32,40,43,37,33, 0,41, 0,44,38,34,42,45,39,35},  
	{ 7, 1,17,12,24,19,13, 8,28, 2, 0,20,29,25,21, 3}, 
	{ 9, 4,26,14,15,10,30,22,27, 5,31,23,18,16,11, 6}    
},
} ;


// I could optimize this into an array on the first call
static u_int get_ifee_mask(int sec1, int rdo1)
{
	u_int mask = 0 ;

	for(int i=0;i<16;i++) {
		if(itpc_fee_map[sec1-1][rdo1-1][i]) mask |= (1<<i) ;
	}

	return mask ;
}

void itpc23::itpc_fee_kill(int s0, int r0, int p0)
{
	itpc_fee_map[s0][r0][p0] = 0 ;
}
