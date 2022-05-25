#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>

#include <rtsLog.h>

#include <rtsSystems.h>

#include <DAQ1000/ddl_struct.h>
#include <DAQ1000/rdo_cmds.h>



#include "tpx23.h"

inline void tpx23::set_rdo(int s, int r)
{
	sector1 = s ;
	rdo1 = r ;
}


u_int *tpx23::fee_scan() 
{
	get_token((char *)d_start,words) ;



#if 0
	int fee_wds = (d_end+1) - (d_start+2) ;

	LOG(TERR,"Evt %d: S%02d:%d: T %d, trg %d, daq %d: fee words %d vs %d",evt_trgd,
	    sector1,rdo1,
	    token,trg_cmd,daq_cmd,
	    fee_wds,words) ;

#endif

	// first valid FEE word is at d_start+2 ;
	// last valid FEE word is at d_end
	u_int *h = d_end ;

	// NOTE: ALTRO scans from the end!!!
	while(h>(d_start+2)) {
		u_int hi, lo ;

		lo = *h-- ;
		hi = *h-- ;

		int wc = ((hi&0x3F)<<4)|((lo&0xF0000)>>16) ;    // altro's word count
		if(wc==0) continue ;

		int id = (lo&0xFF0) >> 4 ;      // altro id
		int ch = lo & 0xF ;

		//LOG(TERR,"%d: A%03d:%02d: words %d",rdo1,id,ch,wc) ;

		while(wc%4) wc++ ;

		// this now needs to go into the canonical format!
		int row ;
		int pad ;

		// get row,pad & flags and skip the pad if there are flags
		if(flags_row_pad(id,ch,row,pad)) {
			h -= wc/2 ;
			continue ;
		}

		u_short *d = s1_dta + last_ix ;	// this is where the raw data goes...
		//u_short d[512] ;

		//LOG(TERR,"%d: rp %d:%d; last_ix %d %p",rdo1,row,pad,last_ix,d) ;

		int ix = 0 ;
		for(int i=0;i<wc;) {	// NOTE: no increment!
			lo = *h-- ;
			hi = *h-- ;

			if(ix==0) {	// see if I need to skip the first dummies!!!
				u_short dd[4] ;

				dd[0] = (hi>>10)&0x3FF ;
				dd[1] = hi & 0x3FF ;
				dd[2] = (lo>>10)&0x3FF ;
				dd[3] = lo & 0x3FF ;

				int s = 0 ;
				if(dd[0]==0x2AA) {
					s = 1 ;
					if(dd[1]==0x2AA) {
						s = 2 ;
						if(dd[2]==0x2AA) {
							s = 3 ;
						}
					}
				}

				for(;s<4;s++) {
					d[ix++] = dd[s] ;
				}
			}
			else {
				d[ix++] = (hi>>10)&0x3FF ;
				d[ix++] = hi & 0x3FF ;
				d[ix++] = (lo>>10)&0x3FF ;
				d[ix++] = lo & 0x3FF ;
			}

			i += 4 ;
		}

		u_short *dd = d ;

		int seq_ix = 0 ;
		struct seq_t *seq = s1[row][pad].seq ;


		//LOG(TERR,"Here 1") ;
		struct sseq_t {
			u_short t_lo ;
			u_short t_hi ;
			u_short d[512] ;
		} sseq[SEQ_MAX] ;

		while(dd<(d+ix)) {
			u_short t_lo ;

			u_short t_len = *dd++ - 2 ;
			u_short t_hi = *dd++ ;

			if(seq_ix>=(SEQ_MAX-1)) {	// break if too many: note that I need to leave space for the sentinel!
				break ;
			}

			t_lo = t_hi - t_len + 1 ;

			//printf("rp %d:%d: seq %d: t_len %d, t_lo:hi %d:%d\n",row,pad,seq_ix,t_len,t_lo,t_hi) ;

			sseq[seq_ix].t_lo = t_lo ;
			sseq[seq_ix].t_hi = t_hi ;
			//sseq[seq_ix].dta_p = (dd-d) ;	// I'm at the data now

			for(int i=0;i<t_len;i++) {
				short adc = *dd++ ;
				sseq[seq_ix].d[i] = adc ;
				//printf("    adc %d = %d\n",i,adc) ;
			}
			seq_ix++ ;

			//dd += t_len ;	// skip over data...

		}

		//LOG(TERR,"Here 2") ;
		int s_cou = 0 ;
		dd = d ;
		seq = s1[row][pad].seq ;

		//printf("row %d, pad %d: seq_ix %d\n",row,pad,seq_ix) ;

		for(int i=(seq_ix-1);i>=0;i--) {
			seq[s_cou].t_lo = sseq[i].t_lo;
			seq[s_cou].t_hi = sseq[i].t_hi ;
			seq[s_cou].dta_p = (dd-d) ;
			seq[s_cou].blob_id = 0 ;

			int t_len = sseq[i].t_hi - sseq[i].t_lo + 1 ;
			
			//printf("... new seq %d: lo %d, hi %d\n",s_cou,seq[s_cou].t_lo,seq[s_cou].t_hi) ;

			for(int j=(t_len-1);j>=0;j--) {
				*dd++ = sseq[i].d[j] ;
			}
			s_cou++ ;
		}

		ix = dd - d ;

		seq[s_cou].t_hi = 0 ;	// sentinel; marker



#if 0
		seq = s1[row][pad].seq ;
		while(seq->t_hi) {
			int t_len = seq->t_hi - seq->t_lo + 1 ;
			printf("t_lo %d, t_hi %d\n",seq->t_lo,seq->t_hi) ;

			dd = d + seq->dta_p ;
			for(int j=0;j<t_len;j++) {
				printf("   adc %d: %d\n",j,dd[j]) ;
			}

			seq++ ;
		}
#endif

		s1[row][pad].ix = last_ix ;	// remember where this data is
		sequence_cou += s_cou ;
		last_ix += ix ;
	}

	return 0 ;
}

u_int tpx23::get_token_s(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;
	int t, trg, daq ;

	t = 4097 ;
	trg = 0 ;
	daq = 0 ;

	d += wds ;
	d-- ;		// at the last datum

	d -= 2 ;	// skip 2 words trailer

	int trg_cou = *d ;

	d -= trg_cou * (sizeof(struct trg_data)/4) ;

	struct trg_data *trg_d = (struct trg_data *)d ;
	for(int i=0;i<trg_cou;i++) {
//		LOG(TERR,"trg_data %d: 0x%X 0x%X 0x%X",i,trg[i].rhic_counter, trg[i].csr, trg[i].data) ;

		switch(trg_d[i].csr & 0xFF000000) {
		case 0xFF000000 :	// FIFO stuff?
		case 0xDD000000 :	// FIFO stuff?
			break ;
		case 0xEE000000 :	// prompt token
		default :
			t = trg_d[i].data & 0xFFF ;
			daq = (trg_d[i].data >> 12)&0xF ;
			trg = (trg_d[i].data >> 16)&0xF ;
			goto done ;
		}
	}

	done:;

	return (trg<<16)|(daq<<12)|t ;
}

u_int tpx23::get_token(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;

	token = 4097 ;
	trg_cmd = 0 ;
	daq_cmd = 0 ;

	d += wds ;
	d-- ;		// at the last datum

	d -= 2 ;	// skip 2 words trailer

	int trg_cou = *d ;

	d -= trg_cou * (sizeof(struct trg_data)/4) ;

	struct trg_data *trg = (struct trg_data *)d ;
	for(int i=0;i<trg_cou;i++) {
//		LOG(TERR,"trg_data %d: 0x%X 0x%X 0x%X",i,trg[i].rhic_counter, trg[i].csr, trg[i].data) ;

		switch(trg[i].csr & 0xFF000000) {
		case 0xFF000000 :	// FIFO stuff?
		case 0xDD000000 :	// FIFO stuff?
			break ;
		case 0xEE000000 :	// prompt token
		default :
			token = trg[i].data & 0xFFF ;
			daq_cmd = (trg[i].data >> 12)&0xF ;
			trg_cmd = (trg[i].data >> 16)&0xF ;
			goto done ;
		}
	}

	done:;

	d_end = d - 3 ;	// very last ALTRO datum

	return (trg_cmd<<16)|(daq_cmd<<12)|token ;
}


int tpx23::rdo_scan(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;
	char obuff[1024] ;
	char *c ;
	int max_cou ;
	int cou ;

	int rdo = (d[0]>>8)&0xF ;
	int sec = (d[0]>>12)&0xFF ;

	subtype = (d[0]>>4)&0xF ;	//class
	type = (d[0]>>0)&0xF ;		//class
				
	d_start = d ;
	token = 4096 ;
	trg_cmd = 0 ;
	daq_cmd = 0 ;
	err = 0 ;

	words = wds ;

	evt++ ;



	if(rdo!=rdo1 || sec!=sector1) {
		LOG(ERR,"%d: wrong sec,rdo: rdo expect %d is %d; sector expect %d is %d",rdo1,
		    rdo1,rdo,sector1,sec) ;
	}


	switch(type) {
	case DDL_TYPE_LOG :
		LOG(WARN,"%d: event %d: S%02d:%d: type %d:%d, words %d",rdo1,d[1],sec,rdo,type,subtype,words) ;
		max_cou = (words-4)*4 ;
		cou = 0 ;
		c = (char *)(d+2) ;
		while(max_cou) {
			obuff[cou] = *c ;
			if(*c=='\n') {
				obuff[cou] = 0 ;
				LOG(INFO,"%d: [%s]",rdo1,obuff) ;
				cou = 0 ;
			}
			else {
				obuff[cou] = *c ;
				cou++ ;
			}
			c++ ;
			max_cou-- ;
		}
		break ;
	case DDL_TYPE_MSC :
		LOG(WARN,"%d: event %d: S%02d:%d: type %d:%d, words %d",rdo1,d[1],sec,rdo,type,subtype,words) ;
		cou = words ;
		if(cou>16) cou=16 ;
		
		if(subtype!=2) {	// skip heartbeat
			for(int w=0;w<cou;w++) {
				LOG(TERR,"%d: MSC_%d  %d/%d = 0x%08X",rdo1,subtype,w,words,d[w]) ;
			}
		}

		break ;
	default :
		evt_trgd++ ;
		fee_scan() ;
		break ;
	}

	return 0 ;

}


tpx23::tpx23()
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

	rts_id = TPX_ID ;

	if(rp_gain_tpx==0) {
		rp_gain_tpx = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;
	}

	rp_gain = rp_gain_tpx ;
	
}


#include <DAQ_TPX/tpxCore.h>

u_char tpx23::flags_row_pad(int asic, int channel, int &row, int &pad)
{
	// I will rewrite this to make it super-fast

	tpx_from_altro(rdo1-1,asic,channel,row,pad) ;	// from tpxCore!
	if(row==0 || row==255) return 0xFF ;
	if(pad==0 || pad==255) return 0xFF ;

//	row -= 13 ;	// HACK: row 14 becomes row 1

	return rp_gain[sector1-1][row][pad].flags ;
}

