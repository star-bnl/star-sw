#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>
#include <ctype.h>
#include <pthread.h>

#include <rtsLog.h>

#include <rtsSystems.h>

#include <DAQ1000/ddl_struct.h>
#include <DAQ1000/rdo_cmds.h>

#include <TPC/rowlen.h>
#include <TPX/tpx_altro_to_pad.h>

#include <DAQ_TPX/tpxCore.h>
#include <DAQ_TPX/tpxPed.h>
#include <DAQ_TPX/tpxGain.h>

#include "tpx23.h"


tpxPed *tpx23::peds ;


int tpx23::from22to23(char *c_addr, int words)
{ 
	// does NOTHING yet
	return words ;
} 

inline void tpx23::set_rdo(int s, int r)
{

	sector1 = s ;
	rdo1 = r ;
}

u_int *tpx23::fee_scan() 
{
	u_int *h ;

	get_token((char *)d_start,words) ;


	if(run_type==1 || run_type==5) {
		int r0_logical = tpx36_from_real(subdet_id,sector1,rdo1) - 1 ;


//		pthread_mutex_lock(&peds_mutex) ;

		peds->valid_evts[r0_logical]++ ;
		peds->evts[r0_logical]++ ;

		peds->sector = subdet_id ;	// this is the PC id 1..36!

		if(peds->evts[r0_logical]<=3) goto done ;	// skip first 3 events
		if(peds->valid_evts[r0_logical]>1000) goto done ;	// enough...


	}

#if 0
	int fee_wds = (d_end+1) - (d_start+2) ;

	LOG(TERR,"Evt %d: S%02d:%d: T %d, trg %d, daq %d: fee words %d vs %d",evt_trgd,
	    sector1,rdo1,
	    token,trg_cmd,daq_cmd,
	    fee_wds,words) ;

#endif

	// first valid FEE word is at d_start+2 ;
	// last valid FEE word is at d_end
	h = d_end ;

	// NOTE: ALTRO scans from the end!!!
	while(h>(d_start+2)) {
		u_int hi, lo ;

		lo = *h-- ;
		hi = *h-- ;

		int wc = ((hi&0x3F)<<4)|((lo&0xF0000)>>16) ;    // altro's word count
		if(wc==0) continue ;

		int id = (lo&0xFF0) >> 4 ;      // altro id
		int ch = lo & 0xF ;


		for(int i=0;i<tpx_fee_override_cou;i++) {
			if(sector1 == tpx_fee_override[i].sector) {
			if(rdo1==tpx_fee_override[i].rdo) {
				int fee = id & 0xFE ;

				if(fee == tpx_fee_override[i].curr_altro) {
					int should = tpx_fee_override[i].orig_altro ;

					if(id & 1) should |= 1 ;

					LOG(WARN,"S%2:%d overriding ALTRO id %d with %d",id,should) ;
					id = should ;
					break ;
				}
			}
			}
		}
					

		//LOG(TERR,"%d: A%03d:%02d: words %d",rdo1,id,ch,wc) ;

		while(wc%4) wc++ ;

		// this now needs to go into the canonical format!
		int row ;
		int pad ;

		// get row,pad & flags and skip the pad if there are flags
		int flags = flags_row_pad(id,ch,row,pad) ;

		// if this is a physics run: skip pads which have flags
		// hmm... is this right?
		if(flags && run_type==3) {
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

		if(run_type==1 || run_type==5) {
			tpx_altro_struct a ;

			memset(&a,0,sizeof(a)) ;
			a.what = TPX_ALTRO_DO_ADC ;
			a.t = 1	; //invent token
			a.row = row ;
			a.pad = pad ;
			a.id = id ;
			a.ch = ch ;
			a.rdo = rdo1-1 ;
			a.sector = sector1 ;
			a.count = 435 ;	// matches tb from 0..434

			int aix = 0 ;

			for(int i=(seq_ix-1);i>=0;i--) {
				int t_len = sseq[i].t_hi - sseq[i].t_lo + 1 ;

				int ii = 0 ;
				for(int j=(t_len-1);j>=0;j--) {
					int adc = sseq[i].d[j] ;
					a.adc[aix] = adc ;
					a.tb[aix] = sseq[i].t_lo + ii ;
					ii++ ;
					aix++ ;
				}

				peds->accum(&a) ;
			}
		
		}

		


		//LOG(TERR,"Here 2") ;
		int s_cou = 0 ;
		dd = d ;
		seq = s1[row][pad].seq ;

//		printf("row %d, pad %d: seq_ix %d\n",row,pad,seq_ix) ;

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

//		seq[s_cou].t_hi = -1 ;	// sentinel; marker


		sequence_cou += s_cou ;

		seq[s_cou].t_hi = -1 ;
		s1[row][pad].ix = last_ix ;	// remember where this data is

		last_ix += ix ;


#if 0
		seq = s1[row][pad].seq ;
		while(seq->t_hi>=0) {
			int t_len = seq->t_hi - seq->t_lo + 1 ;
			printf("rp %d:%d, t_lo %d, t_hi %d\n",row,pad,seq->t_lo,seq->t_hi) ;

			dd = d + seq->dta_p ;
			for(int j=0;j<t_len;j++) {
				printf("   adc %d: %d\n",j,dd[j]) ;
			}

			seq++ ;
		}
#endif


		
	}

	done:;

//	if(run_type==1 || run_type==5) {
//		pthread_mutex_unlock(&peds_mutex) ;
//	}

	return 0 ;
}

u_int tpx23::get_token_s(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;
	int t, trg, daq ;
	int trg_cou ;
	struct trg_data *trg_d ;

	t = 4097 ;
	trg = 0 ;
	daq = 0 ;

//	int rdo = (d[0]>>8)&0xF ;
//	int sec = (d[0]>>12)&0xFF ;
//	int subtype = (d[0]>>4)&0xF ;
	int type = (d[0]>>0)&0xF ;
				

	

	switch(type) {
	case DDL_TYPE_LOG :
//		LOG(WARN,"log event") ;
		goto done ;
	case DDL_TYPE_MSC :
//		LOG(WARN,"msc event") ;
		goto done ;
	default :
//		LOG(WARN,"%d: event %d: S%02d:%d: type %d:%d, words %d",rdo,d[1],sec,rdo,type,subtype,wds) ;
//		goto done ;
		break ;
	}


//	for(int i=0;i<16;i++) {
//		LOG(TERR,"%2d = 0x%08X",i,d[i]) ;
//	}


	d += wds ;
	d-- ;		// at the last datum

//	for(int i=-16;i<=0;i++) {
//		LOG(TERR,"%2d = 0x%08X",i,d[i]) ;
//	}


	d -= 2 ;	// skip 2 words trailer

//	for(int i=0;i<16;i++) {
//		LOG(TERR,"%2d = 0x%08X",i,d[i]) ;
//	}


	trg_cou = *d ;

	d -= trg_cou * (sizeof(struct trg_data)/4) ;

//	LOG(TERR,"get_token_s: trg_cou %d, %p",trg_cou,d) ;


	trg_d = (struct trg_data *)d ;

	for(int i=0;i<trg_cou;i++) {
//		LOG(WARN,"trg_data %d: 0x%X 0x%X 0x%X",i,trg_d[i].rhic_counter, trg_d[i].csr, trg_d[i].data) ;

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


// This unfortunately needs to be globally visible as per tpxCore.cxx
//struct tpx_rdo tpx_rdo[24][6] ;

int tpx23::msc_dump(char *c_addr, int wds)
{


	LOG(INFO,"msc_dump: %d bytes, should be %d: Sreal%d:%d",wds*4,sizeof(struct tpx_rdo),
	    sector1,rdo1) ;

	// modelled after tpxCore.cxx tpx_show_status
	// struct tpx_rdo has
	//
	c_addr += 2*4 ;	// skip 2 header words?

	memcpy(&(tpx_rdo[sector1-1][rdo1-1]),c_addr,sizeof(struct tpx_rdo)) ;

	// modify tpx_show_status to have a pointer to the data instead of this thread-unsafe tpx_rdo static!
	tpx_show_status(sector1,1<<(rdo1-1),0) ;

	return 0 ;
}

int tpx23::log_dump(char *c_addr, int wds)
{
	int len, cou ;




	char *tmpbuff ;
	int non_ascii ;
	int do_log ;
	u_int err_status = 0 ;
	char *rdobuff ;
	

	int s_real, r_real ;
//	tpx36_to_real(subdet_id,rdo,s_real,r_real) ;
	s_real = sector1 ;
	r_real = rdo1 ;

	int rdo = rdo1 ;

//	do_log = (log_file) ? 0 : 1 ;
	do_log = 1 ;	// to tpx.log

//	LOG(TERR,"USing long_dump: rdo %d, log to LOG %d", rdo, do_log) ;

	non_ascii = 0 ;	

//	max_cou = (words-4)*4 ;
	rdobuff = (char *)(c_addr+2*4) ;	// skip header

	// one liner or more?
	len = strlen(rdobuff) ;
	cou = 0 ;

	tmpbuff = (char *) malloc(len+1) ;
	memcpy(tmpbuff,rdobuff,len+1) ;

	// tokenize into strings
	for(int i=0;i<len;i++) {
		if(tmpbuff[i]=='\n') {
			cou++ ;
			tmpbuff[i] = 0 ;
		}
	}

//	LOG(TERR,"%d lines %s",cou,rdobuff) ;
	
	for(int i=0;i<(len);) {	// was len+1?

		int st = i ;
		int err = 0 ;

		// check for non-printable chars; should be the same as the
		// new SRAM check
		for(int j=st;j<len;j++) {
			if(tmpbuff[j] == 0) break ;	// new line

			if(!isprint(tmpbuff[j])) {
				if(tmpbuff[j] == 9) ;	// skip tab
				else {
					LOG(WARN,"---> [%d LOG] Unprintable character 0x%02X? -- powercycle",rdo,tmpbuff[j]) ;
					LOG(WARN,"But ignored for FY22") ;
					//err_status |= DET_ERR_OPER_PS ;
					err = -1 ;
					tmpbuff[j] = '?' ;
				}
			}

		}


		if(strstr(tmpbuff+st,"SPECIAL_0 code")) {
			LOG(ERR,"---> SPECIAL code: RDO %d",rdo) ;
		}

		// check for error string but just print a warning, we'll be more selective later
		if(strstr(tmpbuff+st,"ERR")) {
			err = -1 ;
			LOG(ERR,"---> [%d LOG]: contains ERR \"%s\"",rdo,tmpbuff+st) ;
		}

		// check for question mark in CPLD status
		if(strstr(tmpbuff+st,"Status: Xil config")) {
			if(strstr(tmpbuff+st,"!")) {
				//err_status |= DET_ERR_LOCAL_PS ;
				LOG(WARN,"---> [S%d:%d LOG] CPLD status has \"!\" -- ignored ",s_real,r_real) ;
				//err = -1 ;
			}
			
			if(strstr(tmpbuff+st,"FEE power BAD")) {
				//err_status |= DET_ERR_OPER_PS ;
				LOG(WARN,"---> [S%d:%d LOG] FEE power BAD -- powercycle (ignored)",s_real,r_real) ;
				//err = -1 ;
			}
		}


		if(strstr(tmpbuff+st,"SRAM check failed")) {
			err = -1 ;
			LOG(WARN,"---> [%d LOG] SRAM check failed -- powercycle",rdo) ;
		}

		if(strstr(tmpbuff+st,"CPLD claims error")) {
			err = -1 ;
			LOG(WARN,"---> [%d LOG] CPLD claims error -- reconfig 0x300",rdo) ;
		}
		
		if(strstr(tmpbuff+st,"can't configure RDO!")) {
			LOG(ERR,"---> [%d LOG] \"can't configure RDO\" -- reconfig 0x300",rdo) ;
//			err = -1 ;
		}	


		// mostly run related
		if(strstr(tmpbuff+st,"lost RHIC")) {
			LOG(WARN,"---> [%d LOG] \"lost RHIC\" -- restart run",rdo) ;
			err = -1 ;
		}	
		if(strstr(tmpbuff+st,"NO RHIC CLOCK")) {
			LOG(WARN,"---> [%d LOG] \"NO RHIC CLOCK\" -- restart run",rdo) ;
			err = -1 ;
		}	

		if(strstr(tmpbuff+st,"DRIFT")) {
			LOG(WARN,"---> [%d LOG] \"DRIFT/clock problems\" -- restart run",rdo) ;
			err = -1 ;
		}	


		if(strstr(tmpbuff+st,"CRIT")) {
			err = -1 ;
			LOG(WARN,"---> [%d LOG] CRIT string in log -- restart run",rdo) ;
		}
	
		if(strstr(tmpbuff+st,"altro error")) {
			err = -1 ;
			LOG(WARN,"---> [%d LOG] altro error -- restart run",rdo) ;
		}
	

		if(strstr(tmpbuff+st,"ERR ALTRO")) {
			err = -1 ;
			LOG(WARN,"---> [%d LOG] ERR ALTRO -- CHECK THIS",rdo) ;
		}
	

		
		
		if(err<0) {
			LOG(ERR,"[S%d:%d LOG %d]: %s",s_real,r_real,rdo,tmpbuff+st) ;
		}
		else if(do_log) {
			LOG(INFO,"[S%d:%d LOG %d]: %s",s_real,r_real,rdo,tmpbuff+st) ;
		}
		
		while(tmpbuff[i]) {
			//LOG(WARN,"%d: %d [%c]",i,tmpbuff[i],tmpbuff[i]) ;
			i++ ;
			if(i>=len) break ;
		}
		i++ ;
			
	}

//	TLOG() ;
	free(tmpbuff) ;

#if 0

	t = time(NULL) ;
	ctime_r(&t,tm) ;
	tm[strlen(tm)-1] = 0 ;	// kill the new-line

	switch(cou) {
	case 0 :	// no newline found
		term_char = "\n" ;
		buff = rdobuff ;
		break ;
	case 1 :	// 1 newline aka 1 liner
		buff = rdobuff ;
		term_char = "" ;
		break ;
	default :
		buff = "" ;
		term_char = "\n" ;
		break ;
	}

	if(log_file == 0) {
		return err_status;
	}



	pthread_mutex_lock(&log_file_mutex) ;


	fprintf(log_file,"%s[RDO %d] (%s) %s%s%s",ANSI_BLUE,rdo,
		tm,
		buff,
		term_char,ANSI_RESET) ;

	if(cou > 1) {
		fprintf(log_file,"%s",rdobuff) ;
	}

	fflush(log_file) ;

	pthread_mutex_unlock(&log_file_mutex) ;
#endif	

	return err_status ;

}


int tpx23::rdo_scan(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;





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

		err = log_dump(c_addr,wds) ;

		break ;
	case DDL_TYPE_MSC :
		if(subtype==2) {	// heartbeat; ignore
			break ;
		}

		LOG(WARN,"%d: MSC: event %d: S%02d:%d: type %d:%d, words %d",rdo1,d[1],sec,rdo,type,subtype,words) ;

		msc_dump(c_addr, wds) ;

		break ;
	default :	// ALTRO data -- and we're off
		evt_trgd++ ;
		fee_scan() ;

		break ;
	}

	return 0 ;

}


tpx23::tpx23()
{
	LOG(TERR,"%s %d %p",__PRETTY_FUNCTION__,sizeof(rp_gain),rp_gain) ;

	rts_id = TPX_ID ;

#if 0
	if(rp_gain_tpx==0) {
		rp_gain_tpx = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;
	}
	rp_gain = rp_gain_tpx ;
#else
	if(rp_gain==0) {
		LOG(TERR,"id %d: allocating rp_gain",id) ;
		rp_gain = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;
	}	
#endif

	if(peds==0) {
		peds = new class tpxPed ;
	}

	row_min = 14 ;
	row_max = 45 ;
	for(int row=1;row<=45;row++) rowlen[row] = tpc_rowlen[row] ;


	
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

