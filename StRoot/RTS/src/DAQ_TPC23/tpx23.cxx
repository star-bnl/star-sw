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

#include <DAQ_ITPC/itpcPed.h>	// only for itpcData!

#ifdef THREAD_DBG_USE

#include <MISC_LIBS/thread_dbg.h>

#else

#define TLOG()
#define TLOGX(x)

#endif

#include "tpx23.h"


tpxPed *tpx23::peds ;
tpc23_base::row_pad_t (*tpx23::rp_gain_tpx)[ROW_MAX+1][PAD_MAX+1] ;


int tpx23::from22to23(char *c_addr, int words)
{ 
	// does NOTHING yet
	return words ;
} 

inline u_int tpx23::set_rdo(int s, int r)
{

	sector1 = s ;
	rdo1 = r ;

	return 0 ;	// should be fee_mask
}

int tpx23::fee_scan() 
{
	u_int *h ;
	err = 0 ;	// in class

//	u_char altro_present[256][16] ;

	
	get_token((char *)d_start,words) ;

	TLOG() ;

	if(run_type==1 || run_type==5) {
		int r0_logical = tpx36_from_real(subdet_id,sector1,rdo1) - 1 ;


		peds->valid_evts[r0_logical]++ ;
		peds->evts[r0_logical]++ ;

		peds->sector = subdet_id ;	// this is the PC id 1..36!

		if(peds->evts[r0_logical]<=3) goto done ;	// skip first 3 events
		if(peds->valid_evts[r0_logical]>1000) goto done ;	// enough...


	}

	// first valid FEE word is at d_start+2 ;
	// last valid FEE word is at d_end
	h = d_end ;


//	memset(altro_present,0,sizeof(altro_present)) ;
	if(hdr_version) {
		

	}

	TLOGX(rdo1) ;

	if(log_level>0) LOG(TERR,"%d: fee_scan",rdo1) ;

	// NOTE: ALTRO scans from the end!!!
	while(h>(d_start+2)) {
		u_int hi, lo ;

		lo = *h-- ;
		hi = *h-- ;

		// for intermediate hdr version
		lo &= 0xFFFFF ;
		hi &= 0xFFFFF ;

		int wc = ((hi&0x3F)<<4)|((lo&0xF0000)>>16) ;    // altro's word count
		if(wc==0) continue ;

		int id = (lo&0xFF0) >> 4 ;      // altro id
		int ch = lo & 0xF ;

		TLOGX(id) ;

		for(int i=0;i<tpx_fee_override_cou;i++) {
			if(sector1 == tpx_fee_override[i].sector) {
			if(rdo1==tpx_fee_override[i].rdo) {
				int fee = id & 0xFE ;

				if(fee == tpx_fee_override[i].curr_altro) {
					int should = tpx_fee_override[i].orig_altro ;

					if(id & 1) should |= 1 ;

					//LOG(WARN,"S%02d:%d overriding ALTRO id %d with %d",sector1,rdo1,id,should) ;
					id = should ;
					break ;
				}
			}
			}
		}
					

		// this now needs to go into the canonical format!
		int row ;
		int pad ;

		// get row,pad & flags and skip the pad if there are flags
		int flags = flags_row_pad(id,ch,row,pad) ;

		// max wc in pedestal runs is 437
		if(wc>437) {	// garbage in the event... and now what???
			run_errors++ ;
			if(run_errors<10) {
				if(online) LOG(ERR,"S%02d:%d: rp %d:%d (aid %d:%d) : wc %d",sector1,rdo1,row,pad,id,ch,wc) ;
			}
			//err |= 0x10000 ;	// signal an error because I am breaking out
			break ;	
		}

		while(wc%4) wc++ ;

		// if this is a physics run: skip pads which have flags
		// hmm... is this right?
		if(flags && run_type==3) {
			if(log_level>0) {
				LOG(TERR,"%d: rp %d:%d, flags 0x%X",rdo1,row,pad,flags) ;
			}
			h -= wc/2 ;
			continue ;
		}

#if 0
		// fixing a bug in fee_23a FY23 version!
		altro_present[id][ch]++ ;

		if(altro_present[id][ch]>1) {
			run_errors++ ;
			if(run_errors<20) {
				if(online) LOG(ERR,"S%02:%d: AID %d:%d already present %d",sector1,rdo1,id,ch,altro_present[id][ch]) ;
			}
			h -= wc/2 ;
			continue ;
		}
#endif

		u_short *d = s1_dta + last_ix ;	// this is where the raw data goes...
		//u_short d[512] ;

		if(log_level>0) {
			LOG(TERR,"%d: rp %d:%d; last_ix %d %p",rdo1,row,pad,last_ix,d) ;
		}

		int ix = 0 ;

		//TLOGX(row) ;


		for(int i=0;i<wc;) {	// NOTE: no increment!
			lo = *h-- ;
			hi = *h-- ;

			//lo &= 0xFFFFF ;
			//hi &= 0xFFFFF ;

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

		//TLOGX(row) ;

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

			if(t_len>440 || t_hi>440 || t_lo>440) {
				run_errors++ ;
				if(run_errors<20) {
					if(online) LOG(ERR,"S%02d:%d: rp %d:%d (aid %d:%d), t_len %d, t_lo %d, t_hi %d",sector1,rdo1,row,pad,
					    id,ch,
					    t_len,t_lo,t_hi) ;
				}
				if(t_len>510 || t_hi>510 || t_lo>510) {
					//err |= 0x20000 ; 
					break ;
				}

				//if(t_hi>510) break ;
				//if(t_lo>510) break ;
			}

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

		//TLOG() ;
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
		else if(tpx_d) {
			tpx_d->sector = sector1 ;
			tpx_d->rdo = rdo1 ;
			tpx_d->row = row ;
			tpx_d->pad = pad ;
			tpx_d->altro = id ;

			//LOG(TERR,"%d:%d %d:%d %d:%d",sector1,rdo1,row,pad,id,ch) ;

			tpx_d->ch_start(ch) ;	// sets tpx_d->ch within

			for(int i=(seq_ix-1);i>=0;i--) {
				int t_len = sseq[i].t_hi - sseq[i].t_lo + 1 ;

				int ii = 0 ;
				for(int j=(t_len-1);j>=0;j--) {
					int adc = sseq[i].d[j] ;
					int tb ;


					//a.adc[aix] = adc ;

					tb = sseq[i].t_lo + ii ;
					//a.tb[aix] = sseq[i].t_lo + ii ;
					ii++ ;
					//aix++ ;

					tpx_d->accum(tb,adc) ;
				}

			}

			tpx_d->ch_done() ;
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

		//TLOG() ;

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

	TLOG() ;

	return err ;
}

/*
	While not strictly static, this thing should not set or use
	any class members
*/

u_int tpx23::get_token_s(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;
	int t, trg, daq ;
	int trg_cou ;
	struct trg_data *trg_d ;

	t = 4097 ;
	trg = 0 ;
	daq = 0 ;

	TLOGX(rdo1) ;

	int type = (d[0]>>0)&0xF ;
	int hdr_type = (d[0]>>24)&0xF ;	 //0: pre-FY23 headers, 1:FY23 headers
//	hdr_type = 1 ;
	

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

	TLOGX(wds) ;

	d += wds ;
	d-- ;		// at the last datum

	if(hdr_type) {
		//0:tick,1:status,2:1,3:trg

		t = d[-2] & 0xFFF ;
		daq = (d[-2] >> 12)&0xF ;
		trg = (d[-2] >> 16)&0xF ;

		goto done ;

	}

	// here we are with the old, pre-FY23 header format
	TLOGX(rdo1) ;

	d -= 2 ;	// skip 2 words trailer


	trg_cou = *d ;

	d -= trg_cou * (sizeof(struct trg_data)/4) ;


	trg_d = (struct trg_data *)d ;

	TLOGX(trg_cou) ;

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

	TLOGX(rdo1) ;

	done:;

	TLOGX(rdo1) ;

	return (trg<<16)|(daq<<12)|t ;
}

u_int tpx23::get_token(char *c_addr, int wds)
{
	u_int *d = (u_int *)c_addr ;
	u_int *d_first ;
	token = 4097 ;
	trg_cmd = 0 ;
	daq_cmd = 0 ;

	d_first = d ;

	err = 0 ;


//	LOG(TERR,"get_token %u",d[1]) ;

	tdbg[0] = d[1] ;	// RHIC counter

	d += wds ;
	d-- ;		// at the last datum


//	LOG(TERR,"evt %d(hdr %d, wds %d): 0x%08X 0x%08X, 0x%08X 0x%08X 0x%08X 0x%08X",evt,hdr_version,wds,
//	d_first[0],d_first[1],d[-3],d[-2],d[-1],d[0]) ;

	TLOGX(hdr_version) ;

	// for the new FY23 format!
	if(hdr_version) {
		tdbg[0] = d_first[1] ;	// RHIC counter start
		tdbg[1] = d[0] ;	// RHIC counter end
		tdbg[2] = d[-1] ;	// event status
		tdbg[3] = d[-2] ;	// trg word

		tdbg[4] = d[-5] ;	// at start-altro
		tdbg[5] = d[-4] ;	// at end-altr
		tdbg[6] = d[-3] ;	// before DDL

		token = d[-2] & 0xFFF ;
		daq_cmd = (d[-2] >> 12)&0xF ;
		trg_cmd = (d[-2] >> 16)&0xF ;

		
		u_int evt_err = d[-1] ;
		if(evt_err & 0xFF000000) {
			int cou ;

			if(wds>20) cou = 20 ;
			else cou = wds ;

			err |= 0x1 ;

			if(online) {
				LOG(ERR,"evt_err %d:%d: 0x%08X: 0x%08X, wds %u",evt,rdo1,d_first[0],evt_err,wds) ;
				for(int i=0;i<cou;i++) {
					LOG(TERR,"  %d: 0x%08X",i,d_first[i]) ;
				}
			}
		}
		

#if 0
		printf(" delta evt %d: all %d: %d %d %d %d - token %d, wds %d\n",evt,tdbg[1]-tdbg[0],
		       	tdbg[4]-tdbg[0],
		       	tdbg[5]-tdbg[4],
		       	tdbg[6]-tdbg[5],
		       	tdbg[1]-tdbg[6],
			token,wds) ;

#endif

		d_end = d - 6 ;	// last word of the ALTRO contribution
	
		return (trg_cmd<<16)|(daq_cmd<<12)|token ;
		
	}
	
	TLOGX(rdo1) ;

	d -= 2 ;	// skip 2 words trailer to position myself at "trigger count"

	int trg_cou = *d ;

	d -= trg_cou * (sizeof(struct trg_data)/4) ;	// move back 1

	TLOGX(trg_cou) ;

	struct trg_data *trg = (struct trg_data *)d ;
	for(int i=0;i<trg_cou;i++) {

//		if(hdr_version) {
//			LOG(TERR,"trg_data %d: 0x%X 0x%X 0x%X",i,trg[i].rhic_counter, trg[i].csr, trg[i].data) ;
//		}

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

	TLOG() ;

	done:;

	TLOG() ;

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
	err = tpx_show_status(sector1,1<<(rdo1-1),0) ;
	if(err) {
		if(online) LOG(ERR,"S%02d:%d: tpx_show_status %d",sector1,rdo1,err) ;
	}

	return err ;
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

	int max_cou = (words-4)*4 ;
	rdobuff = (char *)(c_addr+2*4) ;	// skip header

	// one liner or more?
	len = strlen(rdobuff) ;
	if(len>max_cou) len = max_cou ;

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
					LOG(WARN,"---> [%d LOG] Unprintable character 0x%02X? -- powercycle",rdo,(u_char)tmpbuff[j]) ;
					//LOG(ERR,"But ignored for FY22") ;
					err_status |= 1;
					err = -1 ;
					tmpbuff[j] = '?' ;
				}
			}

		}

#if 1
		if(strstr(tmpbuff+st,"RHIC clock: ")) {
			if(strstr(tmpbuff+st,"EXTERNAL")) {
				rhic_clock = 1 ;
			}
			else {
				rhic_clock = 0 ;	// internal
			}
		}

		if(strstr(tmpbuff+st,"JTAG dev ")) {
			int ret, dev ;
			u_int dev_id, user ;

			dev = -1 ;

//			LOG(WARN,"[S%02d:%d LOG]: JTAG:",s_real,r_real,tmpbuff+st) ;

			ret = sscanf(tmpbuff+st,"JTAG dev %d: ID 0x%X, USERcode 0x%X",&dev,&dev_id,&user) ;
			LOG(WARN,"JTAG:   ret %d, dev %d, dev_id 0x%08X, user 0x%08X",ret, dev, dev_id,user) ;

			if(ret==3 && dev>=0 && dev<5) {
				fpga_usercode[dev] = user ;
			}
		}
#endif

		if(strstr(tmpbuff+st,"SPECIAL_0 code")) {
			LOG(ERR,"---> SPECIAL code: RDO %d",rdo) ;
		}

		// check for error string but just print a warning, we'll be more selective later
		if(strstr(tmpbuff+st,"ERR")) {
			if(strstr(tmpbuff+st,"FLASH Id")) {
				LOG(WARN,"[S%02d:%d LOG]: contains ERR \"%s\"",s_real,r_real,tmpbuff+st) ;
			}
			else {
				//err = -1 ;
				//LOG(ERR,"[S%02d:%d LOG]: contains ERR \"%s\"",s_real,r_real,tmpbuff+st) ;
			}
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
				LOG(ERR,"---> [S%d:%d LOG] FEE power BAD -- powercycle (ignored)",s_real,r_real) ;
				//err = -1 ;
			}
		}


		if(strstr(tmpbuff+st,"SRAM check failed")) {
			err = -1 ;	
			err_status |= 2 ;
			LOG(ERR,"---> [%d LOG] SRAM check failed -- powercycle",rdo) ;
		}

		if(strstr(tmpbuff+st,"CPLD claims error")) {
			err = -1 ;
			LOG(ERR,"---> [%d LOG] CPLD claims error -- reconfig 0x300",rdo) ;
		}
		
		if(strstr(tmpbuff+st,"can't configure RDO!")) {
			LOG(ERR,"---> [%d LOG] \"can't configure RDO\" -- reconfig 0x300",rdo) ;
//			err = -1 ;
		}	


		// mostly run related
		if(strstr(tmpbuff+st,"lost RHIC")) {
			LOG(ERR,"---> [%d LOG] \"lost RHIC\" -- restart run",rdo) ;
			err = -1 ;
		}	
		if(strstr(tmpbuff+st,"NO RHIC CLOCK")) {
			LOG(ERR,"---> [%d LOG] \"NO RHIC CLOCK\" -- restart run",rdo) ;
			err = -1 ;
		}	

		if(strstr(tmpbuff+st,"DRIFT")) {
			LOG(ERR,"---> [%d LOG] \"DRIFT/clock problems\" -- restart run",rdo) ;
			err = -1 ;
		}	


		if(strstr(tmpbuff+st,"CRIT")) {
			err = -1 ;
			LOG(ERR,"---> [%d LOG] CRIT string in log -- restart run",rdo) ;
		}
	
		if(strstr(tmpbuff+st,"altro error")) {
			err = -1 ;
			LOG(ERR,"---> [%d LOG] altro error -- restart run",rdo) ;
		}
	

		if(strstr(tmpbuff+st,"ERR ALTRO")) {
			//err = -1 ;
			//LOG(WARN,"---> [%d LOG] ERR ALTRO -- CHECK THIS",rdo) ;
		}
	

		
		
		if(err<0) {
			LOG(ERR,"[S%02d:%d %d]: %s",s_real,r_real,evt,tmpbuff+st) ;
			log_is_error = 1 ;
		}
		else if(do_log) {
			LOG(INFO,"[S%02d:%d %d]: %s",s_real,r_real,evt,tmpbuff+st) ;
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
	int ret = 0 ;

	u_int *d = (u_int *)c_addr ;



	int rdo = (d[0]>>8)&0xF ;
	int sec = (d[0]>>12)&0xFF ;

	subtype = (d[0]>>4)&0xF ;	//class
	type = (d[0]>>0)&0xF ;		//class
		
	hdr_version = (d[0]>>24)&0xF ;
//	hdr_version = 1 ;

	d_start = d ;
	token = 4096 ;
	trg_cmd = 0 ;
	daq_cmd = 0 ;
	err = 0 ;

	words = wds ;

	evt++ ;


	if((d[0]&0xF0000000)!=0xF0000000) {
		LOG(ERR,"%d:%d: bad header 0x%08X",evt,rdo1,d[0]) ;
	}

	if(rdo!=rdo1 || sec!=sector1) {
		LOG(ERR,"%d:%d: wrong sec,rdo: rdo expect %d is %d; sector expect %d is %d [0x%08X]",evt,rdo1,
		    rdo1,rdo,sector1,sec,d[0]) ;
	}


	TLOG() ;

	switch(type) {
	case DDL_TYPE_LOG :

		TLOG() ;
		ret = log_dump(c_addr,wds) ;
		TLOG() ;
		break ;
	case DDL_TYPE_MSC :
		if(subtype==2) {	// heartbeat; ignore
			break ;
		}

		LOG(WARN,"%d: MSC: event %d: S%02d:%d: type %d:%d, words %d",rdo1,d[1],sec,rdo,type,subtype,words) ;

		ret = msc_dump(c_addr, wds) ;

		break ;
	default :	// ALTRO data -- and we're off
		evt_trgd++ ;
		TLOG() ;
		ret = fee_scan() ;
		TLOG() ;
		break ;
	}

	return ret ;	// should be ret

}


tpx23::tpx23()
{
//	LOG(TERR,"%s %d %p",__PRETTY_FUNCTION__,sizeof(rp_gain),rp_gain) ;

	rts_id = TPX_ID ;


	if(rp_gain_tpx==0) {
		rp_gain_tpx = (row_pad_t (*)[ROW_MAX+1][PAD_MAX+1]) malloc(sizeof(row_pad_t)*24*(ROW_MAX+1)*(PAD_MAX+1)) ;

		// initialize here!
		for(int s=0;s<24;s++) {
		for(int r=0;r<=ROW_MAX;r++) {
		for(int p=0;p<=PAD_MAX;p++) {
			rp_gain_tpx[s][r][p].gain = 1.0 ;
			rp_gain_tpx[s][r][p].t0 = 0.0 ;
			rp_gain_tpx[s][r][p].flags = 0 ;
		}}}

	}

	rp_gain = rp_gain_tpx ;

	if(peds==0) {
		peds = new class tpxPed ;
	}

	row_min = 14 ;
	row_max = 45 ;
	for(int row=1;row<=45;row++) rowlen[row] = tpc_rowlen[row] ;

	hdr_version = 0 ;	// 0:pre FY23

	memset(fpga_usercode,0,sizeof(fpga_usercode)) ;

	tpx_d = 0 ;
}


#include <DAQ_TPX/tpxCore.h>

u_char tpx23::flags_row_pad(int asic, int channel, int &row, int &pad)
{
	row = 255 ;
	pad = 255 ;

	if(rdo1<1||rdo1>6) return 0xFF ;

	// I will rewrite this to make it super-fast

	tpx_from_altro(rdo1-1,asic,channel,row,pad) ;	// from tpxCore!
	if(row==0 || row==255) return 0xFF ;
	if(pad==0 || pad==255) return 0xFF ;

//	row -= 13 ;	// HACK: row 14 becomes row 1

	return rp_gain[sector1-1][row][pad].flags ;
}

#if 0
int tpx23::run_start() 
{
//	LOG(WARN,"TPX23 run_start") ;

	rhic_clock = -1 ;	// unknown
	log_is_error = 0 ;

	return 0 ;
}
#endif
