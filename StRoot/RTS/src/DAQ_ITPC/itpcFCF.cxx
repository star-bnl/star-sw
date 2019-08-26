#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <errno.h>
#include <assert.h>

#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
//#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

//#include <trgDataDefs.h>
//#include "trgConfNum.h"

// only the detectors we will use need to be included
// for their structure definitions...
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxFCF_flags.h>
#include <TPC/rowlen.h>

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcCore.h>
#include <DAQ_ITPC/itpcPed.h>
#include <DAQ_ITPC/itpcInterpreter.h>
#include <DAQ_ITPC/itpc_rowlen.h>

#include <DAQ_ITPC/itpcFCF.h>

#define VERSION		0x20180002
// 0x20180001 until Jun 6 -- had cuts in do_ch()


//#define DO_DBG1	1

static double mark(void)
{
	struct timeval tmval ;

	gettimeofday(&tmval,0) ;

	return ((double)tmval.tv_sec*1000000.0 + (double)tmval.tv_usec) ;
}

static double delta(double v)
{
	return mark() - v ;
}

itpc_fcf_c::itpc_fcf_c()
{	
        want_data = 1 ;		// ALWAYS!
        my_id = 0 ;		// normally starts from 1 so 0 is "un-initialized"

        version = VERSION ;
	sector_id = 0 ;		// 0 is ALL sectors
	offline = 0 ;		// not Offline!


	det_type = 1 ;		// ITPC
	y_is_timebin = 1 ;	// normal
	max_x = 120 ;
	max_y = 512 ;
	max_slice = 40 ;

	use_gain = 1 ;

	row_pad_store = 0 ;
        words_per_cluster = 2 ; 

	track_dta = 0 ;

	// just in case
	run_start() ;

	f_stat.s1_found = 0 ;
	blob_id = 0 ;

} ;

itpc_fcf_c::~itpc_fcf_c()
{
//	LOG(TERR,"%s: destructor %d",__PRETTY_FUNCTION__,my_id) ;

	for(int i=0;i<=MAX_SEC;i++) {
		if(sec_gains[i]) free(sec_gains[i]) ;
		sec_gains[i] = 0 ;
	}

	if(track_dta) {
		free(track_dta) ;
		track_dta = 0 ;
	}

	if(row_pad_store) {
		free(row_pad_store) ;
		row_pad_store = 0 ;
	}
} ;

// generic "rowlen" function
int itpc_fcf_c::x_max(int slice, int y)
{
	if(det_type == 0) {	// TPX
		if(y_is_timebin) {	// normal
			if(slice>45) return -1 ;
			return tpc_rowlen[slice] ;
		}
		else {			// y is row
			if(y>45) return -1 ;
			return 182 ; // tpc_rowlen[y] ;
		}
	}
	else if(det_type == 1) {
		if(y_is_timebin) {
			if(slice>40) return -1 ;
			return itpc_rowlen[slice] ;
		}
		else {
			if(y>40) return -1 ;
			return 120 ; //itpc_rowlen[slice] ;
		}
	}
	else {	// generic
		return max_x ;
	}
}

int itpc_fcf_c::x_min(int slice, int y)
{
	switch(det_type) {
	case 0 :		
	case 1 :
		if(y_is_timebin) return 1 ;
		break ;
	}

	return 1 ;	// generic case
}

int itpc_fcf_c::y_min()
{
	return 0 ;	// always!
}

int itpc_fcf_c::y_max()
{
	switch(det_type) {
	case 0 :
		if(y_is_timebin) return 512 ;
		return 45 ;
	case 1:
		if(y_is_timebin) return 425 ;
		return 40 ;
	}

	return max_y ;
}

		
	
// Go from raw channel (aka pad) to x position
// Needed for reverse clusterfinding because of the pizza shape
int itpc_fcf_c::pad_to_x(int pad, int row)
{
	switch(det_type) {
	case 0 :
		if(y_is_timebin) return pad ;
		return (182/2 - tpc_rowlen[row]/2 + pad) ;
	case 1 :
		if(y_is_timebin) return pad ;
		return (120/2 - itpc_rowlen[row]/2 + pad) ;
	}

	// all other detectors are assumed to be square
	return pad ;
}

	

//static member
//int itpc_fcf_c::rowlen[45+1] ;

itpc_fcf_c::gain_rp_t *itpc_fcf_c::sec_gains[MAX_SEC+1] ;

int itpc_fcf_c::get_bad(int sec1, int row1, int pad1)
{
	gain_rp_t (*gain_p)[MAX_PHYS_PAD+1] ;
	
	if(sec_gains[sec1]==0) return 0 ;	// good

	gain_p = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sec1] ;

	if(gain_p[row1][pad1].gain==0.0) return 1 ;

	return 0 ;
}

float itpc_fcf_c::get_gain(int sec1, int row1, int pad1)
{
	if(row1==0 || pad1==0) return 0.0 ;

	gain_rp_t (*gain_p)[MAX_PHYS_PAD+1] ;
	
	if(sec_gains[sec1]==0) return 0.0 ;	// bad

	gain_p = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sec1] ;

	return gain_p[row1][pad1].gain ;

}



struct itpc_fcf_c::rp_t *itpc_fcf_c::get_row_pad(int row, int pad)
{
	int max_pad_all = max_x + 1 ;

	if(offline) s1_data_length = 1 + max_y * 2 ;	// need more for track_id
	else s1_data_length = 1 + max_y ;

	if(row_pad_store==0) {	// initialize on first use...
		int max_row_all = max_slice + 1 ;
		int chunks = max_row_all * max_pad_all ;

		row_pad_store = (u_short *) valloc(chunks *s1_data_length * sizeof(u_short)) ;
		assert(row_pad_store) ;
		memset(row_pad_store,0,chunks *s1_data_length * sizeof(u_short)) ;	// superstition...
	}

	u_short *p = row_pad_store + ((row*max_pad_all) + pad)*s1_data_length ;

	return (rp_t *)p ;
}


// This is only called if I'm in Offline and AFTER the usual init(sector,"")!!!
int itpc_fcf_c::init(daq_dta *gain)
{
	if(gain==0) return -1 ;

	int bad_ch = 0 ;
	int tot_ch = 0 ;

	while(gain->iterate()) {
		int s = gain->sec ;
		int row = gain->row ;

		if(sec_gains[s]==0) continue ;	// not for me
		if(row==0) continue ;

		gain_rp_t (*sr)[MAX_PHYS_PAD+1] = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[s] ;

		daq_det_gain *gp = (daq_det_gain *) gain->Void ;

		for(u_int p=0;p<gain->ncontent;p++) {
			if(p==0) continue ;
//			if(p>(u_int)rowlen[row]) continue ;
			if(p>(u_int)x_max(row,0)) continue ;

			tot_ch++ ;

			sr[row][p].gain = gp[p].gain ;
			sr[row][p].t0 = gp[p].t0 ;


			if(gp[p].gain < 0.01) {
				int p1 = p - 1;
				int p2 = p + 1 ;

				if(p1<1) p1 = 1;
				//if(p2>rowlen[row]) p2 = rowlen[row] ;
				if(p2>x_max(row,0)) p2 = x_max(row,0) ;

				// mark this and neighbours as bad
				sr[row][p].flags |= 3 ;	// bad and edge
				sr[row][p1].flags |= 2 ;	// just edge
				sr[row][p2].flags |= 2 ;	// just edge



				bad_ch++ ;
			}
		}
		

	}

	if(bad_ch) {
		LOG(WARN,"%d/%d bad channels",bad_ch,tot_ch) ;
	}

	return 0 ;
}

// det_type and various other max_x,max_y,max_slice etc HAVE to be set before!!!!
int itpc_fcf_c::init(int sec, const char *fname)
{

	int s_min, s_max ;
	gain_rp_t (*gain_p)[MAX_PHYS_PAD+1] ;

	if(sec>MAX_SEC) return -1 ;

	if(sec<=0) {
		s_min = 1 ;
		s_max = MAX_SEC ;
	}
	else {
		s_min = s_max = sec ;
	}

	const char *s_type, *s_orient ;


	switch(det_type) {
	case 0 :
		s_type = "TPX" ;
		if(y_is_timebin) {
			use_gain = 1 ;

			max_x = 182 ;
			max_y = 512 ;
			max_slice = 45 ;
			
		}
		else {
			use_gain = 0 ;

			max_x = 182 ;
			max_y = 45 ;
			max_slice = 512;
		}
		break ;
	case 1 :
		s_type = "ITPC" ;
		if(y_is_timebin) {
			use_gain = 1 ;

			max_x = 120 ;
			max_y = 512 ;
			max_slice = 40 ;
		}
		else {
			use_gain = 0 ;

			max_x = 120 ;
			max_y = 40 ;
			max_slice = 512 ;
		}
		break ;
	default :
		s_type = "OTHER" ;
		use_gain = 0 ;
		break ;
	}

	if(y_is_timebin) s_orient="across-rows" ;
	else s_orient = "across-timebins" ;

	LOG(INFO,"%s: det_type %s(%s): max_x(pad) %d, max_y(timebin) %d, max_slice(row) %d",__FUNCTION__,
	    s_type,s_orient,
	    max_x, max_y, max_slice) ;

	//alocation first
	for(int s=s_min;s<=s_max;s++) {

		if(sec_gains[s]) continue ;	// silently continue ;

		sec_gains[s] = (gain_rp_t *) malloc((MAX_PHYS_ROW+1)*(MAX_PHYS_PAD+1)*sizeof(gain_rp_t)) ;
		
		//defaults now
		gain_p = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[s] ;

		for(int r=1;r<=MAX_PHYS_ROW;r++) {

		for(int p=1;p<=MAX_PHYS_PAD;p++) {

			gain_p[r][p].gain = 1.0 ;
			gain_p[r][p].t0 = 0.0 ;

			//if(p==1 || p==rowlen[r]) gain_p[r][p].flags = 0x2 ;	// edge

			if(p==1 || p==x_max(r,0)) gain_p[r][p].flags = 0x2 ;	// edge
			else gain_p[r][p].flags = 0 ;

		}
		}


	}



	// do we have a file?
	if(fname==0) return 0 ;



	// yes, open it
	FILE *f = fopen(fname,"r") ;
	if(f==0) {
		LOG(ERR,"%s: %s [%s]",__FUNCTION__,fname,strerror(errno)) ;
		return -1 ;
	}

	LOG(INFO,"Using gain file %s",fname) ;

	int ch_bad = 0 ;
	int ch_all = 0 ;

	// we now have an opened gain file
	while(!feof(f)) {
		int sec,rdo,port,ch,row,pad ;
		float g, t ;

		char buff[128] ;

		if(fgets(buff,sizeof(buff),f)==0) continue ;

		if(buff[0]=='#') continue ;
		if(strlen(buff)<1) continue ;

		int ret = sscanf(buff,"%d %d %d %d %d %d %f %f",&sec,&rdo,&port,&ch,&row,&pad,&g,&t) ;

		if(ret != 8) continue ;
		
		if(sec_gains[sec]==0) continue ;	// not for me!

		if(ch<0) {	// kill entire FEE!
			LOG(ERR,"%d %d %d kill FEE not implemented",sec,rdo,port) ;
			continue ;
		}


		gain_rp_t (*sr)[MAX_PHYS_PAD+1] = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sec] ;

		sr[row][pad].gain = g ;
		sr[row][pad].t0 = t ;

		ch_all++ ;

		if(g<0.01) {	// some really small number close to 0.0
			int p1 = pad - 1;
			int p2 = pad + 1 ;

			if(p1<1) p1 = 1;
			//if(p2>rowlen[row]) p2 = rowlen[row] ;
			if(p2>x_max(row,0)) p2 = x_max(row,0) ;

			// mark this and neighbours as bad
			sr[row][pad].flags |= 3 ;	// bad and edge
			sr[row][p1].flags |= 2 ;	// just edge
			sr[row][p2].flags |= 2 ;	// just edge

			ch_bad++ ;
		}

	}

	fclose(f) ;

	if(ch_bad) LOG(WARN,"...with %d/%d bad channels",ch_bad,ch_all) ;
	else LOG(TERR,"...with %d/%d no bad channels",ch_bad,ch_all) ;

	return 0 ;
}






// At start event
void itpc_fcf_c::event_start()
{
	//for statistics, so not really necessary
	f_stat.s1_found = 0 ;

	if(offline) {
		for(int r=0;r<=max_slice;r++) {
		for(int p=0;p<=max_x;p++) {
			get_row_pad(r,p)->s1_len = 0 ;
		}
		}
	}

}


int itpc_fcf_c::fcf_decode(u_int *p_buff, daq_sim_cld_x *dc, u_int version)
{
	daq_cld cld ;
	int words_used ;

	words_used = fcf_decode(p_buff,&cld,version) ;

	memcpy(&dc->cld,&cld,sizeof(cld)) ;

	p_buff += words_used ;	// point to track id

	dc->track_id = *p_buff & 0xFFFF ;
	dc->quality = (*p_buff)>>16 ;
	
	p_buff++ ;		// point to adc

	dc->max_adc = *p_buff & 0xFFFF ;
	dc->pixels = (*p_buff)>>16 ;

	return words_used + 2 ;
	
}

int itpc_fcf_c::fcf_decode(u_int *p_buff, daq_cld *dc, u_int version)
{
	double p, t ;
	int p1,p2,t1,t2,cha,fla ;
	u_int p_tmp, t_tmp ;

	// pad
	p_tmp = *p_buff & 0xFFFF ;

	// time
	t_tmp = *p_buff >> 16 ;


	p = (double)(p_tmp & 0x3FFF) / 64.0 ;
	t = (double)(t_tmp & 0x7FFF) / 64.0 ;

	fla = 0 ;
	if(p_tmp & 0x8000) fla |= FCF_MERGED ;
	if(p_tmp & 0x4000) fla |= FCF_DEAD_EDGE ;
	if(t_tmp & 0x8000) fla |= FCF_ONEPAD ;


	p_buff++ ;	// advance to next word
	cha = *p_buff >> 16 ;

	if(cha >= 0x8000) {	// special case of very large charge...
//printf("1: Big cha: 0x%08X %d; %f %f\n",cha,cha,p,t) ;

		fla |= FCF_BIG_CHARGE ;
		cha = (cha & 0x7FFF) * 1024 ;
//		if(cha == 0) cha = 0x8000;	// exaclty, but can't be I think...

//printf("2: Big cha: 0x%08X %d\n",cha,cha) ;

		// quasi fix of the very large problem...
		if(cha > 0xFFFF) cha = 0xFFFF ;	// because the daq_cld structure has charge as a short... damn...

	}

	p_tmp = *p_buff & 0xFFFF ;	// reuse p_tmp

	if(p_tmp & 0x8000) fla |= FCF_ROW_EDGE ;
	if(p_tmp & 0x4000) fla |= FCF_BROKEN_EDGE ;

	t1 = p_tmp & 0xF ;
	t2 = (p_tmp >> 4) & 0xF ;


	p1 = (p_tmp >> 8) & 0x7 ;
	p2 = (p_tmp >> 11) & 0x7 ;


	t1 = (int)t - t1 ;
	t2 = (int)t + t2 ;


	p1 = (int)p - p1 ;
	p2 = (int)p + p2 ;

	dc->t1 = t1 ;
	dc->t2 = t2 ;
	dc->p1 = p1 ;
	dc->p2 = p2 ;
	dc->charge = cha ;	// this is a problem for BIG_CHARGE... it will strip the upper bits... unsolved.
	dc->flags = fla ;
	dc->pad = p ;
	dc->tb = t ;


	return 2 ;	// 2 u_ints used

}


// Called by the raw data unpacker: fee_id and fee_ch are _physical_ channels
// Main purpose is unpack the raw data into the canonical form ready for FCF.
// Canonical form is:
//	seq_ix		(set to 0 before cluster finder begins)
//	timebin_count
//	timebin_start
//	adc...
//	adc...

int itpc_fcf_c::do_ch(int fee_id, int fee_ch, u_int *data, int words) 
{
	int row, pad ;
	int seq_cou ;
	int s_count ;
	int t_stop_last ;
	u_short *s1_data ;
	int t_cou ;
	int err = 0 ;

	seq_cou = 0;
	int word32 = (words/3) + (words%3?1:0) ;

	itpc_ifee_to_rowpad(fee_id, fee_ch, row, pad) ;

	gain_rp_t (*gain_row_pad)[MAX_PHYS_PAD+1] = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sector_id] ;

	if(row==0) goto err_ret ;	// unphysical pad


//	LOG(TERR,"RP %d:%d = %d %d",row,pad,gain_row_pad[row][pad].flags,word32) ;

	if(gain_row_pad[row][pad].flags & 1) {
		//LOG(WARN,"RP %d:%d killed",row,pad) ;
		goto err_ret ;	// skip dead pads!
	}



	if((word32*3)>=MAX_TB_EVER) {
		LOG(ERR,"%d:#%d: FEE %d:%d, words %d",rdo,port,fee_id,fee_ch,words) ;
		err |= 1 ;
		goto err_ret ;
	}

	t_cou = 0 ;
	for(int i=0;i<word32;i++) {
		u_int d = *data++ ;

		if(d & 0xC0000000) {
			seq_cou = 0 ;
			LOG(ERR,"%d:#%d: FEE %d:%d, words %d",rdo,port,fee_id,fee_ch,words) ;
			err |= 2 ;
			goto err_ret ;
		}

		tb_buff[t_cou++] = (d>>20) & 0x3FF ;
		tb_buff[t_cou++] = (d>>10) & 0x3FF ;
		tb_buff[t_cou++] = d & 0x3FF ;
	}



//	s1_data = row_pad[row][pad].s1_data ;
	s1_data = get_row_pad(row,pad)->s1_data ;

	// I MUST put protection against broken data!!!
	t_stop_last = -1 ;

	for(int i=0;i<words;) {		// now timebins!
		int t_cou = tb_buff[i++] ;
		int t_start = tb_buff[i++] ;
		int t_stop = t_start + t_cou - 1 ;

		if(t_start <= t_stop_last) {
			LOG(ERR,"%d:#%d: FEE %d:%d, words %d: %d <= %d",rdo,port,fee_id,fee_ch,words,t_start,t_stop_last) ;
			seq_cou = 0 ;
			err |= 4 ;
			goto err_ret ;
		}
		if(t_stop > 511) {
			LOG(ERR,"%d:#%d: FEE %d:%d, words %d: %d > 511",rdo,port,fee_id,fee_ch,words,t_stop) ;
			seq_cou = 0 ;
			err |= 4 ;
			goto err_ret ;
		}

		t_stop_last = t_stop ;

		seq_cou++ ;

		*s1_data++ = 0 ;	// index now 0!
		*s1_data++ = t_cou ;
		*s1_data++ = t_start ;

		

		for(int t=t_start;t<=t_stop;t++) {
			// initial cuts, where I blow of data
#if 0
			// cut timebin due to gating grid pickup
			if(t>425) {
				*s1_data = 0 ;
			}
			else if((t>=26)&&(t<=31)) *s1_data = 0 ;
			else {
				if(tb_buff[i]<=4) *s1_data = 0 ;	// cut low ADC data
				else *s1_data = tb_buff[i] ;
			}


			i++ ;
			s1_data++ ;
#else
			*s1_data++ = tb_buff[i++] ; 
#endif

		}
	}

	*s1_data++ = 0xFFFF ;	// end sentinel

	// check for data overrun!
//	s_count = s1_data - row_pad[row][pad].s1_data ;
	s_count = s1_data - get_row_pad(row,pad)->s1_data ;
	if(s_count >= s1_data_length) {
		err |= 8 ;
		LOG(ERR,"In trouble at RP %d:%d",row,pad) ;
	}

	// for later optimization!
	if(s_count >= (int)f_stat.max_s1_len) {
		f_stat.max_s1_len = s_count ;
	}

	f_stat.s1_found += seq_cou ;
	
//	if(seq_cou) LOG(TERR,"RP %d:%d = seq_cou %d",row,pad,seq_cou) ;

	err_ret:;

//	row_pad[row][pad].s1_len = seq_cou ;
	get_row_pad(row,pad)->s1_len = seq_cou ;	// sequence count!

	return err ;	
}

// Called by the raw data unpacker: fee_id and fee_ch are _physical_ channels
// Main purpose is unpack the raw data into the canonical form ready for FCF.
// Canonical form is:
//	seq_ix		(set to 0 before cluster finder begins)
//	timebin_count
//	timebin_start
//	adc...
//	adc...

int itpc_fcf_c::do_ch_sim(int row, int pad, u_short *tb_buff, u_short *track_id) 
{
	int seq_cou ;
	int s_count ;

	u_short *s1_data ;

	int t_start ;
	int t_cou ;
	u_short *p_start = 0 ;

	offline = 1 ;	// juuuuust in case!
 	words_per_cluster = 4 ;	// added stuff
	if(track_dta==0) {
		track_dta = (u_short *)malloc(MAX_BLOB_SIZE*2) ;
	}

	seq_cou = 0;



	if(row==0) goto err_ret ;	// unphysical pad

	if(use_gain) {
		gain_rp_t (*gain_row_pad)[MAX_PHYS_PAD+1] = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sector_id] ;
		if(gain_row_pad[row][pad].flags & 1) {
			goto err_ret ;	// skip dead pads!
		}
	}

//	s1_data = row_pad[row][pad].s1_data ;
	s1_data = get_row_pad(row,pad)->s1_data ;

	t_start = -1 ;
	t_cou = 0 ;

	//condition data ala cuts
	if(det_type==1 && y_is_timebin) {	// only for ITPC in normal orientation!

// also removed the cuts in the Offline version on 26-Jan-2019
#if 0
		for(int i=0;i<512;i++) {
			if(i>425) tb_buff[i] = 0 ;
			else if((i>=26)&&(i<=31)) tb_buff[i]= 0 ;
			else {
				if(tb_buff[i]<=4) tb_buff[i] = 0 ;
			}
		}
#endif
	}

	for(int i=0;i<MAX_TB_EVER;i++) {
		if(tb_buff[i]) {
			if(t_start<0) {
				*s1_data++ = 0 ;
				p_start = s1_data ;
				*s1_data++ = 0 ;	//t_cou
				*s1_data++ = i ;	//t_start ;

				t_start = i ;
				seq_cou++ ;
			}
			*s1_data++ = tb_buff[i] ;
			*s1_data++ = track_id[i] ;
			t_cou++ ;
		}
		else {
			if(t_start >= 0) {	//was started
				*p_start = t_cou ;
				t_start = -1 ;
				t_cou = 0 ;
			}
		}
	}
				
	if(t_start >= 0) {
		*p_start = t_cou ;
		t_start = -1 ;
	}


	*s1_data++ = 0xFFFF ;	// end sentinel

	// check for data overrun!
//	s_count = s1_data - row_pad[row][pad].s1_data ;
	s_count = s1_data - get_row_pad(row,pad)->s1_data ;
	if(s_count >= s1_data_length) {
		LOG(ERR,"Data too big at RP %d:%d",row,pad) ;
	}

//	LOG(TERR,"RP %d %d = %d",row,pad,s_count) ;

	// for later optimization!
	if(s_count >= (int)f_stat.max_s1_len) {
		f_stat.max_s1_len = s_count ;
	}

	f_stat.s1_found += seq_cou ;
	
//	if(seq_cou) LOG(TERR,"RP %d:%d = seq_cou %d",row,pad,seq_cou) ;

	err_ret:;

//	row_pad[row][pad].s1_len = seq_cou ;	// sequence count!
	get_row_pad(row,pad)->s1_len = seq_cou ;

	return 0 ;	
}


// Returns words(ints) of storage.
int itpc_fcf_c::do_fcf(void *v_store, int bytes)
{
	out_store = (u_int *) v_store ;
	max_out_bytes = bytes ;

	u_int *store_start = out_store ;
		
	f_stat.evt_cou++ ;
	
	blob_id = 0 ;

	for(int row=1;row<=max_slice;row++) {
		double tmx ;
		int found_ints ;

		u_int *row_store = out_store++ ;	// leave space for row number
		out_store++ ;	// leave space for version
		out_store++ ;	// leave space for cluster count


		tmx = mark() ;
		do_blobs_stage1(row) ;
		tmx = delta(tmx) ;

		f_stat.tm[1] += tmx ;	

		//do_row_check(row) ;

		tmx = mark() ;
		do_blobs_stage2(row) ;
		tmx = delta(tmx) ;

		f_stat.tm[2] += tmx ;

		tmx = mark() ;
		found_ints = do_blobs_stage3(row) ;
		tmx = delta(tmx) ;

		f_stat.tm[3] += tmx ;


		if(found_ints) {

			*row_store++ = (words_per_cluster<<16)|row ;	// words-per-cluster | row
			*row_store++ = version ;
			*row_store++ = found_ints  ;	// in ints!

			out_store += found_ints ;
		}
		else {
			out_store = row_store ;	// rewind 
		}

		if((out_store-store_start)>(max_out_bytes/4-1000)) {
			LOG(ERR,"not enough ints %d vs %d",out_store-store_start,max_out_bytes/4) ;
			break ;
		}		
	}


	if(f_stat.s1_found > f_stat.max_s1_found) {
		f_stat.max_s1_found = f_stat.s1_found ;
	}

	return (out_store-store_start) ;	// in ints
}

void itpc_fcf_c::run_start()
{
	// all just for statistics and monitoring...
	LOG(NOTE,"%s: %d",__PRETTY_FUNCTION__,my_id) ;

	memset(&f_stat,0,sizeof(f_stat)) ;


}

void itpc_fcf_c::run_stop()
{
	// all just for statistics and moniroing

//	LOG(TERR,"%s: %d",__PRETTY_FUNCTION__,my_id) ;

	for(int i=0;i<10;i++) {
		f_stat.tm[i] /= f_stat.evt_cou ;
	}
	
	LOG(INFO,"itpcFCF: %d: events %d, times %f %f %f %f",my_id,
	    f_stat.evt_cou,
	    f_stat.tm[0],f_stat.tm[1],f_stat.tm[2],f_stat.tm[3]) ;

	LOG(INFO,"   times %f %f %f %f",
	    f_stat.tm[4],f_stat.tm[5],f_stat.tm[6],f_stat.tm[7]) ;


	LOG(INFO,"   max s1_found %d, s1_len %d, blob_cou %d",f_stat.max_s1_found,f_stat.max_s1_len,f_stat.max_blob_cou) ;
	if(f_stat.toobigs) {
		LOG(WARN,"   toobigs %d",f_stat.toobigs) ;
	}

}


int itpc_fcf_c::do_blobs_stage1(int row)
{
	int pads = 0 ;
	int late_merges = 0 ;

//	rp_t *rp = &(row_pad[row][0]) ;
//	rp_t *rp = get_row_pad(row,0) ;

//	int rl = rowlen[row] ;
	int rl = x_max(row,0) ;

	blob_cou = 1 ;

//	LOG(TERR,"stage1: row %d, x_max %d",row,rl) ;

	for(int p=1;p<=rl;p++) {	// < is on purpose!!!
		int t1_cou, t1_lo, t1_hi ;
		int t2_cou, t2_lo, t2_hi ;
		u_short *ix1_p ;
		u_short *ix2_p ;

		rp_t *rp1 = get_row_pad(row,p) ;

		if(rp1->s1_len==0) continue ;

		//LOG(TERR,"Row %d: s1_len %d",row,rp[p].s1_len) ;

		u_short *d1 = rp1->s1_data ;

		for(int i=0;i<rp1->s1_len;i++) {
			u_short *d2 ;

			rp_t *rp2 = get_row_pad(row,p+1) ;

			ix1_p = d1++ ;	
			


			t1_cou = *d1++ ;	// actually tb_cou
			t1_lo = *d1++ ;	// actually t_start ;

			t1_hi = t1_lo + t1_cou - 1 ;	// now is really

			if(offline) {
				d1 += t1_cou * 2 ;	// also track_id!
			}
			else {
				d1 += t1_cou ;			// advance to next 
			}

			if(p==rl) goto sweep_unmerged ;	// since there is no pad after that

			d2 = rp2->s1_data ;

			for(int j=0;j<rp2->s1_len;j++) {
				ix2_p = d2++ ;

				t2_cou = *d2++ ;
				t2_lo = *d2++ ;

				t2_hi = t2_lo + t2_cou - 1 ;

				if(offline) d2 += t2_cou * 2 ;
				else d2 += t2_cou ;

				int merge = 0 ;

				if(t1_lo > t2_hi) merge = 0 ;
				else if(t2_lo > t1_hi) merge = 0 ;
				else merge = 1 ;

				if(merge==0) continue ;


				if(*ix1_p > 0) {
					if(*ix2_p > 0) {
						if(*ix1_p != *ix2_p) {
							// I have to merge 2 distinct blobs
							// I will have to decide later what to do
							//LOG(TERR,"Late merge %d %d",*ix1_p,*ix2_p) ;
							late_merges++ ;

							if(blob_ix[*ix1_p] < blob_ix[*ix2_p]) {
								blob_ix[*ix2_p] = blob_ix[*ix1_p] ;

								blob[blob_ix[*ix2_p]].merges++ ;
							}
							else {
								blob_ix[*ix1_p] = blob_ix[*ix2_p] ;

								blob[blob_ix[*ix1_p]].merges++ ;
							}

						}
					}
					else {
						*ix2_p = *ix1_p ;
					}
				}
				else {
					if(*ix2_p > 0) {
						*ix1_p = *ix2_p ;
					}
					else {
						// new blob!
						*ix1_p = blob_cou ;
						*ix2_p = blob_cou ;
						blob_ix[blob_cou] = blob_cou ;
						blob[blob_cou].merges = 0 ;
						blob_cou++ ;
					}
				}

			}

			sweep_unmerged: ;

			if(*ix1_p == 0) {	// still unassigned!
				// new blob
				*ix1_p = blob_cou ;
				blob_ix[blob_cou] = blob_cou ;
				blob[blob_cou].merges = 0 ;
				blob_cou++ ;
			}
		}
			
		
		pads++ ;
	}

	if(blob_cou > f_stat.max_blob_cou) {
		f_stat.max_blob_cou = blob_cou ;
	}


//	LOG(TERR,"Row %d: got %d pads, %d blobs, %d late merges",row,pads,blob_cou,late_merges) ;


	return blob_cou ;
}

int itpc_fcf_c::do_blobs_stage2(int row)
{
//	rp_t *rp = &(row_pad[row][0]) ;
//	rp_t *rp = get_row_pad(row,0) ;

//	int rl = rowlen[row] ;
	int rl = x_max(row,0) ;

	//LOG(TERR,"stage2: row %d",row) ;

	for(int i=0;i<blob_cou;i++) {
		blob[i].seq_cou = 0 ;	// mark as unused

		// initialize extents
		blob[i].p1 = 0xFFFF ;
		blob[i].p2 = 0 ;

		blob[i].t1 = 0xFFFF ;
		blob[i].t2 = 0 ;

		blob[i].flags = 0 ;
	}

	for(int p=1;p<=rl;p++) {
		rp_t *rp = get_row_pad(row,p) ;

		if(rp->s1_len==0) continue ;

		u_short *d = rp->s1_data ;

		for(int i=0;i<rp->s1_len;i++) {	// loop over sequnces
			int t_cou, t_start, t_stop ;

			int ix = *d++ ;

			int b_ix = blob_ix[ix] ;


			if(b_ix==0) {
				LOG(ERR,"Can't be: %d %d, RP %d:%d",b_ix,ix,row,p) ;
			}

			blob_t *bl = &(blob[b_ix]) ;
			

			t_cou = *d++ ;
			t_start = *d++ ;
			t_stop = t_start + t_cou - 1 ;

			if(offline) d += 2*t_cou ;
			else d += t_cou ;


			//LOG(TERR,"%d %d: %d %d",t_start,t_cou,bl->t1,bl->t2) ;

			if(p > bl->p2) bl->p2 = p ;
			if(p < bl->p1) bl->p1 = p ;

			if(t_stop > bl->t2) bl->t2 = t_stop ;
			if(t_start < bl->t1) bl->t1 = t_start ;


			bl->seq_cou++ ;
		}
	}

	//cleanup: cuts etc
	int blob_ok = 0 ;
	for(int i=0;i<blob_cou;i++) {
		if(blob[i].seq_cou == 0) continue ;

		int dp = blob[i].p2 - blob[i].p1 + 1 ;
		int dt = blob[i].t2 - blob[i].t1 + 1;

		if(dp<=0) {
			blob[i].seq_cou = 0 ;
			LOG(ERR,"%d: dp %d",rdo,dp) ;
			continue ;
		}

		if(dt<=0) {
			blob[i].seq_cou = 0 ;
			LOG(ERR,"%d: dt %d",rdo,dt) ;
			continue ;
		}


//		if(blob[i].flags & FCF_FLAGS_TRUNC) {
//			//LOG(WARN,"truncated") ;
//			blob[i].seq_cou = 0 ;	// kill it
//			continue ;
//		}

		if(dp<=1) {	// one pad
			//LOG(WARN,"%d: 1pad %d %d: %d",i,dp,dt,blob[i].seq_cou) ;
			//LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
			blob[i].seq_cou = 0 ;	// kill it
			continue ;
		}



		if(dt<=1 || (dt<=3 && y_is_timebin)) {	// tb range < 3
			//LOG(WARN,"%d: 3tb %d %d %d",i,dp,dt,blob[i].seq_cou) ;
			//LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
			blob[i].seq_cou = 0 ;	// kill it
			continue ;
		}

		u_int bytes_for_blob = (dp+2)*(dt+2)*2*2 ;

		if(bytes_for_blob > sizeof(smooth_dta)) {
//		if((u_int)(dt*dp) >  sizeof(smooth_dta)/sizeof(smooth_dta[0])) {	// too big!
			f_stat.toobigs++ ;

//			LOG(ERR,"row %d: %d: toobig %d X %d",row,i,dp,dt) ;
			//LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
			blob[i].seq_cou = 0 ;
			continue ;
		}

		blob_ok++ ;
	}




#if 0
	LOG(TERR,"Blobs OK %d/%d",blob_ok,blob_cou) ;

	for(int i=0;i<blob_cou;i++) {
		if(blob[i].seq_cou==0) continue ;

		LOG(TERR,"Blob %d: seq %d: flags 0x%X: pad %d:%d, tb %d:%d",i,blob[i].seq_cou,blob[i].flags,
		    blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
	}

#endif

	return blob_ok ;
}

// Returns the number of bytes
int itpc_fcf_c::do_blobs_stage3(int row)
{
	short *dta_s ;
//	rp_t *rp = &(row_pad[row][0]) ;
//	rp_t *rp = get_row_pad(row,0) ;
	double tm ;
	
	u_int *obuff = (u_int *)out_store ;

	//LOG(TERR,"stage3: row %d",row) ;


	gain_rp_t (*gain_row_pad)[MAX_PHYS_PAD+1] = (gain_rp_t (*)[MAX_PHYS_PAD+1]) sec_gains[sector_id] ;


	int clusters_cou = 0 ;

//	LOG(TERR,"stage3: row %d, blob_cou %d",row,blob_cou) ;

	for(int ix=0;ix<blob_cou;ix++) {
		if(blob[ix].seq_cou==0) continue ;



		tm = mark() ;



		blob[ix].tot_charge = 0 ;
		blob[ix].pixels = 0 ;

		// calculate the necessary size
		int dt = blob[ix].t2 - blob[ix].t1 + 1 ;
		int dp = blob[ix].p2 - blob[ix].p1 + 1 ;

		
		int dt_2 = dt + 2 ;


#ifdef DO_DBG1
		printf("...bytes %d vs %d\n",dt_2*(dp+2)*sizeof(short)*2,sizeof(smooth_dta)) ;
#endif
		memset(smooth_dta,0,dt_2*(dp+2)*sizeof(short)*2) ;	// clear both storages which is why there's a 2

		if(offline) {
			memset(track_dta,0xFF,dt_2*(dp+2)*sizeof(short)*2) ;	// set to 0xFF!
		}

		dta_s = smooth_dta + (dp+2)*dt_2 ;	// for smoothed data

		int p1 = blob[ix].p1 ;
		int p2 = blob[ix].p2 ;

#ifdef DO_DBG1
		printf("BLOB START: %d X %d\n",dp,dt) ;
#endif
		// now let's rip though the data and bring it in our XY structure
		for(int p=p1;p<=p2;p++) {
			rp_t *rp = get_row_pad(row,p) ;

			if(rp->s1_len==0) continue ;

			u_short *d = rp->s1_data ;

			int pp = p - p1 + 1 ;

			short *adc_p = smooth_dta + dt_2 * pp ;
			u_short *track_p = track_dta + dt_2 * pp ;

			for(int j=0;j<rp->s1_len;j++) {	// loop over sequnces
				int t_cou, t_start ;

				int ixx = *d++ ;

				int b_ix = blob_ix[ixx] ;


				t_cou = *d++ ;
				t_start = *d++ ;


				if(b_ix != ix) {
					if(offline) d += 2*t_cou ;
					else d += t_cou ;

					continue ;
				}

				blob_t *bl = &(blob[ix]) ;
				
				// asign ptr
				// d now points to ADC
				for(int t=0;t<t_cou;t++) {
					u_short adc = *d++ ;

					u_short tb = t_start++ ;

					int ttb = tb - bl->t1 + 1 ;
					adc_p[ttb] = adc ;
					
					if(offline) {
						track_p[ttb] = *d++ ;
					}

					bl->tot_charge += adc ;
					bl->pixels++ ;
				}
			}
		}

		if(blob[ix].pixels == 0) {	// possible if I blew off pixels due to GG or ADC cuts
			//LOG(ERR,"Blob %d: No pixels???",ix) ;
			blob[ix].seq_cou = 0 ;
			continue ;
		}

		f_stat.tm[4] += delta(tm) ;

		// do 3x3 averaging
		for(int i=1;i<=dp;i++) {
		for(int j=1;j<=dt;j++) {
			short *adc_p = smooth_dta + dt_2 * i + j ;

			short sum = *adc_p ;	// weight by 2*value!
			
			if(sum <= 10) {		// a peak can't have less than that in ADC counts!
				sum = 0 ;
				goto do_store ;
			}
			
			// sum up around the real ADC
			for(int ii=-1;ii<=1;ii++) {
			for(int jj=-1;jj<=1;jj++) {

				int iii = ii*jj ;
				if(iii==1 || iii==-1) continue ;

				short *adc_p = smooth_dta + dt_2 * (i+ii) + (j+jj) ;
				sum += *adc_p ;
			}
			}

			do_store: ;

			// and store it in the smoothed ADC
			adc_p = dta_s + dt_2 * i + j ;

			*adc_p = sum ;	
		}
		}

		f_stat.tm[5] += delta(tm) ;

		// do peak finding over the smoothed ADC
		int peaks_cou = 0 ;
		
		// I think this should not include the edges but shold go from [2,dp-1] and [2,dt-1]
		// I'll see later.

		for(int i=1;i<=dp;i++) {
		for(int j=1;j<=dt;j++) {
			short *adc_p = dta_s + dt_2 * i + j ;	// smoothed

			short adc = *adc_p - 5 ;	// the others around MUST be at least 3 ADCs greater!

			// chop off small peaks, below 1; this might cause peaks_cou to be 0 incorrectly!
			if(adc < 1) continue ;	

			for(int ii=-1;ii<=1;ii++) {
			for(int jj=-1;jj<=1;jj++) {
				if(ii==0 && jj==0) continue ;

				short *adc_p = dta_s + dt_2 * (i+ii) + (j+jj) ;
				short s_adc = *adc_p ;

				if(adc < s_adc) goto skip_calc ;
				if(s_adc < 0) goto skip_calc ;
			}
			}

			#if 0
			// Mask out all around?
			for(int ii=-1;ii<=1;ii++) {
			for(int jj=-1;jj<=1;jj++) {
				short *adc_p = dta_s + dt_2 * (i+ii) + (j+jj) ;

				*adc_p = -1 ;
			}
			}
			#endif

			// Aha -- we have a peak!

			*adc_p = -adc ; // -100*adc ;	// mark as used!

			peaks[peaks_cou].i = i ;
			peaks[peaks_cou].j = j ;
			peaks[peaks_cou].adc = adc ;
			peaks_cou++ ;

			if(peaks_cou >= MAX_PEAKS) {
				LOG(WARN,"Too many peaks %d/%d in row %d",peaks_cou,MAX_PEAKS,row) ;
				// At this point I could go over the found peaks and blow off the ones which are too low!
				// but later...
				goto peaks_done ;
			}

			j += 5 ;	// skip some timebins (at least 2) as to not have them close together in time

			skip_calc: ;
		}
		}

		peaks_done: ;
		
		f_stat.tm[6] += delta(tm) ;

		//LOG(TERR,"Blob %d: peaks %d",ix,peaks_cou) ;
		blob_id++ ;



		if(peaks_cou <= 1) {	// do usual averaging!
			double f_charge = 0.0 ;
			double f_t_ave = 0.0 ;
			double f_p_ave = 0.0 ;
			int flags = 0 ;

			int adc_max = 0 ;
			int pixels = 0 ;
			u_short track_id = 0xFFFF ;
			u_short quality = 0 ;

			if(offline) {	// various track id things
				u_int i_charge = 0 ;
				u_int t_charge = 0 ;

				for(int i=1;i<=dp;i++) {

					short *adc_p = smooth_dta + dt_2 * i + 1;
					u_short *track_p = track_dta + dt_2 * i + 1 ;


					for(int j=1;j<=dt;j++) {
					
						int adc = *adc_p++ ;
						u_short t_id = *track_p++ ;

						// count pixels
						if(adc) pixels++ ;
						else continue ;

						// find the ADC max and asign its trackid
						if(adc > adc_max) {
							//also asign the trackid!
							track_id = t_id ;
							adc_max = adc ;
						}

						//sum up the pixels as well
						i_charge += adc ;
						
					}
				}

				//now sum up the only pixels which belong to the trackid
				for(int i=1;i<=dp;i++) {

					short *adc_p = smooth_dta + dt_2 * i + 1;
					u_short *track_p = track_dta + dt_2 * i + 1 ;


					for(int j=1;j<=dt;j++) {
					
						int adc = *adc_p++ ;
						u_short t_id = *track_p++ ;

						if(t_id == track_id) t_charge += adc ;
						
					}
				}
				
				if(i_charge) {
					quality = (u_short)(100.0*(double)t_charge/(double)i_charge+0.5) ;
				}

				
				
			}

			for(int i=1;i<=dp;i++) {
				int pad = p1 + i - 1 ;

				//LOG(TERR,"Gain RP %d:%d = %f %d",row,pad,gain_row_pad[row][pad].gain,gain_row_pad[row][pad].flags) ;

				if(use_gain) flags |= gain_row_pad[row][pad].flags ;

				short *adc_p = smooth_dta + dt_2 * i + 1;

				u_int i_charge = 0 ;
				u_int i_t_ave = 0 ;

				for(int j=1;j<=dt;j++) {
					
					int adc = *adc_p++ ;

					if(!adc) continue ;

					i_charge += adc ;
					i_t_ave += j * adc ;
				}

				if(i_charge==0) continue ;



				if(use_gain) {
					double corr_charge = (double) i_charge * gain_row_pad[row][pad].gain ;
				
					f_charge += corr_charge ;
					f_t_ave += i_t_ave * gain_row_pad[row][pad].gain + gain_row_pad[row][pad].t0 * corr_charge ;
					f_p_ave += i * corr_charge ;
				}
				else {
					f_charge += (double) i_charge ;
					f_p_ave += i * (double) i_charge ;
					f_t_ave += (double) i_t_ave ;
				}
			}

			if(f_charge<0.1) {
				goto done_peaks;
			}

			f_t_ave /= f_charge ;
			f_p_ave /= f_charge ;

			f_p_ave += p1 - 1 ;
			f_t_ave += blob[ix].t1 - 1 ;

			// and dump out to storage!

			// integerized values
			u_int time_c = (u_int)(f_t_ave * 64.0 + 0.5) ;
			u_int pad_c = (u_int)(f_p_ave * 64.0 + 0.5) ;
			u_int cha = (u_int)(f_charge + 0.5) ;

			// cant happen
			//if(cha==0) printf("WTF cha %f\n",f_charge*1000.0) ;

			//extents 
			u_int tmp_fl ;

			int p_lo = (pad_c/64) - blob[ix].p1 ;
			int p_hi = blob[ix].p2 - (pad_c/64) ;
			
			if(p_lo < 0) p_lo = 0 ;
			if(p_hi < 0) p_hi = 0 ;
			if(p_lo > 7) p_lo = 7 ;
			if(p_hi > 7) p_hi = 7 ;

			tmp_fl = (p_lo<<8)|(p_hi<<11) ;

			int t_lo = (time_c/64) - blob[ix].t1 ;
			int t_hi = blob[ix].t2 - (time_c/64) ;

			if(t_lo < 0) t_lo = 0 ;
			if(t_hi < 0) t_hi = 0 ;
			if(t_lo > 15) t_lo = 15 ;
			if(t_hi > 15) t_hi = 15 ;

			tmp_fl |= (t_hi << 4) | t_lo ;

			if(cha > 0x7FFF) cha = 0x8000 | (cha/1024) ;

			if(flags & 3) tmp_fl |= 0x8000 ;	// ROW_EDGE


			*obuff++ = (time_c << 16) | pad_c ;
			*obuff++ = (cha << 16) | tmp_fl ;

			if(words_per_cluster>2) {
				*obuff++ = (quality<<16)|track_id ;
			}
			if(words_per_cluster>3) {
				*obuff++ = (pixels<<16)|adc_max ;
			}

#ifdef DO_DBG1
//			LOG(TERR,"**** S %d: row %d: %f %f %f",clusters_cou,row,f_p_ave,f_t_ave,f_charge) ;
#endif
			clusters_cou++ ;
		}
		else {	// multiple peak hauristics
			int ip1, ip2 ;
			int it1, it2 ;

			//LOG(TERR,"peaks_cou %d",peaks_cou) ;

			for(int pk=0;pk<peaks_cou;pk++) {
				double f_charge = 0.0 ;
				double f_t_ave = 0.0 ;
				double f_p_ave = 0.0 ;
				int flags = 0 ;

				int adc_max = 0 ;
				int pixels = 0 ;
				u_short track_id = 0xFFFF ;
				u_short quality = 0 ;

				ip1 = peaks[pk].i - 1 ;
				if(ip1 < 1) ip1 = 1 ;
				ip2 = peaks[pk].i + 1 ;
				if(ip2 > dp) ip2 = dp ;

				it1 = peaks[pk].j - 2 ;
				if(it1 < 1) it1 = 1 ;

				it2 = peaks[pk].j + 2 ;
				if(it2 > dt) it2 = dt ;

#ifdef DO_DBG1
				printf("from %d:%d %d:%d\n",ip1,ip2,it1,it2) ;
#endif
				for(int i=ip1;i<=ip2;i++) {
					int pad = p1 + i - 1 ;

					if(use_gain) {
						flags |= gain_row_pad[row][pad].flags ;
					}

					short *adc_p = smooth_dta + dt_2 * i + it1 ;

					u_int i_charge = 0 ;
					u_int i_t_ave = 0 ;


					
					for(int j=it1;j<=it2;j++) {
					
						int adc = *adc_p++ ;

						
#ifdef DO_DBG1						
						printf("%d %d = %d\n",i,j,adc) ;
#endif
						if(!adc) continue ;
						else pixels++ ;

						if(adc>adc_max) adc_max = adc ;

						i_charge += adc ;
						i_t_ave += j * adc ;
					}

					if(i_charge==0) continue ;
				
					if(use_gain) {
						double corr_charge = (double) i_charge * gain_row_pad[row][pad].gain ;
				
						f_charge += corr_charge ;
						f_t_ave += (double) i_t_ave * gain_row_pad[row][pad].gain + gain_row_pad[row][pad].t0 * corr_charge ;
						f_p_ave += i * corr_charge ;
					}
					else {
						f_charge += (double) i_charge ;
						f_p_ave += i * (double)i_charge ;
						f_t_ave += (double) i_t_ave ;
					}
				}


				if(f_charge < 0.1) continue ;

				f_t_ave /= f_charge ;
				f_p_ave /= f_charge ;

				f_p_ave += p1 - 1 ;
				f_t_ave += blob[ix].t1 - 1 ;

				// and dump out to storage!

				// integerized values
				u_int time_c = (u_int)(f_t_ave * 64.0 + 0.5) ;
				u_int pad_c = (u_int)(f_p_ave * 64.0 + 0.5) ;
				u_int cha = (u_int)(f_charge + 0.5) ;

				//if(cha==0) {
				//	printf("WTF Multi peak %f\n",f_charge) ;
				//}

				//extents 
				u_int tmp_fl ;

				int p_lo = 1 ;
				int p_hi = 1 ;
				
				tmp_fl = (p_lo<<8)|(p_hi<<11) ;

				int t_lo = 2 ;
				int t_hi = 2 ;

				tmp_fl |= (t_hi << 4) | t_lo ;

				if(cha > 0x7FFF) cha = 0x8000 | (0xFFFF & (cha/1024)) ;

				// add flags here
				pad_c |= 0x8000 ;					// merged flag
				if(flags & 3) tmp_fl |= 0x8000 ;			// ROW_EDGE



				*obuff++ = (time_c << 16) | pad_c ;
				*obuff++ = (cha << 16) | tmp_fl ;

				if(words_per_cluster > 2) {
					*obuff++ = (quality<<16)|track_id ;
				}
				if(words_per_cluster > 3) {
					*obuff++ = (pixels<<16)|adc_max ;
				}
				
#ifdef DO_DBG1
//				LOG(TERR,"0x%X 0x%X 0x%X 0x%X - 0x%X 0x%X",pad_c,time_c,cha,tmp_fl, (time_c<16)|pad_c,(cha<<16)|tmp_fl) ;
//				LOG(TERR,"**** D %d: %f %f %f",clusters_cou,f_p_ave,f_t_ave,f_charge) ;
#endif				
				clusters_cou++ ;
			}
		}


		done_peaks:;
		f_stat.tm[7] += delta(tm) ;

#ifdef DO_DBG1
		printf("BLOB start: row %d, peaks %d: %d:%d, %d:%d\n",row,peaks_cou,blob[ix].p1,blob[ix].p2,blob[ix].t1,blob[ix].t2) ;

		for(int j=1;j<=dt;j++) {

		printf("TB %3d ",blob[ix].t1+j-1) ;
		for(int i=1;i<=dp;i++) {
			short *adc_s_p = dta_s + dt_2 * i + j ;	// smoothed

			short *adc_p = smooth_dta + dt_2 * i + j ;	// real

			if(*adc_s_p < 0) {
				printf("%s%4d(%4d)%s ",ANSI_RED,*adc_p,*adc_s_p,ANSI_RESET) ;
			}
			else {
				printf("%4d(%4d) ",*adc_p,*adc_s_p) ;
			}
		}
		printf("\n") ;
		}
		fflush(stdout) ;
#endif
	}

//	LOG(TERR,"row %d: clusters %d, bytes %d",row,clusters_cou,(obuff-(u_int *)out_store)*4) ;

	return (obuff - out_store) ;	// in ints!!!
}

int itpc_fcf_c::do_row_check(int row)
{
//	rp_t *rp = &(row_pad[row][0]) ;
//	rp_t *rp = get_row_pad(row,0) ;

//	int rl = rowlen[row] ;
	int rl = x_max(row,0) ;

	for(int p=1;p<=rl;p++) {	// < is on purpose!!!
		int t1_cou, t1_lo ;
		u_short *ix1_p ;
		rp_t *rp = get_row_pad(row,p) ;

		if(rp->s1_len==0) continue ;
		
		u_short *d1 = rp->s1_data ;

		for(int i=0;i<rp->s1_len;i++) {
			ix1_p = d1++ ;	
			
			t1_cou = *d1++ ;	// actually tb_cou
			t1_lo = *d1++ ;	// actually t_start ;

			if(offline) d1 += 2*t1_cou ;
			else d1 += t1_cou ;			// advance to next 

			if(*ix1_p==0 || *ix1_p==0xFFFF) {
				LOG(ERR,"sequence unassigned %d:%d %d:%d = %u [%d]",p,rl,i,rp[p].s1_len,*ix1_p,t1_lo) ;
			}
		}
		
	}

	
	int max_ix = 0 ;

	for(int i=1;i<blob_cou;i++) {
		blob[i].cou = 0 ;
	}

	for(int i=1;i<blob_cou;i++) {
		blob[blob_ix[i]].cou++ ;
	}
	
	for(int i=1;i<blob_cou;i++) {
		LOG(TERR,"blob_ix[%d] = %d",i,blob_ix[i]) ;

	}

	for(int i=1;i<blob_cou;i++) {
		if(blob[i].cou) {
			LOG(TERR," Blob %d: %d",i,blob[i].cou) ;
			max_ix++ ;
		}
	}

	for(int i=1;i<max_ix;i++) {
		if(blob[i].merges) {
			LOG(TERR,"blob %d: merged %d",i,blob[i].merges) ;
		}
	}


	LOG(TERR,"Final blob count %d/%d",max_ix,blob_cou) ;

	return 0 ;
	
}

