#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>
#include <arpa/inet.h>
#include <sys/time.h>


#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include <trgDataDefs.h>
#include "trgConfNum.h"

// only the detectors we will use need to be included
// for their structure definitions...
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxFCF_flags.h>

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcCore.h>
#include <DAQ_ITPC/itpcPed.h>
#include <DAQ_ITPC/itpcInterpreter.h>
#include <DAQ_ITPC/itpc_rowlen.h>

#include <DAQ_ITPC/itpcFCF.h>

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




// At start of epoch, as soon as we know the sector
int itpc_fcf_c::init(int sec)
{
	LOG(TERR,"%s: sector %d; sizeof(rp)=%u, sizeof class %u",__FUNCTION__,sec,sizeof(row_pad),sizeof(itpc_fcf_c)) ;

	sector = sec ;

	// set defaults

	for(int i=0;i<=MAX_ROW;i++) {
		rowlen[i] = itpc_rowlen[i] ;
		
		for(int j=0;j<=MAX_PAD;j++) {
			gain_row_pad[i][j].gain = 1.0 ;
			gain_row_pad[i][j].t0 = 0.0 ;
			gain_row_pad[i][j].flags = 0 ;
		}
	}

	//bring in the gain & T0 corrections from file
	if(sector < 0) return 0 ;


	return 0 ;
}

// At start event
void itpc_fcf_c::event_start()
{
	s1_found = 0 ;

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
	u_short tb_buff[MAX_TB] ;

	itpc_ifee_to_rowpad(fee_id, fee_ch, row, pad) ;

//	LOG(TERR,"FEE %d, CH %d: RP %d:%d, words %d",fee_id,fee_ch,row,pad,words) ;

//	row_pad[row][pad].raw_data = data ;
//	row_pad[row][pad].raw_words = words ;
	

	int word32 = (words/3) + (words%3?1:0) ;

	int t_cou = 0 ;
	for(int i=0;i<word32;i++) {
		u_int d = *data++ ;

		tb_buff[t_cou++] = (d>>20) & 0x3FF ;
		tb_buff[t_cou++] = (d>>10) & 0x3FF ;
		tb_buff[t_cou++] = d & 0x3FF ;
	}

	int seq_cou = 0  ;

	u_short *s1_data = (u_short *)row_pad[row][pad].s1_data ;

	for(int i=0;i<words;) {		// now timebins!
		int t_cou = tb_buff[i++] ;
		int t_start = tb_buff[i++] ;
		int t_stop = t_start + t_cou - 1 ;

		seq_cou++ ;

		*s1_data++ = 0 ;	// index now 0!
		*s1_data++ = t_cou ;
		*s1_data++ = t_start ;



		for(int t=t_start;t<=t_stop;t++) {
			// initial cuts, where I blow of data
#if 1
			// cut timebin due to gating grid pickup
			if(t>425) {
				*s1_data = 0 ;
			}
			else if((t>=26)&&(t<=31)) *s1_data = 0 ;
			else {
				if(tb_buff[i]<=4) *s1_data = 0 ;	// cut low data
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
	int s_count = s1_data - (u_short *)row_pad[row][pad].s1_data ;
	if(s_count >= MAX_TB) {
		LOG(ERR,"In trouble at RP %d:%d",row,pad) ;
	}

	// for later optimization!
	if(s_count >= max_s1_len) {
		max_s1_len = s_count ;
	}

	s1_found += seq_cou ;
	

	row_pad[row][pad].s1_len = seq_cou ;	// sequence count!

	return 0 ;	
}

// Note that the work is done in shorts! (2 bytes)
// Returns bytes of storage.
int itpc_fcf_c::do_fcf(void *v_store, int bytes)
{
	out_store = (u_int *) v_store ;
	max_out_bytes = bytes ;

	u_int *store_start = out_store ;
		
	double tm[6] ;
	memset(tm,0,sizeof(tm)) ;

	for(int row=1;row<=MAX_ROW;row++) {
		double tmx ;
		int found_ints ;

		u_int *row_store = out_store++ ;	// leave space for row number
		out_store++ ;	// leave space for version
		out_store++ ;	// leave space for cluster count


//		LOG(INFO,"Row %d: start",row) ;

		tmx = mark() ;
		do_blobs_stage1(row) ;
		tmx = delta(tmx) ;

		tm[0] += tmx ;	

		//do_row_check(row) ;

		tmx = mark() ;
		do_blobs_stage2(row) ;
		tmx = delta(tmx) ;

		tm[1] += tmx ;

		tmx = mark() ;
		found_ints = do_blobs_stage3(row) ;
		tmx = delta(tmx) ;

		tm[2] += tmx ;

		if(found_ints) {

			*row_store++ = (words_per_cluster<<16)|row ;	// words-per-cluster | row
			*row_store++ = version ;
			*row_store++ = found_ints  ;	// in ints!

			out_store += found_ints ;
		}
		else {
			out_store = row_store ;	// rewind 
		}

//		LOG(INFO,"Row %d: end",row) ;
	}


	if(s1_found > max_s1_found) {
		max_s1_found = s1_found ;
	}

//	LOG(TERR,"Had %d sequences, total bytes used %u",s1_found,(store-store_start)*2) ;
//	LOG(TERR,"So far: max_s1_found %d, max_s1_len %d, max_blob_cou %d", max_s1_found,max_s1_len,max_blob_cou) ;
//	LOG(TERR,"delta stage1 %f, stage2 %f, stage3 %f",tm[0],tm[1],tm[2]) ;

	return (out_store-store_start) ;	// in ints
}

int itpc_fcf_c::do_blobs_stage1(int row)
{
	int pads = 0 ;
	int late_merges = 0 ;

	rp_t *rp = &(row_pad[row][0]) ;
	int rl = rowlen[row] ;

	blob_cou = 1 ;


	for(int p=1;p<=rl;p++) {	// < is on purpose!!!
		int t1_cou, t1_lo, t1_hi ;
		int t2_cou, t2_lo, t2_hi ;
		u_short *ix1_p ;
		u_short *ix2_p ;

		if(rp[p].s1_len==0) continue ;
		
		u_short *d1 = rp[p].s1_data ;

		for(int i=0;i<rp[p].s1_len;i++) {
			u_short *d2 ;

			ix1_p = d1++ ;	
			


			t1_cou = *d1++ ;	// actually tb_cou
			t1_lo = *d1++ ;	// actually t_start ;

			t1_hi = t1_lo + t1_cou - 1 ;	// now is really

			d1 += t1_cou ;			// advance to next 

			if(p==rl) goto sweep_unmerged ;	// since there is no pad after that

			d2 = rp[p+1].s1_data ;

			for(int j=0;j<rp[p+1].s1_len;j++) {
				ix2_p = d2++ ;

				t2_cou = *d2++ ;
				t2_lo = *d2++ ;

				t2_hi = t2_lo + t2_cou - 1 ;

				d2 += t2_cou ;

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

	if(blob_cou > max_blob_cou) {
		max_blob_cou = blob_cou ;
	}


//	LOG(TERR,"Row: got %d pads, %d blobs, %d late merges",pads,blob_cou,late_merges) ;


	return blob_cou ;
}

int itpc_fcf_c::do_blobs_stage2(int row)
{
	rp_t *rp = &(row_pad[row][0]) ;
	int rl = rowlen[row] ;

	for(int i=0;i<blob_cou;i++) {
		blob[i].seq_cou = 0 ;	// mark as unused

		// initialize extents
//		blob[i].p1 = MAX_PAD+1 ;
		blob[i].p1 = 10000 ;
		blob[i].p2 = 0 ;
//		blob[i].t1 = MAX_TB+1 ;
		blob[i].t1 = 10000 ;
		blob[i].t2 = 0 ;

		blob[i].flags = 0 ;
	}

	for(int p=1;p<=rl;p++) {
		if(rp[p].s1_len==0) continue ;

		u_short *d = rp[p].s1_data ;

		for(int i=0;i<rp[p].s1_len;i++) {	// loop over sequnces
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

			d += t_cou ;


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
			LOG(ERR,"dp %d",dp) ;
		}

		if(dt<=0) {
			LOG(ERR,"dt %d",dt) ;
		}


//		if(blob[i].flags & FCF_FLAGS_TRUNC) {
//			//LOG(WARN,"truncated") ;
//			blob[i].seq_cou = 0 ;	// kill it
//			continue ;
//		}

		if(dp==1) {	// one pad
			//LOG(WARN,"%d: 1pad %d %d: %d",i,dp,dt,blob[i].seq_cou) ;
			//LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
			blob[i].seq_cou = 0 ;	// kill it
			continue ;
		}



		if(dt<=3) {	// tb range < 3
			//LOG(WARN,"%d: 3tb %d %d %d",i,dp,dt,blob[i].seq_cou) ;
			//LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
			blob[i].seq_cou = 0 ;	// kill it
			continue ;
		}

		u_int bytes_for_blob = (dp+2)*(dt+2)*2*2 ;

		if(bytes_for_blob > sizeof(smooth_dta)) {
//		if((u_int)(dt*dp) >  sizeof(smooth_dta)/sizeof(smooth_dta[0])) {	// too big!
			LOG(ERR,"row %d: %d: toobig %d X %d",row,i,dp,dt) ;
			LOG(WARN,"%d %d %d %d",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
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
	rp_t *rp = &(row_pad[row][0]) ;

	
	u_int *obuff = (u_int *)out_store ;
	int clusters_cou = 0 ;


	for(int ix=0;ix<blob_cou;ix++) {
		if(blob[ix].seq_cou==0) continue ;

//		LOG(TERR,"Blob %d in play",ix) ;

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

		dta_s = smooth_dta + (dp+2)*dt_2 ;	// for smoothed data

		int p1 = blob[ix].p1 ;
		int p2 = blob[ix].p2 ;

#ifdef DO_DBG1
		printf("BLOB START: %d X %d\n",dp,dt) ;
#endif
		// now let's rip though the data and bring it in our XY structure
		for(int p=p1;p<=p2;p++) {
			if(rp[p].s1_len==0) continue ;

			u_short *d = rp[p].s1_data ;

			int pp = p - p1 + 1 ;
			short *adc_p = smooth_dta + dt_2 * pp ;

			for(int j=0;j<rp[p].s1_len;j++) {	// loop over sequnces
				int t_cou, t_start ;

				int ixx = *d++ ;

				int b_ix = blob_ix[ixx] ;


				t_cou = *d++ ;
				t_start = *d++ ;

				//d += t_cou ;

				if(b_ix != ix) {
					d += t_cou ;
					continue ;
				}

				blob_t *bl = &(blob[ix]) ;
				
				// asign ptr
				// d now points to ADC
				for(int t=0;t<t_cou;t++) {
					u_short adc = *d++ ;
					u_short tb = t_start++ ;

					//TB & ADC cuts
					//if((tb>=26)&&(tb<=31)) continue ;
					//if((tb>430)) continue ;

					//if(adc<=4) continue ;
					
					//if(adc <= 2) continue ;
					//printf("... %d %d = %d\n",p,tb,adc) ;

					int ttb = tb - bl->t1 + 1 ;
					adc_p[ttb] = adc ;


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
		
		//LOG(TERR,"Blob %d: peaks %d",ix,peaks_cou) ;
		

		words_per_cluster = 3 ;

		if(peaks_cou <= 1) {	// do usual averaging!
			double f_charge = 0.0 ;
			double f_t_ave = 0.0 ;
			double f_p_ave = 0.0 ;

			int adc_max = 0 ;

			for(int i=1;i<=dp;i++) {

				short *adc_p = smooth_dta + dt_2 * i + 1;

				u_int i_charge = 0 ;
				u_int i_t_ave = 0 ;

				for(int j=1;j<=dt;j++) {
					
					int adc = *adc_p++ ;


					if(adc > adc_max) adc_max = adc ;


					i_charge += adc ;
					i_t_ave += j * adc ;
				}

				if(i_charge==0) continue ;

				int pad = p1 + i - 1 ;
				
				double corr_charge = (double) i_charge * gain_row_pad[row][pad].gain ;
				
				f_charge += corr_charge ;
				f_t_ave += i_t_ave * gain_row_pad[row][pad].gain + gain_row_pad[row][pad].t0 * corr_charge ;
				f_p_ave += i * corr_charge ;
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

			if(blob[ix].p1==1 || blob[ix].p2==rowlen[row]-1) tmp_fl |= 0x8000 ;	// ROW EDGE

			*obuff++ = (time_c << 16) | pad_c ;
			*obuff++ = (cha << 16) | tmp_fl ;
			*obuff++ = adc_max ;

			words_per_cluster = 3 ;

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
				
				int adc_max = 0 ;

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
					short *adc_p = smooth_dta + dt_2 * i + it1 ;

					u_int i_charge = 0 ;
					u_int i_t_ave = 0 ;


					
					for(int j=it1;j<=it2;j++) {
					
						int adc = *adc_p++ ;
#ifdef DO_DBG1						
						printf("%d %d = %d\n",i,j,adc) ;
#endif

						if(adc>adc_max) adc_max = adc ;

						i_charge += adc ;
						i_t_ave += j * adc ;
					}

					if(i_charge==0) continue ;

					int pad = p1 + i - 1 ;
				
					double corr_charge = (double) i_charge * gain_row_pad[row][pad].gain ;
				
					f_charge += corr_charge ;
					f_t_ave += (double) i_t_ave * gain_row_pad[row][pad].gain + gain_row_pad[row][pad].t0 * corr_charge ;
					f_p_ave += i * corr_charge ;
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
				if((ip1==1)||(ip2==rowlen[row]-1)) tmp_fl |= 0x8000 ;	// ROW_EDGE

				*obuff++ = (time_c << 16) | pad_c ;
				*obuff++ = (cha << 16) | tmp_fl ;
				*obuff++ = adc_max ;
				
				words_per_cluster = 3 ;

#ifdef DO_DBG1
//				LOG(TERR,"0x%X 0x%X 0x%X 0x%X - 0x%X 0x%X",pad_c,time_c,cha,tmp_fl, (time_c<16)|pad_c,(cha<<16)|tmp_fl) ;
//				LOG(TERR,"**** D %d: %f %f %f",clusters_cou,f_p_ave,f_t_ave,f_charge) ;
#endif				
				clusters_cou++ ;
			}
		}


		done_peaks:;

#ifdef DO_DBG1
		printf("BLOB start: row %d, peaks %d: %d:%d, %d:%d\n",row,peaks_cou,blob[ix].p1,blob[ix].p2,blob[ix].t1,blob[ix].t2) ;

		for(int j=1;j<=dt;j++) {

		printf("TB %3d ",blob[ix].t1+j-1) ;
		for(int i=1;i<=dp;i++) {
			short *adc_s_p = dta_s + dt_2 * i + j ;	// smoothed
			short *adc_p = smooth_dta + dt_2 * i + j ;	// real

			printf("%4d(%4d) ",*adc_p,*adc_s_p) ;
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
	rp_t *rp = &(row_pad[row][0]) ;
	int rl = rowlen[row] ;

	for(int p=1;p<=rl;p++) {	// < is on purpose!!!
		int t1_cou, t1_lo ;
		u_short *ix1_p ;

		if(rp[p].s1_len==0) continue ;
		
		u_short *d1 = rp[p].s1_data ;

		for(int i=0;i<rp[p].s1_len;i++) {
			ix1_p = d1++ ;	
			
			t1_cou = *d1++ ;	// actually tb_cou
			t1_lo = *d1++ ;	// actually t_start ;

			d1 += t1_cou ;			// advance to next 

			if(*ix1_p==0 || *ix1_p==0xFFFF) {
				LOG(ERR,"sequence unassigned %d:%d %d:%d = %u [%d]",p,rowlen,i,rp[p].s1_len,*ix1_p,t1_lo) ;
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

