#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#include <rtsLog.h>

#include <rtsSystems.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_READER/daq_det.h>

#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxFCF_flags.h>
#include <DAQ_TPX/tpxPed.h>

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcFCF.h>
#include <DAQ_ITPC/itpcPed.h>


//#define DBG_PRINT	1

#include "tpc23_base.h"


//tpc23_base::row_pad_t (*tpc23_base::rp_gain)[ROW_MAX+1][PAD_MAX+1] ;

//tpc23_base::row_pad_t (*tpc23_base::rp_gain_tpx)[ROW_MAX+1][PAD_MAX+1] ;
//tpc23_base::row_pad_t (*tpc23_base::rp_gain_itpc)[ROW_MAX+1][PAD_MAX+1] ;

short tpc23_base::bad_fee_cou[24][6] ;
short tpc23_base::bad_fee[24][6][36] ;

//int tpc23_base::rowlen[ROW_MAX+1] ;
//int tpc23_base::row_min ;
//int tpc23_base::row_max ;

itpcData *tpc23_base::data_c ;

//tpxPed *tpc23_base::peds ;
//pthread_mutex_t tpc23_base::peds_mutex ;

int tpc23_base::fcf_decode(u_int *p_buff, daq_sim_cld_x *dc, u_int version)
{
	daq_cld cld ;
	int words_used ;
	itpc_fcf_c *old_fcf ;

	words_used = old_fcf->fcf_decode(p_buff,&cld,version) ;

	memcpy(&dc->cld,&cld,sizeof(cld)) ;

	p_buff += words_used ;

	dc->track_id = 0 ;
	dc->quality = (*p_buff)>>16;

	p_buff++ ;

	dc->max_adc = *p_buff & 0xFFFF ;
	dc->pixels = (*p_buff)>>16 ;

	p_buff++ ;

	dc->reserved[0] = *p_buff ;	// NOTE: this is where the extended 32 bit track ID is
	dc->track_id = dc->reserved[0]&0xFFFF ;	// but use the lower 16 bits...

	p_buff++ ;

	return words_used + 3 ;
}

void tpc23_base::sim_evt_start(int sec1)
{
	sector1 = sec1 ;
	
	if(s1_dta==0) {
		s1_dta = (u_short *) malloc((ROW_MAX+1)*(PAD_MAX+1)*512*sizeof(*s1)) ;
	}
	if(s1_track_id==0) {
		s1_track_id = (int *) malloc((ROW_MAX+1)*(PAD_MAX+1)*512*sizeof(*s1_track_id)) ;
	}
	if(store_track_id==0) {
		store_track_id = (int *) malloc((PAD_MAX+1)*512*sizeof(*store_track_id)) ;
	}

	sequence_cou = 0 ;
	err = 0 ;
	last_ix = 0 ;

	evt++ ;
	evt_trgd++ ;
}


int tpc23_base::do_ch_sim(int row, int pad, u_short *adc, int *track_id)
{
	int t_start = -1 ;

	struct seq_t *seq = s1[row][pad].seq ;
	
	int s_cou = 0 ;
	int dta_p_ix = 0 ;

	s1[row][pad].ix = last_ix ;

	u_short *dta = s1_dta + last_ix ;	// where I store the data
	int *track = s1_track_id + last_ix ;	// and corresponding track_id

#if 1
	// SAMPA allows up to 2 zeros in a row so let's emulate it
	for(int t=1;t<510;t++) {
		if(adc[t-1]!=0 && adc[t]==0 && adc[t+1]!=0) {
			adc[t] = 0xFFFF ;
		}
		else if(adc[t-1]!=0 && adc[t]==0 && adc[t+1]==0 && adc[t+2]!=0) {
			adc[t] = 0xFFFF ;
			adc[t+1] = 0xFFFF ;
			t += 1 ;
		}

	}
#endif

	int t_err = 0 ;

	for(int t=0;t<512;t++) {
		if(adc[t]) {
			if(t_start<0) {
				// starting
				seq[s_cou].t_lo = t ;
				seq[s_cou].dta_p = dta_p_ix ;
				seq[s_cou].blob_id = 0 ;	// clear it here

				t_start = t ;
			}
			
			if(adc[t]==0xFFFF) {
				//adc[t] = 0 ;
				dta[dta_p_ix] = 0 ;
			}
			else {
				dta[dta_p_ix] = adc[t] ;
			}
			track[dta_p_ix] = track_id[t] ;

			dta_p_ix++ ;
		}
		else {	// data is now 0
			if(t_start>=0) {	// started!
				// so stop it
				seq[s_cou].t_hi = t-1 ;

				if(t<=0) {
					LOG(ERR,"rp %d:%d t is %d, t_start is %d",row,pad,t,t_start) ;
					t_err = 1 ;
				}

				s_cou++ ;

				t_start = -1 ;

				if(s_cou>=SEQ_MAX) {
					LOG(WARN,"too many sequences %d: sec %d, row %d, pad %d, tb %d",s_cou,sector1,row,pad,t-1) ;
					goto done ;
				}
			}
		}
	}

	if(t_err) {
		for(int t=0;t<511;t++) {
			LOG(TERR,"  tb %3d %d",t,adc[t]) ;
		}
	}

	if(t_start>=0) {	// a sequence started but was never finished before the timebins ran out
		// stop it
		seq[s_cou].t_hi = 511 ;
		s_cou++ ;
	}

	if(s_cou>=SEQ_MAX) {
		LOG(ERR,"still too many sequences %d: sec %d, row %d, pad %d",s_cou,sector1,row,pad) ;
	}

	done:;

	sequence_cou += s_cou ;

	seq[s_cou].t_hi = -1 ;	// sentinel

	last_ix += dta_p_ix ;

	return 0 ;
}

#if 0
void tpc23_base::sim_do_pad(int row, int pad, short *adc, int *track_id)
{
	int t_start = -1 ;

	struct seq_t *seq = s1[row][pad].seq ;
	
	int s_cou = 0 ;
	int dta_p_ix = 0 ;

	s1[row][pad].ix = last_ix ;

	u_short *dta = s1_dta + last_ix ;	// where I store the data
	int *track = s1_track_id + last_ix ;	// and corresponding track_id

	for(int t=0;t<512;t++) {
		if(adc[t]) {
			if(t_start<0) {
				// starting
				seq[s_cou].t_lo = t ;
				seq[s_cou].dta_p = dta_p_ix ;
				seq[s_cou].blob_id = 0 ;	// clear it here

				t_start = t ;
			}
			dta[dta_p_ix] = adc[t] ;
			track[dta_p_ix] = track_id[t] ;
			dta_p_ix++ ;
		}
		else {	// data is now 0
			if(t_start>=0) {	// started!
				// so stop it
				seq[s_cou].t_hi = t-1 ;

				if(t<=1) {
					LOG(ERR,"t is %d, t_start is %d",t,t_start) ;
				}

				s_cou++ ;

				t_start = -1 ;

				if(s_cou>=SEQ_MAX) {
					LOG(WARN,"too many sequences %d: sec %d, row %d, pad %d, tb %d",s_cou,sector1,row,pad,t-1) ;
					goto done ;
				}
			}
		}
	}

	if(t_start>=0) {	// a sequence started but was never finished before the timebins ran out
		// stop it
		seq[s_cou].t_hi = 511 ;
		s_cou++ ;
	}

	if(s_cou>=SEQ_MAX) {
		LOG(ERR,"still too many sequences %d: sec %d, row %d, pad %d",s_cou,sector1,row,pad) ;
	}

	done:;

	sequence_cou += s_cou ;

	seq[s_cou].t_hi = -1 ;	// sentinel

	last_ix += dta_p_ix ;

}
#endif


int tpc23_base::row_stage2(int row)
{
	int blob_good = 0 ;
#ifdef DBG_PRINT
	printf("ROW %2d: STAGE2: BLOBS: blob_cou %d\n",row,blob_cou-1) ; fflush(stdout) ;
#endif
	u_int *s2_marker = s2_dta ;


	// I can preapply some cuts here already

	for(int i=1;i<blob_cou;i++) {

		// apply morphological cuts too
		int t_len = blob[i].t2-blob[i].t1+1 ;

		if(t_len<=1) blob[i].flags |= FCF_BROKEN_EDGE ;	// lenght in time is 2 or less

		// to match older itpcFCF
		if(t_len<=3) blob[i].flags |= FCF_BROKEN_EDGE ;	// lenght in time is 2 or less


		if(blob[i].area<=4) blob[i].flags |= FCF_BROKEN_EDGE ;	// pixel count is 4 or less


#ifdef DBG_PRINT
		printf("blob %2d(%2d): pad %d:%d, tb %d:%d, flags 0x%X, area %d\n",i,blob_ix[i],
		       blob[i].p1,blob[i].p2,
		       blob[i].t1,blob[i].t2,
		       blob[i].flags,
		       blob[i].area) ;
#endif
		if(log_level>=2) {
			LOG(TERR,"blob %2d: pad %d:%d, tb %d:%d, flags 0x%X, area %d",i,
		       blob[i].p1,blob[i].p2,
		       blob[i].t1,blob[i].t2,
		       blob[i].flags,
		       blob[i].area) ;


		}

//		if(blob[i].flags) continue ;
		if(blob[i].flags&FCF_BROKEN_EDGE) continue ;

		blob_good++ ;
	}

	if(log_level>=1) LOG(TERR,"ROW %2d: STAGE2: BLOBS: blob_cou %d/%d",row,blob_good,blob_cou) ;
						
	// and now loop over blobs, extract data
	for(int i=1;i<blob_cou;i++) {
		if(blob[i].flags & FCF_BROKEN_EDGE) continue ;	// skip SMALL clusters!
		if(blob[i].flags & 0x80) continue ;


		
#ifdef DBG_PRINT
		printf("BLOB %d(%d)/%d: row %2d:\n",i,i,blob_cou,row) ;
		printf(" pad %d:%d, tb %d:%d\n",blob[i].p1,blob[i].p2,blob[i].t1,blob[i].t2) ;
#endif

		int td = blob[i].t2 - blob[i].t1 + 1 ;
		int pd = blob[i].p2 - blob[i].p1 + 1 ;

		for(int pad=blob[i].p1;pad<=blob[i].p2;pad++) {
			struct seq_t *seq = s1[row][pad].seq ;

			if(log_level>=2) {
				LOG(TERR,"blob %d: pad %d, t_hi %d; ix %d",i,pad,seq->t_hi,s1[row][pad].ix) ;
			}

			if(seq->t_hi==-1) continue ;

			u_short *d = s1_dta + s1[row][pad].ix ;



			int px = pad - blob[i].p1 ;

			memset(store[px],0,td*sizeof(store[px][0])) ;

			while(seq->t_hi>=0) {
				int j ;

				{
				int bid = blob_ix[seq->blob_id] ;


				if(log_level>=2) {
					LOG(TERR,"   i %d, bid %d, blob_id %d",i,bid,seq->blob_id) ;
				}

				if(bid!=i) {	// we want only data which comes from the blob I am looking for!
					seq++ ;
					continue ;
				}
				}



				j=0 ;
				for(int t=seq->t_lo;t<=seq->t_hi;t++) {
					u_short dta = *(d+seq->dta_p+j) ;
#ifdef DBG_PRINT
					printf("    pad %3d, tb %3d, adc %3d %5u\n",pad,t,dta,dta) ;
#endif
					int tx = t - blob[i].t1 ;

					store[px][tx] = dta ;

					j++ ;
				}

#ifdef DBG_PRINT
//				LOG(TERR,"here: online %d, RP%d:%d, %d:%d",online,row,pad,seq->t_lo,seq->t_hi) ;
#endif
				if(online==0) {	// also store track_ids in parallel
					int *d_track_id = s1_track_id + s1[row][pad].ix ;
					
					j=0 ;
					for(int t=seq->t_lo;t<=seq->t_hi;t++) {
						int (*tr)[512] ;

						tr = (int (*)[512]) store_track_id ;

						int dta = *(d_track_id+seq->dta_p+j) ;

						int tx = t - blob[i].t1 ;

						tr[px][tx] = dta ;
						j++ ;
					}
				}


#ifdef DBG_PRINT
//				LOG(TERR,"here") ;
#endif

				seq++ ;
			}
		}	


#ifdef DBG_PRINT
		printf("Here: %d %d\n",pd,td) ; fflush(stdout) ;
#endif

		if(log_level>=2) LOG(TERR,"   pd %d, td %d",pd,td) ;

		for(int p=0;p<pd;p++) {
			for(int t=0;t<td;t++) {

				int sum  ;

				sum = store[p][t] ;

				if(sum <= 10) {		// a peak can't have less than that ADC counts
							// so skip the 3x3 sum to save time
					sum = 0 ;
					goto store ;
				}

				//sum = 0 ;	// if I leave this out I will have an additional count of the middle ADC...

				for(int ip=-1;ip<=1;ip++) {
					for(int it=-1;it<=1;it++) {
						int d1 = 0 ;
						int adc ;

						int iii = ip * it ;
						if(iii==1 || iii==-1) continue ;	// skip corners ala itpcFCF

						// check for blob bounds
						if((p+ip)<0) d1 = 1 ;
						if((p+ip)>=pd) d1 = 1 ;
						if((t+it)<0) d1 = 1 ;
						if((t+it)>=td) d1 = 1 ;

						if(d1) adc = 0 ;
						else adc = store[p+ip][t+it] ;

						sum += adc ;

					}
				}

				store: ;
				
				smooth[p][t] = sum ;

			}
		}

#ifdef DBG_PRINT
		printf("Here 1: %d %d\n",pd,td) ; fflush(stdout) ;
#endif

		// and now find peaks using smoothed data
		peaks_cou = 0 ;

		if(log_level>=2) LOG(TERR,"   here %d %d",pd,td) ;

		for(int p=0;p<pd;p++) {
			for(int t=0;t<td;t++) {
				// a peak has to be at least this much above any of the surrounding 8 pixels
				int adc = smooth[p][t] - 5 ;	// I should put this constant out

				if(adc < 1) continue ;

				for(int ip=-1;ip<=1;ip++) {
					if((p+ip)<0) continue ;
					if((p+ip)>=pd) continue ;

					for(int it=-1;it<=1;it++) {
						if((t+it)<0) continue ;
						if((t+it)>=td) continue  ;

						if(ip==0 && it==0) continue ;

						


						int s_adc = smooth[p+ip][t+it] ;

						//printf("peak %d,%d: %d,%d -->  mid %d < %d -- skips\n",p,t,ip,it,adc,s_adc) ;
						
						if(adc < s_adc) goto skip_calc ;
						if(s_adc < 0) goto skip_calc ;

					}
				}

				// we have a peak here!!!
				peaks[peaks_cou].t = t ;
				peaks[peaks_cou].p = p ;
				peaks_cou++ ;

				smooth[p][t] = -adc ;	// mark as used

				t += 5 ;	// skip some timebins so I don't have close peaks
						// should put this as a parameter too!

				skip_calc: ;
			}

			
		}

#ifdef DBG_PRINT
		printf("Here 2: %d %d\n",pd,td) ; fflush(stdout) ;
#endif

#ifdef DBG_PRINT
		printf("PEAKS %d\n",peaks_cou) ;

		printf("PAD   : ") ;
		for(int p=0;p<pd;p++) {
			printf("%3d ",blob[i].p1+p) ;
		}
		printf("\n") ;
		for(int t=0;t<td;t++) {
			printf("TB %3d: ",blob[i].t1+t) ;
			for(int p=0;p<pd;p++) {
				int pk = 0 ;
				for(int j=0;j<peaks_cou;j++) {
					if(peaks[j].p == p && peaks[j].t==t) pk = 1 ;
				}

				printf("%3d[%4d]%c ",store[p][t],smooth[p][t],pk?'*':' ') ;
			}
			printf("\n") ;
		}
		fflush(stdout) ;
#endif
		// and now rip am out
		int one_peak = 0 ;
		if(peaks_cou<=1) one_peak = 1 ;
		

		if(log_level>=2) LOG(TERR,"row %d: blob %d: flags 0x%X: peaks_cou %d, one_peak %d",row,i,blob[i].flags,peaks_cou,one_peak) ;

		//if(blob[i].flags & 1) one_peak = 0 ;

#ifdef DBG_PRINT
		printf("Here: %d %d: onepeak %d\n",pd,td,one_peak) ; fflush(stdout) ;
#endif
			
//		if(peaks_cou<=1) {	// not that it could have been 0 in some patho cases
		if(one_peak) {	// not that it could have been 0 in some patho cases
			double f_charge = 0.0 ;
			double f_t_ave = 0.0 ;
			double f_p_ave = 0.0 ;
			u_short flags = blob[i].flags ;

			if(log_level>=2) LOG(TERR,"   flags again %d: %d %d",flags,pd,td) ;

			if(flags) goto done_peaks ;	// under all circumstances

			for(int p=0;p<pd;p++) {
				int pad = blob[i].p1+p ;
				double gain = rp_gain[sector1-1][row][pad].gain ;
				double t0 = rp_gain[sector1-1][row][pad].t0 ;
				int i_charge = 0 ;
				int i_t_ave = 0 ;

				if(log_level>=2) LOG(TERR,"   gain %d %d %d %f",sector1,row,pad,gain) ;
#ifdef DBG_PRINT
				printf("... gain %d %d %d = %f\n",sector1,row,pad,gain) ;
				fflush(stdout) ;
#endif
				for(int t=0;t<td;t++) {
					int adc = store[p][t] ;

					i_charge += adc ;
					i_t_ave += t * adc ;
				}

				if(i_charge==0) continue ;

				double corr_charge = (double)i_charge * gain ;

				f_charge += corr_charge ;
				f_t_ave += i_t_ave * gain + t0 * corr_charge ;
				f_p_ave += p * corr_charge ;

			}
			
			if(log_level>=2) LOG(TERR,"      here %f",f_charge) ;

			if(f_charge<0.1) goto done_peaks ;


			if(online==0) {	// craft stuff for the simulation
				int (*tr)[512] ;
				tr = (int (*)[512]) store_track_id ;

				sim_max_adc = 0 ;
				int max_p = 0 ;
				int max_t = 0 ;

				for(int p=0;p<pd;p++) {
					for(int t=0;t<td;t++) {
						if(store[p][t] > sim_max_adc) {
							sim_max_adc = store[p][t] ;
							max_p = p ;
							max_t = t ;
						}
					}
				}

				sim_track_id = tr[max_p][max_t] ;
				
				int sim_all = 0 ;
				sim_quality = 0 ;
				for(int p=0;p<pd;p++) {
					for(int t=0;t<td;t++) {
						if(tr[p][t]==sim_track_id) {
							sim_quality++ ;
						}
						if(store[p][t]) sim_all++ ;
					}
				}

				if(sim_all != 0) {
					float yada = (float)sim_quality/(float)sim_all ;
					yada = (100.0 * yada) + 0.5 ;
					sim_quality = (int) yada ;
				}

			}


			f_t_ave /= f_charge ;
			f_p_ave /= f_charge ;

			f_p_ave += blob[i].p1 ;
			f_t_ave += blob[i].t1 ;

#ifdef DBG_PRINT			
//			printf("CLD2 row %2d, p_ave %.3f %d %d, t_ave %.3f %d %d, charge %.1f, flags 0x%02X\n",
//			       row, f_p_ave,blob[i].p1,blob[i].p2,
//			       f_t_ave,blob[i].t1,blob[i].t2,
//			       f_charge,blob[i].flags) ;
#endif
			// and now packing for output
			u_int time_c = (u_int)(f_t_ave*64.0+0.5) ;
			u_int pad_c = (u_int)(f_p_ave*64.0+0.5) ;
			u_int cha = (u_int)(f_charge+0.5) ;

			if(cha > 0x7FFF) cha = 0x8000 | (cha/1024) ;


			u_int tmp_fl ;

			int p_lo = (pad_c/64) - blob[i].p1 ;
			int p_hi = blob[i].p2 - (pad_c/64) ;

			if(p_lo<0) p_lo = 0 ;
			if(p_hi<0) p_hi = 0 ;
			if(p_lo>7) p_lo = 7 ;
			if(p_hi>7) p_hi = 7 ;

			tmp_fl = (p_lo<<8)|(p_hi<<11) ;

			int t_lo = (time_c/64) - blob[i].t1 ;
			int t_hi = blob[i].t2 - (time_c/64) ;


			if(t_lo<0) t_lo = 0 ;
			if(t_hi<0) t_hi = 0 ;
			if(t_lo>7) t_lo = 15 ;
			if(t_hi>7) t_hi = 15 ;

			tmp_fl |= (t_hi<<4)|t_lo ;

			if(flags & FCF_MERGED) pad_c |= 0x8000 ;
			if(flags & FCF_DEAD_EDGE) pad_c |= 0x4000 ;

			if(flags & FCF_ONEPAD) time_c |= 0x8000 ;

			if(flags & FCF_ROW_EDGE) tmp_fl |= 0x8000 ;
			if(flags & FCF_BROKEN_EDGE) tmp_fl |= 0x4000 ;


			*s2_dta++ = (time_c<<16)|pad_c ;
			*s2_dta++ = (cha<<16)|tmp_fl ;

			if(online==0) {	//simulation!
				*s2_dta++ = sim_quality<<16 ;	// quality
				*s2_dta++ = (blob[i].area<<16)|sim_max_adc ;	// pixels|max_adc
				*s2_dta++ = sim_track_id ;	// track id
			}


#ifdef DBG_PRINT

			// and decode...

			double ppp = (double)(pad_c & 0x3FFF)/64.0 ;
			double ttt = (double)(time_c & 0x7FFF)/64.0 ;

			printf("CLD3 row %2d, p_ave %f %d %d, t_ave %f %d %d, charge %d, flags 0x%02X\n",
			       row, ppp,blob[i].p1,blob[i].p2,
			       ttt,blob[i].t1,blob[i].t2,
			       cha,blob[i].flags) ;
			fflush(stdout) ;
#endif

			if(log_level>=2) {
				double ppp = (double)(pad_c & 0x3FFF)/64.0 ;
				double ttt = (double)(time_c & 0x7FFF)/64.0 ;

				LOG(TERR,"CLD3 row %2d, p_ave %f %d %d, t_ave %f %d %d, charge %d, flags 0x%02X",
			       row, ppp,blob[i].p1,blob[i].p2,
			       ttt,blob[i].t1,blob[i].t2,
			       cha,flags) ;

			}

		}
		else {
			for(int pk=0;pk<peaks_cou;pk++) {
				double f_charge = 0.0 ;
				double f_t_ave = 0.0 ;
				double f_p_ave = 0.0 ;
				u_short flags = blob[i].flags ;

				flags |= FCF_MERGED ;

				int ip1, ip2 ;
				int it1, it2 ;

				ip1 = peaks[pk].p - 1 ;
				if(ip1<0) ip1 = 0 ;
				ip2 = peaks[pk].p + 1;
				if(ip2>=pd) ip2 = pd-1 ;

				it1 = peaks[pk].t - 2 ;
				if(it1<0) it1 = 0 ;
				it2 = peaks[pk].t + 2;
				if(it2>=td) it2 = td-1 ;

				for(int p=ip1;p<=ip2;p++) {
					int pad = blob[i].p1 + p ;

					double gain = rp_gain[sector1-1][row][pad].gain ;
					double t0 = rp_gain[sector1-1][row][pad].t0 ;

					
					u_int i_charge = 0 ;
					u_int i_t_ave = 0 ;

					for(int t=it1;t<=it2;t++) {
						int adc = store[p][t] ;

						i_charge += adc ;
						i_t_ave += t*adc ;
					}

					if(i_charge==0) continue ;

					double corr_charge = (double)i_charge * gain ;
					
					f_charge += corr_charge ;
					f_t_ave += i_t_ave * gain + t0 * corr_charge ;
					f_p_ave += p * corr_charge ;
				}

				if(f_charge<0.1) continue ;




				f_t_ave /= f_charge ;
				f_p_ave /= f_charge ;

				f_p_ave += blob[i].p1 ;
				f_t_ave += blob[i].t1 ;

				int p_lo = blob[i].p1 + peaks[pk].p-1 ;
				int p_hi = blob[i].p1 + peaks[pk].p+1 ;

				int t_lo = blob[i].t1 + peaks[pk].t-2 ;
				int t_hi = blob[i].t1 + peaks[pk].t+2 ;


#ifdef DBG_PRINT
//				printf("CLD2 row %2d, p_ave %.3f %d %d, t_ave %.3f %d %d, charge %.1f, flags 0x%02X\n",
//					row, f_p_ave,p_lo,p_hi,
//					f_t_ave,t_lo,t_hi,
//					f_charge,flags) ;
#endif

				// and now packing for output
				u_int time_c = (u_int)(f_t_ave*64.0+0.5) ;
				u_int pad_c = (u_int)(f_p_ave*64.0+0.5) ;
				u_int cha = (u_int)(f_charge+0.5) ;

				if(cha > 0x7FFF) cha = 0x8000 | (cha/1024) ;

				u_int tmp_fl ;

				p_lo = 1 ;
				p_hi = 1 ;

				if(p_lo<0) p_lo = 0 ;
				if(p_hi<0) p_hi = 0 ;
				if(p_lo>7) p_lo = 7 ;
				if(p_hi>7) p_hi = 7 ;

				tmp_fl = (p_lo<<8)|(p_hi<<11) ;

				t_lo = 2 ;
				t_hi = 2 ;


				if(t_lo<0) t_lo = 0 ;
				if(t_hi<0) t_hi = 0 ;
				if(t_lo>7) t_lo = 15 ;
				if(t_hi>7) t_hi = 15 ;

				tmp_fl |= (t_hi<<4)|t_lo ;



				if(flags & FCF_MERGED) pad_c |= 0x8000 ;
				if(flags & FCF_DEAD_EDGE) pad_c |= 0x4000 ;

				if(flags & FCF_ONEPAD) time_c |= 0x8000 ;

				if(flags & FCF_ROW_EDGE) tmp_fl |= 0x8000 ;
				if(flags & FCF_BROKEN_EDGE) tmp_fl |= 0x4000 ;


				*s2_dta++ = (time_c<<16)|pad_c ;
				*s2_dta++ = (cha<<16)|tmp_fl ;

				if(online==0) {	//simulation!
					int (*tr)[512] ;

					tr = (int (*)[512]) store_track_id ;

					sim_max_adc = 0 ;

					for(int p=ip1;p<=ip2;p++) {
						for(int t=it1;t<=it2;t++) {
							int adc = store[p][t] ;

							if(adc > sim_max_adc) {
								sim_max_adc = adc ;
								sim_track_id = tr[p][t] ;
							}
						}
					}
					
					int sim_all = 0 ;
					sim_quality = 0 ;
					for(int p=ip1;p<=ip2;p++) {
						for(int t=it1;t<=it2;t++) {
							if(tr[p][t]==sim_track_id) {
								sim_quality++ ;
							}
							if(store[p][t]) sim_all++ ;
						}
					}

					if(sim_all != 0) {
						float yada = (float)sim_quality/(float)sim_all ;
						yada = (100.0 * yada) + 0.5 ;
						sim_quality = (int) yada ;
					}

					
					*s2_dta++ = sim_quality<<16 ;	// quality
					*s2_dta++ = (15<<16)|sim_max_adc ;	// pixels(always 15)|max_adc
					*s2_dta++ = sim_track_id ;	// track id
				}

#ifdef DBG_PRINT
				double ppp = (double)(pad_c & 0x3FFF)/64.0 ;
				double ttt = (double)(time_c & 0x7FFF)/64.0 ;



				printf("CLD3 row %2d, p_ave %f %d %d, t_ave %f %d %d, charge %d, flags 0x%02X\n",
				       row, ppp,p_lo,p_hi,
				       ttt,t_lo,t_hi,
				       cha,flags) ;
#endif
				if(log_level>=2) {
					double ppp = (double)(pad_c & 0x3FFF)/64.0 ;
					double ttt = (double)(time_c & 0x7FFF)/64.0 ;


					
					LOG(TERR,"CLD3 row %2d, p_ave %f %d %d, t_ave %f %d %d, charge %d, flags 0x%02X",
					    row, ppp,peaks[pk].p,peaks[pk].p,
					    ttt,peaks[pk].t,peaks[pk].t,
					    cha,flags) ;


				}
				

			}
			

		}

		done_peaks:;
#ifdef DBG_PRINT
		printf("ROW %2d: done_peaks: good clusters %d\n",row,blob_good) ; fflush(stdout) ;
#endif

		
	}

#ifdef DBG_PRINT
	printf("ROW %2d: STAGE2: good clusters %d\n",row,blob_good) ;
	fflush(stdout) ;
#endif
	return s2_dta-s2_marker ;
	
}


// Where I create blobs with known extents...
int tpc23_base::row_stage1(int row)
{
	int got_one = 0 ;
	int p_max ;
	
	blob_cou = 1 ;
	int blob_merges = 0 ;

	p_max = rowlen[row] ;	//r=1 --> r==14
#ifdef DBG_PRINT
	printf("ROW %2d: STAGE1, rowlen %2d\n",row,p_max) ;
#endif
	for(int pad=1;pad<p_max;pad++) {	// < is on purpose!!!
		struct seq_t *seq_l = s1[row][pad].seq ;

#ifdef DBG_PRINT
		printf("   pad %d: t_hi %d\n",pad,seq_l->t_hi) ;
#endif
		if(seq_l->t_hi==-1) continue ; // no data ;

		while(seq_l->t_hi!=-1) {	
			u_int tl_hi = seq_l->t_hi ;
			u_int tl_lo = seq_l->t_lo ;
			int bl = seq_l->blob_id ;
			
			got_one++ ;	// count sequences
#ifdef DBG_PRINT
			printf(" left: pad %d: seq %d: t_lo %d, t_hi %d (bl %d)\n",pad,got_one,tl_lo,tl_hi,bl) ;
#endif
			struct seq_t *seq_r = s1[row][pad+1].seq ;

			char flags = rp_gain[sector1-1][row][pad].flags ;
			char flags_r = rp_gain[sector1-1][row][pad+1].flags ;
			
			flags |= flags_r ;

			while(seq_r->t_hi!=-1) {
				u_int tr_hi = seq_r->t_hi ;
				u_int tr_lo = seq_r->t_lo ;
				int br = seq_r->blob_id ;

				if(tr_hi>=512 || tr_lo>=512) LOG(ERR,"tr_hi %d, tr_lo %d: row %d, pad %d",
								 tr_hi,tr_lo,row,pad) ;

				//printf("tr_hi %d, tr_lo %d: row %d, pad %d\n", tr_hi,tr_lo,row,pad) ;

				int merge = 0 ;

				if(tl_lo > tr_hi) merge = 0 ;
				else if(tr_lo > tl_hi) merge = 0 ;
				else merge = 1 ;
#ifdef DBG_PRINT
				printf(" right: pad %d: seq %d: t_lo %d, t_hi %d\n",pad+1,got_one,tr_lo,tr_hi) ;
#endif
				if(merge) {
					//printf("merging: pad %d: %d[%d:%d] with %d[%d:%d]\n",pad,
					//       bl,tl_lo,tl_hi,
					//       br,tr_lo,tr_hi) ;

					if(bl==0 && br==0) {	// USUAL: both sequences don't belong to a blob yet
						seq_l->blob_id = blob_cou ;
						seq_r->blob_id = blob_cou ;
						bl = blob_cou ;

						blob[blob_cou].p1 = pad ;
						blob[blob_cou].p2 = pad+1 ;
						blob[blob_cou].flags = flags ;
						blob[blob_cou].area = (tl_hi-tl_lo+1)+(tr_hi-tr_lo+1) ;

						blob_ix[blob_cou] = blob_cou ;

						if(tl_hi>tr_hi) blob[blob_cou].t2 = tl_hi ;
						else blob[blob_cou].t2 = tr_hi ;

						if(tl_lo<tr_lo) blob[blob_cou].t1 = tl_lo ;
						else blob[blob_cou].t1 = tr_lo ;
#ifdef DBG_PRINT
						printf("  new blob %d: left %d:%d, right %d:%d \n",blob_cou,
						       tl_lo,tl_hi,tr_lo,tr_hi) ;
#endif

						blob_cou++ ;	// and a new blob is created...
					}
					else if(bl==0 && br!=0) {
						// UNUSUAL: left sequence is not assigned to a blob but right is
						// move into right
						seq_l->blob_id = br ;
						bl = br ;

						if(blob[br].p1>pad) {
							//printf("   ERROR pad %d, p1 %d\n",pad,blob[br].p1) ;
						}
						// NOT!
						//blob[br].p1 = pad ;	

						blob[br].flags |= flags ;
						blob[br].area += (tl_hi-tl_lo+1) ;

#ifdef DBG_PRINT
						printf("  WARN: %d: left into right %d: pad in blob %d, pad %d\n",row,br,blob[br].p1,pad) ;

						printf("  left %d:%d, right %d:%d, blob %d:%d\n",tl_lo,tl_hi,
						       tr_lo,tr_hi,
						       blob[br].t1,blob[br].t2) ;
#endif

						if(tl_hi>blob[br].t2) blob[br].t2 = tl_hi ;
						if(tl_lo<blob[br].t1) blob[br].t1 = tl_lo ;

					}
					else if(bl!=0 && br==0) {
						// USUAL: right sequence is not asigned -- move into left
						seq_r->blob_id = bl ;

						blob[bl].p2 = pad+1 ;
						blob[bl].flags |= flags ;
						blob[bl].area += (tr_hi-tr_lo+1) ;

						//printf("  right into left %d\n",bl) ;
#ifdef DBG_PRINT
						printf("  merge left %d: left %d:%d, right %d:%d \n",blob_cou,
						       tl_lo,tl_hi,tr_lo,tr_hi) ;
#endif

						if(tr_hi>blob[bl].t2) blob[bl].t2 = tr_hi ;
						if(tr_lo<blob[bl].t1) blob[bl].t1 = tr_lo ;

					}
					else {
						if(bl==br) {
							blob[bl].area += (tr_hi-tr_lo+1) ;
#ifdef DBG_PRINT
							printf(" WARN: %d: already %d: p2 %d, r_pad %d\n",row,bl,blob[br].p2,pad+1) ;	
							//printf(" WARN:   t1 %d vs %d, t2 %d vs %d\n",blob[bl].t1,tr_lo,blob[bl].t2,tr_hi) ;


							printf(" WARN: pad %d: left: %d:%d, right %d:%d\n",pad,
							       blob[bl].p1,blob[bl].p2,
							       blob[br].p1,blob[br].p2) ;
							
							printf(" WARN:  tb %d:%d, left: %d:%d, right %d:%d\n",tr_lo,tr_hi,
							       blob[bl].t1,blob[bl].t2,
							       blob[br].t1,blob[br].t2) ;
#endif


							blob[bl].p2 = pad+1 ;
							blob[bl].flags |= flags ;
							

							if(tr_hi>blob[bl].t2) blob[bl].t2 = tr_hi ;
							if(tr_lo<blob[bl].t1) blob[bl].t1 = tr_lo ;
							


						}
						else {
#ifdef DBG_PRINT
							printf("  WARN: %d: blob left %d, blob right %d???\n",row,bl,br) ;
							printf("  WARN: pad %d: left: %d:%d, right %d:%d\n",pad,
							       blob[bl].p1,blob[bl].p2,
							       blob[br].p1,blob[br].p2) ;
							
							printf("  WARN: tb %d:%d, left: %d:%d, right %d:%d\n",tr_lo,tr_hi,
							       blob[bl].t1,blob[bl].t2,
							       blob[br].t1,blob[br].t2) ;
#endif

							blob_merges++ ;
							
							//blob[bl].flags |= 1 ;
							//blob[br].flags |= 1 ;

							// merge into smaller index

							if(bl < br) {	
#ifdef DBG_PRINT
								printf("BLOB %d right -- killed, merged into left %d\n",br,bl) ;
#endif


								// killing BR
								blob[br].flags |= 0x80 ;

								blob_ix[br] = blob_ix[bl] ;
							}
							else {
#ifdef DBG_PRINT
								printf("BLOB %d left -- killed, merged into right %d\n",bl,br) ;
#endif
								blob[bl].flags |= 0x80 ;

								blob_ix[bl] = blob_ix[br] ;
							}


							merge = 0 ;	// chaos... don't merge or do anything...
						}

					}


				}
				else {
					//printf("merging-NOT: pad %d: [%d:%d] with [%d:%d]\n",pad,tl_lo,tl_hi,tr_lo,tr_hi) ;
				}
				
				seq_r++ ;
				if(merge==0) continue ;

			}
			// I need to sweep unasigned here
			seq_l++ ;		// move to next sequence
		}

	}



#ifdef DBG_PRINT
	printf("ROW %2d: STAGE1: %d blobs, blob_merges %d\n",row,blob_cou-1,blob_merges) ;
#endif

	if(blob_merges) {
		for(int i=0;i<blob_cou;i++) {
			int ix = blob_ix[i] ;

			if(ix==i) continue ;

			//merge i into ix
			if(blob[i].p1 < blob[ix].p1) blob[ix].p1 = blob[i].p1 ;
			if(blob[i].p2 > blob[ix].p2) blob[ix].p2 = blob[i].p2 ;
			if(blob[i].t1 < blob[ix].t1) blob[ix].t1 = blob[i].t1 ;
			if(blob[i].t2 > blob[ix].t2) blob[ix].t2 = blob[i].t2 ;

			
		}
	}

	return blob_cou ;
}


int tpc23_base::evt_stop()
{
	int words = 0 ;

	s2_dta = s2_start ;

	// here I run stage2!
	if(log_level>=1) {
		LOG(TERR,"evt_stop: S%02d: had %d last_ix, %d sequences",sector1,last_ix,sequence_cou) ;
		LOG(TERR,"  token %d, run_type %d, rows %d:%d",token,run_type,row_min,row_max) ;
	}

	if((token<=0)||(token>=4096)) {	// non-triggered events, at least for now...
		goto cleanup ;
	}


	if(run_type==1 || run_type==5 || no_cld) {	// ped, pulser
		//peds_accum() ;
		goto cleanup ;	// for now...
	}

	

	for(int row=row_min;row<=row_max;row++) {
		u_int *row_store = s2_dta ;
		int wds ;

		if((s2_dta-s2_start)>(s2_max_words-1000)) {
			LOG(ERR,"T %d: row %d: lots of CLD words %d vs %d, sequences %d -- skipping the rest",token,row,
			    s2_dta-s2_start, s2_max_words,
			    sequence_cou) ;

			break ;
		}

		// slighly different formats: let's keep the compatibility with associated reader
		if(rts_id==ITPC_ID) {
			s2_dta += 3 ;	// skip 3 words of row header
		}
		else {
			s2_dta += 2 ;
		}

		// where I form blobs
		row_stage1(row) ;

		// where I loop over blobs, extract data, smooth
		// and dump_out
		wds =  row_stage2(row) ;


		if(log_level>=2) LOG(TERR,"row %d: words %d",row,wds) ;

		if(wds) {
			int words_per_cluster ;

			if(online) words_per_cluster = 2 ;
			else words_per_cluster = 5 ;

			if(rts_id==ITPC_ID) {	// compatibility with the Reader
				row_store[0] = (words_per_cluster<<16)|row ;	// words_per_cluster<<16 | row
				row_store[1] = 0x20220508 ;	// version: yyyy,mm,dd
				row_store[2] = wds ;	// words
			}
			else {			// TPX is a little different
				row_store[0] = 0x20230000 | row ;
				row_store[1] = wds ;	// hits
			}
		}
		else {
			if(rts_id==ITPC_ID) {
				s2_dta -= 3 ;	// roll back
			}
			else {
				s2_dta -= 2 ;
			}
		}

	}	// loop over rows


	// CLEANUP after the event!
	cleanup: ;

//	LOG(TERR,"Cleanup") ;

	if(s2_start) {
		words = s2_dta - s2_start ;
		if(log_level>=1) LOG(TERR,"T %d: %d CLD words",token,words) ;
		s2_dta = s2_start ;	// return back
		s2_words = words  ;
	}
	else {
		if(s2_dta) {
			LOG(ERR,"WTF: T %d: s2_dta???",token) ;
		}
		s2_words = 0 ;
	}

	for(int row=row_min;row<=row_max;row++) {
		int p_max = rowlen[row] ;
		for(int pad=1;pad<=p_max;pad++) {
			s1[row][pad].seq->t_hi = -1 ; ;
		}
	}

	return words ;	// will return words of
}



int tpc23_base::evt_start()
{
	if(online==0) {
		sim_evt_start(1) ;
		return 0 ;
	}

	err = 0 ;
	sequence_cou = 0 ;
	last_ix = 0 ;	// important!

	evt++ ;
	evt_trgd++ ;

//	memset(&s1,0xFF,sizeof(s1)) ;

	return 0 ;
}

// Called at run-start
int tpc23_base::run_start()
{
	LOG(NOTE,"%d: run_start: detector %d",id,rts_id) ;

	if(s1_dta==0) {
		if(rts_id==ITPC_ID) {	// ITPC
			s1_bytes = (ROW_MAX*PAD_MAX)*512*2 ;

		}
		else {		// TPX
			s1_bytes = (ROW_MAX*PAD_MAX)*512*2 ;
		}

		LOG(INFO,"%d: allocing %d s1_bytes, s1_t %d, blobs %d",id,s1_bytes,sizeof(s1),sizeof(blob)) ;

		s1_dta = (u_short *) malloc(s1_bytes) ;
	}

	// must make sure I am clearing the various counters, pointers, etc
//	memset(&s1,0,sizeof(s1)) ;
	memset(&s1,0xFF,sizeof(s1)) ;

	evt = 0 ;
	evt_trgd = 0 ;

	
	return 0 ;

}


// Called at run-stop: generally dumps statistics
int tpc23_base::run_stop()
{
	LOG(NOTE,"%d: run_stop: %d/%d events",id,evt_trgd,evt) ;

	return 0 ;
}


tpc23_base::tpc23_base()
{
	LOG(NOTE,"%s",__PRETTY_FUNCTION__) ;

	online = 0 ;	// assume offline!

	s1_dta = 0 ;
	s1_track_id = 0 ;
	s1_bytes = 0 ;

	s2_start = 0 ;
	s2_dta = 0 ;
	s2_words = 0 ;

	no_cld = 0 ;

//	rp_gain_tpx = 0 ;
//	rp_gain_itpc = 0 ;

	rp_gain = 0 ;


	
	for(int f=0;f<SIM_FIFOS;f++) {
		for(int r=0;r<6;r++) {
			sim_dta[f].rb[r].mem = 0 ;
		}
	}

	log_level = 0 ;

	id = 0 ;

	run_type = 3 ;	// physics default

	// for sanity
	fmt = 23 ;	// new data
	rts_id = ITPC_ID ;	// TPX
	sector1 = 1 ;
	rdo1 = 1 ;
	subdet_id = 1;

	data_c = 0 ;

	token = 1 ;	// for ease of simulation
}

tpc23_base::~tpc23_base()
{
//	LOG(TERR,"%s",__PRETTY_FUNCTION__) ;

	if(s1_dta) free(s1_dta) ;
	if(s1_track_id) free(s1_track_id) ;
	if(s2_start) free(s2_start) ;
	if(store_track_id) free(store_track_id) ;

//	LOG(TERR,"des: %p %p",s1_dta,s2_start) ;

	for(int f=0;f<SIM_FIFOS;f++) {
		for(int r=0;r<6;r++) {
			if(sim_dta[f].rb[r].mem) {
				//LOG(TERR,"des: %d %d",f,r) ;
				free(sim_dta[f].rb[r].mem) ;
			}
		}
	}

	//LOG(TERR,"des: done") ;
}


int tpc23_base::rdo_scan(char *mem, int words)
{
	LOG(ERR,"%s: can't be",__PRETTY_FUNCTION__) ;

	return -1 ;
}

int tpc23_base::from22to23(char *dta, int words)	// rewrite the old FY22 raw data foramt to FY23
{
	LOG(ERR,"%s: can't be",__PRETTY_FUNCTION__) ;

	return words ;

}

// statics

//tpc23_base::row_pad_t tpc23_base::rp_gain[24][ROW_MAX+1][PAD_MAX+1] ;	// max for both dets

int tpc23_base::gains_from_cache(const char *fname)
{
	int ret = 0 ;

//	LOG(TERR,"%s [%s;%p]",__PRETTY_FUNCTION__,fname,rp_gain) ;

	// set defaults, again
	for(int s=0;s<24;s++) {
	for(int r=0;r<=ROW_MAX;r++) {
	for(int p=0;p<=PAD_MAX;p++) {
		rp_gain[s][r][p].gain = 1.0 ;
		rp_gain[s][r][p].t0 = 0.0 ;
		rp_gain[s][r][p].flags = 0 ;

	}}}

	for(int s=0;s<24;s++) {
	for(int r=0;r<6;r++) {
		bad_fee_cou[s][r] = 0 ;
	}}

	

	if(strcasecmp(fname,"none")==0) {
		LOG(WARN,"Requesting no gain correction") ;
		return 0 ;
	}

	// load gains from cache
	if(fname==0) {
		if(rts_id==ITPC_ID) fname = "/RTS/conf/itpc/itpc_gains.txt" ;
		else fname = "/RTS/conf/tpx/tpx_gains.txt" ;
	}

	FILE *f = fopen(fname,"r") ;
	if(f) {
		// load stuff here....
		LOG(INFO,"gains_from_cache: opened %s",fname) ;

		
		while(!feof(f)) {
			int sec,rdo,port,ch,row,pad ;
			float g, t ;
			char buff[256] ;
			int ret ;

			if(fgets(buff,sizeof(buff),f)==0) continue ;

			if(buff[0]=='#') continue ;

			if(strlen(buff)<1) continue ;


			if(rts_id==ITPC_ID) {
				ret = sscanf(buff,"%d %d %d %d %d %d %f %f",&sec,&rdo,&port,&ch,&row,&pad,&g,&t) ;
				if(ret != 8) continue ;

				if(ch<0) {	//kill FEE == NOT DONE YET!!!
					LOG(WARN,"S%02d:%d FEE #%d -- killed",sec,rdo,port) ;

					bad_fee[sec-1][rdo-1][bad_fee_cou[sec-1][rdo-1]] = port ;
					bad_fee_cou[sec-1][rdo-1]++ ;
					continue ;
				}

			}
			else {
				ret = sscanf(buff,"%d %d %d %f %f",&sec,&row,&pad,&g,&t) ;
				if(ret != 5) continue ;
				
				if(sec<0) {	// kill FEE
					sec *= -1 ;

					
					LOG(WARN,"S%02d:%d FEE #%d  -- killed",sec,row,pad) ;

					bad_fee[sec-1][row-1][bad_fee_cou[sec-1][row-1]] = pad ;
					bad_fee_cou[sec-1][row-1]++ ;

					continue ;
				}

			}

#ifdef DBG_PRINT
			//printf("gain %d: row %d, row_max %d\n",rts_id,row,row_max) ;
#endif

			if(row>row_max) continue ;

#ifdef DBG_PRINT
			//printf("gain %d %d %d = %f %f\n",sec,row,pad,g,t) ;
#endif 
			rp_gain[sec-1][row][pad].gain = g ;
			rp_gain[sec-1][row][pad].t0 = t ;
		
			if(g<0.01) {
				int p1 = pad - 1 ;
				int p2 = pad + 1 ;

				if(p1<1) p1 = 1 ;
				if(p2>rowlen[row]) p2 = rowlen[row] ;

				rp_gain[sec-1][row][pad].flags |= FCF_DEAD_EDGE ;
				rp_gain[sec-1][row][p1].flags |= FCF_DEAD_EDGE ;
				rp_gain[sec-1][row][p2].flags |= FCF_DEAD_EDGE ;

			}

		}

		fclose(f) ;
	}
	else {
		ret = -1 ;
		LOG(ERR,"gains_from_cache: %s [%s]",fname,strerror(errno)) ;
	}



	// I need to put RDO edges in case of masked RDOs: iTPC

	// I need to put pad edges into flags: depends on detector
	for(int s=0;s<24;s++) {
	for(int r=1;r<=row_max;r++) {



// SHOULD BE ENABLED
#if 1

		int p_max ;

		rp_gain[s][r][1].flags |= FCF_ROW_EDGE ;	// row edge

		if(rts_id==TPX_ID) {
			p_max = rowlen[r] ; // tpc_rowlen[r] ;	//1-->14
		}
		else {
			p_max = rowlen[r] ; // itpc_rowlen[r] ;
		}

		
		rp_gain[s][r][p_max].flags |= FCF_ROW_EDGE ;

//		if(s==0) {
//			printf(".... gains: row %d, p_max %d\n",r,p_max) ;
//		}
#endif
	}
	}



	return ret ;
}


tpc23_base::sim_dta_t tpc23_base::sim_dta[SIM_FIFOS] ;

int tpc23_base::load_replay(const char *fname, int sec_soft)
{
	daqReader *rdr = 0 ;
	int good = 0 ;
	int s_start ;
	int s_stop ;
	const char *det ;
	int fix = 0 ;
	int offset = 0 ;
	daq_tpx *daq_tpx = 0 ;
	daq_itpc *daq_itpc = 0 ;

	if(rts_id==ITPC_ID) {
		s_start = sec_soft ;
		s_stop = sec_soft ;
		det = "itpc" ;

		daq_itpc = new class daq_itpc ;

	}
	else {
		det = "tpx" ;

		if(sec_soft<=24) {
			s_start = sec_soft ;
			s_stop = sec_soft ;
		}
		else {
			s_start = (sec_soft-25)*2+1 ;
			s_stop = s_start + 1 ;
		}

		daq_tpx = new class daq_tpx ;
	}


	LOG(INFO,"Loading replay data from %s for sector %d[%d:%d], offset %d",fname,sec_soft,s_start,s_stop,offset) ;


	rdr = new daqReader((char *)fname) ;


	for(;;) {

	if(rdr->get(0,EVP_TYPE_ANY)) ;
	else break ;

	int got_one = 0 ;

	for(int s=s_start;s<=s_stop;s++) {
		daq_dta *dd = rdr->det(det)->get("raw",s) ;

		while(dd && dd->iterate()) {
			got_one = 1 ;
			rdo1 = dd->rdo ;
			sector1 = dd->sec ;

			int rix ;

			if(rts_id==ITPC_ID) {
				rix = rdo1 - 1 ;
			}
			else {
				
				if(sec_soft<=24) {
					rix = rdo1 - 3 ;	// 3 goes to 0
				}
				else {
					if(rdo1<5) continue ;

					if(s==s_start) rix = rdo1 - 5 ;	
					else rix = rdo1 - 5 + 2 ;
				}
			}

			int bytes = dd->ncontent ;
			char *c_addr = (char *) dd->Void ;

			
			LOG(TERR,"FIFO %d, RB %d, SR %d:%d: bytes %d",fix,rix,dd->sec,dd->rdo,bytes) ;

			int words = from22to23(c_addr,bytes/4) ;	// format change
			int n_bytes = words * 4 ;

			//LOG(TERR,"   alloced %d bytes",n_bytes) ;

			sim_dta[fix].rb[rix].bytes = n_bytes ;
			sim_dta[fix].rb[rix].mem = (char *) malloc(n_bytes) ;
			memcpy(sim_dta[fix].rb[rix].mem,c_addr,n_bytes) ;
			
		}
	

		
	}

	if(got_one) {
		fix++ ;
		if(fix==SIM_FIFOS) break ;
	}
	good++ ;

	

	}
	
	LOG(INFO,"Loaded %d FIFOs",fix) ;

	if(rdr) delete rdr ;
	if(daq_tpx) delete daq_tpx ;
	if(daq_itpc) delete daq_itpc ;

	return fix ;
}
