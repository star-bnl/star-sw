#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <rtsLog.h>

#include <TPC/rowlen.h>
#include <DAQ_TPX/tpxCore.h>
#include <DAQ_TPX/tpxFCF_flags.h>
#include <DAQ_TPX/tpxGain.h>

#include "tpxFCF_2D.h"

#define CHECK_SANITY

//#include <GL3/profiler.hh>
//PROFILER_DECLARE ;
#define PROFILER(x) (x)

#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)


#define DTA(i,j) (dta + (dt_2)*(i) + (j))
#define DTA_S(i,j) (dta_s + (dt_2)*(i) + (j))


static inline u_int get10(u_int *l, u_int p)
{
  u_int ret ;

  l -= 2*(p/4) ;

  switch(p&3) {
  case 0 :
    ret = (l[-1] & 0xFFC00) >> 10 ;
    break ;
  case 1 :
    ret = (l[-1] & 0x3FF) ;
    break ;
  case 2 :
    ret = (l[0] & 0xFFC00) >> 10 ;
    break ;
  case 3 :
  default:
    ret = l[0] & 0x3FF ;
    break ;
  }

  //printf("P %d, data 0x%X\n",p,ret) ;

  return ret ;
}

u_int *tpxFCF_2D_scan_to_next(tpxFCF_2D *fcf, u_int *end, u_int *start, tpx_altro_struct *a)
{
	u_int *h = end ;
	int wc, lo, hi ;
	
	a->count = 0 ;
	a->err = 0 ;
	a->row = 0 ;
	a->pad = 0 ;
	a->id = 0 ;
	a->ch = 0 ;

	lo = *h-- ;
	hi = *h-- ;

	if((lo & 0xCFF00000) || (hi & 0xCFF00000)) {
		a->err = 1 ;
		return 0 ;
	}

	
	if((hi & 0xFFFC0) != 0xAAA80) {	// typical error for random data...
		a->err = 2 ;
		return 0 ;
	}

	if((lo & 0x0F000) != 0x0A000) {
		a->err = 3 ;
		return 0 ;
	}

	// need for monitoring!
	a->id = (lo & 0xFF0) >> 4 ;
	a->ch = lo & 0xF ;


	int rrow, ppad ;

	tpx_from_altro(fcf->rdo-1,a->id,a->ch,rrow,ppad) ;

	// need for monitoring!
	a->row = rrow ;
	a->pad = ppad ;

	wc = ((hi&0x3F)<<4) | ((lo&0xF0000)>>16) ;


	if((wc > 529) || (wc < 0)) {
		a->err = 4 ;
		return 0 ;
	}



	if(wc==0) return h ;
	if(fcf->is_pad_valid(rrow,ppad)==0) return h ;


	int l10 = wc ;
	
	while(l10 % 4) l10++ ;

	int p10 = 0 ;

	switch(wc&3) {
	case 0 :
		break ;
	case 1 :
		p10 += 3 ;
		break ;
	case 2 :
		p10 += 2 ;
		break ;
	case 3 :
		p10++ ;
		break ;
	}

	*fcf->data_raw_p++ = rrow ;
	*fcf->data_raw_p++ = ppad ;

	short *remember_data_raw_p = fcf->data_raw_p ;
	
	int tb_prev = 512 ;

	while(p10 < l10) {
		int tb_cou = get10(h,p10++) ;
		int tb_last = get10(h,p10++) ;

		tb_cou -= 2 ;

		if((tb_cou > wc) || (tb_cou <= 0) || (tb_cou > 512)) {
			a->err = 10 ;
			return 0 ;
		}

		if((tb_last >= 512) || (tb_last < 0) || (tb_last >= tb_prev)) {
			a->err = 11 ;
			return 0 ;
		}

		a->count += tb_cou ;
		if((a->count >= 512)) {
			a->err = 12 ;
			return 0 ;
		}

		tb_prev = tb_last - tb_cou ;

		*fcf->data_raw_p++ = tb_cou ;
		*fcf->data_raw_p++ = -1 ;
		*fcf->data_raw_p++ = tb_last ;


		for(;tb_last>tb_prev;tb_last--) {

			u_short adc = get10(h,p10++) ;
			*fcf->data_raw_p++ = adc ;
		}
		


	}
	

	// all OK here
	if(a->count) {
		fcf->p_row_pad[rrow][ppad] = remember_data_raw_p ;
		*fcf->data_raw_p++ = 0 ;
		*fcf->data_raw_p = 0 ;
	}

	l10 /= 2 ;
	
	h -= l10 ;

	return h ;

}

int tpxFCF_2D::do_print(int row)
{
	printf("++++ Doing row %2d\n",row) ;

	for(int p=1;p<=tpx_rowlen[row];p++) {
		short *b1 = p_row_pad[row][p] ;

		if(b1==0) {
			printf("row %d, pad %d -- empty\n",row,p) ;
			continue ;
		}

		int seq_cou = 0 ;

		for(;;) {
			short tb_cou = *b1++ ;

			if(tb_cou == 0) {
				printf("row %d, pad %d -- done\n",row,p) ;
				break ;
			}

			
			seq_cou++ ;

			short ix = *b1++ ;
			short tb_hi = *b1++ ;

			printf("row %d, pad %d, tb_cou %d, ix %d, tb_hi %d\n",row,p,tb_cou,ix,tb_hi) ;

			for(int i=0;i<tb_cou;i++) {
				printf("    adc %4d\n",*b1++) ;
			}
		}

		printf("Row %2d, pad %3d: seq_cou %d\n",row,p,seq_cou) ;
	}

	return 0 ;

}

	
int tpxFCF_2D::stage_2d(u_int *buff, int max_bytes)
{
	u_int *locbuff = buff ;
	u_int *startbuff = buff ;
	

	int gprof0 = PROFILER(0) ;

	for(int row=1;row<=45;row++) {	// need row count here!!!

	int do_debug = 0 ;


	// check to see if we have this row...
	if(rdo) {
		if(gain_storage[sector-1][rdo-1].row_ix[row] < 0) continue ;
	}

//	LOG(TERR,"doing sector %d, rdo %d, row %d",sector,rdo,row) ;

	int gprof1 = PROFILER(gprof0) ;


	struct {
		u_short hi, lo ;
	} late_merge[MAX_LATE_MERGE] ;

	int late_merge_cou = 0 ;
	
	u_int *row_cache = locbuff++ ;	// reserve space
	u_int *clust_count_cache = locbuff++ ;
	u_int *outbuff = locbuff ;
	
	short blob_ix = 0 ;

	short b1_cou, t1_lo, t1_hi ;
	short b2_cou, t2_lo, t2_hi ;

	short *ix1, *ix2 ;

#ifdef DO_DBG1
	printf("Doing row %2d\n",row) ;

	for(int p=1;p<=tpx_rowlen[row];p++) {
		short *b1 = p_row_pad[row][p] ;

		if(b1==0) {
			printf("row %d, pad %d -- empty\n",row,p) ;
			continue ;
		}

		int seq_cou = 0 ;

		for(;;) {
			short tb_cou = *b1++ ;

			if(tb_cou == 0) {
				#ifdef DO_DBG1
				printf("row %d, pad %d -- done\n",row,p) ;
				#endif
				break ;
			}

			
			seq_cou++ ;

			#ifdef DO_DBG1
			short ix = *b1++ ;
			short tb_hi = *b1++ ;

			printf("row %d, pad %d, tb_cou %d, ix %d, tb_hi %d\n",row,p,tb_cou,ix,tb_hi) ;

			for(int i=0;i<tb_cou;i++) {
				printf("    adc %4d\n",*b1++) ;
			}
			#else
			b1 += tb_cou + 2 ;
			#endif
		}

		printf("Row %2d, pad %3d: seq_cou %d\n",row,p,seq_cou) ;
	}
#endif


	for(int p=1;p<tpx_rowlen[row];p++) {	// the < is on purpose!
		short *b1 = p_row_pad[row][p] ;

		if(b1==0) continue ;	// pad missing?

#ifdef CHECK_SANITY
		int rr, aa, cc ;
		if(rdo) {

			tpx_to_altro(row,p,rr,aa,cc) ;
			if(rr != rdo) {
				LOG(ERR,"Huh? rdo %d: rp %d:%d",rdo,row,p) ;
				continue ;
			}
		}


		if(get_static(row,p)->f & FCF_KILLED_PAD) {
			LOG(ERR,"Killed pad in data: sec %d, rp %d:%d; count %d",sector,row,p,*b1) ;

			continue ;
		}
#endif


		for(;;) {

			b1_cou = *b1++ ;	// count of adcs

			if(b1_cou == 0) break ; ;

			ix1 = b1++ ;
			t1_hi = *b1++ ;	// tb_hi ;
			t1_lo = t1_hi - b1_cou + 1 ;


			b1 += b1_cou  ;	// advance to next...
		

			short *b2 = p_row_pad[row][p+1] ;

			if(b2 == 0) {	// no second pad?
				goto sweep_unmerged ;
			}

#ifdef CHECK_SANITY
			if(rdo) {
				tpx_to_altro(row,p+1,rr,aa,cc) ;
				if(rr != rdo) {
					LOG(ERR,"Huh?") ;
					goto sweep_unmerged ;	// different RDO
				}
			}

			if(get_static(row,p+1)->f & FCF_KILLED_PAD) {
				LOG(ERR,"Killed pad in data: sec %d, rp %d:%d; count %d",sector,row,p+1,*b2) ;
				goto sweep_unmerged ;
			}
#endif

			for(;;) {
				b2_cou = *b2++ ;

				if(b2_cou == 0) break ;

				ix2 = b2++ ;
				t2_hi = *b2++ ;
				t2_lo = t2_hi - b2_cou + 1 ;



				b2 += b2_cou ;
				

				int merge ;

				if(t1_lo > t2_hi) merge = 0 ;
				else if(t2_lo > t1_hi) merge = 0 ;
				else merge = 1 ;

				#ifdef DO_DBG1
				printf("MMMM: %d: [%d,%d]%d vs. %d [%d,%d]%d %s\n",
				       p,t1_lo,t1_hi,*ix1,
				       p+1,t2_lo,t2_hi,*ix2,
				       merge?"-- merged":"") ;
				#endif


				if(merge) {
					if(*ix1 >= 0) {
						if(*ix2 >= 0) {
							if(*ix1 != *ix2) {

							//printf("Late merge: r:p %d:%d, merges %d, ix1 %d, ix2 %d\n",row,p,late_merge_cou,*ix1,*ix2) ;
							if(late_merge_cou >= MAX_LATE_MERGE) {
								LOG(WARN,"Too many late merges %d/%d: rp %d:%d",late_merge_cou,MAX_LATE_MERGE,row,p) ;
								do_debug = 1 ;

								#ifdef DO_DBG3
								printf("***** WARN Too many late merges\n") ;
								#endif
							}
							else {
								late_merge[late_merge_cou].lo = *ix1 ;
								late_merge[late_merge_cou].hi = *ix2 ;
								late_merge_cou++ ;
							}

							#if DO_DBG3
							printf("   late merge pad %d: %d %d\n",p,*ix1,*ix2) ;
							#endif

							}
						}
						else {
							*ix2 = *ix1 ;
						}
					}
					else {
						if(*ix2 >= 0) {
							*ix1 = *ix2 ;
						}
						else {
							*ix1 = blob_ix ;
							*ix2 = blob_ix ;
							blobs[blob_ix].cou = 0 ;
							blob_ix++ ;
						}
					}
					#ifdef DO_DBG1
					printf("   merge: %d: %d %d\n",p,*ix1,*ix2) ;
					#endif
				}

			}

			sweep_unmerged: ;

			// sweep unmerged sequences
			if(*ix1 < 0) {	// unassigned
				*ix1 = blob_ix ;
				blobs[blob_ix].cou = 0 ;
				blob_ix++ ;

				#ifdef DO_DBG1
				printf("   was unmerged: %d: %d\n",p,*ix1) ;
				#endif
			}
			

		}		

	}

	// do very last pad in padrow by hand...
	// PROBLEM for row 8!
	int p = tpx_rowlen[row] ;
	short *b1 = p_row_pad[row][p] ;

	if(b1) {
		int rr = rdo ;
#ifdef CHECK_SANITY
		if(rdo) {
			int aa, cc ;
			tpx_to_altro(row,p,rr,aa,cc) ;
		}

		if(rr != rdo) {
			LOG(ERR,"Huh?") ;
		} ;
#endif
		if(rr==rdo) {
			short b1_cou ;
			short *ix1 ;

			for(;;) {

			b1_cou = *b1++ ;	// count of adcs

			if(b1_cou == 0) break ; ;

			ix1 = b1++ ;

			if(*ix1 < 0) {
				*ix1 = blob_ix ;
				blobs[blob_ix].cou = 0 ;
				blob_ix++ ;

				#ifdef DO_DBG1
				printf("    last pad: %d: %d\n",p,*ix1) ;
				#endif
			}

			b1 += b1_cou + 1 ;

			}
		}

	}

	LOG(DBG,"Doing 1") ;
	// end of row; asign blobs
	int gprof2 = PROFILER(gprof1) ;


	// first; wrap up the late merges
#define MAX_CHAIN_COU	32
#define MAX_CHAIN_IX	32
	struct chain_t {
		short ix[MAX_CHAIN_IX] ;
		short cou ;
		short leader ;
	} chain[MAX_CHAIN_COU] ;

	int chain_cou = 0 ;

	for(int i=0;i<late_merge_cou;i++) {
		int lo = late_merge[i].lo ;
		int hi = late_merge[i].hi ;

		int got_lo = 0 ;
		int got_hi = 0 ;

		for(int j=0;j<chain_cou;j++) {

			for(int k=0;k<chain[j].cou;k++) {
				if(chain[j].ix[k] == lo) got_lo = j + 1 ;
				if(chain[j].ix[k] == hi) got_hi = j + 1;
			}

			if(got_hi || got_lo) break ;
		}

		if(got_hi && got_lo) continue ;

		if(got_hi) {
			chain[got_hi-1].ix[chain[got_hi-1].cou] = lo ;
			chain[got_hi-1].cou++ ;

			if(chain[got_hi-1].cou >= MAX_CHAIN_IX) {
				do_debug = 1 ;
				LOG(WARN,"Too many hi") ;
				goto start_chain ;
			}

		}
		else if(got_lo) {
			chain[got_lo-1].ix[chain[got_lo-1].cou] = hi ;
			chain[got_lo-1].cou++ ;

			if(chain[got_lo-1].cou >= MAX_CHAIN_IX) {
				do_debug = 1 ;
				LOG(WARN,"Too many lo") ;
				goto start_chain ;
			}


		}
		else {
			if(chain_cou >= MAX_CHAIN_COU) {
				LOG(WARN,"Too many chains") ;
				goto start_chain ;
			}
			else {
				chain[chain_cou].ix[0] = lo ;
				chain[chain_cou].ix[1] = hi ;
				chain[chain_cou].cou = 2 ;
				chain_cou++ ;
			}
		}

	}

	start_chain: ;

	// find the chain leader: probably unnecessary, can use the 1st one...
	for(int i=0;i<chain_cou;i++) {
		short min = 0x7FFF ;
		for(int j=0;j<chain[i].cou;j++) {
			if(chain[i].ix[j] < min) {	
				min = chain[i].ix[j];
			}
			chain[i].leader = min ;
		}
	}

	#ifdef DO_DBG3
	for(int i=0;i<chain_cou;i++) {
		printf("chain %d: ",i) ;
		for(int j=0;j<chain[i].cou;j++) {
			printf("%2d ",chain[i].ix[j]) ;
		}
		printf("\n") ;
	}
	#endif

	if(blob_ix >= MAX_BLOB_COUNT) {
		LOG(WARN,"Too many blobs %d/%d",blob_ix,MAX_BLOB_COUNT) ;
		do_debug = 1 ;
		blob_ix = MAX_BLOB_COUNT - 1 ;	// cut it off by hand!
	}

	LOG(DBG,"Doing 2") ;
	int gprof3 = PROFILER(gprof2) ;

	for(int pad=1;pad<=tpx_rowlen[row];pad++) {
		register short *b = p_row_pad[row][pad] ;

		if(b==0) continue ;	// pad missing?

#ifdef CHECK_SANITY
		if(rdo) {
			int rr, aa, cc ;
			tpx_to_altro(row,pad,rr,aa,cc) ;
			if(rr != rdo) {
				LOG(ERR,"Huh?") ;
				continue ;
			}
		}
#endif
		short flags = get_static(row,pad)->f & 0x0FFF ;
		flags &= ~FCF_ONEPAD ;	// need to fix the default ala tpxFCF...
		if(rdo==0) flags &= ~FCF_BROKEN_EDGE ;

		register short cou ;

		while((cou = *b)) {
//			int prof4 = PROFILER(0) ;

			short *bs = b ;	

			short ix = bs[1] ;	// get the index out

			b += cou + 3 ;		// advance pointer to next sequence
		

			// check for late merge via chains
			for(int l=0;l<chain_cou;l++) {
				for(int k=0;k<chain[l].cou;k++) {
					if(ix == chain[l].ix[k]) {

						#ifdef DO_DBG3
						printf(" pad %3d, tb_hi %3d: chain merge of %d into %d\n",pad,bs[2],ix,chain[l].leader) ;
						#endif
						ix = chain[l].leader ;
						goto chain_done ;
					}
				}
			}

			chain_done:;
			

			if((ix < 0) || (ix > blob_ix)){
				LOG(WARN,"Too many ix: r:p %d:%d: ix %d/%d",
				    row,pad,ix,blob_ix) ;
				#ifdef DO_DBG
				printf("***** WARN Too many ix: r:p %d:%d: ix %d/%d\n",
				    row,pad,ix,blob_ix) ;

				#endif
				continue ;
			}



			blob_t *bl = blobs + ix ;

			short seq_cou = bl->cou ;

			if(seq_cou >= MAX_SEQ_PER_BLOB) {	
				LOG(WARN,"Too many seqs: r:p %d:%d: ix %d: seq_cou %d/%d",row,pad,ix,seq_cou,MAX_SEQ_PER_BLOB) ;

				#ifdef DO_DBG
				printf("***** WARN Too many seqs: r:p %d:%d: ix %d: %d/%d\n",
				    row,pad,ix,seq_cou,MAX_SEQ_PER_BLOB) ;

				#endif
				continue ;
			}

			if(seq_cou ==0) {
				bl->flags = flags ;
			}
			else {
				bl->flags |= flags ;
			}

			bl->cou++ ;


			blob_seq_t *seq = bl->seq + seq_cou ;

			seq->s = bs ;
			seq->pad = pad ;


//			PROFILER(prof4) ;
		}
		
	}


	// do stage 2: extract stuff out...
	LOG(DBG,"Doing 3") ;
	int gprof4 = PROFILER(gprof3) ;


	// ************************ loop over all the blobs
	for(int ix=0;ix<blob_ix;ix++) {
		#ifdef DO_DBG3
		printf("===> BLOB %2d: row %d, %d pads, flags 0x%X\n",ix,row,blobs[ix].cou,blobs[ix].flags) ;
		#endif

		if(blobs[ix].cou == 0) continue ;	// can only be from a late merge

		short flags = blobs[ix].flags ;

		short p1 = 1000 ;
		short t1 = 1000 ;
		short p2 = 0 ;
		short t2 = 0 ;

		int prof1 = PROFILER(0) ;

		// get the extents while loooping over sequences/strips
		for(int j=0;j<blobs[ix].cou;j++) {
			short *s = blobs[ix].seq[j].s ;
			short pad = blobs[ix].seq[j].pad ;

			if(pad < p1) p1 = pad ;
			if(pad > p2) p2 = pad ;

			short tb_cou = *s++ ;

			s++ ;	// skip ix
			
			int tb_hi = (int) *s++ ;

			int tb_lo = tb_hi - tb_cou + 1 ;

#ifdef CHECK_SANITY
			// cases exist when there's just 1 pixel on timebin 0 or last
			if(tb_hi>=500 || (tb_lo > tb_hi)) {
				LOG(ERR,"tb_cou  %d, tb_hi %d, tb_lo %d",tb_cou,tb_hi,tb_lo) ;
			}
#endif
			if(tb_hi > t2) t2 = tb_hi ;
			if(tb_lo < t1) t1 = tb_lo ;

		}

		// p1,p2 & t1,t2 are the real extents of the blob

		short dp = p2 - p1 + 1 ;
		short dt = t2 - t1 + 1 ;
		short dt_2 = dt + 2 ;	// for indexing


#ifdef CHECK_SANITY
		if((t1>500)||(t2>500)||(dp<=0)||(dt<=0)) {
			LOG(ERR,"What dp %d, dt %d:  %d:%d p, %d:%d t???",dp,dt,p1,p2,t1,t2) ;
		}
#endif
		// cut off blobs where dt is very small
		if((dt <= 1) && do_cuts && !(flags & FCF_BROKEN_EDGE)) continue ;

		// ONEPAD cut but only if not before the trigger (aka tb=15, aka wire hits))
		if(dp <= 1) {
			if((t1>15) && do_cuts && !(flags & FCF_BROKEN_EDGE)) continue ;

			flags |= FCF_ONEPAD ;
		}


		int prof2 = PROFILER(prof1) ;	// 0.1 us


		// ************** copy over to local storage in a NxM matrix form
		
		// check total bytes
		u_int tot_size = (dp+2)*dt_2*sizeof(short)*2 ;
		if(tot_size > sizeof(dta)) {
			LOG(WARN,"Cluster too big: sec %d:%d, row %d, dp %d, dt %d: %d/%d-- skipping",sector,rdo,row,dp,dt,tot_size,sizeof(dta)) ;
			continue ;
		}

		// clear local storage
		memset(dta, 0, (dp+2)*dt_2*sizeof(short)*2) ;	// clear both storages in one shot

		dta_s = dta + (dp+2)*dt_2 ;	// for the filtered data


		int prof3 = PROFILER(prof2) ;	// now 0.15us (was 0.47 us)

		

		for(int j=0;j<blobs[ix].cou;j++) {
			short *s = blobs[ix].seq[j].s ;
			short p0 = blobs[ix].seq[j].pad - p1 + 1;	// start from 1


			short tb_cou = *s++ ;

			s++ ;	// skip ix
			
			int tb_hi = (int) *s++ ;

			int tb_lo = tb_hi - tb_cou + 1 ;


			int tb = tb_hi - t1 + 1 ;			// start from 1

			short *udd = DTA(p0,0) ;

			for(;tb_hi>=tb_lo;tb_hi--) {
				short adc = *s++ ;

				*(udd + tb) = adc ;
				tb-- ;

			}
		}

		// dta starts at [1,1]

		int prof4 = PROFILER(prof3) ;	// 0.17 us


		// ******************* now do the filtering (average) to get rid of the noise for peak-finding...
		for(int i=1;i<=(dp);i++) {
		for(int j=1;j<=(dt);j++) {

			short sum = 0 ;

			int iy = j - 1 ;

			for(int ii=-1;ii<=1;ii++) {
				register short *udd = DTA(i+ii,iy) ;

				for(int jj=0;jj<3;jj++) {
					sum += *udd++ ;
				}	
			}

			*DTA_S(i,j) = sum ;
		}
		}

		int prof5 = PROFILER(prof4) ;	// 0.29 0.26 (0.36 us)


		// ********************* find the count of peaks...
		int peaks_cou = 0 ;

#ifdef DO_DBG
		int pix_cou = 0 ;
#endif
		for(int i=1;i<=(dp);i++) {
		for(int j=1;j<=(dt);j++) {
			short adc ;
			short adc_peak ;

			// ignore the peak location if the original ADC is smaller
			adc_peak = *DTA(i,j) ;

#ifdef DO_DBG		
			if(adc_peak) pix_cou++ ;
#endif

			if(unlikely(adc_peak < 1)) continue ;

			adc = *DTA_S(i,j) ;

			if(unlikely(adc < 1)) continue ;	// this can cause the number of peaks to be 0...

			int iy = j - 1 ;

			for(int ii=-1;ii<=1;ii++) {				
				short *dd = DTA_S(i+ii,iy) ;

				for(int jj=0;jj<3;jj++) {
					short s_adc = *dd++ ;

					if(likely(adc < s_adc)) goto skip_calc ;
					if(unlikely(s_adc < 0)) goto skip_calc ;
				}	
			}

			*DTA_S(i,j) = -100 ;	// mark as used!
			
			peaks[peaks_cou].i = i ;
			peaks[peaks_cou].j = j ;
			peaks[peaks_cou].aux_flags = adc_peak ;
			peaks_cou++ ;

			if(unlikely(peaks_cou >= MAX_PEAKS_PER_BLOB)) goto peaks_done ;

			j += 2 ;

			skip_calc:;
			
		}
		}

		peaks_done:;

		int prof6 = PROFILER(prof5) ;	// 0.49 0.53 us

		// It is possible that I didn't find any peaks in the blob:
		//   - peak finding can miss a wide peak
		//   - the peak(s) were beyond the cuts

		// if I see no peaks, just use the maximum as the starting point
		if((peaks_cou==0)||(peaks_cou>=MAX_PEAKS_PER_BLOB)) {
			//if((dp==1) && (dt==1)) ;
//			LOG(WARN,"Peaks_cou = %d: dp %d, dt %d, flags 0x%X",peaks_cou,dp,dt,flags) ;

			#ifdef DO_DBG3
			printf("Warning: peaks_cou %d vs %d\n",peaks_cou,MAX_PEAKS_PER_BLOB) ;
			#endif
		}




		#ifdef DO_DBG

if((peaks_cou==0) || (t2==512)) {		
		printf("+++++ sec %2d, row %2d: p [%d,%d], t [%d,%d]; peaks %d\n",sector,row,p1,p2,t1,t2,peaks_cou) ;
		printf("peaks: ") ;
		for(int i=0;i<peaks_cou;i++) {
			printf("[%d,%d] ",peaks[i].i,peaks[i].j) ;	
		}
		printf("\n") ;

		printf("      ") ;
		for(int i=0;i<=(dp+1);i++) {
			printf("p%03d ",p1+i-1) ;
		}
		printf("\n") ;

		for(int j=(dt+1);j>=0;j--) {
			printf("t%03d ",t1+j-1) ;

			for(int i=0;i<=(dp+1);i++) {
				printf("%4d ",*DTA(i,j)) ;
			}
			printf("\n") ;
		}


		printf("##### p [%d,%d], t [%d,%d]; peaks %d\n",p1,p2,t1,t2,peaks_cou) ;

		printf("      ") ;
		for(int i=0;i<=(dp+1);i++) {
			printf("p%03d ",p1+i-1) ;
		}
		printf("\n") ;

		for(int j=(dt+1);j>=0;j--) {
			printf("t%03d ",t1+j-1) ;

			for(int i=0;i<=(dp+1);i++) {
				printf("%4d ",*DTA_S(i,j)) ;
			}
			printf("\n") ;
		}


		printf("\n") ;

}
		#endif


		int prof7 = 0 ;

		peaks[0].p1 = p1 ;
		peaks[0].p2 = p2 ;
		peaks[0].t1 = t1 ;
		peaks[0].t2 = t2 ;
		peaks[0].flags = flags ;

		if(peaks_cou <= 1 ) {	// single peak, full mean!

			double f_charge = 0.0 ;
			double f_t_ave = 0.0 ;
			double f_p_ave = 0.0 ;

			int pix_cou = 0 ;

			for(int i=1;i<=(dp);i++) {	// data starts on 2


				int tb = t1  ;

				int i_charge = 0 ;
				int i_t_ave = 0 ;

				short *adc_p = DTA(i,1) ;

				for(int j=1;j<=(dt);j++) {	// data starts on 2
					
					//register int adc = (int) dta[i][j] ;

					register int adc = *adc_p++ ;

					//if(adc) pix_cou++ ;

					//if(adc==0) {
					//	tb++ ;
					//	continue ;
					//}

					#ifdef DO_DBG2
					printf("pad:tb %d:%d, i:j %d:%d = %d\n",pad,tb,i,j,adc) ;
					#endif

					i_charge += adc ;
					i_t_ave += (tb++) * adc ;
				}


				if(i_charge == 0) continue ;


				int pad = p1 + i - 1 ;

				double gain = get_static(row,pad)->g ;
				double t0 = get_static(row,pad)->t0 ;

#ifdef CHECK_SANITY
				if(gain < 0.8) {
					LOG(ERR,"BAD Gain 0: sector %d, rdo %d: rp %d %d, flags 0x%X",sector,rdo,row,pad,flags) ;
				}
#endif

				double corr_charge = i_charge * gain ;

				f_charge += corr_charge ;
				f_t_ave += i_t_ave * gain + t0 * corr_charge ;
				f_p_ave += pad * corr_charge ;

			}


			peaks[0].f_t_ave = f_t_ave ;
			peaks[0].f_p_ave = f_p_ave ;
			peaks[0].f_charge = f_charge ;
			peaks[0].pix_cou = pix_cou ;


			peaks_cou = 1 ;	// force it!

			outbuff += do_dump(0,outbuff) ;

			prof7 = PROFILER(prof6) ;	// 0.19 us

		}
		else {

			do_peaks(row,peaks_cou) ;
			for(int p=0;p<peaks_cou;p++) {
				outbuff += do_dump(p,outbuff) ;
			}
	
			prof7 = PROFILER(prof6) ;	// 3.3 us

		}

		PROFILER(prof7) ;	// 0.12 us
		PROFILER(prof1) ;	// 2.0 us total per blob
	}



	u_int cl_cou  ;
	if(modes) {
		cl_cou = (outbuff - locbuff)/3 ;
	}
	else {
		cl_cou = (outbuff - locbuff)/2 ;
	}


	#ifdef DO_DBG
	//printf("Row %2d: %d hits\n",row,cl_cou) ;
	#endif

	if(cl_cou) {
		*row_cache = (FCF_2D_V_FY13 << 16) | row ;
		*clust_count_cache = cl_cou ;
		locbuff = outbuff ;
	}
	else {	// roll back
		locbuff -= 2 ;
	}


	PROFILER(gprof4) ;

	PROFILER(gprof1) ;	// start of row

	if(do_debug) do_print(row) ;

	}	// end of all rows

	PROFILER(gprof0) ;	// start of RDO

	return (u_int *)locbuff - (u_int *)startbuff ;	// return number of intes stored
}



int tpxFCF_2D::do_pad_2d(tpx_altro_struct *a, daq_sim_adc_tb *sim_adc)
{
	int row = a->row ;
	int pad = a->pad ;


	if(is_pad_valid(row,pad)==0) {
		//LOG(WARN,"Sec %d, rp %d:%d -- not valid",sector,row,pad) ;
		return 0 ;
	}

	if(a->count == 0) return 0 ;


	// NEED to sort in falling timebin!

	short *start = data_raw_p ;

	*data_raw_p++ = row ;
	*data_raw_p++ = pad ;

	p_row_pad[row][pad] = data_raw_p ;	// remember pointer

	int tb_cou = 0 ;
	short *tb_cou_p = data_raw_p ;
	int tb_last = 600 ;

	for(int i=0;i<a->count;i++) {
		int tb = a->tb[i] ;
		int aa = a->adc[i];

		if(tb >= tb_last) {
			LOG(ERR,"%d: %d %d",i,tb,aa) ;
		}

		if(tb != (tb_last-1)) {	// new sequence

			*tb_cou_p = tb_cou ;

			tb_cou_p = data_raw_p++ ;
			*data_raw_p++ = -1 ;
			*data_raw_p++ = tb ;

			tb_cou = 1 ;
			
			*data_raw_p++ = aa ;
		}
		else {

			tb_cou++ ;
			*data_raw_p++ = aa ;
		}
		
		tb_last = tb ;

	}


	*tb_cou_p = tb_cou ;


	*data_raw_p++ = 0 ;
	*data_raw_p = 0 ;

	int s_cou = data_raw_p - start ;

/*
	for(int i=0;i<s_cou;i++) {
		LOG(TERR,"%d: %d:%d:   0x%04X (%d dec)",i,row,pad,start[i],start[i]) ;

	}
*/

	return s_cou ;

#if 0
	short adc[512] ;
	memset(adc,0,sizeof(adc)) ;
	
//	LOG(TERR,"do_pad_2d: row %d, pad %d, cou %d",row,pad,a->count) ;

	for(int i=0;i<a->count;i++) {
		int tb = a->tb[i] ;
		int aa = a->adc[i] ;

		adc[tb] = aa ;
	}

	short *start = data_raw_p ;

	*data_raw_p++ = row ;
	*data_raw_p++ = pad ;

	p_row_pad[row][pad] = data_raw_p ;	// remember pointer

	int t_hi = -1 ;
	int tb_cou = 0 ;
	short *tb_cou_p = data_raw_p ;

	for(int i=511;i>=0;i--) {

		
		if(adc[i]) {
			if(t_hi < 0) {	// start new

				t_hi = i ;

				tb_cou_p = data_raw_p++ ;
				*data_raw_p++ = -1 ;
				*data_raw_p++ = t_hi ;


				tb_cou = 1 ;

				*data_raw_p++ = adc[i] ;
			}
			else {
				tb_cou++ ;
				*data_raw_p++ = adc[i] ;
			}
		}
		else {
			if(t_hi < 0) ;
			else {
				*tb_cou_p = tb_cou ;

				t_hi = -1 ;
			}
		}
	}
			

	if(t_hi < 0) ;
	else {
		*tb_cou_p = tb_cou ;
	}

	*data_raw_p++ = 0 ;
	*data_raw_p = 0 ;

	int s_cou = data_raw_p - start ;

/*
	for(int i=0;i<s_cou;i++) {
		LOG(TERR,"%d: %d:%d:   0x%04X (%d dec)",i,row,pad,start[i],start[i]) ;

	}
*/

	return s_cou ;
#endif

#if 0
	short tmp_store[512+512], a_tmp[512] ;
	
	short *tmp_buff = tmp_store ;
	
	short *start_raw_p = data_raw_p ;


	int any = 0 ;

	short t_hi, t_lo, t_prev ;

	t_prev = 0 ;
	t_hi = -1 ;

	for(int i=511;i>=0;i--) {
		if(adc[i]) {
			if(t_hi == -1) t_hi = i ;

			t_prev = i ;
		}
		else {
			if(t_hi > 0) {	// dump seq
				t_lo = t_prev ;

				*tmp_buff++ = (t_hi - t_lo + 1) ;
				*tmp_buff++ = t_hi ;

				for(int k=t_hi;k>=t_lo;k--) {
					*tmp_buff++ = adc[k] ;
				}

				any = 1 ;
				t_hi = -1 ;
			}
		}
	}


	#ifdef DO_DBG1
	int tmp_count = tmp_buff - tmp_store ;
	for(int i=0;i<tmp_count;i++) {
		printf("Row %d, pad %d: %d: %d\n",row,pad,i,tmp_store[i]) ;
	}
	#endif

	*data_raw_p++ = row ;
	*data_raw_p++ = pad ;

	p_row_pad[row][pad] = data_raw_p ;	// remember pointer


	if(any == 0) {
		*data_raw_p++ = 0 ;
		*data_raw_p = 0 ;
		return 3 ;	// 3 items...
	}

	short *a_buff = tmp_store ;

	short tb_prev = 512 ;
	t_hi = -1 ;

	while(a_buff < tmp_buff) {
		short tb_cou = *a_buff++ ;
		short tb_last = *a_buff++ ;

		tb_prev = tb_last - tb_cou ;

		for(;tb_last > tb_prev; tb_last--) {
			short adc = *a_buff++ ;

			a_tmp[tb_last] = adc ;


			if(adc > 0) {
				if(t_hi == -1) t_hi = tb_last ;

				t_prev = tb_last ;
			}
			else {
				if(t_hi >= 0) {
					t_lo = t_prev ;

					*data_raw_p++ = (t_hi - t_lo + 1) ;
					*data_raw_p++ = -1 ;
					*data_raw_p++ = t_hi ;

					for(int k=t_hi; k>=t_lo; k--) {
						*data_raw_p++ = a_tmp[k] ;
					}

					any = 1 ;
					t_hi = -1 ;
				}
			}

		}

		if(t_hi >= 0) {	// unfinished
			t_lo = t_prev ;

			*data_raw_p++ = (t_hi - t_lo + 1) ;
			*data_raw_p++ = -1 ;
			*data_raw_p++ = t_hi ;

			for(int k=t_hi; k>=t_lo;k--) {
				*data_raw_p++ = a_tmp[k] ;
			}

			t_hi = -1 ;

		}
	}

	*data_raw_p++ = 0 ;	// final tb_cou
	*data_raw_p = 0 ;	// next row sentinel


	int cou = data_raw_p - start_raw_p ;


	#ifdef DO_DBG1
	for(int i=0;i<cou;i++) {
		printf("row %d, pad %d: %d: %d\n",row,pad,i,start_raw_p[i]) ;
	}
	#endif

	return cou ;
#endif

}

#if 0
int tpxFCF_2D::fill(short row, short pad, short adc[512])
{
	short tmp_store[512+512], a_tmp[512] ;
	
	short *tmp_buff = tmp_store ;
	
	short *start_raw_p = data_raw_p ;


	int any = 0 ;

	short t_hi, t_lo, t_prev ;

	t_prev = 0 ;
	t_hi = -1 ;

	for(int i=511;i>=0;i--) {
		if(adc[i]) {
			if(t_hi == -1) t_hi = i ;

			t_prev = i ;
		}
		else {
			if(t_hi > 0) {	// dump seq
				t_lo = t_prev ;

				*tmp_buff++ = (t_hi - t_lo + 1) ;
				*tmp_buff++ = t_hi ;

				for(int k=t_hi;k>=t_lo;k--) {
					*tmp_buff++ = adc[k] ;
				}

				any = 1 ;
				t_hi = -1 ;
			}
		}
	}

	#ifdef DO_DBG1
	int tmp_count = tmp_buff - tmp_store ;
	for(int i=0;i<tmp_count;i++) {
		printf("Row %d, pad %d: %d: %d\n",row,pad,i,tmp_store[i]) ;
	}
	#endif

	*data_raw_p++ = row ;
	*data_raw_p++ = pad ;


	p_row_pad[row][pad] = data_raw_p ;	// remember pointer



	if(any == 0) {
		*data_raw_p++ = 0 ;
		*data_raw_p = 0 ;
		return 3 ;	// 3 items...
	}

	short *a_buff = tmp_store ;

	short tb_prev = 512 ;
	t_hi = -1 ;

	while(a_buff < tmp_buff) {
		short tb_cou = *a_buff++ ;
		short tb_last = *a_buff++ ;

		tb_prev = tb_last - tb_cou ;

		for(;tb_last > tb_prev; tb_last--) {
			short adc = *a_buff++ ;

			a_tmp[tb_last] = adc ;


			if(adc > 2) {
				if(t_hi == -1) t_hi = tb_last ;

				t_prev = tb_last ;
			}
			else {
				if(t_hi >= 0) {
					t_lo = t_prev ;

					*data_raw_p++ = (t_hi - t_lo + 1) ;
					*data_raw_p++ = -1 ;
					*data_raw_p++ = t_hi ;

					for(int k=t_hi; k>=t_lo; k--) {
						*data_raw_p++ = a_tmp[k] ;
					}

					any = 1 ;
					t_hi = -1 ;
				}
			}

		}

		if(t_hi >= 0) {	// unfinished
			t_lo = t_prev ;

			*data_raw_p++ = (t_hi - t_lo + 1) ;
			*data_raw_p++ = -1 ;
			*data_raw_p++ = t_hi ;

			for(int k=t_hi; k>=t_lo;k--) {
				*data_raw_p++ = a_tmp[k] ;
			}

			t_hi = -1 ;

		}
	}

	*data_raw_p++ = 0 ;	// final tb_cou
	*data_raw_p = 0 ;	// next row sentinel


	int cou = data_raw_p - start_raw_p ;


	#ifdef DO_DBG1
	for(int i=0;i<cou;i++) {
		printf("row %d, pad %d: %d: %d\n",row,pad,i,start_raw_p[i]) ;
	}
	#endif

	return cou ;
}

#endif

#if 0
void tpxFCF_2D::output(void *buff, int count)
{
	int cou = 0 ;
	int row = 0 ;
	fcf2d_res_t *res = (fcf2d_res_t *) buff ;

	for(int i=0;i<count;i++) {
		if(res[i].pad < 0) {
			row = (int) - res[i].pad ;
			//printf("===> Row %2d, clusters: %d\n",row,res[i].ix) ;
			continue ;
		}



		if(row==1) {
		if((res[i].flags & FCF_DEAD_EDGE) || (res[i].flags & FCF_ROW_EDGE)) continue ;

//		if(res[i].flags != 1) {
		cou++ ;
		printf("FCF2D: row %2d: %3d: pad %f[%d], tb %f[%d], cha %d, fla %d, pixs %d, ix %d\n",
		       row,
		       cou,
		       res[i].pad, res[i].dp,
		       res[i].tb, res[i].dt, 
		       res[i].charge, res[i].flags,res[i].pix_cou,
		       res[i].ix) ;
		}
	} ;

}
#endif

tpxFCF_2D::tpxFCF_2D()
{


	data_raw = (short *) valloc(2*MAX_PADS_PER_RDO*512) ;	// pad_num X 512tb X 2bytes (short)
}

int tpxFCF_2D::do_dump(int ix, u_int *obuff)
{
	int ret = 0 ;	// how many ints did we use...



	double f_p_ave, f_t_ave ;


	short flags = peaks[ix].flags ;
	short p1 = peaks[ix].p1 ;
	short p2 = peaks[ix].p2 ;
	short t1 = peaks[ix].t1 ;
	short t2 = peaks[ix].t2 ;

	if(peaks[ix].f_charge) {

		f_p_ave = peaks[ix].f_p_ave / peaks[ix].f_charge ;
		f_t_ave = peaks[ix].f_t_ave / peaks[ix].f_charge ;
	}
	else {
		f_t_ave = 0 ;
		f_p_ave = 0 ;
		LOG(WARN,"no charge: ix %d: %d-%d, %d-%d: fla 0x%X: ij %d-%d, pixs %d, aux 0x%X",
		    ix,
		    p1,p2,t1,t2,
		    flags,
		    peaks[ix].i,peaks[ix].j,
		    peaks[ix].pix_cou,
		    peaks[ix].aux_flags) ;
		return 0 ;
	}

	u_int time_c = (u_int)(f_t_ave * 64.0 + 0.5) ;
	u_int pad_c = (u_int)(f_p_ave * 64.0 + 0.5) ;
	u_int cha = (u_int) (peaks[ix].f_charge + 0.5) ;



	u_int tmp_fl ;

	// get extents
	u_int tmp_p = pad_c / 64 ;

	p1 = tmp_p - p1 ;
	p2 = p2 - tmp_p ;

	if(p1 > 7) p1 = 7 ;
	if(p2 > 7) p2 = 7 ;


	if(p1 < 0) p1 = 0 ;
	if(p2 < 0) p2 = 0 ;

	tmp_fl = (p1 << 8) | (p2 << 11) ;

	tmp_p = time_c / 64 ;
			
	t1 = tmp_p - t1 ;
	t2 = t2 - tmp_p ;

			
	if(t1 > 15) t1 = 15 ;
	if(t2 > 15) t2 = 15 ;

	if(t1 < 0) t1 = 0 ;
	if(t2 < 0) t2 = 0 ;

	tmp_fl |= (t2 << 4) | t1 ;

	if(flags & FCF_ONEPAD) time_c |= 0x8000 ;
			
	if(flags & FCF_MERGED) pad_c |= 0x8000 ;
	if(flags & FCF_DEAD_EDGE) pad_c |= 0x4000 ;

	if(flags & FCF_ROW_EDGE) tmp_fl |= 0x8000 ;
	if(flags & FCF_BROKEN_EDGE) tmp_fl |= 0x4000 ;

	if(cha > 0x7FFF) cha = 0x8000 | (cha/1024) ;

	*obuff++ = (time_c << 16) | pad_c ;
	*obuff++ = (cha << 16) | tmp_fl ;

	ret = 2 ;
//	LOG(TERR,"Modes %d",modes) ;
	if(modes) {
		int quality = 1 ;
		int track_id = 2 ;

		*obuff++ = (quality << 16) | track_id ;
		ret++ ;
	}

	return ret ;
}

int tpxFCF_2D::do_peaks(int row, int peaks_cou)
{
	short p1 = peaks[0].p1 ;
	short p2 = peaks[0].p2 ;
	short t1 = peaks[0].t1 ;
	short t2 = peaks[0].t2 ;

	short dt, dt_2 ;
	short dp ;


#ifdef CHECK_SANITY
	if((p1>p2)||(t1>t2)||(p1<1)||(t1<0)) {
		LOG(ERR,"What??? %d %d %d %d",p1,p2,t1,t2) ;
	}
#endif

	dp = p2 - p1 + 1 ;
	dt = t2 - t1 + 1 ;
	dt_2 = dt + 2 ;	// MUST be called dt_2 due to the DTA() define!!!


	for(int p=0;p<peaks_cou;p++) {
		peaks[p].f_charge = 0.0 ;
		peaks[p].f_t_ave = 0.0 ;
		peaks[p].f_p_ave = 0.0 ;
		peaks[p].pix_cou = 0 ;
		peaks[p].t1 = 10000 ;
		peaks[p].p1 = 10000 ;
		peaks[p].t2 = 0 ;
		peaks[p].p2 = 0 ;
		peaks[p].flags = 0 ;

		
	}

	for(int i=1;i<=(dp);i++) {
		int pad = p1 + i - 1 ;

		double gain = get_static(row,pad)->g ;
		double t0 = get_static(row,pad)->t0 ;
	
		short flags = get_static(row,pad)->f & 0xFFF ;
		if(rdo==0) flags &= ~FCF_BROKEN_EDGE ;

#ifdef CHECK_SANITY
		if(gain < 0.8) {
			LOG(ERR,"What: lo gain %f %f, flags 0x%X: sec %d: rp %d:%d; i %d, dp %d",gain,t0,flags,sector,row,pad,i,dp) ;
		}
#endif
		if(peaks_cou > 1) flags |= FCF_MERGED ;

		for(int j=1;j<=(dt);j++) {
		
			short adc = *DTA(i,j) ;

			if(adc==0) continue ;

			int min_diff = 10000000 ;
			int min_p = -1 ;

			for(int p=0;p<peaks_cou;p++) {
			
				int dx = i - peaks[p].i ;
				int dy = j - peaks[p].j ;

				int diff = dx*dx + dy*dy ;

				if(diff < min_diff) {
					min_diff = diff ;
					min_p = p ;
				}
			}

			double corr_charge = adc * gain ;

			if(min_p<0) {
				LOG(ERR,"What: %d: rp %d:%d",min_p,row,pad) ;
				continue ;
			}

			peaks[min_p].f_charge += corr_charge ;
			peaks[min_p].f_t_ave += corr_charge*(t0+(double)(j+t1-1)) ;
			peaks[min_p].f_p_ave += corr_charge*(pad) ;
			peaks[min_p].pix_cou++ ;


			peaks[min_p].flags |= flags ;

			if(i < peaks[min_p].p1) peaks[min_p].p1 = i ;
			if(i > peaks[min_p].p2) peaks[min_p].p2 = i ;
			if(j < peaks[min_p].t1) peaks[min_p].t1 = j ;
			if(j > peaks[min_p].t2) peaks[min_p].t2 = j ;
		}

	}

	for(int p=0;p<peaks_cou;p++) {
		peaks[p].t1 += t1 - 1 ;
		peaks[p].p1 += p1 - 1 ;
		peaks[p].t2 += t1 - 1 ;
		peaks[p].p2 += p1 - 1 ;		

		if((peaks[p].p2 - peaks[p].p1)==0) peaks[p].flags |= FCF_ONEPAD ;
		else peaks[p].flags &= ~FCF_ONEPAD ;

		PROFILER(0) ;
	}

	return 0 ;
}
