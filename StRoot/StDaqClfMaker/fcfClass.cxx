/*
	
	Version 4.20	08/08/2002	Gain correction explicitly included
			Version loaded into /RTS/src/FCF on 08/09/02

	Version 4.10	05/31/2002	pad-to-pad T0 corrections added
	Version 4.01	05/31/2002	Small bug fixes..
	Version 4.00	05/20/2002	50 Hz - really good!
	Version 3.00	01/15/2002	Major revamp
	Version 3.10	01/16/2002	Revamped major internal strucutres
	Version 3.12	01/17/2002	Minor tweaks and redefinitions...
	Version 3.13	02/27/2002	Added maxClusters protection
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdarg.h>

#define FCF_VERSION	"4.20"	



// need to define "ROOT" for use in offline
#ifdef __ROOT__


#define FCF_10BIT_ADC	// ADC data is already in 10 bits!
#define FCF_DONT_USE_LOG

#endif

#define FCF_T0_CORRECTION
#define FCF_GAIN_CORRECTION


//#define FCF_TEST



// use for systems without the RTS Logging facility!
#ifdef FCF_DONT_USE_LOG
#define LOG(x1,x2,x3,x4,x5,x6,x7)
#else
#include <rtsLog.h>
#endif

//static UINT32 delta[20] ;


#include <daqFormats.h>
#include "rtsSystems.h"

#include "fcfClass.hh"



#define FCF_MAX_RES_COU_FAST	21



//#pragma align 1
//#pragma pack 1

struct fcfResx {	// 5 words !
	u_int t ;
	u_int charge ;
	int scharge ;

	u_short flags ;		// short
	short mean ;		// short

	u_int pad ;


#ifdef FCF_TEST
	// new - test only...
	u_short	 t1, t2, p1, p2 ;	
	u_int pix ;		// unused?
	u_int adc_max ;
	u_int id ;
#endif

} ;


//#pragma pack 0
//#pragma align 0

#ifdef FCF_TEST
void mergeTest(struct fcfResx *nw, struct fcfResx *old);
void doTest(struct fcfResx *r, int pad, int a, int b, int max) ;

struct testADC {
	u_short adc ;
	u_short id ;
} testADC[256][512] ;

#endif

#ifdef __unix__



#define preburst4(x)	// meaningless for UNIX

#define FCF_960_R8
#define FCF_960_R9
#define FCF_960_R10
#define FCF_960_R11
#define FCF_960_R13


extern __inline volatile void mstore(struct fcfResx *rr, u_int av, u_int ch, u_int mean, u_int flags)
{

	rr->t = av ;
	rr->charge = rr->scharge = ch ;

	rr->flags = flags ;
	rr->mean = mean ;

	return ;
}


#else	// I960

#include "mzGlobal.h"
#include "mzInlines.h"
#include "mzFastTimer.h"

#define FCF_960_R8	asm ("r8") 
#define FCF_960_R9	asm ("r9") 
#define FCF_960_R10	asm ("r10") 
#define FCF_960_R11	asm ("r11") 
#define FCF_960_R13	asm ("r13") 

extern __inline volatile void mstore(struct fcfResx *r, u_int av, u_int ch, u_int mean, u_int flags)
{
	__asm__ volatile (" \
	\n \
	shlo 16, r11, r11 \n \
	lda (r9), r10 \n \
	or r11, %5, r11 \n \
	stq r8, (%0) \n \
	" \
	: "r=" (r) : "0" (r), "r" (av), "r" (ch), "r" (mean), "r" (flags) : "r10") ;

	return ;
}

#endif	// UNIX



	
	
// returns the total length of words (4bytes) used in outres
// including the row and cluster count

int fcfClass::finder(u_char *adcin, u_short *cppin, u_int *outres)
{
	int i, j, pad ;
	int next_pad ;
	
	u_short *cpp_off = (u_short *)cppOff ;

	int new_res_ix, new_res_cou, old_res_cou ;


	u_int *cl_found_pointer, *row_pointer ;


	struct fcfResx **new_res, **old_res ;


	row_pointer = outres++ ;	// mark the row
	cl_found_pointer = outres++ ;	// mark the count

	*cl_found_pointer = 0 ;
	*row_pointer = row ;


	new_res_ix = 0 ;
	new_res_cou = old_res_cou = 0 ;


	new_res = old_res = NULL ;
	next_pad = 0 ;

//	memset(delta,0,sizeof(delta)) ;

	u_int start_flags = FCF_LEFT_EDGE ;

#ifdef FCF_TEST
	memset(testADC,0,sizeof(testADC)) ;
	int globalId = 1 ;
#endif

	for(pad=padStart;pad<=padStop;pad++) {


//		UINT32 mark = mzFastTimerMark() ;


#ifdef __unix__
		static UINT32 cppStore[32] ;
		UINT32 *ptrs = cppStore ;
#else
		UINT32 *ptrs = fastMem->cppStore ;
#endif
		register UINT32 *ptrs_r = ptrs ;
		register UINT32 *cpp_r = (u_int *)((u_int)cppin + cpp_off[pad]) ;
		register UINT32 fe00  = 0xFE00FE00 ;

		UINT32 *ptrs_end = ptrs_r + 31 ;


		//LOG(DBG,"cppin 0x%X, cpp_off[] 0x%X, cpp_r 0x%X, cpp_off 0x%X",(u_int)cppin,cpp_off[pad],(u_int)cpp_r,(u_int)cpp_off,0) ;

		while(ptrs_r < ptrs_end) {
			//asm("#tada") ;


			register unsigned int first FCF_960_R8 ;
			register unsigned int second FCF_960_R9 ;
			register unsigned int third FCF_960_R10 ;
			register unsigned int fourth FCF_960_R11 ;

			preburst4(cpp_r) ;	// this guy puts the results in r8 and r9!!!
#ifdef __unix__
			first = *cpp_r ;
			second = *(cpp_r+1) ;
			third = *(cpp_r+2) ;
			fourth = *(cpp_r+3) ;
			//LOG(DBG,"%X %X %X %X",first,second,third,fourth,0) ;
#endif
			if(first & fe00) goto go_out ;
			*ptrs_r++ = first ;

			if(second & fe00) goto go_out ;
			*ptrs_r++ = second ;

			if(third & fe00) goto go_out ;
			*ptrs_r++ = third ;

			if(fourth & fe00) goto go_out ;
			*ptrs_r++ = fourth ;

			cpp_r += 4 ;
		}

		// NOTICE! go-to label!
		go_out : ;

		u_int cou_ptrs = (ptrs_r - ptrs) ;


		//LOG(DBG,"Running pad %d with %d clusters, pad %d had %d protoclusters...",pad,cou_ptrs,next_pad-1,new_res_cou,0) ;

		if(cou_ptrs == 0) {
			continue ;	// continue with the next pad...
		}

		if(pad == padStart) start_flags = FCF_LEFT_EDGE ;
		else if(pad == padStop) start_flags = FCF_RIGHT_EDGE ;
		else start_flags = 0 ;

#ifdef FCF_10BIT_ADC
		u_short *val = (u_short *)((u_int)adcin + adcOff[pad]) ;
#else
		u_char *val = (u_char *)((u_int)adcin + adcOff[pad]) ;
#endif



		if((next_pad != pad) && new_res_cou) {	// we skipped a pad and there
							// was some stuff remaining!
							// so we have to flush it out
			int wrote ;

			// check for space!
			if((*cl_found_pointer + new_res_cou) >= maxClusters) {
				LOG(TERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
				return outres - row_pointer ;
			}

			LOG(WARN,"Pad skipped: this %d, expect %d, res count %d",pad,next_pad,new_res_cou,0,0) ;

			// could be less than requested due to cuts...
			wrote = saveRes(new_res, new_res_cou, outres) ;
			outres += wrote*2 ;	// occupies 8 bytes (2 words)
			*cl_found_pointer += wrote ;

			// since we skipped a pad and stored the old results we must zap the old results, V4.01
			new_res_cou = 0 ;
		}


		// swap the banks...
		if(new_res_ix == 1) {
			new_res_ix = 0 ;

			old_res = resx[1] ;
			new_res = resx[0] ;
			
		}
		else {
			new_res_ix = 1 ;

			old_res = resx[0] ;
			new_res = resx[1] ;

		}

		old_res_cou = new_res_cou ;	// the last guys are the old guys now...
		new_res_cou = 0 ;		// start from scratch for the expected guys


//		delta[0] = mzFastTimerDelta(mark) ;


		UINT16 *ptrs_16 = (UINT16 *)ptrs ;
		u_int cl_counter ;

		for(cl_counter=0;cl_counter<cou_ptrs;cl_counter++) {
			register u_int start, stop ;

			start = (u_int)*ptrs_16++ ;
			stop = (u_int) *ptrs_16++ ;

			// paranoia
			stop &= 0x1FF ;

			//LOG(DBG,"Cluster %d: start %d, stop %d",cl_counter,start,stop,0,0) ;

			// ignore certain ranges
			if((stop < timebinLo) || (start >= timebinHi)) {
				LOG(WARN,"Ignoring cluster start %d, stop %d",start,stop,0,0,0) ;
				continue ;
			}



			//asm("#first") ;
			
#ifdef FCF_10BIT_ADC
			register u_short *adc_p = val + start ;
			register u_short *adc_end = val + stop + 1;
#else
			register u_char *adc_p = val + start ;
			register u_char *adc_end = val + stop + 1;
#endif

/* this used to be an attempt to optimize for speed - leave it in as a comment
#ifdef THIS_WAY
			{
				u_int p1 = (UINT32)adc_p & 0xFFFFFFF0 ;
				u_int p2 = (UINT32)adc_end & 0xFFFFFFF0 ;

				for(;p1<=p2;p1+=16) {
					preburst4((u_int *)p1) ;
				}
			}
#else
			preburst4((u_int *)((u_int)adc_p & 0xFFFFFFF0)) ;
#endif		
*/
			register int flags = FCF_ONEPAD | start_flags ; 
			register int adc = *adc_p++ ;

			// move these to registers....
			register int min_adc = minAdcT ;

#ifndef FCF_10BIT_ADC
			register u_short *adc8to10 FCF_960_R13 = a8to10 ;
#endif

			//LOG(WARN,"Pad %d, cl %d, len %d",pad,cl_counter,stop-start+1,0,0) ;

			//delta[1] = mzFastTimerDelta(mark) ;
			do {
				register int last_falling = 0 ;
				register int max_a = 0 ;
				register int mean FCF_960_R11  = 0 ;
				register int charge FCF_960_R9  = 0 ;
				register int av FCF_960_R8 = 0 ;

				register int last_a = 0 ;
#ifdef FCF_TEST
				stop = start ;	// take this out when running!
#endif
				//delta[1] = mzFastTimerDelta(mark) ;
				//asm("#second") ;
				do {

					register int a ;

					if(last_falling) {
						if(adc > (last_a + min_adc)) {	// mostly false
							flags |= FCF_DOUBLE_T ;
							break ;
						}
					}
					else {
						if(adc < (last_a - min_adc)) {	// mostly false
							last_falling = 1 ;	// Im falling
						}						
						else {
							if(adc >= max_a) {	// find the maximum and keep it...
								max_a = adc ;
								mean = start ;	// this is not the 

							}

						}
					}

#ifdef FCF_10BIT_ADC 
					// no lookup - already in 10 bits!
					a = adc ;
#else
					a = adc8to10[adc] ;  // transfer to 10 bits
#endif

					last_a = adc ;


#ifdef FCF_TEST
					testADC[pad][start].adc = adc ;
					testADC[pad][start].id = globalId ;
#endif


					av += start * a ;

					charge += a ;

					//LOG(DBG,"Pad %d, start %d, adc %d, charge %d",pad,start,a,charge,0) ;

					adc = *adc_p++ ;	// advance here!

					//LOG(DBG,"... adc_p 0x%08X (end 0x%08X)",(UINT32)adc_p,(UINT32)adc_end,0,0,0) ;

					start++ ;

				} while(adc_p <= adc_end) ;
				//asm("#third") ;


#ifdef FCF_GAIN_CORRECTION
				charge *= gainCorr[pad] ;	// charge is now 64 times larger!

				av *= gainCorr[pad] ;		// time sum is now 64 times larger!
#endif

#ifdef FCF_T0_CORRECTION
				// keep in mind that the average is already multiplied by
				// 64 now!!!
				av = (av << 6) + t0Corr[pad]*charge ;	// time sum is now 64 times larges on top of any other (gain) coorection

#endif
				//delta[2] += mzFastTimerDelta(mark) -delta[1];

				// get the next storage location
				struct fcfResx *rn = new_res[new_res_cou++];


				rn->pad = charge*pad ;	// this may be 64 times larger if gain correction is employed!

				// store the new results via some assembly inline...
				mstore(rn,av,charge,mean,flags) ;

#ifdef FCF_TEST
				doTest(rn, pad, stop, start, max_a) ;
				rn->id = globalId ;
				globalId++ ;
#endif				

				//if(((UINT32)adc_p & 0xF)==0) preburst4(adc_p) ;

			} while(adc_p <= adc_end) ;

			//delta[2] += mzFastTimerDelta(mark) -delta[1];
			//asm("#fourth") ;

		}// end of the pad or cl_counter


		//delta[3] += mzFastTimerDelta(mark) - delta[0];

		
		if(new_res_cou) {	// some results
			next_pad = pad + 1 ;	// we expect this pad as the next one!
		}

		

		//LOG(WARN,"Pad %d: cl %d, res %d",pad,cou_ptrs,new_res_cou,0,0) ;


		//asm("#double") ;
		int new_start = 0 ;

		for(i=0;i<old_res_cou;i++) {	// loop over old sequneces
			register struct fcfResx *rr ;
			register int start ;

			rr = old_res[i] ;

			register int merged = 0 ;

			register int old_mean = rr->mean ;
			register int old_mean_m = old_mean - param1 ;
			register int old_mean_p = old_mean + param1 ;

			register int min_adc = minAdcPad ;

			register int old_scharge = rr->scharge ;
			register int old_flags = rr->flags ;

			start = new_start ;

#ifdef FCF_GAIN_CORRECTION
			// aritifically multiply by 64!
			min_adc *= 64 ;
#endif
			for(j=start;j<new_res_cou;j++) {	// loop over new sequences
				register struct fcfResx *nresx ;
				register int mean ;

				nresx = new_res[j] ;

				mean = nresx->mean ;
			
				if(mean < old_mean_m) {
					new_start = j + 1 ;
					continue ;
				}
				else if(mean <= old_mean_p) {	
					register int charge ;

					charge = nresx->charge ;


					if(old_flags & FCF_FALLING) {

						if(charge > (old_scharge + min_adc)) {
							register u_int sc_tmp, sc_p_tmp ;



							sc_tmp = old_scharge / 2 ;
							sc_p_tmp = sc_tmp * (pad-1) ;

							nresx->flags |= FCF_DOUBLE_PAD ;	// new one
								
							// the new one will get half of previous share

							nresx->charge += sc_tmp ;
							nresx->pad += sc_p_tmp ;
#ifdef FCF_T0_CORRECTION
							nresx->t += (mean * sc_tmp)<<6 ;
#else
							nresx->t += (mean * sc_tmp) ;
#endif
							// the old guy gets half of the previous less
							rr->charge -= sc_tmp ;
							rr->pad -= sc_p_tmp ;

#ifdef FCF_T0_CORRECTION
							rr->t -= (old_mean * sc_tmp)<<6 ;
#else
							rr->t -= old_mean * sc_tmp ;
#endif
							rr->flags = old_flags | FCF_DOUBLE_PAD ;	// old one
								
							new_start = j+1 ;

							break ;	// and create a new one; break out of old results scan

						}
						// maintain "falling" character
						nresx->flags |= FCF_FALLING ;
					}
					else {
						if(charge < (old_scharge - min_adc)) {
							//l::log("MARK!") ;
							nresx->flags |= FCF_FALLING ;
						}
					}


					merged = 1 ;

					nresx->flags |= old_flags ;
					nresx->flags &= (~FCF_ONEPAD) ;  // ...clear the one pad flag
			

					nresx->scharge = charge ;	// last pad charge
					nresx->charge += rr->charge ;
					nresx->pad += rr->pad ;
					nresx->t += rr->t ;

					// mean stays the same..
#ifdef FCF_TEST
					mergeTest(nresx, rr) ;
#endif
					new_start = j + 1 ;

					break ;
				}
				else {
					new_start = j ;

					break ;
				}
			
			}


			if(!merged) {	// didn't merge - must store!
				int wrote ;

				// check for space!
				if((*cl_found_pointer + 1) >= maxClusters) {
					LOG(TERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
					return outres - row_pointer ;
				}

				//LOG(DBG,"Mean diff old-new %d [%d], new res cou %d",old_mean-new_res[j]->mean,reason,new_res_cou-j,0,0) ;


				// could be less than requested due to cuts...
				wrote = saveRes(&rr, 1, outres) ;
				outres += wrote*2 ;	// occupies 8 bytes
				*cl_found_pointer += wrote ;
			}

		}
		//asm("#triple") ;

		//delta[4] += mzFastTimerDelta(mark) -delta[3];
	}// for minpad to maxpad
	
	// check for leftover in the last pass
	if(new_res_cou) {
		int wrote ;
		// check for space!
		if((*cl_found_pointer + new_res_cou) >= maxClusters) {
			LOG(TERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
			return outres - row_pointer ;
		}


		// could be less than requested due to cuts...
		wrote = saveRes(new_res, new_res_cou, outres) ;
		outres += wrote*2 ;	// occupies 8 bytes
		*cl_found_pointer += wrote ;
	}


	//LOG(WARN,"One_pad %d, pad decon %d, seq_total %d, low_adc %d, delta %d",one_pad,pad_decon,seq_total,adc_low,delta[2]) ;

	//LOG(WARN,"Delta2 %d, delta3 %d, delta4 %d",delta[2],delta[3],delta[4],0,0) ;

	return (outres - row_pointer) ;	// length in qwords

} 


fcfClass::fcfClass(int det, u_short *table) 
{
	detector = det ;

#ifdef __unix__
	a8to10 = adc8to10_storage ;

#else	// I960
	a8to10 = fastMem->adc8to10 ;
#endif


	if(table == NULL) {
		noADCconversion = 1 ;
	}
	else {
		noADCconversion = 0 ;
	}
		

	svtPedestal = 0 ;


	deconTime = 1 ;
	deconPad = 1 ;
	doCuts = 1 ;

	param1 = FCF_PARAM1 ;
	minAdcT = FCF_MIN_ADC_T ;
	minAdcPad = FCF_MIN_ADC_PAD ;

	switch(det) {
	case TPC_ID :
		maxCPP = 31 ;
		maxTimebin = 511 ;
		timebinLo = 0 ;
		timebinHi = 511 ;	// should be 400 normally
		chargeMin = 10 ;	// reasonable estimate was 50
		break ;

	case FTP_ID :
		maxCPP = 31 ;
		maxTimebin = 511 ;
		timebinLo = 0 ;
		timebinHi = 255 ;
		chargeMin = 40 ;
		break ;
	case SVT_ID :
		maxCPP = 8 ;
		maxTimebin = 127 ;
		timebinLo = 0 ;
		timebinHi = 127 ;
		chargeMin = 20 ;
		break ;
	default :
		LOG(CRIT,"Cluster Finder can't work with DET Type %d",det,0,0,0,0) ;
		return ;
	}

	static struct fcfResx res_slow[2][512] ;
#ifdef __unix__
	static struct fcfResx res_fast_ux[2][FCF_MAX_RES_COU_FAST] ;
	struct fcfResx *res_fast[2] = { res_fast_ux[0], res_fast_ux[1] } ;
#else
	// hand adjusted!!! Beware of changes!!!
	struct fcfResx *res_fast[2] = {
		(struct fcfResx *) fastMem->fcfStore,
		(struct fcfResx *) &(fastMem->fcfStore[8*21]) } ;
		

#endif


	int i, j ;
	for(i=0;i<2;i++) {
		for(j=0;j<FCF_MAX_RES_COU_FAST;j++) {
			resx[i][j] = res_fast[i] + j ;
			//LOG(WARN,"B %d:%d = 0x%08X",i,j,(UINT32)resx[i][j],0,0) ;
		}
	}
	for(i=0;i<2;i++) {
		for(j=FCF_MAX_RES_COU_FAST;j<512;j++) {
			resx[i][j] = &(res_slow[i][j]) ;
		}
	}

	return ;
}


// copy over the ADC conversion table...
void fcfClass::set8to10(u_short *table)
{
	int i ;

	if(table) {
		for(i=0;i<256;i++) {
			a8to10[i] = *table++ ;
		}
		noADCconversion = 0 ;
	}
	else {
		noADCconversion = 1 ;
	}
	return ;
}

// returns the number of clusters saved
inline int fcfClass::saveRes(struct fcfResx *res_p[], int cou, u_int *output)
{
	//asm("#saved") ;
	int i ;
	int saved = 0 ;
	register u_int ch_min = chargeMin ;
	register int do_cuts = doCuts ;

	for(i=0;i<cou;i++) {
		struct fcfResx *rr ;

		rr = res_p[i] ;


		u_int fla ;
		fla = rr->flags ;

		u_int cha ;
		cha = rr->charge ;


		// skip single pad hits
		// as well as those with too little charge

		//LOG(DBG,"saveRes: flags 0x%X, charge %d",fla,cha,0,0,0) ;

		if(((fla & FCF_ONEPAD) || (cha <= ch_min)) && do_cuts) {
			continue ;
		}

		saved++ ;

		// clear the FALLING flag 
		fla &= (~FCF_FALLING) ;

		u_int pad_c, time_c ;

		pad_c = (rr->pad << 6) / cha ;


#ifdef FCF_T0_CORRECTION
		// it was already multipled by 64!
		time_c = rr->t / cha ;	
#else
		time_c = (rr->t << 6) / cha ;
#endif

#ifdef FCF_GAIN_CORRECTION
		cha >>= 6 ;	// it was multiplied by 64
#endif

#ifdef __unix__	// watchout for the ordering!
		unsigned short *sh = (unsigned short *)output ;

		*sh++ = pad_c ;
		*sh++ = time_c ;
		*sh++ = fla ;
		*sh++ = cha ;

		output += 2 ;
#else	// embedded I960 - little endian
		*output++ = (time_c << 16) | pad_c ;
		*output++ = (cha << 16) | fla ;
#endif


#ifdef FCF_TEST
		printf("%f %f %d %d %d %d %d %d\n",(double)pad_c/64.0,(double)time_c/64.0,fla,cha,rr->pix,rr->adc_max,rr->p2-rr->p1+1,rr->t2-rr->t1+1) ;
#endif


	}

	//LOG(DBG,"saveRes saved %d clusters...",saved,0,0,0,0) ;
	return saved ;
}

#ifdef FCF_TEST
void doTest(struct fcfResx *r, int pad, int a, int b, int max)
{

	r->p1 = r->p2 = pad ;
	r->t1 = a ;
	r->t2 = b ;
	r->pix = b-a+1 ;
	r->adc_max = max ;

	return ;
}

void mergeTest(struct fcfResx *nw, struct fcfResx *old)
{
	int i, j ;

	if(nw->p1 > old->p1) {
		nw->p1 = old->p1 ;
	}

	if(nw->p2 < old->p2) {
		nw->p2 = old->p2 ;
	}

	if(nw->t1 > old->t1) {
		nw->t1 = old->t1 ;
	}

	if(nw->t2 < old->t2) {
		nw->t2 = old->t2 ;
	}

	if(nw->adc_max < old->adc_max) {
		nw->adc_max = old->adc_max ;
	}

	nw->pix += old->pix ;

	for(i=0;i<256;i++) {
		for(j=0;j<512;j++) {
			if(testADC[i][j].id == old->id) {
				testADC[i][j].id = nw->id ;
			}
		}
	}

	return ;
}
#endif

