/*
	This code is property of Brookhaven National Laboratory, USA.	
	Authored by Tonko Ljubicic.

	No modifications are to be made without the express consent of
	the author.

	Version 5.31	12/14/2004 undef-ed FCF_NEW_ADC_ROUNDOFF
	Version 5.30	Added ADC roundoff
	Version 5.21	General Cleanup
	Version 5.20	Added ADC cluster back-annotation if defined
	Version 5.12	Added track ID
	Version 5.11	Production for FY04
	Version 5.00	07/11/2003	Many small fixes
	Version 4.31	11/19/2002	Still fixing on I960s...
	Version 4.30	11/13/2002	Re-implemented Gain & T0
					They are now mandated!
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
#include <ctype.h>

#define FCF_VERSION	"5.31"	



#include <rts.h>
#include <rtsSystems.h>
#include <fcfClass.hh>

static char *fcf_cvs_revision = "$Revision: 1.5 $" ;

#ifdef __ROOT__	// STAR Offline

#include "StDaqLib/TPC/trans_table.hh"

#define FCF_10BIT_ADC	// ADC data is already in 10 bits!
#define LOG(x1,x2,x3,x4,x5,x6,x7) 

//#define FCF_NEW_ADC_ROUNDOFF

#else

#include <rtsLog.h>
	
#endif



//static u_int delta[20] ;





#if __GNUC__ == 2 && __GNUC_MINOR__ < 8
#pragma align 1
#pragma pack 1
#endif

struct fcfResx {	// 5 words or 7 if EXTENTS are defined...
	int t ;	// this may occasionally be negative!
	u_int charge ;
	u_int scharge ;

	u_short flags ;		// short
	short mean ;		// short

	u_int pad ;

	u_short	 t1, t2, p1, p2 ;	// extents

#if defined(FCF_SIM_ON) || defined(FCF_ANNOTATE_CLUSTERS)
	// new - test only...
	u_int pix ;		// pixel count
	u_int adc_max ;		// maximum adc

	short id ;		// sim. id
	u_short cl_id ;		// global cluster id!
#endif

} ;

#if __GNUC__ == 2 && __GNUC_MINOR__ < 8
#pragma pack 0
#pragma align 0
#endif





#if defined(__unix) || defined(__APPLE__)


#define FCF_MAX_RES_COU_FAST	8

#define preburst4(x)	// meaningless for UNIX

#define FCF_960_R8
#define FCF_960_R9
#define FCF_960_R10
#define FCF_960_R11
#define FCF_960_R13

// simulation global
#ifdef FCF_SIM_ON
static u_int *simout ;
static fcfPixAnnotate pixStruct[183][512];
#endif


#ifdef FCF_ANNOTATE_CLUSTERS
// annotation data - GLOBAL!

#ifndef __ROOT__
struct fcfPixAnnotate fcfPixA[45][182][512] ;
#endif

#endif



#else	// I960


#include "mzGlobal.h"
#include "mzInlines.h"
#include "mzFastTimer.h"


#define FCF_MAX_RES_COU_FAST	((sizeof(fastMem->fcfStore)/sizeof(struct fcfResx))/2)

#define FCF_960_R8	asm ("r8") 
#define FCF_960_R9	asm ("r9") 
#define FCF_960_R10	asm ("r10") 
#define FCF_960_R11	asm ("r11") 
#define FCF_960_R13	asm ("r13") 



#endif	// UNIX


extern __inline volatile void mstore(struct fcfResx *rr, int av, int ch, u_int mean, u_int flags)
{

	rr->t = av ;
	rr->charge = rr->scharge = ch ;

	rr->flags = flags ;
	rr->mean = mean ;

	return ;
}

	
	
// Returns the total length of words (4bytes) used in outres
// including the row and cluster count.
// Since the row and cluster count makes 2 words the routine returns 
// 2 in case of no clusters found!

int fcfClass::finder(u_char *adcin, u_short *cppin, u_int *outres)
{
	int i, j, pad ;
	int next_pad ;
	u_int start_flags ;

	chargeMinCorrect = chargeMin * FCF_GAIN_FACTOR ;

	int new_res_ix, new_res_cou, old_res_cou ;


	u_int *cl_found_pointer, *row_pointer ;


	struct fcfResx **new_res, **old_res ;


	row_pointer = outres++ ;	// mark the row
	cl_found_pointer = outres++ ;	// mark the count

	*cl_found_pointer = 0 ;
	*row_pointer = row ;


#ifdef FCF_SIM_ON
	u_int *sim_found_ptr, *sim_row_ptr ;
	short *simin ;

	simin = simIn ;	// copy the originals
	simout = simOut ;

	if(simin && simout) {
		sim_row_ptr = simout++ ;
		sim_found_ptr = simout++ ;

		*sim_found_ptr = 0 ;
		*sim_row_ptr = row ;
	}
	else {
		simout = 0 ;
		// WATCH IT!
		sim_found_ptr = simout ;
		sim_row_ptr = simout ;
	}

	u_short cl_id = 1 ;	// start global cluster id with 1
	memset(pixStruct,0,sizeof(pixStruct)) ;

#endif


	new_res_ix = 0 ;
	new_res_cou = old_res_cou = 0 ;


	new_res = old_res = NULL ;
	next_pad = 0 ;

//	memset(delta,0,sizeof(delta)) ;




	for(pad=padStart;pad<=padStop;pad++) {
		u_int GC ;	// gain correction
		int  T0C ;	// T0 correction

		// gain and t0 corrections!
		GC = gainCorr[pad] ;
		T0C = t0Corr[pad] ;

		if(startFlags) {
			start_flags = startFlags[pad] ;
		}
		else {
			start_flags = 0 ;
		}

		if(GC == 0) {
			//LOG(WARN,"Gain Correction 0 on row %d, pad %d - skipping!",row,pad,0,0,0) ;
			continue ;
		}

		//LOG(WARN,"row %d, pad %d, GC %d, T0C %d",row,pad,GC,T0C,0) ;

//		u_int mark = mzFastTimerMark() ;


#if defined(__unix) || defined(__APPLE__)
		static u_int cppStore[32] ;
		u_int *ptrs = cppStore ;
#else
		u_int *ptrs = fastMem->cppStore ;
#endif
		register u_int *ptrs_r = ptrs ;
		register u_int *cpp_r = (u_int *)((char *)cppin + cppOff[pad]) ;
		register u_int fe00  = 0xFE00FE00 ;

		u_int *ptrs_end = ptrs_r + 31 ;


		//LOG(DBG,"cppin 0x%X, cpp_off[] 0x%X, cpp_r 0x%X, cpp_off 0x%X",cppin,cpp_off[pad],cpp_r,cpp_off,0) ;

		while(ptrs_r < ptrs_end) {
			//asm("#tada") ;


			register unsigned int first FCF_960_R8 ;
			register unsigned int second FCF_960_R9 ;
			register unsigned int third FCF_960_R10 ;
			register unsigned int fourth FCF_960_R11 ;

			preburst4(cpp_r) ;	// this guy puts the results in r8 and r9!!!
#if defined(__unix) || defined(__APPLE__)
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



#ifdef FCF_10BIT_ADC
		u_short *val = (u_short *)((char *)adcin + adcOff[pad]) ;
#else
		u_char *val = (u_char *)((char *)adcin + adcOff[pad]) ;
#endif

#ifdef FCF_SIM_ON
		u_short *simval ;
		if(simout) {
			simval = (u_short *)((char *)simin + adcOff[pad]) ;
		}
		else {
			simval = 0 ;
		}
#endif

		if((next_pad != pad) && new_res_cou) {	// we skipped a pad and there
							// was some stuff remaining!
							// so we have to flush it out
			int wrote ;

			// check for space!
			if((*cl_found_pointer + new_res_cou) >= maxClusters) {
				LOG(ERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
				return outres - row_pointer ;
			}

			//LOG(WARN,"Pad skipped: this %d, expect %d, res count %d",pad,next_pad,new_res_cou,0,0) ;

			// could be less than requested due to cuts...
			wrote = saveRes(new_res, new_res_cou, outres) ;
			outres += wrote*2 ;	// occupies 8 bytes (2 words)
			*cl_found_pointer += wrote ;

#ifdef FCF_SIM_ON
			if(simout) {
				*sim_found_ptr += wrote ;
			}
#endif
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


		u_short *ptrs_16 = (u_short *)ptrs ;
		u_int cl_counter ;

		for(cl_counter=0;cl_counter<cou_ptrs;cl_counter++) {
			register u_int start, stop ;

			start = (u_int)*ptrs_16++ ;
			stop = (u_int) *ptrs_16++ ;

			// paranoia
			// don;t need this anymore
			//stop &= 0x1FF ;

			//LOG(DBG,"Cluster %d: start %d, stop %d",cl_counter,start,stop,0,0) ;

			// ignore certain ranges
			// Tonko, 04/11/03 modified
			if(start < timebinLo) start = timebinLo ;
			if(stop > timebinHi) stop = timebinHi ;
			if(stop < start) continue ;


			//if((stop < timebinLo) || (start >= timebinHi)) {
			//	//LOG(WARN,"Ignoring cluster start %d, stop %d",start,stop,0,0,0) ;
			//	continue ;
			//}



			//asm("#first") ;
#ifdef FCF_10BIT_ADC
			register u_short *adc_p = val + start ;
			register u_short *adc_end = val + stop + 1;
#else
			register u_char *adc_p = val + start ;
			register u_char *adc_end = val + stop + 1;
#endif

#ifdef FCF_SIM_ON
			u_short *sim_p  ;
			u_short sim_id  ;

			if(simout) {
				sim_p = simval + start ;
				sim_id = *sim_p++ ;
			}
			else {
				sim_p = 0 ;
				sim_id = 0 ;
			}
#endif

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
				register u_int charge FCF_960_R9  = 0 ;
				register int av FCF_960_R8 = 0 ;

				register int last_a = 0 ;

#ifdef FCF_SIM_ON
				int max_sim  = sim_id ;
#endif


				stop = start ;	// necessary for the EXTENETS calc

				//delta[1] = mzFastTimerDelta(mark) ;
				//asm("#second") ;
				do {

					register int a ;


					if(unlikely(last_falling)) {
						if(unlikely(adc > (last_a + min_adc))) {	// mostly false
							flags |= FCF_DOUBLE_T ;
							//LOG(WARN,"T Split before adc %d, at %d...",adc,start,0,0,0) ;
							break ;
						}
					}
					else {
						if(unlikely(adc < (last_a - min_adc))) {	// mostly false
							last_falling = 1 ;	// Im falling
						}						
						else {
							if(unlikely(adc >= max_a)) {	// find the maximum and keep it...
								max_a = adc ;
#ifdef FCF_SIM_ON
								max_sim = sim_id ;
#endif
								mean = start ;	// this is not the mean but the position of the max!

							}

						}
					}

#ifdef FCF_10BIT_ADC 
					// no lookup - already in 10 bits!

					//a = adc ;           // jml 1/27/02 even when FCF_10BIT_ADC really means the size is short
					a = log8to10_table[adc];    // we still need to apply the 8to10 bit conversion.

					//if(row==14) printf("Row %d, pad %d, timebin %d, data %d\n",row,pad,start,a) ;

					//if(a > 255) printf("Datum is %d\n",a) ;
#else
					a = adc8to10[adc] ;  // transfer to 10 bits
#endif
					last_a = adc ;

#ifdef FCF_NEW_ADC_ROUNDOFF
					av += start*a + (start+1)/2 ;
#else
					av += start * a ;
#endif
					charge += a ;

					//LOG(WARN,"Pad %d, start %d, adc %d, charge %d",pad,start,adc,charge,0) ;

					adc = *adc_p++ ;	// advance here!
#ifdef FCF_SIM_ON
					pixStruct[pad][start].adc = a ;
					pixStruct[pad][start].cl_id = cl_id ;
					pixStruct[pad][start].id_simtrk = sim_id ;

					if(simout) {
						sim_id = *sim_p++ ;
					}
#endif

					//LOG(DBG,"... adc_p 0x%08X (end 0x%08X)",adc_p,adc_end,0,0,0) ;

					start++ ;

				} while(likely(adc_p <= adc_end)) ;
				//asm("#third") ;

#ifdef FCF_NEW_ADC_ROUNDOFF
				// new! correcting ADC roundoff errors
				charge += ((start-stop)+1)/2 ;	// Note that "start-1" is the last while "stop" is the first
								// pixel. Note that I also try to roundoff correctly the div. by 2.
#endif
				// t0Corr is (gain*t0*64)!
				// This value may go negative here due to the t0 corrections!!!
				// Time sum is now 64 times larger!!!
				av = GC * av + T0C * charge ;	


				charge *= GC ;	// charge is now 64 times larger!


#if defined(__unix) || defined(__APPLE__)
				if(charge > 0x7FFFFFFF) {
					LOG(ERR,"Whoa charge 0x%08X, %d GC",charge,GC,0,0,0) ;
				}
#endif
				//delta[2] += mzFastTimerDelta(mark) -delta[1];

				// get the next storage location
				struct fcfResx *rn = new_res[new_res_cou++];


				rn->pad = charge*pad ;	// this may be 64 times larger if gain correction is employed!

				// store the new results via some assembly inline...
				mstore(rn,av,charge,mean,flags) ;


				rn->p1 = pad ;
				rn->p2 = pad ;
				rn->t1 = stop ;		// this is actually the first...
				rn->t2 = start-1 ;	// this is actually the last...


				//LOG(WARN,"Pad %d, stored max at %d, t1 %d, t2 %d",pad,mean,rn->t1,rn->t2,0) ;
				
#ifdef FCF_SIM_ON
				//printf("**** %d %d\n",cl_id,max_a) ;

				rn->pix = start-stop ;
				rn->adc_max = max_a ;
				rn->id = max_sim ;
				rn->cl_id = cl_id++ ;		// global cluster id...
#endif				

				//if(((u_int)adc_p & 0xF)==0) preburst4(adc_p) ;

			} while(likely(adc_p <= adc_end)) ;

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

		for(i=0;likely(i<old_res_cou);i++) {	// loop over old sequneces
			register struct fcfResx *rr ;
			register int start ;

			rr = old_res[i] ;

			register int merged = 0 ;

			register int old_mean = rr->mean ;
			register int old_mean_m = old_mean - param1 ;
			register int old_mean_p = old_mean + param1 ;

			register int min_adc = minAdcPad ;

			register u_int old_scharge = rr->scharge ;
			register u_int old_flags = rr->flags ;

			start = new_start ;


			for(j=start;likely(j<new_res_cou);j++) {	// loop over new sequences
				register struct fcfResx *nresx ;
				register int mean ;

				nresx = new_res[j] ;

				mean = nresx->mean ;
			
				if(mean < old_mean_m) {
					new_start = j + 1 ;
					continue ;
				}
				else if(mean <= old_mean_p) {	
					register u_int charge ;

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

							nresx->t += (mean * sc_tmp) ;


							// the old guy gets half of the previous less

							if(unlikely(sc_tmp > rr->charge)) {
								LOG(WARN,"oops - going negative 0x%08X - 0x%08X",rr->charge,sc_tmp,0,0,0) ;
							}


							rr->charge -= sc_tmp ;
							rr->pad -= sc_p_tmp ;

							// watch it! rr->t may go negative!!!
							rr->t -= old_mean * sc_tmp ;

							rr->flags = old_flags | FCF_DOUBLE_PAD ;	// old one
								

							nresx->p1 = pad - 1 ;
							rr->p2 = pad ;

							new_start = j+1 ;

							break ;	// and create a new one; break out of old results scan

						}
						// maintain "falling" character
						nresx->flags |= FCF_FALLING ;
					}
					else {
						// need to go signed for a moment...
						if((int)charge < ((int)old_scharge - min_adc)) {
							//llog("MARK!") ;
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
					// mean stays the same????
					// I'm not sure this is good!


					nresx->p1 = rr->p1 ;	// the old guy has the smaller pad
					if(rr->t1 < nresx->t1) nresx->t1 = rr->t1 ;
					if(rr->t2 > nresx->t2) nresx->t2 = rr->t2 ; 


#ifdef FCF_SIM_ON
					nresx->pix += rr->pix ;

					//printf("%d:now %d, old %d\n",rr->cl_id,nresx->adc_max,rr->adc_max) ;

					if(rr->adc_max > nresx->adc_max) {
						nresx->adc_max = rr->adc_max ;
						
						nresx->id = rr->id ;
					}

					// copy over the pixel data
					// and take over the old clusters ID
					for(int ii=0;ii<512;ii++) {
						if(nresx->cl_id == pixStruct[pad][ii].cl_id) {
							pixStruct[pad][ii].cl_id = rr->cl_id ;
						}
					}
					nresx->cl_id = rr->cl_id ;	// the new id

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
					LOG(ERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
					return outres - row_pointer ;
				}

				//LOG(DBG,"Mean diff old-new %d [%d], new res cou %d",old_mean-new_res[j]->mean,reason,new_res_cou-j,0,0) ;


				// could be less than requested due to cuts...
				wrote = saveRes(&rr, 1, outres) ;
				outres += wrote*2 ;	// occupies 8 bytes
				*cl_found_pointer += wrote ;
#ifdef FCF_SIM_ON
				if(simout) {
					*sim_found_ptr += wrote ;
				}
#endif
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
			//printf("FCF:Too many clusters!\n") ;
			LOG(ERR,"Too many clusters in pad %d - breaking!",pad,0,0,0,0) ;
			return outres - row_pointer ;
		}


		// could be less than requested due to cuts...
		wrote = saveRes(new_res, new_res_cou, outres) ;
		outres += wrote*2 ;	// occupies 8 bytes
		*cl_found_pointer += wrote ;
#ifdef FCF_SIM_ON
		if(simout) {
			*sim_found_ptr += wrote ;
		}
#endif
	}

	//if(row == 14) printf("Row %d: length %d\n",row,outres-row_pointer) ;

	//LOG(WARN,"One_pad %d, pad decon %d, seq_total %d, low_adc %d, delta %d",one_pad,pad_decon,seq_total,adc_low,delta[2]) ;

	//LOG(WARN,"Delta2 %d, delta3 %d, delta4 %d",delta[2],delta[3],delta[4],0,0) ;

	return (outres - row_pointer) ;	// length in qwords

} 


fcfClass::fcfClass(int det, u_short *table) 
{
	detector = det ;

#if defined(__unix) || defined(__APPLE__)
	a8to10 = adc8to10_storage ;
#else	// I960
	a8to10 = fastMem->adc8to10 ;
#endif


	// de-mangle the revision...
	cvs_revision = 0 ;	// let's start with 0 here...

	{	// bracket stuff
	size_t i ;
	char tmp[10] ;
	int num_start, num_end, num_dot ;

	for(i=0;i<strlen(fcf_cvs_revision);i++) {
		if(isdigit(fcf_cvs_revision[i])) {
			num_start = i ;
			break ;
		}
	}
	for(i=num_start;i<strlen(fcf_cvs_revision);i++) {
		if(fcf_cvs_revision[i]==0x20) {	// space
			num_end = i-1 ;
			break ;
		}
		if(fcf_cvs_revision[i]=='.') {	// dot
			num_dot = i ;
		}
	}

	strncpy(tmp,&(fcf_cvs_revision[num_start]),1) ;
	cvs_revision = atoi(tmp) << 16 ;	// major
	strncpy(tmp,&(fcf_cvs_revision[num_dot+1]),num_end-num_dot) ;
	cvs_revision |= atoi(tmp) ;

//	LOG(INFO,"Working with CVS code %s [0x%08X]",fcf_cvs_revision,cvs_revision,0,0,0) ;

	}

	simIn = 0 ;		// assume no simulation unless explicitly overriden later
	simOut = 0 ;

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
		timebinHi = 400 ;	// should be 400 normally
		chargeMin = 40 ;	// reasonable estimate was 40
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
		LOG(WARN,"Cluster Finder can't work with DET Type %d",det,0,0,0,0) ;
		return ;
	}

	// take into account the multipication due to
	// gain being used
	minAdcPad *= FCF_GAIN_FACTOR ;

	static struct fcfResx res_slow[2][512] ;
#if defined(__unix) || defined(__APPLE__)
	static struct fcfResx res_fast_ux[2][FCF_MAX_RES_COU_FAST] ;
	struct fcfResx *res_fast[2] = { res_fast_ux[0], res_fast_ux[1] } ;
#else

#define RES_COU	(sizeof(struct fcfResx)*2*FCF_MAX_RES_COU_FAST)
#define I960_MAX_RES (sizeof(fastMem->fcfStore))

	if(RES_COU > I960_MAX_RES) {
		LOG(CRIT,"Error in MAX_RES_COU size %d > %d!!!",RES_COU,I960_MAX_RES,0,0,0) ;
	}

	// hand adjusted!!! Beware of changes!!!
	struct fcfResx *res_fast[2] = {
		(struct fcfResx *) fastMem->fcfStore,
		(struct fcfResx *) &(fastMem->fcfStore[(sizeof(struct fcfResx)/4)*FCF_MAX_RES_COU_FAST]) } ;
		

	//LOG(WARN,"size %d, count %d at %u %u (max %d bytes)",sizeof(struct fcfResx),FCF_MAX_RES_COU_FAST,(UINT32)res_fast[0],(UINT32)res_fast[1],I960_MAX_RES) ;

#endif


	u_int i, j ;
	for(i=0;i<2;i++) {
		for(j=0;j<FCF_MAX_RES_COU_FAST;j++) {
			resx[i][j] = res_fast[i] + j ;
			//LOG(WARN,"B %d:%d = 0x%08X",i,j,resx[i][j],0,0) ;
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
	register u_int ch_min = chargeMinCorrect ;
	register int do_cuts = doCuts ;
	u_int fla, cha ;
	u_int pad_c ;
	u_int time_c ;
	u_int t_cha ;

	for(i=0;i<cou;i++) {
		struct fcfResx *rr ;

		rr = res_p[i] ;


		fla = rr->flags ;
		cha = rr->charge ;


		// skip single pad hits
		// as well as those with too little charge
		//LOG(NOTE,"saveRes: flags 0x%X, charge %d",fla,cha,0,0,0) ;

		if(do_cuts) {
			if(fla & FCF_BROKEN_EDGE) {	// always pass!
				;
			}
			else if(fla & (FCF_ROW_EDGE | FCF_DEAD_EDGE | FCF_ONEPAD)) continue ;	// kill!
			else if(cha <= ch_min) continue ;	// kill!
			else if(rr->t1 == 0) continue ;		// kill if they touch timebin 0
			else if((rr->t2-rr->t1) <= 3) continue ;
		}


		if((cha == 0) || (cha > 0x7FFFFFFF)) {
			//LOG(WARN,"Bad charge: pad 0x%08X, time 0x%08X, charge 0x%08X, flags 0x%04X",rr->pad,rr->t,rr->charge,fla,0) ;
			continue ;
		}


		pad_c = rr->pad ;
		time_c = rr->t ;

		if(pad_c > 0x04000000) {
			t_cha = cha >> 6 ;

			if(t_cha) {
				pad_c = (pad_c + t_cha/2) / t_cha  ;
			}
			else {
				pad_c = ((pad_c + cha/2) / cha) << 6 ;
			}

			//LOG(WARN,"Precision lost: orig pad %u, orig cha %u, t_cha %u, pad_c %u",rr->pad,cha,t_cha,pad_c,0) ;
		}
		else {
			// increased resolution
			pad_c = ((pad_c << 6) + (cha/2)) / cha ;
		}

		if(time_c > 0x04000000) {
			t_cha = cha >> 6 ;

			if(t_cha) {
				time_c = (time_c + t_cha/2) / t_cha ;
			}
			else {
				time_c = ((time_c + cha/2) / cha) << 6 ;
			}
		}
		else {
			// increase resoultion
			time_c = ((time_c << 6) + (cha/2)) / cha ;	
		}


		// do the integerized rounded division
		cha = (cha+32)/64 ;



		if((pad_c > 0xFFFF) || (time_c > 0xFFFF)) {
			//LOG(WARN,"Overload pad 0x%08X, time 0x%08X, charge 0x%08X, flags 0x%04X - discarding ",pad_c,time_c,cha,fla,0) ;
			continue ;
		}

		if(cha > 0xFFFF) {
			//LOG(WARN,"Charge too big 0x%08X - setting to 0xffff (pad 0x%08X, time 0x%08X)...",cha,rr->pad,rr->t,0,0) ;
			cha = 0xFFFF ;
			fla |= FCF_DOUBLE_T | FCF_DOUBLE_PAD ;	// mark it somehow!
		}

		// clear the FALLING flag 
		fla &= (~FCF_FALLING) ;


		// thse bits should never be set!!! - will use them for flags!
		if((pad_c & 0xc000) || (time_c & 0x8000)) {
			LOG(ERR,"Strange pad 0x%04X, time 0x%04X...",pad_c,time_c,0,0,0) ;
			continue ;
		}


		u_int p = (pad_c >> 6)  ;
		u_int fl ;
		
		u_int p1 = p - rr->p1 ;
		u_int p2 = rr->p2 - p ;

		if(p1 > 7) p1 = 7 ;
		if(p2 > 7) p2 = 7 ;


//		if((p1 == 15) || (p2 ==15)) {
//			fprintf(stderr,"--- %d %d %d %d %d - 0x%04X - ",rr->pad,rr->charge,p,rr->p1,rr->p2,fla) ;
//			fprintf(stderr,"%f\n",(double)pad_c/64.0) ;
//		}


		fl = (p1 << 8) | (p2 << 11)  ;


		p = (time_c >> 6)  ;

		
		p1 = p - rr->t1 ;
		p2 = rr->t2 - p ;

		if(p1 > 15) p1 = 15 ;
		if(p2 > 15) p2 = 15 ;

//		if((p1 == 15) || (p2 ==15)) {
//			fprintf(stderr,"--- %d %d %d %d %d - 0x%04X - ",rr->t,rr->charge,p,rr->t1,rr->t2,fla) ;
//			fprintf(stderr,"%f\n",(double)time_c/64.0) ;
//		}

		fl |= (p2 << 4) | p1 ;


//		printf("%d %f %f %d %d %d %d %d %d %d %d %d\n",row,(double)(pad_c)/64.0-1.0,(double)time_c/64.0-0.5,
//		       cha, fla, rr->p1, rr->p2, rr->t1, rr->t2, rr->pix, rr->adc_max, rr->id) ;



		if(fla & FCF_ONEPAD) time_c |= 0x8000 ;
		if(fla & (FCF_DOUBLE_T | FCF_DOUBLE_PAD)) pad_c |= 0x8000 ;
		if(fla & FCF_DEAD_EDGE) pad_c |= 0x4000 ;

		if(fla & FCF_ROW_EDGE) fl |= 0x8000 ;
		if(fla & FCF_BROKEN_EDGE) fl |= 0x4000 ;

//		fla = fl ;



		// watchout for ordering!
		*output++ = (time_c << 16) | pad_c ;
		*output++ = (cha << 16) | fl ;

#ifdef FCF_SIM_ON
		//if(fla==0) printf("PIX %d %d %d %d %d\n",rr->cl_id,fla,cha,rr->pix,rr->adc_max) ;

		{
		int i, j ;

		int quality = 1 ;

		if(simout) {

			u_int sim_cha, all_cha ;

			sim_cha = all_cha = 0 ;

			for(i=1;i<=182;i++) {
				for(j=0;j<512;j++) {
					if(pixStruct[i][j].cl_id == rr->cl_id) {
						if(rr->id == pixStruct[i][j].id_simtrk) {
							sim_cha += pixStruct[i][j].adc ;
						}
						all_cha += pixStruct[i][j].adc ;
					}
				}
			}

			if(all_cha) {
				quality = (int)(100.0*(double)sim_cha/(double)all_cha) ;
			}
			else {
				quality = 0 ;
			}


			struct FcfSimOutput *s = (struct FcfSimOutput *) simout ;

			s->id_simtrk = rr->id ;
			s->id_quality = quality ;

			s->cl_id = rr->cl_id ;	// put the local cluster id

			simout += sizeof(struct FcfSimOutput)/4 ;	// advance here
		
		}

#if defined(FCF_ANNOTATE_CLUSTERS) && !defined(__ROOT__)
		for(i=1;i<=182;i++) {
			for(j=0;j<512;j++) {
				if(pixStruct[i][j].adc) {
					fcfPixA[row-1][i-1][j] = pixStruct[i][j] ;
				}
			}
		}
#endif
		}

#endif	// FCF_SIM_ON!




		//LOG(WARN,"time 0x%02X, pad 0x%02X, cha 0x%02X, fla 0x%02X",
		//   time_c, pad_c, cha, fl,0) ;

		saved++ ;


	}

	//LOG(DBG,"saveRes saved %d clusters...",saved,0,0,0,0) ;
	return saved ;
}

