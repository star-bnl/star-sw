/*
	Version 3.00	01/15/2002	Major revamp
	Version 3.10	01/16/2002	Revamped major internal strucutres
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h>


#ifdef __ROOT__

#define FCF_10BIT_ADC
#define DONT_USE_LOG

#endif


// use for systems without the RTS Logging facility!
//#define DONT_USE_LOG
#ifdef DONT_USE_LOG
#define LOG(x1,x2,x3,x4,x5,x6,x7)
#else
#include <rtsLog.h>
#endif



#include <daqFormats.h>
#include "rtsSystems.h"

#include "fcfClass.hh"

#define FCF_VERSION	"3.11"

#define FCF_MAX_RES_COU_FAST	16


struct fcfResx {
	u_int t ;
	u_int pad ;
	u_int charge ;
	int scharge ;

	u_char flags ;		// u_char
	u_char falling ;	// u_char
	short mean ;		// short


	// new - test only...
	u_short	 t1, t2, p1, p2 ;	
	u_int pix ;		// unused?
} ;


#ifdef unix




#define preburst(x)	// meaningless for UNIX



extern __inline volatile void mstore(struct fcfResx *rr, u_int av, u_int pad, u_int ch, u_int flags, int mean)
{

	rr->t = av ;
	rr->pad = pad ;
	rr->charge = rr->scharge = ch ;

	rr->flags = flags ;
	rr->falling = 0 ;
	rr->mean = mean ;


/*
	*r++ = av ;
	*r++ = pad ;
	*r++ = ch ;
	*r++ = flags ;
	*r++ = mean ;
	*r++ = 0 ;	// falling
	*r++ = ch ;	// scharge
*/
	return ;
}

#else	// I960



extern __inline volatile void preburst(u_int where)
{
	__asm__ volatile ("ldq (%0), r8" : : "r" (where) : "r8", "r9", "r10", "r11") ;

	return ;
}




extern __inline volatile void mstore(struct fcfResx *r, u_int av, u_int pad, u_int ch, u_int flags, int mean)
{
	__asm__ volatile (" \
	\n \
	lda (%2),r8 \n \
	mov %3,r9 \n \
	lda (%4),r10
	mov %5,r11 \n \
	\n \
	stq r8,(%0) \n \
	\n \
	lda 16(%0),%0 \n \
	\n \
	mov %6,r8 \n \
	ldconst 0,r9 \n \
	\n \
	stt r8,(%0) " \
	: "r=" (r) : "0" (r), "r" (av), "r" (pad), "r" (ch), "r" (flags), "r" (mean) : "r8", "r9", "r10", "r11", "cc") ;

	return ;
}

#endif	// UNIX


int fcfClass::finder(u_char *adcin, u_short *cppin, u_int *outres)
{
	int i, j, pad ;

	// misc. debugging
	int error ;
	int slow_copies ;
	int new_time = 0 ;


	int new_res_ix, new_res_cou, old_res_cou ;


	u_int *cl_found_pointer, *row_pointer ;

	u_int f_start, f_stop ;

	struct fcfResx *new_res, *old_res, *new_res_slow, *old_res_slow ;
	static struct fcfResx res_slow[2][512] ;
#ifdef unix
	static struct fcfResx res_fast_ux[2][FCF_MAX_RES_COU_FAST] ;
	struct fcfResx *res_fast[2] = { res_fast_ux[0], res_fast_ux[1] } ;
#else
	struct fcfResx *res_fast[2] = { (struct fcfResx *) 0x00002000, (struct fcfResx *)0x00001000 } ;
#endif

	row_pointer = outres++ ;	// mark the row
	cl_found_pointer = outres++ ;	// mark the count


	*cl_found_pointer = 0 ;
	*row_pointer = row ;

        error = 0 ;     // clear padrow error

	new_res_ix = 0 ;
	new_res_cou = old_res_cou = 0 ;



	slow_copies = 0 ;

	new_res = old_res = new_res_slow = old_res_slow = NULL ;


	for(pad=padStart;pad<=padStop;pad++) {
		register u_int start, stop ;
		register u_short *cv ;

		register int start_new ;

		// HACK
		u_int cl_counter ;
		u_int prev_start ;

#ifdef FCF_10BIT_ADC
		u_short *val = (u_short *)((u_int)adcin + adcOff[pad]) ;
#else
		u_char *val = (u_char *)((u_int)adcin + adcOff[pad]) ;
#endif
		cv = (u_short *)((u_int)cppin + cppOff[pad]) ;

		preburst((u_int)cv) ;

		// swap the banks...
		if(new_res_ix == 1) {
			new_res_ix = 0 ;

			old_res_slow = res_slow[1] ;
			old_res = res_fast[1] ;

			new_res_slow = res_slow[0] ;
			new_res = res_fast[0] ;
			
		}
		else {
			new_res_ix = 1 ;
			old_res_slow = res_slow[0] ;
			old_res = res_fast[0] ;

			new_res_slow = res_slow[1] ;
			new_res = res_fast[1] ;


		}


		preburst((u_int)cv+16) ;

		old_res_cou = new_res_cou ;
		new_res_cou = 0 ;




		

		start_new = -1 ;


		prev_start = 0 ;

		LOG(DBG,"Doing row %d, pad %d: ADC 0x%08X, CPP 0x%04X",row,pad,adcOff[pad],cppOff[pad],0) ;



		register u_int av ;
		int charge ;
		register int mean, pix_count ;
		register u_int flags ;
		register int last_falling ;
		int asicBug ;


		preburst((u_int)cv+32) ;


		for(cl_counter=0;cl_counter<maxCPP;cl_counter++) {

			start = (u_int)*cv++ ;
			if(start & 0xFE00) break ;

			// debugging check!

			if((start <= prev_start) && (cl_counter !=0)) {
				LOG(WARN,"start 0x%X < prev_start 0x%X, cl_counter %d",start,prev_start,cl_counter,0,0) ;
				return -1 ;
			}
			prev_start = start ;


			stop = (u_int) *cv++ ;

			// paranoia
			stop &= 0x1FF ;

			// ASIC SVT Bug
			if((cl_counter==7) && (detector == SVT_ID)) {	// ASIC bug
				stop &= 0x7F ;
			}

			// ASIC Bug
			if(start == maxTimebin) stop = maxTimebin ;

			// Another ASIC BUG - but just mark
			if(stop == maxTimebin) asicBug = 1 ;
			else asicBug = 0 ;

			// debugging check!
			if((stop < start) || (stop > maxTimebin)) {
				LOG(WARN,"stop %d < start %d, cl_counter %d",stop,start,cl_counter,0,0) ;
				return -1 ;
			}



			LOG(DBG,"start %d, stop %d, cl_counter %d",start,stop,cl_counter,0,0) ;

			// ignore certain ranges
			if((stop < timebinLo) || (start > timebinHi)) {
				LOG(DBG,"Ignoring cluster start %d, stop %d",start,stop,0,0,0) ;
				continue ;
			}


			last_falling = flags = 0 ;


		
			redo: ;	// returns here if we broke the sequence due to a time 2peak...

			new_time++ ;
			if(deconTime) {
				if(start_new > -1) {	// starts from broken time...
					start = start_new ;
					start_new = -1 ;
					flags = FCF_DOUBLE_T ;
				}
				else {
					flags = 0 ;
				}
				// in any case
				last_falling = 0 ;
			}

		


#ifdef FCF_10BIT_ADC
			u_short *val1 = val + start ;   // get the pointer to ADC data
#else
			u_char *val1 = val + start ;   // get the pointer to ADC data
#endif

			preburst((u_int)val1) ;

			register u_short *adc8to10 ;
			int last_a ;
			int max_a  ;

			// move it to a register - help the compiler
			adc8to10 = a8to10 ;
				
			pix_count = mean = max_a = av = charge = last_a = 0 ;

			f_start = start ;
			f_stop = stop ;
		
			for(;start<=stop;start++) {
				register u_int a ;
				register int aa  ;

				// store the ADC data in a register
				if(detector==SVT_ID) {	// special for SVT
					aa = *(val + start*4) ;
				}
				else {
					aa = *val1++ ;     
				}

			
				//printf("VALS pad %d: time %d, val %d; %d %d\n",pad,start,aa,f_start,f_stop) ;
				// due to potential ASIC/VRAM errors
				// we=ll check for 0 in the data which should not be possible
				if(aa == 0) {
					// skip the 511 case - bug in the ASIC...
					if(asicBug) {
						;
					}
					else {
						LOG(WARN,"0 ADC value??? start %d, stop %d, new %d, cpp count %d",start,stop,start_new,cl_counter,0) ;
						//return -1 ;
					}
				}


				if(deconTime) {
				  //printf("deconTime P %d, time %d: T %d , aa %d, last_a %d, start %d, last_falling %d, max_a %d, mean %d => ",
				  //					       pad,start,new_time,aa,last_a,start,last_falling,max_a,mean) ;

					if(last_falling) {
						if(aa > (last_a + minAdcT)) {
							start_new = start ;
							f_stop = start -1 ;
							//printf(" Break\n") ;
							break ;
						}
					}
					else {
						if(last_a && (aa < (last_a - minAdcT))) {
						  //printf("MARK!") ;
							last_falling = 1 ;	// Im falling
						}
						
					}
				
					last_a = aa ;

					//printf("\n") ;
				}

			
				if(aa >= max_a) {
					max_a = aa ;
					mean = start ;
				}

#ifdef FCF_10BIT_ADC
				a = aa ;
#else
				a = adc8to10[aa] ;  // transfer to 10 bits
#endif
			

				// special SVT handling!
				if(svtPedestal) {
					// subtract the pedestals pedestal
					if(a <= svtPedestal) a = 0 ;
					else a -= svtPedestal ;
				}


				charge += a ;
				av += start * a ;
				pix_count++ ;
			}

				
			if(deconTime && (start_new > -1)) {
				flags = FCF_DOUBLE_T ;
			}




			struct fcfResx *rn ;

			if(new_res_cou >= FCF_MAX_RES_COU_FAST) {
				rn = new_res_slow + new_res_cou ;
				slow_copies++ ;
			}
			else {
				rn = new_res + new_res_cou ;
			}
					
			mstore(rn,av,charge*pad,charge,flags|FCF_ONEPAD,mean) ;
			rn->p1 = rn->p2 = pad ;
			rn->t1 = f_start ;
			rn->t2 = f_stop ;
			rn->pix = pix_count ;

			new_res_cou++ ;

			
		
			if(deconTime && (start_new > -1)) goto redo ;

		}	// end of the pad or cl_counter


		if(new_res_cou >= FCF_MAX_RES_COU_FAST) {
			LOG(WARN,"New results %d (> %d)!",new_res_cou,FCF_MAX_RES_COU_FAST,0,0,0) ;
		}

		//printf("PAD %d, new_results %d, old_results %d\n",pad,new_res_cou,old_res_cou) ;


		int new_start = 0 ;

		for(i=0;i<old_res_cou;i++) {	// loop over old sequneces
			struct fcfResx *rr ;

			if(i >= FCF_MAX_RES_COU_FAST) {
				rr = &old_res_slow[i] ;
			}
			else {
				rr = &old_res[i] ;
			}

			int merged = 0 ;

			int old_mean = rr->mean ;
			int old_mean_m = old_mean - param1 ;
			int old_mean_p = old_mean + param1 ;

			start = new_start ;
		
			for(j=start;j<new_res_cou;j++) {
				struct fcfResx *nresx ;

				if(j >= FCF_MAX_RES_COU_FAST) {
					nresx = &new_res_slow[j] ;
				}
				else {
					nresx = &new_res[j] ;
				}


				mean = nresx->mean ;
			
				if(mean < old_mean_m) {
					new_start = j + 1 ;
					continue ;
				}
				else if(mean <= old_mean_p) {	

					charge = nresx->charge ;
					if(deconPad) {
					  //printf("decon P %d : charge %d >? scharge %d, falling? %d => ",
					  //       pad,charge,rr->scharge,rr->falling) ;

						if(rr->falling) {
						  //printf(" FALLING ") ;
							if(charge > (rr->scharge + minAdcPad)) {
								u_int sc_tmp, sc_p_tmp ;

								sc_tmp = rr->scharge / 2 ;
								sc_p_tmp = sc_tmp * (pad-1) ;

								nresx->flags |= FCF_DOUBLE_PAD ;	// new one
								nresx->p1-- ;

								// the new one will get half of previous share

								nresx->charge += sc_tmp ;
								nresx->pad += sc_p_tmp ;
								nresx->t += mean * sc_tmp ;

								rr->charge -= sc_tmp ;
								rr->pad -= sc_p_tmp ;
								rr->t -= rr->mean * sc_tmp ;

								rr->flags |= FCF_DOUBLE_PAD ;	// old one
								//printf("BREAK\n") ;

								new_start = j+1 ;
								break ;	// and create a new one; break out of old results scan

							}
							// maintain "falling" character
							nresx->falling = 1 ;
						}
						else {
							if(rr->scharge && (charge < (rr->scharge - minAdcPad))) {
							  //printf("MARK!") ;
								nresx->falling = 1 ;
							}

						}



					}
				



					//printf(" MERGED: charge %d\n",charge) ;
					merged = 1 ;

					nresx->flags |= rr->flags ;
					nresx->flags &= (~FCF_ONEPAD) ;  // ...clear the one pad flag
			
					f_stop = rr->t2 ;
					f_start = rr->t1 ;

					nresx->scharge = charge ;	// last pad charge
					nresx->charge += rr->charge ;
					nresx->pad += rr->pad ;
					nresx->t += rr->t ;
					// mean stays the same..

					nresx->p2 = pad ;
					nresx->p1 = rr->p1 ;

					if(f_stop > nresx->t2) nresx->t2 = f_stop ;
					if(f_start < nresx->t1) nresx->t1 = f_start ;

					nresx->pix += rr->pix ;


					new_start = j + 1 ;
					break ;
				}
				else {
					new_start = j ;
					break ;
				}
			
			}

			if(!merged) {
				struct fmt21_c {
					u_short x ;
					u_short t ;
				} *c ;
				struct fmt21_f {
					u_short f ;
					u_short c ;
				} *f ;

			
				u_int tmp1, tmp2 ;
				u_int cha ;
				u_int fla ;

				cha = rr->charge ;
				fla = rr->flags ;

				//printf("Trying to store cluster %d - ",i) ;

				if(doCuts) {
					// skip single pad hits
					// as well as those with too little charge

					if((fla & FCF_ONEPAD) || (cha <= chargeMin)) {
						if(cha <= 0) {	// now that's an error
							LOG(ERR,"Charge <= 0 (%d)???",cha,0,0,0,0) ;
						}
					
					//printf("JUNKED\n") ;
					continue ;
					}
				}

				*cl_found_pointer += 1 ;
				//printf("STORED (%d)\n",*cl_found_pointer) ;


				tmp1 = (rr->pad << 6) / cha ;

				c = (struct fmt21_c *) outres++ ;			
				f = (struct fmt21_f *) outres++ ;

				f->f = fla ;
				f->c = cha ;

				tmp2 = (rr->t << 6) / cha ;

				c->x = tmp1 ;
				c->t = tmp2 ;

				tmp1 >>= 6 ;
				tmp2 >>= 6 ;




				int t1, t2, p1, p2 ;
				t1 = (tmp2 - rr->t1) ;
				t2 = (rr->t2 - tmp2) ;
				p1 = (tmp1 - rr->p1) ;
				p2 = (rr->p2 - tmp1) ;

				f->f = (p1 << 12) | (p2 << 8) | (t1 << 4) | t2 ;
				//printf("==> %d %d %d %d %f %f\n",rr->t1,rr->t2,rr->p1,rr->p2,(double)c->t/64.0,(double)c->x/64.0) ;
				//printf("    %d %d %d %d (t %d, p %d)\n",t1,t2,p1,p2,tmp2,tmp1) ;
				//printf("    PIX %d\n",rr->pix) ;
			}
		}

	}// for minpad to maxpad
	

	// store _all_ the old results
	for(i=0;i<new_res_cou;i++) {
		struct fcfResx *rr ;

		if(i >= FCF_MAX_RES_COU_FAST) {
			rr = &new_res_slow[i] ;
		}
		else {
			rr = &new_res[i] ;
		}


		struct fmt21_c {
			u_short x ;
			u_short t ;
		} *c ;
		struct fmt21_f {
			u_short f ;
			u_short c ;
		} *f ;

			
		u_int tmp1, tmp2 ;
		u_int cha ;
		u_int fla ;

		cha = rr->charge ;
		fla = rr->flags ;

		//printf("Trying to store cluster %d - ",i) ;

		if(doCuts) {
			// skip single pad hits
			// as well as those with too little charge

			if((fla & FCF_ONEPAD) || (cha <= chargeMin)) {
				if(cha <= 0) {	// now that's an error
					LOG(ERR,"Charge <= 0 (%d)???",cha,0,0,0,0) ;
				}
			
				//printf("JUNKED %d %d %d\n",fla,cha,chargeMin) ;
			continue ;
			}
		}

		*cl_found_pointer += 1 ;
		//printf("STORED (%d)\n",*cl_found_pointer) ;


		tmp1 = (rr->pad << 6) / cha ;

		c = (struct fmt21_c *) outres++ ;			
		f = (struct fmt21_f *) outres++ ;

		f->f = fla ;
		f->c = cha ;

		tmp2 = (rr->t << 6) / cha ;

		c->x = tmp1 ;
		c->t = tmp2 ;

		tmp1 >>= 6 ;
		tmp2 >>= 6 ;




		int t1, t2, p1, p2 ;
		t1 = (tmp2 - rr->t1) ;
		t2 = (rr->t2 - tmp2) ;
		p1 = (tmp1 - rr->p1) ;
		p2 = (rr->p2 - tmp1) ;

		f->f = (p1 << 12) | (p2 << 8) | (t1 << 4) | t2 ;
		//printf("==> %d %d %d %d %f %f\n",rr->t1,rr->t2,rr->p1,rr->p2,(double)c->t/64.0,(double)c->x/64.0) ;
		//printf("    %d %d %d %d (t %d, p %d)\n",t1,t2,p1,p2,tmp2,tmp1) ;
		//printf("    PIX %d\n",rr->pix) ;

	}

	return (outres - row_pointer) ;	// length in qwords

} 


fcfClass::fcfClass(int det, u_short *table = NULL) 
{
	detector = det ;

#ifdef unix
	a8to10 = adc8to10_storage ;

#else	// I960
	a8to10 = (u_short *) 0x100 ;		// ADC conversion in fast memory
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
		timebinHi = 511 ;
		chargeMin = 40 ;
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
		LOG(ERR,"Cluster Finder can't work with DET Type %d",det,0,0,0,0) ;
		return ;
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

