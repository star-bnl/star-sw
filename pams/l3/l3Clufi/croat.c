/*//
//      7/21/99 struck. first offline verison
//
//------------------------------------------------------------
//	7/1/99	Tonko. first working version...
//------------------------------------------------------------
*/

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <stdlib.h>

/* Run as unix offline version */
#define UNIX_OFFLINE

/* Due to a bug in the VRAM must NOT use bursts!!!!! */
#define VRAM_BUG


/* must be present before the TPC defines... */
#define ROWS		45
#define PADS_PER_ROW	182 /* Max_PADS = 184 !!! */

#include "trans_table.h"
#include "offsets.h"
#include "padfinder.h"
#include "daqFormats.h"

#include "croat.h"


#define register	/* noticed that it's better to let the compiler decide... */


/* STATICS */

/* the "real" padrow number [1..45] */
static int rowAbs[MAX_LOGICAL_ROWS] ;
/* first pad in the padrow */
static int rowStart[MAX_LOGICAL_ROWS] ;
/* last pad in the padrow */
static int rowEnd[MAX_LOGICAL_ROWS] ;

#ifndef UNIX_OFFLINE
static uint adcOff[MAX_LOGICAL_ROWS][PADS_PER_ROW] ;
static ushort cppOff[MAX_LOGICAL_ROWS][PADS_PER_ROW] ;
#else
extern uint adcOff[ABS_ROWS][MAX_P] ;
extern uint cppOff[ABS_ROWS][MAX_P] ;
#endif


/* local results */
static struct resx 
{
    uint t ;
    uint pad ;
    uint charge ;
    uint flags ;

    int    mean ;
    uint falling ;
    uint scharge ;

} resx[1000] ;

#ifndef UNIX_OFFLINE 
/* structures which reside in the on-chip CPU memory */
/* careful!!!! NEVER CHANGE! */
static ushort *adc8to10 = (ushort *) 0x100 ;	/* size 512 bytes */
static uint *pres1 = (uint *) 0x300 ;		/* size 256 bytes (i.e. MAXIMUM 64 old clusters) */
static uint *pres2 = (uint *) 0x400 ;		/* size 256 bytes (i.e. MAXIMUM 64 new clusters) */
static struct resx *rr = (struct resx *) 0x500 ;	/* sizeof(resx[0]) */
#else  /* UNIX_OFFLINE */
extern ushort adc8to10[256];
static uint pres1[500], pres2[500] ;
static struct resx rr_local ;
static struct resx *rr = &rr_local ;
#endif


/* These assembly routines are meant to speed up excecution on an Intel I960 */
#ifndef UNIX_OFFLINE

#ifdef VRAM_BUG
extern __inline volatile void preburst(uint where)
{
    __asm__ volatile ("ld (%0), r8 " : : "r" (where) : "r8" ) ;
    where += 4 ;
    __asm__ volatile ("ld (%0), r9 " : : "r" (where) : "r9" ) ;
    where += 4 ;
    __asm__ volatile ("ld (%0), r10 " : : "r" (where) : "r10" ) ;
    where += 4 ;
    __asm__ volatile ("ld (%0), r11 " : : "r" (where) : "r11" ) ;

    return ;

}
#else
extern __inline volatile void preburst(uint where)
{
    __asm__ volatile ("ldq (%0), r8" : : "r" (where) : "r8", "r9", "r10", "r11") ;

    return ;
}
#endif

extern __inline volatile void memcpy7(uint *dst, uint *src)
{
    __asm__ volatile (" \
\n \
ldq (%0),r8 \n \
stq r8, (%1) " : : "r" (src), "r" (dst) : "r8", "r9", "r10", "r11" ) ;

    src += 4 ;
    dst += 4 ;

    __asm__ volatile (" \
\n \
ldt (%0),r8 \n \
stt r8,(%1) " : : "r" (src), "r" (dst) : "r8", "r9", "r10", "r11" ) ;

    return ;
}


extern __inline volatile void mstore(uint *r, uint av, uint pad, uint ch, uint flags, int mean)
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

#else /* UNIX_OFFLINE */

void preburst(uint where)
{
    return ;
}

void memcpy7(uint *dst, uint *src)
{
    int i ;

    for(i=0;i<7;i++) {
	*dst++ = *src++ ;
    }

    return ;
}

void mstore(uint *r, uint av, uint pad, uint ch, uint flags, int mean)
{
    struct resx *rr = (struct resx *) r ;

    rr->mean = mean ;
    rr->charge = ch ;
    rr->flags = flags ;
    rr->scharge = ch ;
    rr->pad = pad ;
    rr->t = av ;
    rr->falling = 0 ;

    return ;
}

#endif  /* UNIX_OFFLINE */


int croatFinder(u_char *adcin, ushort *cppin, uint *outres)
{
    int i ;
    uint *padrows ;
    int cl_found ;
    uint rows ;

    register struct resx  *r ;


    padrows = outres ;	/* reserve this space */
    outres++ ;		/* and advance the counter */

    rows = 0 ;		/* count of rows that had data */
	
    /* loop over rows of this mezz */
    for(i=0; i<MAX_LOGICAL_ROWS; i++) 
	{
	    int j ;
	    register int pres_cou1, pres_cou2 ;
	    register uint *r1, *r2 ;
	    /* check if this row is to be done */
	    if(rowAbs[i] == 0) break ;

	    pres_cou1 = pres_cou2 = 0 ;

	    r1 = pres2 ;
	    r2 = pres1 ;
	    
	    /* in r = resx we will store the clustercandidats of this row */
	    r =  resx ;	/* start again per each row ... */

#define DO_STUFF
#ifdef DO_STUFF
	    /* loop over pads in this row */
	    for(j=rowStart[i]; j<=rowEnd[i]; j++) {

		register uint start, stop ;
		register ushort *cv ;
		register u_char *val ;
		register int start_new ;
		int go_on ;

		/* HACK */
		int cl_counter ;
		int prev_start ;

		/* set offsets for adc & cpp */
		val = adcin + adcOff[rowAbs[i]-1][j-1];
		cv  = cppin + cppOff[rowAbs[i]-1][j-1];

		/*  ??? */
		preburst((uint)cv) ;

		if(r2 == pres2) 
		    {
			r1 = pres2 ;
			r2 = pres1 ;
		    }
		else 
		    {
			r1 = pres1 ;
			r2 = pres2 ;
		    }
		
		/*  ???? */
		preburst((uint)cv+16) ;

		/* counters to result pointers */
		pres_cou1 = pres_cou2 ;
		pres_cou2 = 0 ;
		/* pres_cou1 is the previous pad */
		/* pres_cou2 is the current pad */


		preburst((uint)cv+32) ;


		start_new = -1 ;

		/* HACK */ 
		cl_counter = 0 ;
		prev_start = 0 ;

		/*fprintf(stderr,"Doing row %d, pad %d\n", i, j) ; */ 
			
		/*loop over sequenzes */
		/*we'll stop when start is LESS than 349 due to a bug in the ASIC */
		/*for start==349 */
		while((start = (uint)*cv++) < 349) 
		    {
			register u_char *val1 ;
			register uint av, charge ;
			register int mean ;
			register uint flags ;
			register int last_falling ;
			register int k ;
			register uint *ri ;
			uint tmp_charge ;


			/* HACK */
			cl_counter++ ; /* cluster counter */
			if(cl_counter > 31) break ; /* too many clusters on this pad */

			/* HACK */
			if(start < prev_start) break ; 
			prev_start = start ;

			/* get the stop bucket */
			stop = (uint) *cv++ ;
		       	
		       	/* exclude possible nonsense situations */
			if(stop < start) stop = start ;
			if(stop > 349) stop = 349 ;
			
			/* sequenz is not longer than 3 consequtive timebuckets ... */
			if((stop-start)>3) stop = start + 3 ;
			if(stop > 349) stop = 349 ;
			

				

#ifdef DECONVOLUTE_TIME
			/* this is a goto label ... */
		    redo: ;

			/* divide one pixelsequenz into two */  
			if(start_new > -1) 
			    {
				start=start_new ;
				start_new = -1 ;
			    }
#endif
			/* average and charge 0 */
			av = charge = 0 ; 

			go_on = 1 ;

			val1 = val + start ;


#ifdef DECONVOLUTE_TIME
			/* flags = 0 */
			last_falling = flags = 0 ;
#endif
#ifdef DECONVOLUTE_PAD
			flags = 0 ;
#endif
			
			/* block to introduce new variables */
			{
			    register ushort *a8to10 ;
			    register uint last_a ;
			    a8to10 = adc8to10 ;

			    last_a = 0 ;

			    /* loop over this sequenz */
			    for(;start<=stop;start++) 
				{
				    
				    

				    register uint a ;
				    register uint aa  ;

				    aa = *val1++ ;
				
#ifdef DECONVOLUTE_TIME 
				    /* check if last pixel in this sequenz is bigger or smaller */
				    /* than this one */
				    if(aa > last_a) 
					{
					    if(last_falling) 
						{
						    start_new = start ;
						    break ;
						}
					}
				    else last_falling =  1;
				
				    last_a = aa ;
#endif
				    /*dominik change:a = a8to10[aa] ;*/
				    a = log8to10_table[aa] ;
				    
				    /* sum of adc-values equals total charge of this cluster */
				    charge += a ; 
				    
				    av += start* (a) ;
				}
			
			}
#ifdef DECONVOLUTE_TIME
			if(start_new > 0) flags = FLAG_DOUBLE_T ;
#endif
			/* HACK */
			if(charge) 
			    {
				/* mean := center of gravity for this sequenz */
				mean = av/charge ;
			    }
			else 
			    {
				charge = 1 ;
				mean = 1 ;
			    }

			ri = r1 ;	/* point to old results */
		    
			tmp_charge = charge * j;

			/*if(pres_cou1 || pres_cou2) LOG(DBG,"before - pres_cou1 %d [%d]",pres_cou1,pres_cou2,0,0,0) ;*/
			/*if(pres_cou1 >60) pres_cou1 = 60 ; */

				/* compare with previously stored results*/
				
			for(k=0;k<pres_cou1;k++) 
			    {
				register int v ;
				register struct resx *rr_tmp ;
			    
				rr_tmp = (struct resx *) *ri ;
				
				v = mean - rr_tmp->mean ;

								    
				/* since the results are ordered in rising order
				// we can break as soon as the previos is larger than
				// this one...
				// this leads to a new cluster */
				if(v < -PARAM1) break ;
						
				ri++ ;
			    
				/*					if(v < 0) v = -v ;*/

				if(v<=PARAM1) 
				    {
					/* OK - got one to continue with so
					   // we'll "add" the new and the old*/
				    
					/* make a temp. copy to on-CPU RAM (rr)*/
					memcpy7((uint *)rr,(uint *)rr_tmp) ;

					/* in case we found another sequenz-section on this pad
  					 don't loop over all sequenzes on last pad but start 
					 where we found a matching candidate for this sequenz-section*/
					r1 = ri ;
					pres_cou1 -= 1+k ;
					/*LOG(DBG,"after - pres_cou1 %d [%d]",pres_cou1,pres_cou2,0,0,0) ;*/

				    
#ifdef DECONVOLUTE_PAD
					if(charge > rr->scharge) 
					    {
						if(rr->falling) 
						    { /* previous state was falling*/
							flags |= FLAG_DOUBLE_PAD ;
							rr_tmp->flags |= flags ;
							break ;	/* and create a new one*/
						    }
					    }
					else 
					    {
						rr->falling = 1 ;	/* falling */
					    }
#endif

					/* don't create a new one*/
					go_on = 0 ;
				    
					/*if(pres_cou1 || pres_cou2) LOG(DBG,"1. pres_cou2 %d [%d]",pres_cou2,pres_cou1,0,0,0) ;
					//if(pres_cou2 >60) pres_cou2 = 60 ;
					// increment and store the pointer to the new one
					// in "2" */
					r2[pres_cou2++] = (uint) rr_tmp ;
				    
					rr->scharge = charge ;
					rr->flags &= (~FLAG_ONEPAD) ;
				    

					rr->charge += charge ;
					rr->pad += tmp_charge ;
					rr->t += av ;
					rr->mean = mean ;
				    
					/* copy it back to storage */
					memcpy7((uint *)rr_tmp,(uint *)rr) ;
				    
					break ;
				    
				    }
			    }


			/* here we create from scratch new clusters...*/
			if(go_on) 
			    {
				/* store this guy because it's the first*/
				/*if(pres_cou1 || pres_cou2) LOG(DBG,"2. pres_cou2 %d [%d]",pres_cou2,pres_cou1,0,0,0) ;*/
				/*if(pres_cou2 >60) pres_cou2 = 60 ;*/
					
				r2[pres_cou2++] = (uint )r ;
				mstore((uint *)r,av,tmp_charge,charge,flags|FLAG_ONEPAD,mean) ;
				r++ ;
			    }


#ifdef DECONVOLUTE_TIME
			if(start_new>=0) goto redo ;
#endif
		    }	/* for CPPs */
	    }	/* for minpad to maxpad */
	    
#endif	/* DO_STUFF */

	    cl_found = r - resx ;
	    /*LOG(DBG,"Found %d clusters in row %d",cl_found,i,0,0,0) ;*/
	    
#ifndef DO_STUFF
	    /* HACK*/
	    cl_found = 1 ;
#endif
	    /* here we come afte we examined one pad row */
	    if(cl_found == 0) continue ;	/* nothing in this row*/


	    /* there was something in this padrow...*/
	    rows++ ;

	    /* let's output the data in the 2.1 format
	    // we'll do it stupidly for now and later use assembly or something...
	    // now we are filling the TPCMZCLD DATA WORD 2 3*/
	    *outres++ = rowAbs[i] ;
	    *outres++ = cl_found ;

#ifdef DO_STUFF
	    /* now fill the found clusters in the TPCMZCLD bank */
	    for(j=0;j<cl_found;j++) 
		{
		    /* find the pointer where to store it */
		    struct fmt21_c 
		    {
			ushort x ;
			ushort t ;
		    } *c ;

		    struct fmt21_f 
		    {
			ushort f ;
			ushort c ;
		    } *f ;

		    c = (struct fmt21_c *) outres++ ;
		    f = (struct fmt21_f *) outres++ ;

		    /* if cluster is not empty*/
		    if(resx[j].charge) 
			{   
			    /* fill it in 1/64 units*/
			    /* to get the center of gravity we have to devide by charge */
			    c->x = (resx[j].pad << 6) / resx[j].charge ;
			    c->t = (resx[j].t << 6 )  / resx[j].charge ;
			}
		    else 
			{
			    /* if we don't find anything fill it with dummies */
			    c->x = 10 ;
			    c->t = 10 ;
			}
		    /* fill flags and charge */
		    f->f = resx[j].flags ;
		    f->c = resx[j].charge ;
		}
#else

	    *outres++ = rowStart[i] ;
	    *outres++ = rowEnd[i] ;
#endif

	}	/* for MAX_LOGICAL_ROWS */

    /* now we are done with this card */
    /* fill the first data word of TPCMZCLD DATA */
    *padrows = rows ;	

    /* return length in qwords */
    return (outres - padrows) ;	

} 

int croatInit(int myRB, int myMZ)
{
    /* this function fills the rowStart,rowEnd & rowAbs arrays */
    /* with the informations provided by padfinder.h file */
    int i, j ;
    int rel ;

    rel = 0 ;


    bzero((char *)resx, sizeof(resx)) ;


#ifndef UNIX_OFFLINE
    for(i=0; i<256; i++) {
	adc8to10[i] = log8to10_table[i] ;
    }
#endif

    bzero((char *)rowStart, sizeof(rowStart)) ;
    bzero((char *)rowEnd, sizeof(rowEnd)) ;
    bzero((char *)rowAbs, sizeof(rowAbs)) ;

#ifndef UNIX_OFFLINE
    bzero((char *)adcOff, sizeof(adcOff)) ;
    bzero((char *)cppOff, sizeof(cppOff)) ;
#endif

    /* fprintf(stderr, "Initializing RB %d, MZ %d\n", myRB, myMZ); */
	
    /* loop over rows */
    for(i=1;i<=45;i++) 
	{
	    /* loop over up to 3 different mz cards which are responsible for this row */
	    for(j=0;j<3;j++) 
		{
		    /* fprintf(stderr, "RB%d, row %d, j %d: rdo %d, mz %d\n", */
		       /*	myRB, i, j, padfinder[i-1][j].rdo, padfinder[i-1][j].mezz);  */
		    if(padfinder[i-1][j].rdo == 0) break ;
		    if((padfinder[i-1][j].rdo == myRB) && (padfinder[i-1][j].mezz == myMZ)) {
			rowAbs[rel] = i ;
			rowStart[rel] = padfinder[i-1][j].minpad ;
			rowEnd[rel] = padfinder[i-1][j].maxpad ;
			rel++ ;
			if(rel > MAX_LOGICAL_ROWS) {
			    fprintf(stderr, "Too many logical rows!\n");
			    return -1 ;
			}
			else {
			    /*fprintf(sterr, "Logical row %d-> abs %d, start %d, end %d\n", */
			    /*	  rel, rowAbs[rel-1], rowStart[rel-1], rowEnd[rel-1]); */
			}
		    }
		}
	}

#ifndef UNIX_OFFLINE
    for(i=0;i<MAX_LOGICAL_ROWS;i++) {
	if(rowAbs[i] == 0) break ;	/* no more rows*/
	for(j=rowStart[i];j<=rowEnd[i];j++) {
	    cppOff[i][j] = (adc_offset[i][j] & 0x1FF) << 5 ;
	    adcOff[i][j] = (adc_offset[i][j] & 0x1FF) << 9 ;
	}
    }
#endif


    return rel ;	/* number of logical rows... */
}

