//      1/02/00 flierl cleaned up offline version
//------------------------------------------------------------
//      7/21/99 struck. first offline verison
//------------------------------------------------------------
//	7/1/99	Tonko. first working version...
//------------------------------------------------------------


#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <stdlib.h>
#include <Rtypes.h> /* use ROOT variables: ..._t */




/*  must be present before the TPC defines... */
/*  these defines go into offset.h            */
//#define ROWS		45
//#define PADS_PER_ROW	182 
/* Max_PADS = 184 !!! */

#include "trans_table.h"
#include "padfinder.h"
#include "croat.h"


struct resx 
{
    UInt_t t ;
    UInt_t pad ;
    UInt_t charge ;
    UInt_t flags ;
    Int_t  mean ;
    UInt_t falling ;
    UInt_t scharge ;
} ;



void preburst(UInt_t where)
{
    return ;
}

void memcpy7(UInt_t *dst, UInt_t *src)
{
    Int_t i ;
    for(i=0;i<7;i++) 
	{
	    *dst++ = *src++ ;
	}
    return ;
}

void mstore(UInt_t *r, UInt_t av, UInt_t pad, UInt_t ch, UInt_t flags, Int_t mean)
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




Int_t croatFinder(UChar_t *adcin, UShort_t *cppin, UInt_t *outres, Int_t rb, Int_t mz)
{
    /* Define variables */
    Int_t i ;
    UInt_t *padrows ;
    Int_t cl_found ;
    UInt_t rows ;
    
    
    /* Do init number of pads in each padrow belonging to this mz */
    Int_t rowAbs[MAX_LOGICAL_ROWS] ;   /* the "real" padrow number [1..45] */ 
    Int_t rowStart[MAX_LOGICAL_ROWS] ; /* first pad in the padrow */
    Int_t rowEnd[MAX_LOGICAL_ROWS] ;   /* last pad in the padrow */
    /* First set them 0 */
    bzero((char *)rowStart, sizeof(rowStart)) ;
    bzero((char *)rowEnd, sizeof(rowEnd)) ;
    bzero((char *)rowAbs, sizeof(rowAbs)) ;
    /* Then call init routine */
    croatInit(rb, mz, rowAbs , rowStart, rowEnd );


    /* Fill offsets */
    Int_t adcOff[ABS_ROWS][MAX_P] ;
    Int_t cppOff[ABS_ROWS][MAX_P] ;
    for( i=0; i<ABS_ROWS; i++) 
	{
	    Int_t j;
	    for( j=0; j<MAX_P; j++) 
		{
		    adcOff[i][j] = i*MAX_T*MAX_P + j*MAX_T ;
		    cppOff[i][j] = i*MAX_C*MAX_P + j*MAX_C ;	/* 2 entities each...  */
		}
	}


    /* Reserve array for found clusters */
    struct resx resx[1000] ;
    bzero((char *)resx, sizeof(resx)) ;
    
    /* local results */
    register struct resx  *r ;
    UInt_t pres1[500], pres2[500] ;
    struct resx rr_local ;
    struct resx *rr = &rr_local ;


  
    /* preparations */
    padrows = outres ;	/* reserve the beginning of the bank */
    outres++ ;		/* and advance the counter */
    rows = 0 ;		/* count of rows that had data */
	
    /********************************************/
    /* loop over all possible rows of this mezz */
    /********************************************/
    for(i=0; i<MAX_LOGICAL_ROWS; i++) 
	{
	    /* Define variables */
	    Int_t j ;
	    register Int_t pres_cou1, pres_cou2 ;
	    register UInt_t *r1, *r2 ;

	    /* check if this row is to be done */
	    if(rowAbs[i] == 0) break ;

	    /* Set variables for the beginning of each row */
	    pres_cou1 = pres_cou2 = 0 ;
	    r1 = pres2 ;
	    r2 = pres1 ;
	    
	    /* in r = resx we will store the clustercandidats of this row */
	    /* increase only r to get the number of clusters = resx - r */
	    r =  resx ;	/* start again per each row ... */

#define DO_STUFF
#ifdef DO_STUFF
	    /******************************/
	    /* loop over pads in this row */
	    /******************************/
	    for(j=rowStart[i]; j<=rowEnd[i]; j++) 
		{
		    register UInt_t start, stop ;
		    register UShort_t *cv ;
		    register UChar_t *val ;
		    register Int_t start_new ;
		    Int_t go_on ;

		    /* HACK */
		    Int_t cl_counter ;
		    Int_t prev_start ;

		    /* set offsets for adc & cpp */
		    val = adcin + adcOff[rowAbs[i]-1][j-1];
		    cv  = cppin + cppOff[rowAbs[i]-1][j-1];

		    /*  ??? just needed online*/
		    //preburst((UInt_t)cv) ;

		    /* the old results are always in r1 the new go into r2              */
		    /* but one time r2 uses the array pres2 the next time it uses pres1 */
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
		    preburst((UInt_t)cv+16) ;

		    /* counters to result pointers                                               */
		    /* pres_cou1 knows how many entries on previous pad : r1[0..prescou1]        */
		    /* pres_cou2 counts the new found r2[0..prescou2], so it has to start with 0 */		       
		    pres_cou1 = pres_cou2 ;
		    pres_cou2 = 0 ;
		    /* pres_cou1 is the previous pad */
		    /* pres_cou2 is the current pad */

		    /* ?????? just needed online */
		    /*preburst((UInt_t)cv+32) ;*/

		    // time deconvolusion reset
		    start_new = -1 ;

		    // set counters 0 
		    cl_counter = 0 ;
		    prev_start = 0 ;

		    /*fprintf(stderr,"Doing row %d, pad %d\n", i, j) ; */ 
			
		    /**********************/
		    /*loop over sequenzes */
		    /**********************/
		    while((start = (UInt_t)*cv++) < (MAX_T-1)) 
			{
			    register UChar_t *val1 ;
			    register UInt_t av, charge ;
			    register Int_t mean ;
			    register UInt_t flags ;
			    register Int_t last_falling ;
			    register Int_t k ;
			    register UInt_t *ri ;
			    UInt_t tmp_charge ;
			
			    /* Set flag */
			    flags = 0;

			    /* Make sure that we don't find more than 31 clusters in this pad = MAX_C/2 */
			    /* (because in MAX_C we have 2 values per sequenz :start pixel and end pixel) */
			    cl_counter++ ; // cluster counter (for this pad only) 
			    if(cl_counter > MAX_C2) break ; // too many clusters on this pad -> stop this pad 

			    
			    /* Make sure that this sequenz starts behind the previous one */
			    if((Int_t)start < (Int_t)prev_start) break ; 
			    prev_start = start ;

			    /* Get the stop bucket ( the order in cpp is cpp[x]=start, cpp[x+1]=stop for this sequenz */
			    stop = (UInt_t) *cv++ ;
		       	
			    /* exclude possible nonsense situations */
			    if(stop < start) stop = start ;
			    if(stop > MAX_T) stop = MAX_T ;
			
			    /* sequenz is not longer than 3 consequtive timebuckets ... */
			    /*
			      if((stop-start)>3) stop = start + 3 ;
			    */
			    /*if(stop > 349) stop = 349 ;*/
			

				

#ifdef DECONVOLUTE_TIME
			    /* this is a goto label ... */
			redo: ;

			    // divide one pixelsequenz into two in case we found a change of sign in slope 
			    if(start_new > -1) 
				{
				    start=start_new ;
				    start_new = -1 ;
				}
#endif
			    // average and charge 0 
			    av = charge = 0 ; 

			    // yes, create a new cluster
			    go_on = 1 ;

			    // now get the adc value adress 
			    val1 = val + start ;


#ifdef DECONVOLUTE_TIME
			    // flags = 0 
			    last_falling = flags = 0 ;
			    
#endif
#ifdef DECONVOLUTE_PAD
			    flags = 0 ;
#endif
			
			    // block to introduce new variables 
			    {
				// this variable we need for a deconvolusion in time	
				register UInt_t last_a ;
				last_a = 0 ;
				
				/**************************/
				/* loop over this sequenz */
				/**************************/
				for(;start<=stop;start++) 
				    {
					// this variable holds actual adc value					
					register UInt_t a ;
					register UInt_t aa  ;
					
					// go get the adc value
					aa = *val1++ ;
				
#ifdef DECONVOLUTE_TIME 
					// check if last pixel in this sequenz is bigger or smaller than this
					if(aa > last_a) 
					    {
						if(last_falling) 
						    {
							start_new = start ;
							break ;
						    }
					    }
					else last_falling = 1;
					last_a = aa ;
#endif
					
					// 8 to 10 bit conversion :  as late as possible
					a = log8to10_table[aa] ;
				    
					// sum of adc-values equals total charge of this cluster on this pad
					charge += a ; 
				    
					// this one we need to determine the time mean over all pads
					av += start * (a) ;
				    } // loop over sequenz 
			    }// block to introduce new variables 
#ifdef DECONVOLUTE_TIME
			    if(start_new > 0) flags = FLAG_DOUBLE_T ;
#endif
			    // calculate mean in time direction for this sequenz 
			    if(charge) 
				{
				    /* mean := center of gravity for this sequenz */
				    mean = av/charge ;
				}
			    else 
				{
				    /* if charge == 0 : set them to low values, why ? */
				    charge = 1 ;
				    mean = 1 ;
				}

			    // get the pointer to results on previous pad(s) 
			    ri = r1 ;	
		    
			    // center of gravity in pad direction 
			    tmp_charge = charge * j;

			    
			    /******************************************************************/
			    /* compare with previously stored results (=means previous pad(s))*/
			    /******************************************************************/	
			    for(k=0;k<pres_cou1;k++) 
				{
				    // variable to store differenz 
				    register Int_t v ;
				    
				    // struct which contains means(+more) of last pad(s) 
				    register struct resx *rr_tmp ;
			    
				    // go get it 
				    rr_tmp = (struct resx *) *ri ;
				
				    // differenz between this mean and mean of sequenz on previous pad 
				    v = mean - rr_tmp->mean ;

								    
				    // since the results are ordered in rising order
				    // the remaining means of the last pad are for sure too far away -> stop it
				    if(v < -PARAM1) break ;
				    
				    // goto next mean on previous pad
				    ri++ ;
			    
			            /* if(v < 0) v = -v ;*/
					
				    // is there a matching cluster on previous pad(s)
				    if(v<=PARAM1) 
					{
					    // OK - got one to continue with so
					    // we'll "add" the new and the old
					    
					    // make a temp. copy to on-CPU RAM (rr)
					    // just make sure rr points where it should       
					    rr = &rr_local ;

					    // memcpy7(destination,source)
					    memcpy7((UInt_t *)rr,(UInt_t *)rr_tmp) ;

					    // in case we found another sequenz-section on this pad
					    // don't loop over all sequenzes on last pad but start 
					    // where we found a matching candidate for this sequenz-section
					    // adjust upper limit (=prescou1)
					    r1 = ri ;
					    pres_cou1 -= 1+k ;

					    /*LOG(DBG,"after - pres_cou1 %d [%d]",pres_cou1,pres_cou2,0,0,0) ;*/
				    
#ifdef DECONVOLUTE_PAD
					    if(charge > rr->scharge) 
						{
						    if(rr->falling) 
							{ 
							    /* previous state was falling*/
							    flags |= FLAG_DOUBLE_PAD ;
							    rr_tmp->flags |= flags ;
							    /* and create a new one*/
							    break ;    
							}
						}
					    else 
						{
						    rr->falling = 1 ;	/* falling */
						}
#endif

					    // don't create a new one 
					    go_on = 0 ;
					    					    
					    // store the pointer to the matched in "2" 
					    r2[pres_cou2++] = (UInt_t) rr_tmp ;

					    ////////////////////////
					    // calculate and fill the new means, charge etc in rr = rr_tmp
					    ////////////////////////
					    // charge on this pad to determine whether we cut in pad direction
					    rr->scharge = charge ;
					    // unset FLAG_ONEPAD since we have at least 2 pads
					    rr->flags &= (~FLAG_ONEPAD) ;
					    // summ the charges for de/dx
					    rr->charge += charge ;
					    // calculate mean in pad direction
					    rr->pad += tmp_charge ;
					    // calculate mean in time direction
					    rr->t += av ;
					    // store mean of this sequenz on this pad
					    rr->mean = mean ;
					    
					    // copy it back to storage 
					    // memcpy7(destination,source)
					    memcpy7((UInt_t *)rr_tmp,(UInt_t *)rr) ;
					    
					    // we found a matching one so stop looping over means on previous pad
					    break ;
					    
					} /* if PARAM1 */
				} /* loop : means on previous pad (prescou1) */
			    

			    // here we create from scratch new clusters...
			    if(go_on) 
				{
				    // store this guy because it's the first
				    r2[pres_cou2++] = (UInt_t )r ;
				    mstore((UInt_t *)r,av,tmp_charge,charge,flags|FLAG_ONEPAD,mean) ;
				    r++ ;
				}


#ifdef DECONVOLUTE_TIME
			    // maybe we still have to do something
			    if(start_new>=0) goto redo ;
#endif
			}	/* for loop : CPPs on this pad */
		}    /* for loop : minpad to maxpad */
	    
#endif	/* DO_STUFF */
	    // number of clusters found
	    cl_found = r - resx ;
	  
	    
#ifndef DO_STUFF
	    /* HACK*/
	    cl_found = 1 ;
#endif
	    /* here we come after we examined one pad row */
	    if(cl_found == 0) continue ;	/* nothing in this row goto next row */

	   
	    // write this row out
	    {
	      // now we store TPCMZCLD DATA WORD 2 3 : rowid and number of clusters for this row
	      UInt_t *rows_ok_p, *cl_ok_p ;
	      // good cluster counter
	      UInt_t cls_ok ;
	      // reset good cluster counter
	      cls_ok = 0 ;	

	    
#ifdef DO_STUFF
	    // now fill the found clusters in the TPCMZCLD bank 
	    for(j=0;j<cl_found;j++) 
		{
		    // find the pointer where to store it 
		    struct fmt21_c 
		    {
			UShort_t x ;
			UShort_t t ;
		    } *c ;

		    struct fmt21_f 
		    {
			UShort_t f ;
			UShort_t c ;
		    } *f ;

		     // reject one_pad clusters
		    if(resx[j].flags & FLAG_ONEPAD)
		    {
		      //printf("one pad rejected\n") ;
		      continue ;
		    }

		    // increase pointer of good ones
		    cls_ok++ ;

		    // set pointer to data word 2 and 3
		    if(cls_ok == 1) 
		      {
			rows_ok_p = outres++ ;
			cl_ok_p = outres++ ;
		      }

		    // set c and f to the right address in memory
		    c = (struct fmt21_c *) outres++ ;
		    f = (struct fmt21_f *) outres++ ;

		    // if cluster is not empty
		    if(resx[j].charge) 
			{   
			    // fill it in 1/64 units
			    // to get the center of gravity we have to devide by charge 
			    c->x = (resx[j].pad << 6) / resx[j].charge ;
			    c->t = (resx[j].t << 6 )  / resx[j].charge ;
			}
		    else 
			{
			    // if we don't find anything fill it with dummies 
			    c->x = 700 ;
			    c->t = 700 ;
			}
		    // fill flags and charge 
		    f->f = resx[j].flags ;
		    f->c = resx[j].charge ;

		    // print it out
		    /*printf("rb:%d   mz:%d  row:%d  ncl:%d  x:%d(%f)   t:%d(%f)   c:%d  f:%d\n"
		      ,rb,mz,rowAbs[i],cl_found,c->x,(float)((float)(c->x)/64),c->t,(float)((float)(c->t)/64),f->c,f->f) ;*/
		}
#else
	    // fill first pad and last pad of this row 
	    // this is NOT in the official DAQ-Raw Format 
	    // but it has no impact ...
	    *outres++ = rowStart[i] ;
	    *outres++ = rowEnd[i] ;
#endif
	   
	    if(cls_ok) 
	      {	
		// any acceptable clusters after the cut?
		// fill data word number of clusters in this row
		*cl_ok_p = cls_ok ;
		// fill data word row_id
		*rows_ok_p = rowAbs[i] ;
		// increase number of rows done by this mezzanine
		rows++ ;
	      }
	    
	    } // write this row out

	}	/* for MAX_LOGICAL_ROWS */
    
    /* now we are done with this card */
    /* fill the first data word of TPCMZCLD DATA */
    *padrows = rows ;	

    /* return length in qwords */
    return (outres - padrows) ;	
} 

Int_t croatInit(Int_t myRB, Int_t myMZ,Int_t* rowAbs ,Int_t* rowStart,Int_t* rowEnd )
{
    /* this function fills the rowStart,rowEnd & rowAbs arrays */
    /* with the informations provided by padfinder.h file */
    Int_t i=0 ;
    Int_t j=0 ;
    Int_t rel =0 ;
 
    /* fprintf(stderr, "Initializing RB %d, MZ %d\n", myRB, myMZ); */
	
    /* loop over rows */
    for(i=1;i<=45;i++) 
	{
	    /* loop over up to 3 mz cards which are responsible for this row */
	    /* ( in fact just 2 DIFFERENT mz cards but in case of a row : xxx-------xxx we need 3 */
	    for(j=0;j<3;j++) 
		{
		    /* fprintf(stderr, "RB%d, row %d, j %d: rdo %d, mz %d\n", */
		    /*	myRB, i, j, padfinder[i-1][j].rdo, padfinder[i-1][j].mezz);  */
		    if(padfinder[i-1][j].rdo == 0) break ;
		    if((padfinder[i-1][j].rdo == myRB) && (padfinder[i-1][j].mezz == myMZ)) 
			{
			    rowAbs[rel] = i ;
			    rowStart[rel] = padfinder[i-1][j].minpad ;
			    rowEnd[rel] = padfinder[i-1][j].maxpad ;
			    rel++ ;
			    if(rel > MAX_LOGICAL_ROWS) 
				{
				    fprintf(stderr, "Too many logical rows!\n");
				    return -1 ;
				}
			    else 
				{
				    /*fprintf(sterr, "Logical row %d-> abs %d, start %d, end %d\n", */
				    /*	  rel, rowAbs[rel-1], rowStart[rel-1], rowEnd[rel-1]); */
				}
			} /* if matching entry in padfinder[x] found ... */
		} /* loop over responsible mz cards */
	}  /* loop over rows */
    return rel ;	/* number of logical rows... not needed! */
}

