/*//
//      croat.h
//
//      first version, 07/23/99, struck
//  
//      header file adapted for _OFFLINE_ !!
//*/


#ifndef _CROAT_H_
#define _CROAT_H_

#include <unistd.h>	/* for the types... */

/* TUNABLES */

/* defines the maximum timebin separation before the sequence will
 be broken in 2 */
#define PARAM1		2

/* perform deconv. in the time direction */
#define DECONVOLUTE_TIME

/* perform deconv. in the pad direction */
#define DECONVOLUTE_PAD


/* output flags */
#define FLAG_ONEPAD		1	/* cluster had only one pad */
#define FLAG_DOUBLE_PAD		2	/* cluster deconvoluted in the pad direction */
#define FLAG_DOUBLE_T		4	/* cluster deconv. in the time direction */


/* maximum number of absolute rows that ANY mezzanine can have */
#define MAX_LOGICAL_ROWS	6

#define MAX_T                 350
#define MAX_P                 184
#define MAX_C                  64
#define MAX_C2          (MAX_C/2)
#define MAX_HITS                6       /* max. hits per pad, needed for alloc. of outgoing */
/* buffer */

#define ABS_ROWS               45
#define N_MEZ                  18       /* number of mezzanines for one sector */


/* routines */
/*int croatFinder(uchar_t *adcin, ushort_t *cppin, uint_t *outres); */
int croatInit(int rb, int mz);
/*void write_eve(uint_t *out, unsigned short sector); */
/*int read_eve(const char *filename, unsigned short sector);*/
/*void asic_eve();*/ 
/*int rawToGlobal (int sector, int row, double pad, double tb, */
/*		 double *x, double *y, double *z); */
/*void dump_banks(uint_t *out, uint_t length); */
/*int load_banks (uint_t* buffer);*/

#endif
