#include <stdlib.h>
#include <cfortran.h>
#include <hbook.h>

#if defined(__hpux) || defined(_IBMR2)
# define extname
#endif


#define PAWC_SIZE 50000
/*
 *                     It really shouldbe like that, but for the moment
 *                      we must use the statements after the comment
 *                     The correction needs to be done in cfortran.h
typedef struct 
   {
    float h[PAWC_SIZE];
   } PAWC_DEF;

#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);
*/
/*        This is only temporary */
#ifdef extname
float pawc_[PAWC_SIZE];
#else
float pawc[PAWC_SIZE];
#endif
/*        the above is temporary, should be corrected in cfortran.h  */


#if defined(__vms) || defined(vms)
/*        On VMS rand has a range of 2**31    */
# define RAND_RANGE 2146483648.
#else
# define RAND_RANGE 32768.
#endif

main()
{
	int hid=1;
	int i,j;
	float r;

        HLIMIT(PAWC_SIZE);
	HBOOK1(hid," some random distribution",20000,-4.,4.,0.);

	for (i=0;i<100000;i++){
		for (j=0,r=0.;j<10;r += rand()/RAND_RANGE -0.5 ,j++)  
		;
		HFILL(hid,r,0.,1.);
	}
	HPRINT(hid);
}















