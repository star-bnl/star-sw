#include <stdlib.h>
#include <cfortran.h>
#include <hbook.h>

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


main()
{
	int hid=1,istat=0,icycle=0;
	int i,i1,j;
        int nvar;
	float r[3];
        char chtag_in[3][6]={"X"," Y ","z"};
        char chtag_out[5][8],chtitl[80];
	float rmin[5],rmax[5];
	int record_size=1024;

        HLIMIT(PAWC_SIZE);
	HROPEN(1,"example","ntuple-example.hbook","N",record_size,istat);
	HBOOKN(hid," An Ntuple",3," ",5000,chtag_in);

	for (i=0;i<10000;i++){
		for(i1=0;i1<3;i1++)
		   for (j=0,r[i1]=0.;j<10;r[i1] += rand()/32768. -0.5 ,j++);
		HFN(hid,r);
	}
	nvar=5;
	HGIVEN(hid,chtitl,nvar,chtag_out,rmin[0],rmax[0]);
	printf(" title obtained:%s\n variables %d\ntags:>%s< >%s< >%s<\n",
		chtitl,nvar,chtag_out[0],chtag_out[1],chtag_out[2]);
	printf(" rmin: %e  %e  %e\n",rmin[0],rmin[1],rmin[2]);
	printf(" rmax: %e  %e  %e\n",rmax[0],rmax[1],rmax[2]);

	HROUT(0,icycle," ");
	HREND("example");

}
