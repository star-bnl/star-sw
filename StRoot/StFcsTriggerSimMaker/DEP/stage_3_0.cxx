#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_0.h"

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

int DIFBFMAP[15][15]={
    {0,0,0,0,0, 1,1,1,1,1, 1,1,1,1,1},
    {0,0,0,0,0, 0,1,1,1,1, 1,1,1,1,1},
    {0,0,0,0,0, 0,0,1,1,1, 1,1,1,1,1},
    {0,0,0,0,0, 0,0,0,1,1, 1,1,1,1,1},
    {0,0,0,0,0, 0,0,0,0,1, 1,1,1,1,1},
    {1,0,0,0,0, 0,0,0,0,0, 1,1,1,1,1},
    {1,1,0,0,0, 0,0,0,0,0, 0,1,1,1,1},
    {1,1,1,0,0, 0,0,0,0,0, 0,0,1,1,1},
    {1,1,1,1,0, 0,0,0,0,0, 0,0,0,1,1},
    {1,1,1,1,1, 0,0,0,0,0, 0,0,0,0,1},
    {1,1,1,1,1, 1,0,0,0,0, 0,0,0,0,0},
    {1,1,1,1,1, 1,1,0,0,0, 0,0,0,0,0},
    {1,1,1,1,1, 1,1,1,0,0, 0,0,0,0,0},
    {1,1,1,1,1, 1,1,1,1,0, 0,0,0,0,0},
    {1,1,1,1,1, 1,1,1,1,1, 0,0,0,0,0},
};

int stage_3_0(link_t link[4], u_short *dsm_out)
{
    int NFBF1 = link[0].d[0] + (link[0].d[1]<<8);
    int NFBF2 = link[0].d[2] + (link[0].d[3]<<8);
    int NFBF3 = link[0].d[4] + (link[0].d[5]<<8);
    int NJET  = link[0].d[6];
    int SFBF1 = link[2].d[0] + (link[2].d[1]<<8);
    int SFBF2 = link[2].d[2] + (link[2].d[3]<<8);
    int SFBF3 = link[2].d[4] + (link[2].d[5]<<8);
    int SJET  = link[2].d[4];
   
    int FBF1 = (NFBF1>0) || (SFBF1>0);
    int FBF2 = (NFBF2>0) || (SFBF2>0);
    int FBF3 = (NFBF3>0) || (SFBF3>0);
    int DIFBF1=0, DIFBF2=0;
    if((NFBF1>0) && (SFBF1>0)) DIFBF1=1;
    if((NFBF2>0) && (SFBF2>0)) DIFBF2=1;
    for(int i=0; i<14; i++){
	for(int j=i; j<15; j++){
	    if(DIFBFMAP[i][j]==1){
		if( (NFBF1 & 1<<i)>0 && (NFBF1 & 1<<j)>0) DIFBF1=1;
		if( (SFBF1 & 1<<i)>0 && (SFBF1 & 1<<j)>0) DIFBF1=1;
		if( (NFBF2 & 1<<i)>0 && (NFBF2 & 1<<j)>0) DIFBF2=1;
		if( (SFBF2 & 1<<i)>0 && (SFBF2 & 1<<j)>0) DIFBF2=1;
	    }
	}
    }    

    int JET1   = (NJET    & 0x1) | (SJET    & 0x1);
    int JET2   = (NJET>>1 & 0x1) | (SJET>>1 & 0x1);
    int JET3   = (NJET>>2 & 0x1) | (SJET>>2 & 0x1);
    int DIJET1 = (NJET    & 0x1) & (SJET    & 0x1);
    int DIJET2 = (NJET>>1 & 0x1) & (SJET>>1 & 0x1);

    *dsm_out = 0 ;
    *dsm_out |= FBF1   << 0;
    *dsm_out |= FBF2   << 1;
    *dsm_out |= FBF3   << 2;
    *dsm_out |= DIFBF1 << 3;
    *dsm_out |= DIFBF2 << 4;
    *dsm_out |= JET1   << 5;
    *dsm_out |= JET2   << 6;
    *dsm_out |= JET3   << 7;
    *dsm_out |= DIJET1 << 8;
    *dsm_out |= DIJET2 << 9;

    if(fcs_trgDebug>=2){
        printf("FCS STG3                   : 0x%03x  Trg=",*dsm_out);
	if(FBF1)   printf("FbF1 ");
	if(FBF2)   printf("FbF2 ");
	if(FBF3)   printf("FbF3 ");
	if(DIFBF1) printf("DiFbF1 ");
	if(DIFBF2) printf("DiFbF2 ");
	if(JET1)   printf("Jet1 ");
	if(JET2)   printf("Jet2 ");
	if(JET3)   printf("Jet3 ");
	if(DIJET1) printf("DiJet1 ");
	if(DIJET2) printf("DiJet2 ");
	printf("\n");
    } 
    return 0 ;
}

