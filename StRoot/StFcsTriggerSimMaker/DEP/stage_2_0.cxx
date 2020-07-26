#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_0.h"

// Processing on the North or South DEP/IO flavoured board. 
// Inputs are up to 32 links but I already organized them according to strawman.
// output is 1 link over the external OUT connector.
// Right now we assume we have 20 inputs from ECAL, 6 from HCAL and 4 from PRE.
// We also assume there is no need to know if this is North or South as
// the processing is exactly the same. Right??

static const int EtoHmap[15][9][2] = {
    { { 0, 0},{ 0, 0},{ 0, 1},{ 0, 1},{ 0, 2},{ 0, 3},{ 0, 3},{ 0, 4},{ 0, 4}},
    { { 0, 0},{ 0, 0},{ 0, 1},{ 0, 1},{ 0, 2},{ 0, 3},{ 0, 3},{ 0, 4},{ 0, 4}},
    { { 1, 0},{ 1, 0},{ 1, 1},{ 1, 1},{ 1, 2},{ 1, 3},{ 1, 3},{ 1, 4},{ 1, 4}},
    { { 2, 0},{ 2, 0},{ 2, 1},{ 2, 1},{ 2, 2},{ 2, 3},{ 2, 3},{ 2, 4},{ 2, 4}},
    { { 2, 0},{ 2, 0},{ 2, 1},{ 2, 1},{ 2, 2},{ 2, 3},{ 2, 3},{ 2, 4},{ 2, 4}},
    { { 3, 0},{ 3, 0},{ 3, 1},{ 3, 1},{ 3, 2},{ 3, 3},{ 3, 3},{ 3, 4},{ 3, 4}},
    { { 3, 0},{ 3, 0},{ 3, 1},{ 3, 1},{ 3, 2},{ 3, 3},{ 3, 3},{ 3, 4},{ 3, 4}},
    { { 4, 0},{ 4, 0},{ 4, 1},{ 4, 1},{ 4, 2},{ 4, 3},{ 4, 3},{ 4, 4},{ 4, 4}},
    { { 5, 0},{ 5, 0},{ 5, 1},{ 5, 1},{ 5, 2},{ 5, 3},{ 5, 3},{ 5, 4},{ 5, 4}},
    { { 5, 0},{ 5, 0},{ 5, 1},{ 5, 1},{ 5, 2},{ 5, 3},{ 5, 3},{ 5, 4},{ 5, 4}},
    { { 6, 0},{ 6, 0},{ 6, 1},{ 6, 1},{ 6, 2},{ 6, 3},{ 6, 3},{ 6, 4},{ 6, 4}},
    { { 6, 0},{ 6, 0},{ 6, 1},{ 6, 1},{ 6, 2},{ 6, 3},{ 6, 3},{ 6, 4},{ 6, 4}},
    { { 7, 0},{ 7, 0},{ 7, 1},{ 7, 1},{ 7, 2},{ 7, 3},{ 7, 3},{ 7, 4},{ 7, 4}},
    { { 8, 0},{ 8, 0},{ 8, 1},{ 8, 1},{ 8, 2},{ 8, 3},{ 8, 3},{ 8, 4},{ 8, 4}},
    { { 8, 0},{ 8, 0},{ 8, 1},{ 8, 1},{ 8, 2},{ 8, 3},{ 8, 3},{ 8, 4},{ 8, 4}}
};

int stage_2_0(link_t ecal[20], link_t hcal[8], link_t pre[4], geom_t geo, link_t output[2])
{    
    // creating 2x2 row/column address map when called first time
    static int first=0;
    static u_int ETbTdep[16][10]; //DEP#
    static u_int ETbTadr[16][10]; //Input Link data address
    static u_int HTbTdep[10][6];  //DEP#
    static u_int HTbTadr[10][6];  //Input Link data address
    if(first==0){
	first=1;
	//making map of 2x2 Ecal Sums of [16][10]
	for(int r=0; r<16; r++){
	    printf("Ecal r=%2d : ",r);
	    for(int c=0; c<10; c++){
		ETbTdep[r][c]= c/2 + (r/4)*5;
		ETbTadr[r][c]= c%2 + (r%4)*2;
		printf("%2d-%1d ",ETbTdep[r][c],ETbTadr[r][c]);
	    }
	    printf("\n");
	}	
	//making map of 2x2 Hcal sums of [10][6]
	for(int r=0; r<10; r++){
	    printf("HCal r=%2d : ",r);
	    for(int c=0; c<6; c++){
		if      (r==0){
		    HTbTdep[r][c]= 6;
		    HTbTadr[r][c]= c;
		}else if(r==9){
		    HTbTdep[r][c]= 7;
		    HTbTadr[r][c]= c;
		}else{
		    HTbTdep[r][c]= c/2 + ((r-1)/4)*3;
		    HTbTadr[r][c]= c%2 + ((r-1)%4)*2;
		}
		printf("%2d-%1d ",HTbTdep[r][c],HTbTadr[r][c]);
	    }
	    printf("\n");
	}
    }

    //compute overlapping 4x4 Hcal sums
    u_int hsum[9][5];
    for(int r=0; r<9; r++){
	if(fcs_trgDebug>=2) printf("H4x4 ");
	for(int c=0; c<5; c++){
	    hsum[r][c]
		= hcal[ETbTdep[r  ][c  ]].d[HTbTadr[r  ][c  ]]
		+ hcal[ETbTdep[r  ][c+1]].d[HTbTadr[r  ][c+1]]
		+ hcal[ETbTdep[r+1][c  ]].d[HTbTadr[r+1][c  ]]
		+ hcal[ETbTdep[r+1][c+1]].d[HTbTadr[r+1][c+1]];
	    //if(hsum[r][c] > 0xff) hsum[r][c]=0xff;
	    if(fcs_trgDebug>=2) printf("%5d ",hsum[r][c]);
	}
	if(fcs_trgDebug>=2) printf("\n");
    }

    //compute overlapping 4x4 ecal sums
    //take ratio with the closest hcal 4x4
    u_int esum[15][9];
    u_int sum[15][9];
    float ratio[15][9];
    u_int EM1=0, EM2=0, EM3=0;
    u_int HAD1=0, HAD2=0, HAD3=0;
    for(int r=0; r<15; r++){
	if(fcs_trgDebug>=2) printf("E4x4 ");
	for(int c=0; c<9; c++){
	    esum[r][c]
		= ecal[ETbTdep[r  ][c  ]].d[ETbTadr[r  ][c  ]]
		+ ecal[ETbTdep[r  ][c+1]].d[ETbTadr[r  ][c+1]]
		+ ecal[ETbTdep[r+1][c  ]].d[ETbTadr[r+1][c  ]]
		+ ecal[ETbTdep[r+1][c+1]].d[ETbTadr[r+1][c+1]];
	    //if(esum[r][c] > 0xff) esum[r][c]=0xff;
	    
	    u_int h=hsum[EtoHmap[r][c][0]][EtoHmap[r][c][1]]; //closest H4x4	    
	    sum[r][c] = esum[r][c] + h;

	    //in VHDL we will do esum>hsum*threshold. Ratio is for human
	    if(sum[r][c]==0) {
		ratio[r][c]=0.0;
	    }else{
		ratio[r][c] = float(esum[r][c]) / float(sum[r][c]);
	    }

	    if(esum[r][c] >= h*EM_HERATIO_THR){
		if(sum[r][c] > EMTHR1) EM1 |= (1<<r);
		if(sum[r][c] > EMTHR2) EM2 |= (1<<r);
		if(sum[r][c] > EMTHR3) EM3 |= (1<<r);
	    }
	    if(esum[r][c] < h*HAD_HERATIO_THR){
		if(sum[r][c] > HADTHR1) HAD1 = 1;
		if(sum[r][c] > HADTHR2) HAD2 = 1;
		if(sum[r][c] > HADTHR3) HAD3 = 1;
	    }
	    if(fcs_trgDebug>=2) printf("%5d %3.2f  ",esum[r][c],ratio[r][c]);
	}
	if(fcs_trgDebug>=2) printf("\n");
    }
    
    //Ecal sub-crate sum    
    u_int esub[4];
    esub[0] = esum[ 0][0]+esum[ 0][2]+esum[ 0][4]+esum[ 0][8]
            + esum[ 2][0]+esum[ 2][2]+esum[ 2][4]+esum[ 2][8];
    esub[1] = esum[ 4][0]+esum[ 4][2]+esum[ 4][4]+esum[ 4][8]
            + esum[ 6][0]+esum[ 6][2]+esum[ 6][4]+esum[ 6][8];
    esub[2] = esum[ 8][0]+esum[ 8][2]+esum[ 8][4]+esum[ 8][8]
            + esum[10][0]+esum[10][2]+esum[10][4]+esum[10][8];
    esub[3] = esum[12][0]+esum[12][2]+esum[12][4]+esum[12][8]
            + esum[14][0]+esum[14][2]+esum[14][4]+esum[14][8];
    for(int i=0; i<4; i++) if(esub[i]>0xff) esub[i]=0xff;

    //Hcal sub-crate sum    
    u_int hsub[4];
    hsub[0] = hsum[ 1][0]+hsum[ 1][2]+hsum[ 1][4];
    hsub[1] = hsum[ 3][0]+hsum[ 3][2]+hsum[ 3][4];
    hsub[2] = hsum[ 5][0]+hsum[ 5][2]+hsum[ 5][4];
    hsub[3] = hsum[ 7][0]+hsum[ 7][2]+hsum[ 7][4];
    for(int i=0; i<4; i++) if(hsub[i]>0xff) hsub[i]=0xff;
    
    //Jet sum
    u_int jet[3];
    u_int JP1=0,JP2=0,JP3=0;
    jet[0] = esub[0] + esub[1] + hsub[0] + hsub[1];
    jet[1] = esub[1] + esub[2] + hsub[1] + hsub[2];
    jet[2] = esub[2] + esub[3] + hsub[2] + hsub[3];
    for(int i=0; i<3; i++){
	if(jet[i]>0xff) jet[i]=0xff;
	if(jet[i]>JETTHR1) JP1 = 1;
	if(jet[i]>JETTHR2) JP2 = 1;
	if(jet[i]>JETTHR3) JP3 = 1;
    }
    if(fcs_trgDebug>=2) printf("Jet = %3d %3d %3d\n",jet[0],jet[1],jet[2]);

    //sending 4x4 row# (0-14) with 3 thresholds, and jet 3 threshold bits
    memset(output[0].d,0,sizeof(output[0].d)) ;
    output[0].d[0] = (EM1     ) & 0xff;
    output[0].d[1] = (EM1 >> 8) & 0x7f;
    output[0].d[2] = (EM2     ) & 0xff;
    output[0].d[3] = (EM2 >> 8) & 0x7f;
    output[0].d[4] = (EM3     ) & 0xff;
    output[0].d[5] = (EM3 >> 8) & 0x7f;
    output[0].d[6] = HAD1 + (HAD2<<1) + (HAD3<<2) 
	        + (JP1<<3) + (JP2<<4) + (JP3<<5);

    if(fcs_trgDebug>=2){    
        printf("FCS STG2 NS=%1d out0         : ",geo.ns);
        for(int i=0; i<8; i++) printf("%4d ", output[0].d[i]);
        printf("\n");
        printf("FCS STG2 NS=%1d out1         : ",geo.ns);
        for(int i=0; i<8; i++) printf("%4d ", output[1].d[i]);
        printf("\n");
    }
    return 0 ;
}

