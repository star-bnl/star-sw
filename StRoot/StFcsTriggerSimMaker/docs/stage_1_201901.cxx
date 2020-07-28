#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_201901.h"

// First stage Trigger algorithm: local to any DEP board.
// Inputs are 32 bit ADC values, output is what gets sent on the outgoing link.
// Processing is detector dependent thus one expects to use the geometry.

int stage_1_201901(u_int adc[32], geom_t geo, link_t *output)
{
    for(int i=0; i<8; i++) output->d[i]=0;

    if(geo.det==0 || geo.det==1) { //ecal and hcal, creating 8 4x4 sums (8bit each)
	u_int sum[8]; 
	sum[0]=adc[ 0]+adc[ 1]+adc[ 4]+adc[ 5];
	sum[1]=adc[ 2]+adc[ 3]+adc[ 6]+adc[ 7];
	sum[2]=adc[ 8]+adc[ 9]+adc[12]+adc[13];
	sum[3]=adc[10]+adc[11]+adc[14]+adc[15];
	sum[4]=adc[16]+adc[17]+adc[20]+adc[21];
	sum[5]=adc[18]+adc[19]+adc[22]+adc[23];
	sum[6]=adc[24]+adc[25]+adc[28]+adc[29];
	sum[7]=adc[26]+adc[27]+adc[30]+adc[31];	
	for(int i=0; i<8; i++){
	    if(sum[i] > 0x7fff) sum[i]=0x7fff; //saturate at 15bits
	    output->d[i] = sum[i] >> 7; // cut of lower 7 bits, making 8 bit
	}
    }else if(geo.det==2) { //pres, creating 3 hit bit
	u_int fpsmap[15] = {0x003e,0x007c,0x00e0,0,0,0,0,0,0,0,0,0,0,0,0};
	/*
	u_int fpsmap[8][9]={{0,1,1,1,1,1,0,0,0},   //0x003e
			    {0,0,1,1,1,1,1,0,0},   //0x007c
			    {0,0,0,0,0,1,1,1,0}};  //0x00e0
	*/
	u_int fps=0;
	for(int slat=0; slat<32; slat++){
	    if(adc[slat]>FPSTHR_201901){
		for(int row=0; row<15; row++){
		    if((fpsmap[row] & (0x1 << slat)) > 0) fps |= (0x1 << row);
		}
	    }
	}
	output->d[0] = (fps & 0x0f);
	output->d[1] = (fps & 0xf0) >> 8;
    }

    if(fcs_trgDebug>=3){
	printf("FCS STG1 NS=%1d EHP=%1d DEP=%2d : ",geo.ns,geo.det,geo.dep);
	for(int i=0; i<8; i++) printf("%4d ", output->d[i]);
	printf("\n");
    }
    return 0 ;
}

