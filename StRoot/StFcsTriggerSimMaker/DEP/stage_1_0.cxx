#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_0.h"

// First stage Trigger algorithm: local to any DEP board.
// Inputs are 32 bit ADC values, output is what gets sent on the outgoing link.
// Processing is detector dependent thus one expects to use the geometry.

int stage_1_0(u_int adc[32], geom_t geo, link_t *output)
{

    //Stage1 get 19bit ADC value
    //  RawAdc(t)  [12bit]
    //
    //  TbinSum16 = sum_t=0~15(rawadc[t]) [16bit]
    //    TbinSum16 integrate ~100%, has 0.0053GeV/ch, maxed around 32k for E=180GeV [~15bit]
    //
    //  TbinSum8  = sum_t=0~7(rawadc[t])  [15bit]
    //    TbinSum8 integrate ~75%, has 0.0070GeV/ch, maxed around 26k for E=180GeV [~15bit]
    //    TbinSum8 has ET=0.00024711 GeV/ch (closest to beam)  ETMAX= 6.4GeV
    //              to ET=0.00147035 GeV/ch (far corner)       ETMAX=38.2GeV
    //
    //  Correction = int(EtGain * GainCorr * 64 + 0.5)  
    //               if(Correction > 16) Correction=16     [4.6 bits fixed float]
    //    Normalized "EtGain" are 1.0 (closest to beam in trigger) 
    //                         to 6.0 (far corner in trigger) 
    //    GainCorr are around 1.0 (can cover up to x2.6 within 16 limit)
    // 
    // CorrectedSum = (TbinSum - Ped) * Correction [25bits]
    //
    // FinalSum = CorrectedSum >> 6 [19bit] ===> this is result of stage0 passed stage1
    // 
    // 2x2 sum [21bit] 
    //    Cut off top (4+2) bits            ETMAX=6.4GeV   
    //    Cut off Bottom 7 making 8bits     ET/ch=0.025GeV

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

    if(fcs_trgDebug>=3){
	printf("FCS STG1 NS=%1d EHP=%1d DEP=%2d : ",geo.ns,geo.det,geo.dep);
	for(int i=0; i<8; i++) printf("%4d ", output->d[i]);
	printf("\n");
    }
    return 0 ;
}

