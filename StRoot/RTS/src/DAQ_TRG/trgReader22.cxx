#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>

#include <rtsSystems.h>

// This reader works for TRG_VERSIONS: 0x21 (FY04 run); 0x22 (Nov 04)

// one needs to diffuse the crummy trigger stuff first
#define TRG_VERSION	0x22
#include <daqFormats.h>
#include <rts.h>  // for the sake of swap32 entry


#include "daq_trg.h"

// this maps from the raw data to the following representation
// sector1,slat1,ch1 ... ch5, sector1,slat2,ch1.... ch5, sector2,slat1,ch1...

static unsigned char ctbMap[240] = {
7, 6, 5, 4, 3, 23, 22, 21, 20, 19, 
2, 1, 0, 15, 14, 18, 17, 16, 31, 30, 
13, 12, 11, 10, 9, 29, 28, 27, 26, 25, 
39, 38, 37, 36, 35, 55, 54, 53, 52, 51, 
34, 33, 32, 47, 46, 50, 49, 48, 63, 62, 
45, 44, 43, 42, 41, 61, 60, 59, 58, 57, 
71, 70, 69, 68, 67, 87, 86, 85, 84, 83, 
66, 65, 64, 79, 78, 82, 81, 80, 95, 94, 
77, 76, 75, 74, 73, 93, 92, 91, 90, 89, 
103, 102, 101, 100, 99, 119, 118, 117, 116, 115, 
98, 97, 96, 111, 110, 114, 113, 112, 127, 126, 
109, 108, 107, 106, 105, 125, 124, 123, 122, 121, 
135, 134, 133, 132, 131, 151, 150, 149, 148, 147, 
130, 129, 128, 143, 142, 146, 145, 144, 159, 158, 
141, 140, 139, 138, 137, 157, 156, 155, 154, 153, 
167, 166, 165, 164, 163, 183, 182, 181, 180, 179, 
162, 161, 160, 175, 174, 178, 177, 176, 191, 190, 
173, 172, 171, 170, 169, 189, 188, 187, 186, 185, 
199, 198, 197, 196, 195, 215, 214, 213, 212, 211, 
194, 193, 192, 207, 206, 210, 209, 208, 223, 222, 
205, 204, 203, 202, 201, 221, 220, 219, 218, 217, 
231, 230, 229, 228, 227, 247, 246, 245, 244, 243, 
226, 225, 224, 239, 238, 242, 241, 240, 255, 254, 
237, 236, 235, 234, 233, 253, 252, 251, 250, 249, 
} ;


static unsigned char mwcMap[96] = {
71, 70, 69, 68, 67, 66, 65, 64, 79, 78, 77, 76, 
95, 94, 93, 92, 87, 86, 85, 84, 83, 82, 81, 80, 
99, 98, 97, 96, 111, 110, 109, 108, 103, 102, 101, 100, 
119, 118, 117, 116, 115, 114, 113, 112, 127, 126, 125, 124, 
7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 
31, 30, 29, 28, 23, 22, 21, 20, 19, 18, 17, 16, 
35, 34, 33, 32, 47, 46, 45, 44, 39, 38, 37, 36, 
55, 54, 53, 52, 51, 50, 49, 48, 63, 62, 61, 60, 
} ;


static TrgSumData trg_sum ;

// read the Trigger RAW data
int trgReader22(char *arg, struct trg_t *trg)
{
	int i ;
	int sdes, ssum, sraw ;
	int len ;


	struct TRGD *trgd = (struct TRGD *)arg ;

	int swap = 0;
	if(trgd->bh.byte_order != DAQ_RAW_FORMAT_ORDER) swap=1;

	len = swap ? swap32(trgd->bh.length)*4 : trgd->bh.length*4;

	sdes = sizeof(trgd->bh) + sizeof(trgd->desc) ;
	ssum = sdes + sizeof(trgd->sum) ;
	sraw = ssum + sizeof(trgd->raw[0]) ;

	if(len < ssum) {
		LOG(NOTE,"No TRG Summaries, or Raw...",0,0,0,0,0) ;
		return 0 ;
	}
	else if(len < sraw) {
		LOG(NOTE,"TRG: Summaries ONLY!",0,0,0,0,0) ;
		return 0 ;
	}

	LOG(DBG,"len %d, desc len %d, sum len %d, raw trg 0 len %d, all raw len %d",
	    len,sizeof(trgd->desc),sizeof(trgd->sum),sizeof(trgd->raw[0]),sizeof(trgd->raw)) ;

	LOG(DBG,"evt desc bytes %d, sum bytes %d, raw bytes %d",trgd->desc.TCUdataBytes,
	    trgd->sum.TrgSumBytes,trgd->raw[0].RawDetBytes,0,0) ;


	trg->npre = swap ? swap16(trgd->desc.npre) : trgd->desc.npre ;
	trg->npost = swap ? swap16(trgd->desc.npost) : trgd->desc.npost ;

	trg->xing_lo = swap ? swap32(trgd->desc.bunchXing_lo) : trgd->desc.bunchXing_lo ;
	trg->xing_hi = swap ? swap32(trgd->desc.bunchXing_hi) : trgd->desc.bunchXing_hi ;

	trg->phys_word = swap ? swap16(trgd->desc.physicsWord) : trgd->desc.physicsWord ;
	trg->trg_word = swap ? swap16(trgd->desc.TriggerWord) : trgd->desc.TriggerWord ;

	trg->trgd = (void *) trgd ;

// what the hell did I mean by this "40 is spurious"???
//	int trg_length = len - sizeof(struct bankHeader) - 40 ;	// 40 is supurious

	int trg_length = len - sizeof(struct bankHeader) ;
	int exp_length = sizeof(EvtDescData) + sizeof(trgd->sum) + sizeof(trgd->raw[0]) + 
			sizeof(trgd->raw[0])*trg->npre + sizeof(trgd->raw[0])*trg->npost ;

	if(trg_length != exp_length) {
		LOG(NOTE,"Trigger data: is %d, expect %d bytes, trg->npre %d, trg->npost %d",
		    trg_length, exp_length,trg->npre,trg->npost,0) ;
	}
	else {
		LOG(DBG,"Trigger data: is %d, expect %d bytes, trg->npre %d, trg->npost %d",
		    trg_length, exp_length,trg->npre,trg->npost,0) ;
	}

	// get the last DSM aka TCU bits
	trg->tcubits = swap ? swap16(trgd->desc.DSMInput) : trgd->desc.DSMInput;
	trg->detlive = swap ? swap16(trgd->desc.externalBusy) : trgd->desc.externalBusy ;


	LOG(DBG,"TrgDataFmtVer 0x%X, 0x%04X %c %c", trgd->desc.TrgDataFmtVer,
	    trgd->sum.TrgSumHeader,
	    trgd->raw[0].CTBdataHeader[0], 
	    trgd->raw[0].CTBdataHeader[1],0) ;

	LOG(DBG,"TrgSumBytes %d, 0x%04X %c %c %c", trgd->sum.TrgSumBytes,
	    trgd->sum.L0SumHeader,
	    trgd->raw[0].RawDetHeader[0], 
	    trgd->raw[0].RawDetHeader[1],0) ;




	// Trigger Summary
	// set pointer & clear all to 0
	trg->trg_sum = (void *) &trg_sum ;
	memset(&trg_sum,0,sizeof(trg_sum)) ;

	for(i=0;i<2;i++) {
		trg_sum.L1Sum[i] = swap ? swap32(trgd->sum.L1Sum[i]) : trgd->sum.L1Sum[i] ;
		trg_sum.L2Sum[i] = swap ? swap32(trgd->sum.L2Sum[i]) : trgd->sum.L2Sum[i] ;
	}


// Arghhh, I wish I had some generic programming tools...
#define REAL_BAD_HACK(x)	trg_sum.DSMdata.x = (swap ? swap16(trgd->sum.DSMdata.x) : trgd->sum.DSMdata.x) 

	for(i=0;i<32;i++) {
		REAL_BAD_HACK(CPA[i]) ;
	}
	for(i=0;i<16;i++) {
		REAL_BAD_HACK(BCdata[i]) ;
	}

	for(i=0;i<8;i++) {
		REAL_BAD_HACK(quadDSM[i]) ;
		REAL_BAD_HACK(lastDSM[i]) ;
		REAL_BAD_HACK(VTX[i]) ;
		REAL_BAD_HACK(EMC[i]) ;
		REAL_BAD_HACK(specialTriggers[i]) ;
		REAL_BAD_HACK(FPD[i]) ;
	}

#undef REAL_BAD_HACK


	// RAW data stuff. Just the crossing - NOT pre/post!


	for(i=0;i<240;i++) {
		trg->CTB[i] = trgd->raw[0].CTB[ctbMap[i]] ;
	}

	for(i=0;i<96;i++) {
		trg->MWC[i] = trgd->raw[0].MWC[mwcMap[i]] ;
	}

	// east
	for(i=0;i<240;i++) {
		trg->BEMC[0][i] = trgd->raw[0].BEMCEast[i] ;
	}
	// west
	for(i=0;i<240;i++) {
		trg->BEMC[1][i] = trgd->raw[0].BEMCWest[i] ;
	}
	// layer1
	for(i=0;i<48;i++) {
		trg->BEMC_l1[i] = swap ? swap16(trgd->raw[0].BEMClayer1[i]) : trgd->raw[0].BEMClayer1[i] ;
	}



	for(i=0;i<144;i++) {
		trg->EEMC[i] = trgd->raw[0].EEMC[i] ;
	}
	for(i=0;i<16;i++) {
		trg->EEMC_l1[i] = swap ? swap16(trgd->raw[0].EEMClayer1[i]) :  trgd->raw[0].EEMClayer1[i];
	}

	// FPD:
	// first index is 0 East, 1 West
	// second index is 0 NortSouth, 1 TopBottom
	// East, NS
	for(i=0;i<112;i++) {
		trg->FPD[0][0][i] = trgd->raw[0].FPDEastNSLayer0[i] ;
	}
	for(i=0;i<8;i++) {
	  trg->FPD_l1[0][0][i] = swap ? swap16(trgd->raw[0].FPDEastNSLayer1[i]) : trgd->raw[0].FPDEastNSLayer1[i] ;
	}
	// East, TB
	for(i=0;i<64;i++) {
		trg->FPD[0][1][i] = trgd->raw[0].FPDEastTBLayer0[i] ;
	}
	for(i=0;i<8;i++) {
		trg->FPD_l1[0][1][i] = swap ? swap16(trgd->raw[0].FPDEastTBLayer1[i]) : trgd->raw[0].FPDEastTBLayer1[i] ;
	}
	// West, NS
	for(i=0;i<112;i++) { 
		trg->FPD[1][0][i] = trgd->raw[0].FPDWestNSLayer0[i] ;
	}
	for(i=0;i<8;i++) {
		trg->FPD_l1[1][0][i] = swap ? swap16(trgd->raw[0].FPDWestNSLayer1[i]) : trgd->raw[0].FPDWestNSLayer1[i];
	}
	// West, TB
	for(i=0;i<64;i++) {
		trg->FPD[1][1][i] = trgd->raw[0].FPDWestTBLayer0[i] ;
	}
	for(i=0;i<8;i++) {
		trg->FPD_l1[1][1][i] = swap ? swap16(trgd->raw[0].FPDWestTBLayer1[i]) : trgd->raw[0].FPDWestTBLayer1[i];
	}



	// get the size from trgStructures.h
	for(i=0;i<(int)sizeof(trgd->raw[0].BBC)/(int)sizeof(trgd->raw[0].BBC[0]);i++) {
		trg->BBC[i] = trgd->raw[0].BBC[i] ;
	}

	for(i=0;i<16;i++) {
		trg->BBC_l1[i] = swap ? swap16(trgd->raw[0].BBClayer1[i]) : trgd->raw[0].BBClayer1[i] ;
	}

	for(i=0;i<16;i++) {
		trg->ZDC[i] = trgd->raw[0].ZDC[i] ;
	}
	for(i=0;i<8;i++) {
		trg->ZDC_l1[i] = swap ? swap16(trgd->raw[0].ZDClayer1[i]) : trgd->raw[0].ZDClayer1[i];
	}

	for(i=0;i<32;i++) {
		trg->ZDCSMD[i] = trgd->raw[0].ZDCSMD[i] ;
	}


/*
	int j ;
	for(j=0;j<11;j++) {
	for(i=0;i<16;i++) {
		printf("pre %d, ZDC %d = %u\n",j,i,trgd->raw[j].ZDC[i]) ;
	}
	for(i=0;i<8;i++) {
		printf("pre %d, ZDC_l1 %d = %u\n",j,i,b2h16(trgd->raw[j].ZDClayer1[i])) ;
	}
	}
*/

	return len ;
}

