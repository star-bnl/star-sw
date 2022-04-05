#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

#include <rtsLog.h>
#include <rts.h>  // for the sake of swap32 entry

#include <rtsSystems.h>

// This reader works for TRG_VERSIONS: 0x30 (FY07 run)

// one needs to diffuse the crummy trigger stuff first
#define TRG_VERSION	0x32
#include <daqFormats.h>



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

static TrgSumData trg_sum ;

// read the Trigger RAW data
int trgReader32(char *arg, int bytes, int swap, struct trg_t *trg)
{
	int i ;
	int sdes, ssum, sraw ;

	EvtDescData *desc = (EvtDescData *)arg;
	TrgSumData *sum = (TrgSumData *)(arg + sizeof(EvtDescData));
	//int swap = 1;

	sdes = sizeof(EvtDescData) ;
	ssum = sdes + sizeof(TrgSumData) ;

	if(bytes < ssum + 4) {
		LOG(NOTE,"No TRG Summaries, or Raw...",0,0,0,0,0) ;
		return 0 ;
	}

	RawTrgDet *raw = (RawTrgDet *)(arg + sizeof(EvtDescData) + sizeof(TrgSumData));
	int rdb = swap ? swap16(raw[0].RawDetBytes) : raw[0].RawDetBytes;
      
	sraw = ssum + rdb;

	int minsize = sizeof(EvtDescData) + sizeof(TrgSumData) + 1772;


	if(sraw < minsize) {
	  LOG(NOTE, "Trigger bank size smaller than minimum %d vs %d", sraw, minsize);
	  return 0;
	}

	if(bytes < sraw) {
	  LOG(NOTE,"Trigger Bank too small %d vs %d",bytes,sraw);
		return 0 ;
	}


//	LOG(DBG,"evt desc bytes %d, sum bytes %d, raw bytes %d",desc->TCUdataBytes,
//	    sum->TrgSumBytes,raw[0].RawDetBytes,0,0) ;
	LOG(DBG,"evt desc bytes %d, sum bytes %d, raw bytes %d",sizeof(EvtDescData),sizeof(TrgSumData),rdb,0) ;


	trg->npre = swap ? swap16(desc->npre) : desc->npre ;
	trg->npost = swap ? swap16(desc->npost) : desc->npost ;

	trg->xing_lo = swap ? swap32(desc->bunchXing_lo) : desc->bunchXing_lo ;
	trg->xing_hi = swap ? swap32(desc->bunchXing_hi) : desc->bunchXing_hi ;

	trg->phys_word = swap ? swap16(desc->physicsWord) : desc->physicsWord ;
	trg->trg_word = swap ? swap16(desc->TriggerWord) : desc->TriggerWord ;

// what the hell did I mean by this "40 is spurious"???
//	int trg_length = len - sizeof(struct bankHeader) - 40 ;	// 40 is supurious

	unsigned int trg_length = bytes;

	/* 
	int exp_length = sizeof(EvtDescData) + sizeof(trgd->sum) + sizeof(trgd->raw[0]) + 
			sizeof(trgd->raw[0])*trg->npre + sizeof(trgd->raw[0])*trg->npost ;

	if(trg_length != exp_length) {
		LOG(NOTE,"Trigger data: is %d, expect %d bytes, trg->npre %d, trg->npost %d",
		    trg_length, exp_length,trg->npre,trg->npost,0) ;
	}
	else {
		LOG(DBG,"Trigger data: is %d, expect %d bytes, trg->npre %d, trg->npost %d",
		    trg_length, exp_length,trg->npre,trg->npost,0) ;
	} */

	if(trg_length > sizeof(TrgDataType)) {
	  LOG(ERR, "Trigger data is too large:  %d bytes more than max of %d",trg_length,sizeof(TrgDataType));
	}
	
	if(trg_length < sizeof(EvtDescData) + sizeof(TrgSumData)) {
	  LOG(ERR, "Trigger data is too small:  %d bytes less than min of %d",trg_length,(sizeof(EvtDescData) +sizeof(TrgSumData)));
	}
	  
	// get the last DSM aka TCU bits
	trg->tcubits = swap ? swap16(desc->DSMInput) : desc->DSMInput;
	trg->detlive = swap ? swap16(desc->externalBusy) : desc->externalBusy ;


	LOG(DBG,"TrgDataFmtVer 0x%X, 0x%04X %c %c", desc->TrgDataFmtVer,
	    sum->TrgSumHeader,
	    raw[0].CTBdataHeader[0], 
	    raw[0].CTBdataHeader[1],0) ;

	LOG(DBG,"TrgSumBytes %d, 0x%04X %c %c", sum->TrgSumBytes,
	    sum->L0SumHeader,
	    raw[0].RawDetHeader[0], 
	    raw[0].RawDetHeader[1],0) ;




	// Trigger Summary
	// set pointer & clear all to 0
	trg->trg_sum = (void *) &trg_sum ;
	memset(&trg_sum,0,sizeof(trg_sum)) ;

	for(i=0;i<2;i++) {
		trg_sum.L1Sum[i] = swap ? swap32(sum->L1Sum[i]) : sum->L1Sum[i] ;
		trg_sum.L2Sum[i] = swap ? swap32(sum->L2Sum[i]) : sum->L2Sum[i] ;
	}


// Arghhh, I wish I had some generic programming tools...
#define REAL_BAD_HACK(x)	trg_sum.DSMdata.x = (swap ? swap16(sum->DSMdata.x) : sum->DSMdata.x) 

	for(i=0;i<8;i++) REAL_BAD_HACK(MTD[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(VPD[i]);
	for(i=0;i<16;i++) REAL_BAD_HACK(CPA[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(CTB[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(lastDSM[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(VTX[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(EMC[i]);
	for(i=0;i<16;i++) REAL_BAD_HACK(BCdata[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(specialTriggers[i]);
	for(i=0;i<8;i++) REAL_BAD_HACK(FPD[i]);

#undef REAL_BAD_HACK

	// RAW data stuff. Just the crossing - NOT pre/post!

	for(i=0;i<240;i++) {
		trg->CTB[i] = raw[0].CTB[ctbMap[i]] ;
	}

	// MWC is now gone...
// 	for(i=0;i<96;i++) {
// 		trg->MWC[i] = raw[0].MWC[mwcMap[i]] ;
// 	}
	memset(trg->MWC, 0, sizeof(trg->MWC));
	
	for(i=0;i<32;i++) trg->MTD[i] = raw[0].MTD[i];
	for(i=0;i<64;i++) trg->VPD[i] = raw[0].VPD[i];
	if(desc->TrgDataFmtVer >= 0x32) {
	  for(i=0;i<32;i++) {
	    trg->P2P[i] = raw[0].P2P[i];
	  }
	}
	else {
	  memset(trg->P2P, 0, sizeof(trg->P2P));
	}
	for(i=0;i<16;i++) trg->TOF[i] = raw[0].TOF[i];

	// east
	for(i=0;i<240;i++) {
		trg->BEMC[0][i] = raw[0].BEMCEast[i] ;
	}
	// west
	for(i=0;i<240;i++) {
		trg->BEMC[1][i] = raw[0].BEMCWest[i] ;
	}
	// layer1
	for(i=0;i<48;i++) {
		trg->BEMC_l1[i] = swap ? swap16(raw[0].BEMClayer1[i]) : raw[0].BEMClayer1[i] ;
	}



	for(i=0;i<144;i++) {
		trg->EEMC[i] = raw[0].EEMC[i] ;
	}
	for(i=0;i<16;i++) {
		trg->EEMC_l1[i] = swap ? swap16(raw[0].EEMClayer1[i]) :  raw[0].EEMClayer1[i];
	}

	// FPD:
	// first index is 0 East, 1 West
	// second index is 0 NortSouth, 1 TopBottom
	// East, NS
	for(i=0;i<112;i++) {
		trg->FPD[0][0][i] = raw[0].FPDEastNSLayer0[i] ;
	}
	for(i=0;i<8;i++) {
	  trg->FPD_l1[0][0][i] = swap ? swap16(raw[0].FPDEastNSLayer1[i]) : raw[0].FPDEastNSLayer1[i] ;
	}
	// East, TB
	for(i=0;i<64;i++) {
		trg->FPD[0][1][i] = raw[0].FPDEastTBLayer0[i] ;
	}
	// Not existing in 2007
	/*
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
	*/

	for(i=0;i<256;i++) trg->FPDW[i] = raw[0].FPDW[i];


	// get the size from trgStructures.h
	for(i=0;i<(int)sizeof(raw[0].BBC)/(int)sizeof(raw[0].BBC[0]);i++) {
		trg->BBC[i] = raw[0].BBC[i] ;
	}

	for(i=0;i<16;i++) {
		trg->BBC_l1[i] = swap ? swap16(raw[0].BBClayer1[i]) : raw[0].BBClayer1[i] ;
	}

	for(i=0;i<16;i++) {
		trg->ZDC[i] = raw[0].ZDC[i] ;
	}
	for(i=0;i<8;i++) {
		trg->ZDC_l1[i] = swap ? swap16(raw[0].ZDClayer1[i]) : raw[0].ZDClayer1[i];
	}

	for(i=0;i<32;i++) {
		trg->ZDCSMD[i] = raw[0].ZDCSMD[i] ;
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

	trg->QQTdataBytes = swap ? swap16(raw[0].QQTdataBytes) : raw[0].QQTdataBytes;
	if(trg->QQTdataBytes/4 <= 1600){
	  for(i=0; i<trg->QQTdataBytes/4; i++){
	    trg->QQTdata[i] = swap ? swap32(raw[0].QQTdata[i]) : raw[0].QQTdata[i];
	  }
	}
	
	return bytes ;
}
