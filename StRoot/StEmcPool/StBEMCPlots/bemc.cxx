#include "bemc.h"
#include <TH2.h>
#include <TFile.h>

#ifndef NEW_DAQ_READER
#       include "evpReader.hh"
#       include "emcReader.h"
#       include "trgReader.h"
#define BSMD_FIBERS     12
#define BSMD_DATSIZE    4800
#else
#       include "DAQ_READER/daqReader.h"
#       include "DAQ_READER/daq_dta.h"
#       include "DAQ_BSMD/daq_bsmd.h"
#       include "DAQ_BTOW/daq_btow.h"
#       include "DAQ_EMC/daq_emc.h"
#       include "DAQ_TRG/daq_trg.h"
#endif

#include <iostream>
using namespace std;

int BEMCHISTOINIT = 0;

int BEMCNJPPED[BEMCNJET];
int BEMCJPPED[BEMCNJET];

TH2F* BEMCHIST2[100];
TH1F* BEMCHIST1[100];

int bemcSave(TFile *f)
{
  if(!f) return 0;
  f->cd();
  cout <<"Saving bemc histograms\n";
  for(int i=0;i<100;i++)
    {
      if(BEMCHIST1[i]) BEMCHIST1[i]->Write();
      if(BEMCHIST2[i]) BEMCHIST2[i]->Write();
    }
  cout <<"bemc histograms saved\n";
  return 0;
}


int bemcReset()
{
  cout <<"Reseting bemc histograms\n";
  for(int i=0;i<100;i++)
  {
    if(BEMCHIST1[i]) BEMCHIST1[i]->Reset();
    if(BEMCHIST2[i]) BEMCHIST2[i]->Reset();
  }
  return 0;
  
}

int bemcMakeHisto()
{
  for(int i=0;i<100;i++) { BEMCHIST1[i] = 0; BEMCHIST2[i] = 0;}
  
  BEMCHIST2[0] =  new TH2F("bemc_TDC_status","BEMC TDC Status (0=total 1=OK 2=Not Installed3=Corrupted)",5,-0.5,4.5,30,-0.5,29.5);
  BEMCHIST2[1] =  new TH2F("bemc_SMD_status","BEMC SMD Status (0=total 1=OK 2=Not Installed3=Corrupted)",5,-0.5,4.5,8,-0.5,7.5);
  BEMCHIST2[2] =  new TH2F("bemc_btow_spectra_1","BEMC tower spectrum  0 < TDC < 10 (X = 160*TDC + index)", 1600,  -0.5,1599.5,100,0,1000);
  BEMCHIST2[3] =  new TH2F("bemc_btow_spectra_2","BEMC tower spectrum 10 < TDC < 20 (X = 160*TDC + index)", 1600,1599.5,3199.5,100,0,1000);
  BEMCHIST2[4] =  new TH2F("bemc_btow_spectra_3","BEMC tower spectrum 20 < TDC < 30 (X = 160*TDC + index)", 1600,3199.5,4799.5,100,0,1000);
  BEMCHIST2[5] =  new TH2F("bsmd_smd_capacitor","BEMC SMD capacitor distribution",128,-0.5,127.5,8,-0.5,7.5);
  BEMCHIST2[6] =  new TH2F("bsmd_smd_sum","BEMC SMD total ADC per fiber",250,100000.,1000000.,8,-0.5,7.5);
  BEMCHIST2[7] =  new TH2F("bsmd_psd_capacitor","BEMC PSD capacitor distribution",128,-0.5,127.5,4,-0.5,3.5);
  BEMCHIST2[8] =  new TH2F("bsmd_psd_sum","BEMC PSD total ADC per fiber",250,100000.,1000000.,4,-0.5,3.5);
  BEMCHIST2[9] =  new TH2F("bemc_HT_spectra","BEMC High Tower spectrum", 300,-0.5,299.5,64,-0.5,63.5);
  BEMCHIST2[10] = new TH2F("bemc_PA_spectra","BEMC Patch Sum spectrum", 300,-0.5,299.5,64,-0.5,63.5);
  BEMCHIST2[11] = new TH2F("bemc_HTMAX_spectra","BEMC Maximum High Tower spectrum", 300,-0.5,299.5,64,-0.5,63.5);
  BEMCHIST2[12] = new TH2F("bemc_PAMAX_spectra","BEMC Maximum Patch Sum spectrum", 300,-0.5,299.5,64,-0.5,63.5);
  BEMCHIST2[13] = new TH2F("bemc_PSD_status","BEMC PSD Status (0=total 1=OK 2=Not Installed 3=Corrupted)",5,-0.5,4.5,4,-0.5,3.5);
  BEMCHIST2[14] = new TH2F("bemc_JET_spectra","BEMC Jet sum spectrum", 12,-0.5,11.5,80,-0.5,79.5);
  BEMCHIST2[15] = new TH2F("bemc_JETMAX_spectra","BEMC Maximum Jet sum spectrum", 12,-0.5,11.5,80,-0.5,79.5);
  BEMCHIST2[16] = new TH2F("bemc_JET_ped","BEMC Jet sum pedestal", 12,-0.5,11.5,30,15,45);
  
  BEMCHIST1[0] = new TH1F("bemc_BTOW_Corruption","BEMC TDC corruption frequency (0=total 1=OK 2=Not Installed 3=Corrupted)",5,-0.5,4.5);
  BEMCHIST1[1] = new TH1F("bsmd_smd_spectra","BEMC SMD total ADC",250,100000.,6000000.);
  BEMCHIST1[2] = new TH1F("bsmd_psd_spectra","BEMC PSD total ADC",250,100000.,4000000.);
  BEMCHIST1[3] = new TH1F("bemc_HTMAX_dist","BEMC Maximum High Tower distribution", 300,-0.5,299.5);
  BEMCHIST1[4] = new TH1F("bemc_PAMAX_dist","BEMC Maximum Patch Sum distribution", 300,-0.5,299.5);
  BEMCHIST1[5] = new TH1F("bemc_JETMAX_dist","BEMC Maximum Jet sum distribution", 12,-0.5,11.5);
  
  return 0;
}

int bemcInit()
{
  //  	bemcMakeHisto();
	for(int i=0;i<BEMCNJET;i++) 
	{
	  BEMCJPPED[i] = 0;
	  BEMCNJPPED[i] = 0;
	} 
	BEMCHISTOINIT = 1;
	return 0;
}

int bemcFillHisto(char* rdr, const unsigned char *, const unsigned char *) {
  int ret =  -1;
  if(BEMCHISTOINIT==0) bemcInit();

#ifdef NEW_DAQ_READER
  daqReader *daqrdr = (daqReader *)rdr;
#else
  ret = emcReader(rdr);
  trgReader(rdr);
#endif  

  
  //  int event_size = ret;
  int STATUS = BEMCNOTINSTALLED; //NOT PRESENT

  //////////////////////////////////////////////////////////////////
  // BTOW
  //
#if 0
  daq_dta *ddbtow = daqrdr->det("btow")->get("adc") ; 
  if(ddbtow){
    while(ddbtow->iterate()){
      btow_t* btowdata = (btow_t*)ddbtow->Void;
      for(int i = 0; i < BTOW_MAXFEE; i++){
	for(int j = 0; j < BTOW_PRESIZE; j++){
	  cout<<btowdata->preamble[i][j]<<endl; //This is the header
	}   
	for(int j = 0; j < BTOW_DATSIZE; j++){
	  cout<<btowdata->adc[i][j]<<endl;//This are the adcs      
	}   
      }
    }
  } 
#endif

#ifdef NEW_DAQ_READER
    daq_dta *dd_btow = daqrdr ? (daqrdr->det("btow")->get("adc")) : 0;
    if (dd_btow) while (dd_btow->iterate()) {
        btow_t *d = (btow_t *) dd_btow->Void;
        if (d) {
#else
    if ((ret >= 0) && emc.btow_in) { // Barrel towers are present
        unsigned short *header = emc.btow_raw; // BTOW event header
        if (header) {
#endif
	    int TDCSum[BTOW_MAXFEE];
	    int TDCStatus[BTOW_MAXFEE];
	    int TDCTotal = 0;
	    STATUS = 1; //OK
	    for (int i = 0; i < BTOW_MAXFEE;i++) {
		TDCSum[i] = 0;
		TDCStatus[i] = BEMCNOTINSTALLED; // NOT INSTALLED
#ifdef NEW_DAQ_READER
                int count = d->preamble[i][0];
                int error = d->preamble[i][1];
#else
#ifdef BEMCNOSWAP
	        int count = (*(header+i));
	        int error = (*(header+i+30));
#else
		int count = swap16(*(header+i));
	        int error = swap16(*(header+i+30));
#endif
#endif
		if(error==0 && count==(BTOW_PRESIZE+BTOW_DATSIZE)) TDCStatus[i] = BEMCOK; // OK
	        else if(error==4095 && count==4095) TDCStatus[i] = BEMCNOTINSTALLED; // NOT INSTALLED
	        else TDCStatus[i] = BEMCCORRUPTED; //CORRUPTED    
	        if(TDCStatus[i] == BEMCCORRUPTED) STATUS = BEMCCORRUPTED;
	        if (BEMCHIST2[0]) BEMCHIST2[0]->Fill(0.0, i);
	        if (BEMCHIST2[0]) BEMCHIST2[0]->Fill((float)TDCStatus[i], i);
	    }
            for (int i = 0;i < (BTOW_MAXFEE * BTOW_DATSIZE);i++) {
                int tdc = i % BTOW_MAXFEE;
#ifdef NEW_DAQ_READER
                int tdc_channel = i / BTOW_MAXFEE;
                int count = d->preamble[tdc][0];
                int error = d->preamble[tdc][1];
#else
                int count = (*(header + tdc));
                int error = (*(header + tdc + 30));
#endif
                if((error==0) && (count == (BTOW_PRESIZE + BTOW_DATSIZE))) {
#ifdef NEW_DAQ_READER
                    int adc = d->adc[tdc][tdc_channel];
#else
                    int adc = emc.btow[i];
#endif
	    	    TDCSum[tdc] += adc;
	    	    TDCTotal += adc;
	    	    int daqid = ((tdc * BTOW_DATSIZE) + tdc_channel);
	    	    if(tdc>=0  && tdc<10 && TDCStatus[tdc]!=BEMCNOTINSTALLED && BEMCHIST2[2]) BEMCHIST2[2]->Fill(daqid, adc);
	    	    if(tdc>=10 && tdc<20 && TDCStatus[tdc]!=BEMCNOTINSTALLED && BEMCHIST2[3]) BEMCHIST2[3]->Fill(daqid, adc);
	    	    if(tdc>=20 && tdc<30 && TDCStatus[tdc]!=BEMCNOTINSTALLED && BEMCHIST2[4]) BEMCHIST2[4]->Fill(daqid, adc);
		}
	    }
	}
    }
    if (BEMCHIST1[0]) BEMCHIST1[0]->Fill(0.0);
    if (BEMCHIST1[0]) BEMCHIST1[0]->Fill(STATUS);
  //
  //
  ////////////////////////////////////////////////////////////////
  // BSMD
  //
#if 0
  daq_dta* ddbsmd;
  for(int f=1;f<=12;f++) {
    ddbsmd =  daqrdr->det("bsmd")->get("adc",0,f) ;    
    if(ddbsmd) {
      while(ddbsmd->iterate()) {

	bsmd_t *bsmddata = (bsmd_t *) ddbsmd->Void ;
	printf("BSMD : fiber %2d, capacitor %d:\n",ddbsmd->rdo,bsmddata->cap); 
	for(int i=0;i<BSMD_DATSIZE;i++) {
	  printf("   %4d = %4d\n",i,bsmddata->adc[i]);
	}
      }
    }
  }
#endif
  
    int totalSumSMD = 0;
    int totalSumPSD = 0;
    for (int bsmd_fiber = 0;bsmd_fiber < BSMD_FIBERS;bsmd_fiber++) {
        int bprs_fiber = bsmd_fiber - 8;
#ifdef NEW_DAQ_READER
        daq_dta *dd_bsmd = daqrdr ? (daqrdr->det("bsmd")->get("adc", 0, bsmd_fiber)) : 0;
        if (dd_bsmd) while (dd_bsmd->iterate()) {
            bsmd_t *d = (bsmd_t *) dd_bsmd->Void;
            if (d) {
#else
        if (emc.bsmd_in) {
            {
#endif
#ifdef NEW_DAQ_READER
    		int cap = d->cap;
#else
    		int cap = emc.bsmd_cap[bsmd_fiber];
#endif
		int fiberSum = 0;
        	for (int fiber_channel = 0;fiber_channel < BSMD_DATSIZE;fiber_channel++) {
#ifdef NEW_DAQ_READER
        	    int adc = d->adc[fiber_channel];
#else
        	    int adc = emc.bsmd[bsmd_fiber][fiber_channel];
#endif
    		    fiberSum += adc;
    		    if (bsmd_fiber < 8) totalSumSMD += adc;
    		    else totalSumPSD += adc;
		}
		int STATUS = BEMCOK;
		if(fiberSum==0) STATUS = BEMCNOTINSTALLED;
		//fprintf(stderr,"Sum for fiber %d = %f\n",bsmd_fiber,fiberSum);
		if(bsmd_fiber < 8) {
		    if (BEMCHIST2[1]) BEMCHIST2[1]->Fill(0.0, bsmd_fiber);
    		    if (BEMCHIST2[1]) BEMCHIST2[1]->Fill(STATUS, bsmd_fiber);
		    if(STATUS==BEMCOK) {
    			if (BEMCHIST2[5]) BEMCHIST2[5]->Fill(cap, bsmd_fiber);
    			if (BEMCHIST2[6]) BEMCHIST2[6]->Fill(fiberSum, bsmd_fiber);
		    }
		} else {
    		    if (BEMCHIST2[13]) BEMCHIST2[13]->Fill(0.0, bprs_fiber);
    		    if (BEMCHIST2[13]) BEMCHIST2[13]->Fill(STATUS, bprs_fiber);
		    if(STATUS==BEMCOK) {
			if (BEMCHIST2[7]) BEMCHIST2[7]->Fill(cap, bprs_fiber);
    			if (BEMCHIST2[8]) BEMCHIST2[8]->Fill(fiberSum, bprs_fiber);
    		    }
		}
    	    }
	}
    }
    if (BEMCHIST1[1]) BEMCHIST1[1]->Fill(totalSumSMD);
    if (BEMCHIST1[2]) BEMCHIST1[2]->Fill(totalSumPSD);

  //
  //
  ////////////////////////////////////////////////////////////////
  // BTOW TRIGGER
  //
  int HT[300],PA[300];
  int JP[BEMCNJET];
  
  int JPSTART[BEMCNJET] = {0,30,50,80,100,130,150,180,200,230,250,280};
  int JPEXTRA[BEMCNJET][5] = {{21, 23, 25, 27, 29},
                              {20, 22, 24, 26, 28},
			      {71, 73, 75, 77, 79},
			      {70, 72, 74, 76, 78},
			      {121, 123, 125, 127, 129},
			      {120, 122, 124, 126, 128},
			      {171, 173, 175, 177, 179},
			      {170, 172, 174, 176, 178},
			      {221, 223, 225, 227, 229},
			      {220, 222, 224, 226, 228},
			      {271, 273, 275, 277, 279},
			      {270, 272, 274, 276, 278}};
  
  int MAXHT = 0;
  int MAXPA = 0;
  int MAXHTID = 0;
  int MAXPAID = 0;
  const int nDSMs = 30;
  unsigned char raw[480];
  if(ret>0)
  {
    for(int i=0;i<BEMCNJET;i++) JP[i] = 0;

#ifdef NEW_DAQ_READER
    daq_dta *dd_trg = daqrdr ? (daqrdr->det("trg")->get("legacy")) : 0;
    if (dd_trg) while (dd_trg->iterate()) {
        trg_t *d = (trg_t *) dd_trg->Void;
        if (d) {
	    const unsigned char *dsmL0WestInput = &(d->BEMC[0][0]);
	    const unsigned char *dsmL0EastInput = &(d->BEMC[1][0]);
#else
    {
        {
	    const unsigned char *dsmL0WestInput = &(trg.BEMC[0][0]);
	    const unsigned char *dsmL0EastInput = &(trg.BEMC[1][0]);
#endif
 
	    for(int i=0;i<240;i++) {
    		raw[i]=(unsigned char)dsmL0EastInput[i];
    		raw[i+240]=(unsigned char)dsmL0WestInput[i];
	    }
	}
    }
    for(int i=0;i<nDSMs*10;i++)
    {
      HT[i] = 0;
      PA[i] = 0;
    }
    int patch;
    int dsm_read_map[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
    int tower_map[10]={0,1,2,3,4,5,6,7,8,9};  // map into DSM board
    unsigned char dsmby[30][16];
    unsigned char ch[16];

    for (int i=0; i<30; i++)
      for (int j=0; j<16; j++)
      {
        int k = 16*i + j;
        dsmby[i][j] = raw[k];
      }
    for(int i=0;i<nDSMs;i++)
    {
      patch = i;
      for(int j=0;j<16;j++)
      {
        int k = dsm_read_map[j];
        ch[k]= dsmby[i][j];
      }
      int nt=0;
      for(int k=0;k<5;k++)
      {
        int nby=3*k;
        int hi_tower = (ch[nby]) & 0x3f;
        int sum_tower = ((ch[nby]>>6) & 0x3) + (((ch[nby+1]) & 0xf) << 2);
        int it =  tower_map[nt] + 10*(patch);
        HT[it] = hi_tower;
        PA[it] = sum_tower;
        if(HT[it]>MAXHT && HT[it]<63) {MAXHT = HT[it]; MAXHTID = it;}
        if(PA[it]>MAXPA && PA[it]<63) {MAXPA = PA[it]; MAXPAID = it;}
        nt++;

        hi_tower = ((ch[nby+1]>>4) & 0xf) + (((ch[nby+2]) & 0x3) << 4);
        sum_tower = ((ch[nby+2]>>2) & 0x3f);
        it = tower_map[nt] + 10*(patch);
        HT[it] = hi_tower;
        PA[it] = sum_tower;
        if(HT[it]>MAXHT && HT[it]<63) {MAXHT = HT[it]; MAXHTID = it;}
        if(PA[it]>MAXPA && PA[it]<63) {MAXPA = PA[it]; MAXPAID = it;}
        nt++;
      }
    } //nDSM

    int HTTH = 0;
    int PATH = 0;
    for(int i=0; i<300; i++)
    {
      if((HT[i] > HTTH) && BEMCHIST2[9]) BEMCHIST2[9]->Fill((float)i,(float)HT[i]);
      if((PA[i] > PATH) && BEMCHIST2[10]) BEMCHIST2[10]->Fill((float)i,(float)PA[i]);
    }  
    
    HTTH = 0;
    PATH = 0;
    if((MAXHT > HTTH) && BEMCHIST2[11]) BEMCHIST2[11]->Fill((float)MAXHTID,(float)MAXHT);
    if((MAXPA > PATH) && BEMCHIST2[12]) BEMCHIST2[12]->Fill((float)MAXPAID,(float)MAXPA);
    
    
    HTTH = 12;
    PATH = 12;
    if((MAXHT > HTTH) && BEMCHIST1[13]) BEMCHIST1[3]->Fill((float)MAXHTID);
    if((MAXPA > PATH) && BEMCHIST1[4]) BEMCHIST1[4]->Fill((float)MAXPAID);
    
    // making JET patch
    int MAXJETID =0;
    int MAXJETVALUE =-9999;
    for(int i=0;i<BEMCNJET;i++)
    {
      JP[i] = 0;
      for(int j = 0;j<20;j++) JP[i]+=PA[j+JPSTART[i]];
      for(int j = 0;j<5;j++)  JP[i]+=PA[JPEXTRA[i][j]];  
      if(JP[i]>MAXJETVALUE) { MAXJETVALUE = JP[i]; MAXJETID=i;}  
      if (BEMCHIST2[14]) BEMCHIST2[14]->Fill(i,JP[i]);  
    }
    if (BEMCHIST2[15]) BEMCHIST2[15]->Fill(MAXJETID,MAXJETVALUE);
    
    PATH = 35;
    if((MAXJETVALUE > PATH) && BEMCHIST1[5]) BEMCHIST1[5]->Fill(MAXJETID);
    
    for(int i=0;i<BEMCNJET;i++) if(i!=MAXJETID)
    {
       BEMCJPPED[i]+=JP[i];
       BEMCNJPPED[i]++;
       if(BEMCNJPPED[i]==10)
       {
         if (BEMCHIST2[16]) BEMCHIST2[16]->Fill(i,(float)BEMCJPPED[i]/(float)BEMCNJPPED[i]);
	 BEMCJPPED[i] = 0;
	 BEMCNJPPED[i] = 0;
       } 
    }
    
    
  }
  return 0;
}






/***************************************************************************
 *
 * $Id: bemc.cxx,v 1.4 2009/01/24 01:13:49 ogrebeny Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: bemc.cxx,v $
 * Revision 1.4  2009/01/24 01:13:49  ogrebeny
 * Now uses the new DAQ reader
 *
 * Revision 1.2  2009/01/21 03:22:38  ogrebeny
 * Made it compilable with the old EVP_READER
 *
 * Revision 1.1  2009/01/18 00:58:31  ogrebeny
 * Better separate EMC histogramming from OnlinePlots infrastructure
 *
 * Revision 1.9  2009/01/16 20:47:58  fine
 * emc progress
 *
 * Revision 1.8  2009/01/16 20:26:11  fine
 * unlock Emc histograms
 *
 * Revision 1.7  2009/01/13 00:39:07  fine
 * Add rootrc parameters
 *
 * Revision 1.6  2009/01/09 15:05:36  fine
 * Renee Fatemi vrsion of bemc class
 *
 * Revision 1.5  2009/01/08 20:10:51  fine
 * fix the bemc interfaces
 *
 * Revision 1.4  2009/01/08 19:39:28  fine
 * fix the bemcFillHisto function signature  HistoHandler.cxx
 *
 * Revision 1.3  2008/12/19 17:09:16  fine
 * the first full compilation against of the new DAQ Reader
 *
 * Revision 1.2  2008/12/19 15:51:19  dkettler
 * Added new daqReader
 *
 * Revision 1.1  2007/02/27 15:23:40  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/
