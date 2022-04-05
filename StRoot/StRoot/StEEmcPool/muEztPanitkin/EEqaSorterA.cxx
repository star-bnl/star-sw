// $Id: EEqaSorterA.cxx,v 1.10 2010/04/09 04:48:33 ogrebeny Exp $
#include <string.h>
#include <stdlib.h>

#include <TObjArray.h>
#include <TClonesArray.h>

#include <TH2.h>
#include <TFile.h>

#include <StMessMgr.h>

#include "EEqaSorterA.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(EEqaSorterA)

//-------------------------------------------
EEqaSorterA::EEqaSorterA() 
    : TObject()
    , hCrate(0)
    , hCrateHot(0)
    , hotTwThres(40)// used to count hot towers
{
  memset(feePed,0,sizeof(feePed)); // clear ped values
} 
 
//-------------------------------------------
EEqaSorterA::~EEqaSorterA() {
    for(int icr = 0;icr < MaxAnyCrate;icr++) { 
	if (hCrate && hCrate[icr]) delete hCrate[icr];
    }
    if (hCrate) delete hCrate;
    for(int icr = 0;icr < MaxTwCrateID;icr++) { 
	if (hCrateHot && hCrateHot[icr]) delete hCrateHot[icr];
    }
    if (hCrateHot) delete hCrateHot;
}

//-------------------------------------------
void EEqaSorterA::sort(const EztEmcRawData *t, const EztEmcRawData *s, int ver) {
  sortDaqTower1(t);
  sortDaqMapmt0(s, ver);
  sortDaqTowerHot(t);
}

//-------------------------------------------
void  EEqaSorterA::Finish(){
}

//-------------------------------------------
void  EEqaSorterA::sortDaqTower1(const EztEmcRawData *t){
  /* Goals of this method:
     make histos of adc vs channel for each tower crate
     and count ADC values = n*256 in all data blocks 
  */
  if(!t) return;
  for(int icr=0;icr<t->getNBlocks() && icr<MaxTwCrateID;icr++) {
    if(t->isCrateVoid(icr)) continue;
    int crateID=icr+1;
    const UShort_t* data=t->data(icr);
    for(int i=0;i<t->sizeData(icr) && i<MaxTwCrateCh;i++) {
      int chan=i;
      float adc=data[i];
      if (hCrate && hCrate[crateID - 1]) hCrate[crateID - 1]->Fill(adc, chan);
    }
  }// end of loop over crates
}

//-------------------------------------------
void  EEqaSorterA::sortDaqTowerHot(const EztEmcRawData *t){
  /* Goals of this method:
     increment (hot) towers above some threshold 
  */

  if(!t) return;
  for(int icr=0;icr<t->getNBlocks() && icr<MaxTwCrateID;icr++) {
    if(t->isCrateVoid(icr)) continue;
    int crateID=icr+1;
    const UShort_t* data=t->data(icr);
    int *pedA= &feePed[(crateID-1)*MaxTwCrateCh];
    if (data) for(int i=0;i<t->sizeData(icr) && i<MaxTwCrateCh;i++) {
      int chan=i;
      float adc=data[i]-24+ pedA[i];
      if(adc<hotTwThres) continue;
      if (hCrateHot && hCrateHot[crateID-1]) hCrateHot[crateID-1]->Fill(chan);
    }
  }// end of loop over crates
}

//-------------------------------------------
void  EEqaSorterA::sortDaqMapmt0(const EztEmcRawData *s, int ver){
  /* Goal of this method:
     make histos of adc vs channel for each MAPMT crate
  */
  if(!s) return;
  for(int icr=0;icr<s->getNBlocks();icr++) {
    if(s->isCrateVoid(icr)) continue;
    int crateID=icr+64;
    // in 2004 there was only 16 MAPMT crates for sectors 5-8
    if(ver<0x22) {
      if(icr>=16) break;
      crateID=icr+84;
    }
    const UShort_t* data=s->data(icr);
    for(int i=0;i<s->sizeData(icr) && i<MaxMapmtCrateCh;i++) {
      int chan=i; 
      float adc=data[i];
      if (hCrate && hCrate[crateID-1]) hCrate[crateID-1]->Fill(adc,chan);
    }
  }// end of loop over crates
  
}

//--------------------------------------------------
void EEqaSorterA::initCrateHisto(TObjArray *HList, int nBin, int mxADC) {
  const char *sectL[]={"11TD-1TC", "1TD-3TC", "3TD-5TC", "5TD-7TC", "7TD-9TC", "9TD-11TC"};
  LOG_DEBUG << Form(" EEqaSorterA::initCrateHisto(nb=%d, max=%d)\n", nBin,mxADC) << endm;
  // init histo
  int nOK=0;
  hCrate = new TH2F *[MaxAnyCrate];
  for(int icr=0;icr<MaxAnyCrate;icr++) { 
    //if(icr!=63) continue;
    int crateID=icr+1;
    hCrate[icr]=0;
    int mxChan=0;
    TString physDet;
    if(crateID>=MinTwCrateID && crateID<=MaxTwCrateID) {
      // Towers
      mxChan=MaxTwCrateCh;
      physDet=sectL[crateID-1];
    } else if (crateID>=MinMapmtCrateID && crateID<=MaxMapmtCrateID ){
      //MAPMT
      mxChan=MaxMapmtCrateCh;
      int sec=1+(7+crateID/4)%MaxSectors;
      int box=1+crateID%4;
      physDet = (box==4) ? Form("%dP1",sec) : Form("%dS%d",sec,box);
    }
    
    if(mxChan==0) continue; // skip nonexisting crates
    nOK++;
    hCrate[icr] = new TH2F(Form("cr%d",crateID),Form(" %s",physDet.Data()),nBin,-0.5, mxADC-0.5, mxChan, -0.5, mxChan-0.5);
    if (HList) HList->Add(hCrate[icr]);
    // printf("Histo Init: icr=%d crID=%d %s \n",icr,crateID,h->GetTitle());
  }
  hCrateHot = new TH1F *[MaxTwCrateID];
  for(int icr=0;icr<MaxTwCrateID;icr++) { 
    int crateID=icr+1; 
    hCrateHot[icr]=0;
    int mxChan=MaxTwCrateCh;
    hCrateHot[icr] = new TH1F(Form("cr%dHot",crateID),Form("%s thr=feePed+%d",sectL[crateID-1],hotTwThres), mxChan, -0.5, mxChan-0.5);
    hCrateHot[icr]->SetFillColor(kBlue);
    if (HList) HList->Add(hCrateHot[icr]);
  }
  LOG_DEBUG << "Initialized " << nOK << " 2D carte-histos" << endm;
}  

void EEqaSorterA::saveHisto(TFile *f) const {
    if (f) f->cd();
    for(int icr = 0;icr < MaxAnyCrate;icr++) { 
	if (hCrate && hCrate[icr]) hCrate[icr]->Write();
    }
    for(int icr = 0;icr < MaxTwCrateID;icr++) { 
	if (hCrateHot && hCrateHot[icr]) hCrateHot[icr]->Write();
    }
}  

void EEqaSorterA::resetHisto() {
    for(int icr = 0;icr < MaxAnyCrate;icr++) { 
	if (hCrate && hCrate[icr]) hCrate[icr]->Reset();
    }
    for(int icr = 0;icr < MaxTwCrateID;icr++) { 
	if (hCrateHot && hCrateHot[icr]) hCrateHot[icr]->Reset();
    }
}  

//--------------------------------------------------
int EEqaSorterA::usePed4(const Char_t *filename) {
    int ok = 0;
    //......................... load ped for L-0
    LOG_DEBUG << " EEqaSorterA::usePed4(\"" << filename << "\") ..." << endm;
    FILE *fd=fopen(filename,"r");
    if (fd) {
	ok = 1;
	for(int cr=MinTwCrateID;cr<=MaxTwCrateID;cr++) {
	    for(int ch=0;ch<MaxTwCrateCh; ch++) {
    		int xcr = -1, xch = -1, ped4 = 0;
    		float xped = 0;
    		int ret=fscanf(fd,"%d %d %f %d",&xcr,&xch,&xped,&ped4);
    		if ((ret == 4) && (xcr >= MinTwCrateID) && (xcr <= MaxTwCrateID) && (xch >= 0) && (xch < MaxTwCrateCh)) {
	    	    feePed[(xcr - 1)*MaxTwCrateCh + xch]=4*ped4;
		} else {
		    LOG_ERROR << "Bad format in " << filename << ": read xcr=" << xcr << ", xch=" << xch << ", xped=" << xped << ", ped4=" << ped4 << ", ret=" << ret << "; expected xcr=" << cr << ", xch=" << ch << endm;
		    ok = 0;
		}
	    }
	}
	fclose(fd);
	fd = 0;
	LOG_DEBUG << " EEqaSorterA::usePed4(...) Loaded" << endm;
    } else {
	LOG_ERROR << "Cannot read file " << filename << endm;
    }
    return ok;
}


// $Log: EEqaSorterA.cxx,v $
// Revision 1.10  2010/04/09 04:48:33  ogrebeny
// Added more protection against out-of-boundary errors. See bug report 1903.
//
// Revision 1.9  2010/03/05 23:46:42  ogrebeny
// Make it a little less strict on the ped file format
//
// Revision 1.8  2010/03/05 23:16:44  ogrebeny
// A little more debug output
//
// Revision 1.7  2009/04/30 21:20:31  ogrebeny
// Improved constness after fixing bug 1457
//
// Revision 1.6  2009/02/27 18:34:13  ogrebeny
// Small bug fixed
//
// Revision 1.5  2009/02/24 18:19:47  ogrebeny
// Small workaround until ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1457 is resolved
//
// Revision 1.4  2009/02/24 04:07:45  ogrebeny
// Fixed part of the trigger histograms
//
// Revision 1.3  2009/01/25 01:36:54  ogrebeny
// *** empty log message ***
//
// Revision 1.2  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.8  2004/03/13 22:03:13  balewski
// new plots from Hal added
//
// Revision 1.6  2004/02/26 04:22:24  balewski
// more Hal's plots
//
// Revision 1.5  2004/02/17 03:09:18  balewski
// *** empty log message ***
//
// Revision 1.4  2004/01/29 17:23:14  balewski
// fix for BTOW
//
// Revision 1.3  2004/01/27 16:29:39  balewski
// reset added
//
