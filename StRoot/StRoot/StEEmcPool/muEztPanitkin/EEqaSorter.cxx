// $Id: EEqaSorter.cxx,v 1.11 2010/04/09 04:48:33 ogrebeny Exp $
#include <string.h>
#include <stdlib.h>
//#include <iostream>
//using namespace std;

#include <TObjArray.h>

#include <TFile.h>
#include <TH1.h>

#include "StMessMgr.h"

#include "EEqaSorter.h"

#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"
#include "EEqaSorterA.h"
#include "EEqaSorterC.h" // needs DB
#include "EEdsmAna.h"

#include "SpyCopyCat.h"
#include "SpyCorruption.h"
#include "SpyJPped.h"
#include "SpyJPfreq.h"

ClassImp(EEqaSorter)

//-------------------------------------------
EEqaSorter::EEqaSorter(StEEmcDb *dbx)
    : TObject()
    , H4jpCor(0)
    , sortA(0)
    , sortC(0)
    , dsm(0)
    , eeDb(dbx)
    , H1tot(0)
    , pathInp(0)
    , pathOut(0)
    , timeStamp(0)
    , mySpy(0)
    , mySpyCC(0)
    , nSpy(0)
    , nSpyCC(0)
    , minSecSpy(0)
    , nEveSpy(0)
    , spyMode(0)
    , lastSpyRun(0)
{
    memset(hCorT, 0, sizeof(hCorT));
    memset(hCorS, 0, sizeof(hCorS));
} 

//--------------------------------------------------
EEqaSorter::~EEqaSorter() {
    for (int i = 0;i <= 6;i++) {
	if(hCorT[i]) delete hCorT[i];
	if(hCorS[i]) delete hCorS[i];
    }
    memset(hCorT, 0, sizeof(hCorT));
    memset(hCorS, 0, sizeof(hCorS));
    if (H4jpCor) delete H4jpCor;
    if (sortA) delete sortA;
    sortA = 0;
    if (sortC) delete sortC;
    sortC = 0;
    if (dsm) delete dsm;
    dsm = 0;
    eeDb = 0;
    if (H1tot) delete H1tot;
    H1tot = 0;
    pathInp = 0;
    pathOut = 0;
    timeStamp = 0;
    for (int i = 0;i < nSpy;i++) if (mySpy && mySpy[i]) delete mySpy[i];
    if (mySpy) delete mySpy;
    mySpy = 0;
    for (int i = 0;i < nSpyCC;i++) if (mySpyCC && mySpyCC[i]) delete mySpyCC[i];
    if (mySpyCC) delete mySpyCC;
    mySpyCC = 0;
    nSpy = 0;
    nSpyCC = 0;
    minSecSpy = 0;
    nEveSpy = 0;
    spyMode = 0;
    lastSpyRun = 0;
}

//--------------------------------------------------
void EEqaSorter::initHisto(TObjArray *HList, int nBin, int mxADC) {
  sortA=new EEqaSorterA();
  sortC=new EEqaSorterC(eeDb);

  dsm = new EEdsmAna("allTrig");

  LOG_DEBUG << Form(" EEqaSorter::initHisto(nb=%d, max=%d), pathInp=%s=\n", nBin,mxADC, pathInp) << endm;
  H1tot=new TH1F("eeTot","EEMC total counter",10,0,10);
  if (HList) HList->Add(H1tot);
  sortA->initCrateHisto(HList, nBin,mxADC);
  //sortA->usePed4(Form("%s/eemcPed4.dat","/star/u/rfatemi/TestPPlots/home_local/eemc/defaultPanitkinSetup"));
  sortA->usePed4(Form("%s/eemcPed4.dat", pathInp));

  sortC->initHisto(HList);

  if (dsm) dsm->initHisto(HList);
  
  //.................. my histo .............
  hCorT[0]= new TH1F("ETowHealth","ETOW HEALTH; X=corr. type; events",9,-0.5,8.5);
  hCorS[0]= new TH1F("ESmdHealth","ESMD HEALTH; X=corr. type; events",9,-0.5,8.5);

  hCorT[1]=new TH1F("ETowHeadCorr","# ETOW head corr; crates / eve",6,.5,6.5);
  hCorS[1]=new TH1F("ESmdHeadCorr","# ESMD head corr; crates / eve",48,.5,48.5);

  hCorT[2]=new TH1F("ETowN256","# ETOW n256/eve; chan / eve",50, .5,100.5);
  hCorS[2]=new TH1F("ESmdN256","# ESMD n256/eve; chan / eve",50, .5,100.5);
  
  hCorT[3]=new TH1F("ETowOFF","# ETOW OFF; crates / eve",6,.5,6.5);
  hCorS[3]=new TH1F("ESmdOFF","# ESMD OFF; crates / eve",48,.5,48.5);
  
  hCorT[4]= new TH1F("ETowGhost","# ETOW nGhost/eve; chan / eve",50,.5,100.5);
  hCorS[4]=0;
  
  hCorT[5]=new TH1F("ETowCorrBit","ETOW sanity;   X= crID + bits/10: 0=crID, 1=token,2=len,3=trgCom,4=ErrFlg,5=Ghost,6=n256; events ",60,0.85,6.85);
  hCorS[5]=new TH1F("ESmdCorrBit","ESMD  sanity;   X= crID + bits/10: 0=crID, 1=token,2=len,3=trgCom,4=ErrFlg,5=Ghost,6=n256;events ",480,63.85,111.85);

  hCorT[6]=new TH1F("ETowOFFid","ETOW OFF ID; crate ID",6,.5,6.5);
  hCorS[6]=new TH1F("ESmdOFFid","ESMD OFF ID; crate ID",48,63.5,111.5);

  H4jpCor =new TH1F("JPtotCor","ETOW Corruption  per JP; JP ID",EEnJetPatch,0.5,EEnJetPatch+0.5);   H4jpCor->SetFillColor(kRed);
  if (HList) HList->Add(H4jpCor);
 
  for(int i=0;i<=6;i++) {
    if(hCorT[i] && HList) HList->Add(hCorT[i]);
    if(hCorS[i] && HList) HList->Add(hCorS[i]);
  }

  LOG_DEBUG << Form(" EEqaSorter::initHisto() total %d histos created\n", HList ? HList->GetEntries() : 0) << endl;  
}  
 
//-------------------------------------------
void EEqaSorter::initRun() {
  sortC->initRun();
}

//-------------------------------------------
void EEqaSorter::clear() {
  timeStamp=0;
  if (dsm) dsm->clear();
}

//-------------------------------------------
void  EEqaSorter::saveHisto(TFile *f) const { 
  if (f) f->cd();
  if (H1tot) H1tot->Write();
  if (sortA) sortA->saveHisto(f);
  if (sortC) sortC->saveHisto(f);
  if (dsm) dsm->saveHisto(f);
  if (H4jpCor) H4jpCor->Write();
  for(int i=0;i<=6;i++) {
    if(hCorT[i]) hCorT[i]->Write();
    if(hCorS[i]) hCorS[i]->Write();
  }
}
 
//-------------------------------------------
void EEqaSorter::sort(EztEmcRawData *t, EztEmcRawData *s, int runNo,
		 int token, int daqVer,
		 const unsigned char *dsm0inp, const unsigned short int *dsm1inp,
		 const unsigned short int *dsm2inp, const unsigned short int *dsm3inp)
{
  H1tot->Fill(0.5);  
  xRayETOW(t, token);  // detect corruption
  xRayESMD(s, token,daqVer,runNo);  // detect corruption

  crateHealth(t, hCorT, 1, daqVer); // histogram corruption
  crateHealth(s, hCorS, 2, daqVer); // histogram corruption

  if (sortA) sortA->sort(t, s, daqVer);
  if (sortC) sortC->sort(t, s, daqVer);
  if (dsm) dsm->sort(dsm0inp, dsm1inp, dsm2inp, dsm3inp);
}

//-------------------------------------------
void EEqaSorter::xRayETOW(EztEmcRawData *t, int token) const {
  // Goal:verify content of each header + tower data  

  if (!t) return; // no ETOW data 
  int lenCount=0xa4;
  int errFlag=0;
  int trigComm=0x4; // physics, 9=laser/LED, 8=??

  const int mxN256=10; //per crate
  const int mxGhost=2; //per crate
  int totN256=0, totGhost=0;
  
  int icr;
  for(icr=0;icr<t->getNBlocks();icr++) {
    if(t->isCrateVoid(icr)) continue;
    if(t->purgeCrateOFF(icr)) continue;

    //    eETow->print(icr,0);
    int crID=icr+1;
    //........... hader..........
    t->tagHeadValid(icr,token, crID,lenCount,trigComm,errFlag);
    //............. ghost, N256 ...............
    int i;
    int nGhost=0, n256=0;
    const UShort_t* data=t->data(icr);
    for(i=0;i<t->sizeData(icr) && i<MaxTwCrateCh;i++) {
      if(data[i]>0 && (data[i] &0xff)==0 ) n256++;
      if(i>=121 && data[i]>50) nGhost++;
    }
    totGhost+=nGhost;
    totN256+=n256;

    // .....  replace sanity bits
    UShort_t sanity=t->getCorruption(icr);
    if(nGhost>mxGhost) sanity |=EztEmcRawData::bitGhost; 
    if(n256>mxN256) sanity |=EztEmcRawData::bitN256; 
    t->setCorruption(icr,sanity);
    //t->print(icr,0);
  } 
  hCorT[2]->Fill(totN256);
  hCorT[4]->Fill(totGhost);
}

//-------------------------------------------
void EEqaSorter::xRayESMD(EztEmcRawData *s, int token, int ver, int runNo) const { 
  // Goal:verify content of each header 
  
  if (!s) return; // no ESMD data
  int icr;
  int lenCount=0xc4;
  int errFlag=0x28;
  if(runNo>6001001)  errFlag=0; // Gerard has reporogrammed FEE
  int trigComm=0x4; // physics, 9=laser/LED, 8=??
  const int mxN256=10; //per crate
  int totN256=0;
 
  for(icr=0;icr<s->getNBlocks();icr++) {
    if(s->isCrateVoid(icr)) continue;
    if(s->purgeCrateOFF(icr)) continue;
    //  eESmd->print(icr,0);

    int crateID=icr+64;
    // in 2004 there was only 16 MAPMT crates for sectors 5-8
    if(ver<0x22) {
      if(icr>=16) break;
      crateID=icr+84;
    }
    //........... hader..........
    s->tagHeadValid(icr,token, crateID,lenCount,trigComm,errFlag);
    //.............  N256 ...............
    int i;
    int n256=0;
    const UShort_t* data=s->data(icr);
    for(i=0;i<s->sizeData(icr) && i<MaxMapmtCrateCh;i++) {
      if(data[i]>0 && (data[i] &0xff)==0 ) n256++;
    }
    totN256+=n256;

    UShort_t sanity=s->getCorruption(icr);
    if(n256>mxN256) sanity|= EztEmcRawData::bitN256; 
    s->setCorruption(icr,sanity); // replace sanity bits
    //s->print(icr,0);
  }
  hCorS[2]->Fill(totN256);
}

//-------------------------------------------
void EEqaSorter::crateHealth(EztEmcRawData *eRaw, TH1F **h, int es, int ver) const {
  if (!eRaw || !(es==1 || es==2)) return; // ETOW  or ESMD

  int nN256=0, nGhost=0,nHeadCorr=0, nOFF=0;
  int icr;
  for(icr=0;icr<eRaw->getNBlocks();icr++) {
    if(eRaw->sizeHeader(icr)<=0) continue; // crates not existing, only here scan also carte which are OFF==> isCrateVoid(icr)==true
    //    eETow->print(icr,0);    
    int crID=icr+1;
    if(es==2) { // ESMD
      crID=icr+64;      
      if(ver<0x22) {// in 2004 there was only 16 MAPMT crates for sectors 5-8
	if(icr>=16) break;
	crID=icr+84;
      }
    } 
    const UShort_t sanity=eRaw->getCorruption(icr);
    if(eRaw->isCrateVoid(icr)){// case of 'crateOFF'
      nOFF++; 
      h[6]->Fill(crID);
      continue;
    }

    if(es==1) { // ETOW
      if(sanity & EztEmcRawData::bitGhost) nGhost++;
    }
    
    if(sanity & 0x1f) nHeadCorr++; // any header bit
    if(sanity & EztEmcRawData::bitN256) nN256++;
    
    // printf("aa es=%d icr=%d %d %d  %d san=0x%x\n",es,icr,nHeadCorr,nN256,nGhost,sanity);
    int i;
    for(i=0;i<=6;i++) {// examin & histo all ETOW sanity bits
      if(!(sanity&(1<<i))) continue; // bit not set
      float x=crID+i/10.;
      h[5]->Fill(x);
      if(es==1)H4jpCor->Fill(crID); // any corruption per crate
    }
  } // end of loop over blocks

  if(nOFF>0) h[0]->Fill(0);
  
  int errKey=((nHeadCorr>0)<<0) | ((nN256>0)<<1)| ((nGhost>0)<<2);
  
  if(errKey>0) h[0]->Fill(errKey);
  h[1]->Fill(nHeadCorr);
  h[3]->Fill(nOFF);
  
}

//-------------------------------------------
void EEqaSorter::Finish() {
    if (sortA) sortA->Finish();
    //if (sortC) sortC->Finish();
}

//-------------------------------------------
void EEqaSorter::saveHisto(const Char_t *filename) const {
    TFile f(filename,"recreate");
    if (f.IsOpen()) {
	LOG_DEBUG << "Saving histograms to " << filename<< endm;
	saveHisto(&f);
	f.Close();
	LOG_DEBUG << "                      , save Ok" << endm;
    } else {
	LOG_ERROR << "Cannot write to file " << filename << endm;
    }
}

//-------------------------------------------
void EEqaSorter::resetHisto() {
  if (H1tot) H1tot->Reset();
  if (sortA) sortA->resetHisto();
  if (sortC) sortC->resetHisto();
  if (dsm) dsm->resetHisto();
  if (H4jpCor) H4jpCor->Reset();
  for(int i=0;i<=6;i++) {
    if(hCorT[i]) hCorT[i]->Reset();
    if(hCorS[i]) hCorS[i]->Reset();
  }
}

//-------------------------------------------
void EEqaSorter::initSpy(const TObjArray *HList, int minSec, int mode) { // must be called after histos initialized
  minSecSpy=minSec;
  spyMode=mode;
  int mxSpy=100;
  mySpy=new SpyGeneric*[mxSpy];
  mySpyCC=new SpyCopyCat*[mxSpy];
  memset(mySpy, 0, sizeof(mySpy[0])*mxSpy);
  memset(mySpyCC, 0, sizeof(mySpyCC[0])*mxSpy);
  nSpy=nSpyCC=0;
  int i;
  char txt[100];
  for(i=1;i<=6;i++) {
    sprintf(txt,"JP%d_sum",i);
    if (HList) mySpy[nSpy++]=(new SpyJPped)->set((TH1*)HList->FindObject(txt));
  }
  
  if (HList) mySpy[nSpy++]=(new SpyJPfreq)->set((TH1*)HList->FindObject("JPtotFreq"));

  mySpy[nSpy++]=(new SpyCorruption)->set (hCorT[0],"ETOW corruption ");
  mySpy[nSpy++]=(new SpyCorruption)->set (hCorS[0],"ESMD corruption ");
  
  for(i=0;i<6;i++) mySpyCC[nSpyCC++]=new SpyCopyCat('T',i);
  for(i=0;i<48;i++) mySpyCC[nSpyCC++]=new SpyCopyCat('M',i);
  
  for(i=0;i<nSpyCC;i++) {
    mySpy[nSpy++]= mySpyCC[i];
  }

  nEveSpy=0;
  lastSpyRun=0;
  LOG_DEBUG << Form("EEqaSorter::initSpy() nSpy=%d nSpyCC=%d, timeDelay=%d mode=%d\n",nSpy,nSpyCC,minSecSpy,spyMode) << endm;
}

//-------------------------------------------
void EEqaSorter::spy(EztEmcRawData *t, EztEmcRawData *s, int runNo, int eveId) {
  nEveSpy++;
  static int lastTm=0;
  static int lastNbad=0;
  static int nMail=0; // # if mails per run
  int uTm=time(0);
  if( lastTm==0)lastTm=uTm;

  int i;
  for(i=0;i<nSpyCC;i++) {
    mySpyCC[i]->accumulate(t, s);
  }
  
  //test   hCorT[0]->Fill( 1+(nEveSpy%5));
  //test  hCorS[6]->Fill( 71+(nEveSpy%5));

  if(uTm-lastTm<minSecSpy && nEveSpy<200) return; //tmp, was 100

  //...................evaluate spy criteria 
  printf(" ACTING spy IN delT=%d\n",uTm-lastTm); 
  TArrayF a0;// accumulated 
  char txt[200];  
  char fname[200];
  sprintf(fname,"eeSpy.%d-%d",runNo,uTm);
  printf("EEqaSorter::spy() spyLog=%s=\n",fname);
  TString fullName=TString(pathOut)+fname;
  printf("fullName=%s=\n",fullName.Data());
  FILE *flog=fopen(fullName.Data(),"w"); 

  if(flog==0)return;
  //  flog=stdout; //test
  if(spyMode!=3)fprintf(flog,"\n\n THIS IS TEST - IGNORE \n\n      Jan\n\n");
  fprintf(flog,"BNL=%s" ,ctime((const time_t *)&uTm));
  fprintf(flog,"run R%d, last eveID=%d, \nsampled: %d eve in last %d seconds,  total=%d eve\n",runNo,eveId,nEveSpy,uTm-lastTm,  (int)H1tot->GetEntries());
  fprintf(flog,"unix time=%d\n" ,uTm);   

  lastTm=uTm;
  int nBad=0;
  if(lastSpyRun!=runNo) {
    fprintf(flog,"run has changed from old=R%d, spy tests postponed\n",lastSpyRun);
    lastSpyRun=runNo;
    lastNbad=0;
    nMail=0;
    // printf(" ACTING spy new run, DO not replace array\n");
  } else {
    // printf(" ACTING spy doJob\n");
    int i;
    for(i=0;i<nSpy;i++) {
      // printf("i=%d p=%p\n",i,mySpy[i]);
      nBad+=mySpy[i]->sense(flog);
    }
    fprintf(flog,"\n grand total of %d ALARMS \n",nBad);
  }
  
  if(flog!=stdout) fclose(flog);
  TString fullName0=fullName;
  fullName+="-";  fullName+=nBad; 
  sprintf(txt,"mv %s %s\n",fullName0.Data(),fullName.Data());
  system(txt);

  if(nBad>0) printf(" EE-SPY  ALARMING , logFile=%s-%d nBad=%d\n",fname,nBad,nBad);
  if(nBad>0 &&spyMode!=3 ) printf("\7\7n"); 
  sprintf(txt,"%s/macros/eeSpy.sh  %s-%d %d %d&",pathInp,fname,nBad,nBad,spyMode);
  if(nBad>0 && nMail<3 && lastNbad!=nBad ) {
    nMail++;
    system(txt);
    lastNbad=nBad ;
  }
  nEveSpy=0; // clear # of sampled events
}


// $Log: EEqaSorter.cxx,v $
// Revision 1.11  2010/04/09 04:48:33  ogrebeny
// Added more protection against out-of-boundary errors. See bug report 1903.
//
// Revision 1.10  2009/03/05 02:37:53  ogrebeny
// Updates from Renee and Grant
//
// Revision 1.9  2009/02/24 04:07:45  ogrebeny
// Fixed part of the trigger histograms
//
// Revision 1.8  2009/02/14 03:16:52  ogrebeny
// Updated some histo titles. Removed unnecessary histo Clone() and Delete().
//
// Revision 1.7  2009/02/04 20:33:26  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.6  2009/01/24 03:47:43  ogrebeny
// Fuxed bug - can see ETOW data now
//
// Revision 1.5  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.4  2009/01/18 01:01:28  ogrebeny
// Better separate EMC histogramming from OnlinePlots infrastructure
//
// Revision 1.3  2008/12/19 17:54:34  fine
// Disable the dummy class
//
// Revision 1.2  2005/05/05 22:22:08  balewski
// added spy for JP
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
 
