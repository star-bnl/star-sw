// *-- Author : J.Balewski, R.Fatemi
// 
// $Id: St2eemcFeeRawMaker.cxx,v 1.16 2012/12/12 22:02:49 fisyak Exp $

#ifdef __APPLE__
#include <sys/types.h>
#endif
#include <Stiostream.h>
#include <math.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include <TTree.h>

#include "St2eemcFeeRawMaker.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "EEfeeRaw/EEfeeDataBlock.h"
#include "EEfeeRaw/EEfeeRawEvent.h"
#include "EEfeeRaw/EEmcEventHeader.h"
#include "EEfeeRaw/EEname2Index.h"


ClassImp(St2eemcFeeRawMaker)

//_____________________________________________________________________
St2eemcFeeRawMaker::St2eemcFeeRawMaker(const char *name):StMaker(name){
  moutTTree=0;
  meeDb=0;
  meveTT=new EEfeeRawEvent();
  mrunTT=new EEmcEventHeader();
  mNFeeCrate=6; // max numbers of FEE Data blocks
}

//___________________________________________________________
St2eemcFeeRawMaker::~St2eemcFeeRawMaker(){
  //  outTTree->Show(1);
  // outTTree->Print();
  delete mrunTT;
  delete meveTT;
  delete [] mcrateData;
}


//__________________________________________________
//__________________________________________________
//__________________________________________________

Int_t St2eemcFeeRawMaker::Init(){
  meeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(moutTTree); // all should be initialized prior to use
  assert(meeDb);  // --||--
  assert(meveTT); // --||--
  assert(mrunTT);  // --||--
  assert(mNFeeCrate>0); // --||--
  mcrateData=new EEfeeDataBlock[mNFeeCrate];

  // assigne eveTT with output TTree
  // assigne runTT with output TTree

  moutTTree->Branch("head","EEmcEventHeader",&mrunTT,16000,99);
  //printf("aa runHead Branch added \n");

  moutTTree->Branch("evt","EEfeeRawEvent",&meveTT,16000,99);
  //  printf("aa eve-Tbranch added \n");


   return StMaker::Init();
}


//___________________________________________________
//___________________________________________________
//___________________________________________________

Int_t St2eemcFeeRawMaker::InitRun  (int runNumber){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain or open the right event file
  LOG_INFO<<Form("\n%s  accessing StEvent ID=%d\n",GetName(),mEvent->id())<<endm;
   LOG_INFO<<Form("StEvent time=%d, ID=%d, runID=%d\n",(int)mEvent->time(),(int)mEvent->id(),(int)mEvent->runId())<<endm;

  mrunTT->setTimeStamp(this->GetDBTime().Convert());
  mrunTT->setProcessingTime(time(0));
  char text[200];
  sprintf(text," M-C event file, run=%d , created by %s",mEvent->runId(),GetName());
  mrunTT->setComment(text);
  mrunTT->print();  
 
 LOG_INFO<<Form("\n%s::InitRun(%d) list  DB content \n",GetName(),runNumber)<<endm;
 //meeDb->print();
  return kStOK;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
Int_t St2eemcFeeRawMaker::Make(){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent);// fix your chain or open the right event file
   LOG_INFO<<Form("\n%s  accesing StEvent ID=%d\n",GetName(),mEvent->id())<<endm;

  StEmcCollection* emcCollection = mEvent->emcCollection();
  //  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);
  // assert(twB);

  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(!twE) { 
    LOG_WARN<<"No EEMC twE hits, skip event"<<endm;  
    return kStOK;
  }

  // ....... Access  DB lookup table

  //  const  StEEmcDb::NameXchanItem* index2chan=meeDb->getIndex2chan();

  // ....... Initialize header of new TTree record ..........
  int  nDrop=0; //counts MC hits not assigned to crate/chan
  int icr;
  UShort_t head[4];
  
  UChar_t token=(int)mEvent->id()%256;
  for(icr=0;icr<mNFeeCrate;icr++) {
    int crateID=icr+1;
    mcrateData[icr].clear();
    // crateData[icr].print();// should be empty 
    int j;
    for(j=0;j<2;j++) head[j]=((0xa+j)<<8) +crateID;
    head[2]= token; 
    head[3]=(UChar_t)crateID;// + (mEvent->id()%(1<<4))<<8;
    //  printf(" iHead: raw[4]/hex= %x %x %x %x \n",head[0],head[1],head[2],head[3]); 
   mcrateData[icr].setHead(head);
  }

  // ....... Copy data to Local EEfeeDataBlock's .................

  int i;
  LOG_INFO<<Form("%s:: E_EMC Tower HITS ... %d\n",GetName(),twE->numberOfModules())<<endm;
  for ( i = 0; i < (int)twE->numberOfModules(); i++) { // The E-EMC modules
    // printf("AAA %d\n",i);
    StEmcModule* stmod =   twE->module(i);
    if(stmod==0)	continue;
    StSPtrVecEmcRawHit& emcTowerHits = stmod->hits();
    LOG_INFO<<Form("bbb i=%d %d\n",i,emcTowerHits.size())<<endm;
 
    uint j;
    for ( j = 0; j < emcTowerHits.size(); j++) { 
      int adc= emcTowerHits[j]->adc();
      int sec= emcTowerHits[j]->module()+1;
      int sub= emcTowerHits[j]->sub()+'A';
      int eta= emcTowerHits[j]->eta()+1;
      float energy=emcTowerHits[j]->energy();
          
      const EEmcDbItem *dbItem=meeDb->getT(sec,sub,eta);
      assert(dbItem); //  fatal error in EEmcDb-maker
 
      int chan=dbItem->chan;
      int slot=dbItem->crate;        
      //      adc=adc+(int)dbItem->ped; // add pedestal for each channel
      // tmp

       LOG_INFO<<Form("j=%d, sec=%d, sub=%c, eta=%d rawAdc=%d energy =%g -->crate/chan=%d/%d\n",j,sec,sub,eta,adc,energy,slot,chan)<<endm;


      // record this entry
      if(adc<=0) continue;
      if(chan<0 || slot<0) { nDrop++; continue;}
      icr=slot-1;
      assert(icr>=0);
      assert(icr<mNFeeCrate);
      mcrateData[icr].setData(chan,adc);// printf("  record icr=%d dac=%d\n",icr,adc);
     }
   }

  // ....... Store local EEfeeDataBlock's in TTree .................
  meveTT->clear(); // clear current TTree-event
  meveTT->setID(mEvent->id());  
  int n1=0;
  for(icr=0;icr<mNFeeCrate;icr++) {
    if(mcrateData[icr].getNData(0)<=0) continue; // empty data block

    LOG_INFO<<Form("SS crateID=%d, Npositive=%d\n",icr+1,mcrateData[icr].getNData(0))<<endm;
    // crateData[icr].print(0);
    meveTT->addFeeDataBlock(mcrateData+icr);
    n1++;
  }
  //  mrunTT->print();  
  // eveTT->print();

  moutTTree->Fill();
 
 //  eveTT->print(1);
  LOG_INFO<<Form("%s:: stored TTree with %d=%d data blocks, ID=%d \n",GetName(),n1,meveTT->block->GetEntries(),meveTT->getID())<<endm;


  return kStOK;
}




// $Log: St2eemcFeeRawMaker.cxx,v $
// Revision 1.16  2012/12/12 22:02:49  fisyak
// add sys/types.h include for APPLE
//
// Revision 1.15  2009/02/04 20:33:28  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.14  2007/05/30 02:38:43  balewski
// replace printf -->LOG_XXX
//
// Revision 1.13  2004/04/12 16:20:14  balewski
// DB cleanup & update
//
// Revision 1.12  2003/11/17 15:47:04  balewski
// fix of bug
//
// Revision 1.11  2003/09/02 17:57:55  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.10  2003/04/16 20:33:56  balewski
// small fixes in eemc daq reader
//
// Revision 1.9  2003/03/26 21:16:42  balewski
// *** empty log message ***
//
// Revision 1.8  2003/03/25 18:30:21  balewski
// towards EEMC daq reader
//
// Revision 1.7  2003/03/22 19:37:24  balewski
// *** empty log message ***
//
// Revision 1.6  2003/03/07 15:35:53  balewski
// towards EEMC daq reader
//
// Revision 1.5  2003/02/21 22:21:39  balewski
// time stamp added
//
// Revision 1.4  2003/02/18 22:01:47  balewski
// fixes
//
// Revision 1.3  2003/02/18 19:56:07  balewski
// add pedestals
//
// Revision 1.2  2003/02/17 18:45:40  balewski
// change names
//
// Revision 1.1  2003/01/28 23:15:25  balewski
// start
//
// Revision 1.1  2002/12/17 19:41:35  balewski
// separated from EEmc to avoid some dependeces during compilation
//
// Revision 1.2  2002/12/05 14:21:58  balewski
// cleanup, time stamp corrected
//
// Revision 1.1  2002/11/30 23:00:28  balewski
// male poprawki
//
