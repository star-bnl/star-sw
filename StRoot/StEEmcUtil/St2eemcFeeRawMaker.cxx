// *-- Author : J.Balewski, R.Fatemi
// 
// $Id: St2eemcFeeRawMaker.cxx,v 1.8 2003/03/25 18:30:21 balewski Exp $
// $Log: St2eemcFeeRawMaker.cxx,v $
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

#include <iostream.h>
#include <math.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include <TTree.h>

#include "St2eemcFeeRawMaker.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/StEEmcDbIndexItem1.h"
#include "EEfeeRaw/EEfeeDataBlock.h"
#include "EEfeeRaw/EEfeeRawEvent.h"
#include "EEfeeRaw/EEfeeRunDescr.h"
#include "EEfeeRaw/EEname2Index.h"


ClassImp(St2eemcFeeRawMaker)

//_____________________________________________________________________
St2eemcFeeRawMaker::St2eemcFeeRawMaker(const char *name):StMaker(name){
  moutTTree=0;
  meeDb=0;
  meveTT=new EEfeeRawEvent();
  mrunTT=new EEfeeRunDescr();
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
  assert(moutTTree); // all should be initialized prior to use
  assert(meeDb);  // --||--
  assert(meveTT); // --||--
  assert(mrunTT);  // --||--
  assert(mNFeeCrate>0); // --||--
  mcrateData=new EEfeeDataBlock[mNFeeCrate];

  // assigne eveTT with output TTree
  // assigne runTT with output TTree

  moutTTree->Branch("desc","EEfeeRunDescr",&mrunTT,16000,99);
  //printf("aa runDescr Brunch added \n");

  moutTTree->Branch("evt","EEfeeRawEvent",&meveTT,16000,99);
  //  printf("aa eve-Tbranch added \n");


   return StMaker::Init();
}


//___________________________________________________
//___________________________________________________
//___________________________________________________

Int_t St2eemcFeeRawMaker::InitRun  (int runumber){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain or open the right event file
  printf("\n%s  accessing StEvent ID=%d\n",GetName(),mEvent->id());
  printf("StEvent time=%d, ID=%d, runID=%d\n",(int)mEvent->time(),(int)mEvent->id(),(int)mEvent->runId());

  mrunTT->setTimeStamp(meeDb->getTimeStampUnix());
  mrunTT->setProcessingTime(time(0));
  char text[200];
  sprintf(text," M-C event file, run=%d , created by %s",mEvent->runId(),GetName());
  mrunTT->setComment(text);
  mrunTT->print();  
  
    return kStOK;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
Int_t St2eemcFeeRawMaker::Make(){

  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent);// fix your chain or open the right event file
  printf("\n%s  accesing StEvent ID=%d\n",GetName(),mEvent->id());

  StEmcCollection* emcCollection = mEvent->emcCollection();
  //  StEmcDetector* twB = emcCollection->detector(kBarrelEmcTowerId);
  // assert(twB);

  StEmcDetector* twE = emcCollection->detector(kEndcapEmcTowerId);
  if(!twE) { 
    printf("No EEMC twE hits, skip event\n");  
    return kStOK;
  }

  // ....... Access  DB lookup table

  //  const  StEEmcDbMaker::NameXchanItem* index2chan=meeDb->getIndex2chan();

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
  printf("%s:: E_EMC Tower HITS ... %d\n",GetName(),twE->numberOfModules());
  for ( i = 0; i < (int)twE->numberOfModules(); i++) { // The E-EMC modules
    // printf("AAA %d\n",i);
    StEmcModule* stmod =   twE->module(i);
    if(stmod==0)	continue;
    StSPtrVecEmcRawHit& emcTowerHits = stmod->hits();
    printf("bbb i=%d %d\n",i,emcTowerHits.size());
 
    uint j;
    for ( j = 0; j < emcTowerHits.size(); j++) { 
      int adc= emcTowerHits[j]->adc();
      int sec= emcTowerHits[j]->module()+1;
      int sub= emcTowerHits[j]->sub()+'A';
      int eta= emcTowerHits[j]->eta()+1;
      float energy=emcTowerHits[j]->energy();
      //     float energy= emcTowerHits[j]->energy(); // not used
      // printf("%d sec=%d sub=%c eta=%d adc=%d energy =%g\n", j,sec,sub,eta,adc,energy);
      const StEEmcDbIndexItem1 *dbItem=meeDb->getT(sec,sub,eta);
      assert(dbItem); //  fatal error in EEmcDb-maker
 
      int chan=dbItem->chan;
      int slot=dbItem->crate;        
      adc=adc+(int)dbItem->ped; // add pedestal for each channel

      //printf("j=%d, sec=%d, sub=%c, eta=%d adc=%d  -->crate/chan=%d/%d\n",j,sec,sub,eta,adc,slot,chan);

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

    printf("SS crateID=%d, Npositive=%d\n",icr+1,mcrateData[icr].getNData(0));
    // crateData[icr].print(0);
    meveTT->addFeeDataBlock(mcrateData+icr);
    n1++;
  }
  //  mrunTT->print();  
  // eveTT->print();

  moutTTree->Fill();
 
 //  eveTT->print(1);
  printf("%s:: stored TTree with %d=%d data blocks, ID=%d \n",GetName(),n1,meveTT->block->GetEntries(),meveTT->getID());


  return kStOK;
}



