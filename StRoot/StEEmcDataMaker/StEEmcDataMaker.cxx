// $Id: StEEmcDataMaker.cxx,v 1.13 2004/04/02 06:38:43 balewski Exp $

#include <Stiostream.h>
#include <math.h>
#include <assert.h>

#include "StEventTypes.h"
#include "StEvent.h"

#include "StEEmcDataMaker.h"

#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StEEMCReader.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/StEEmcDbIndexItem1.h"

#include "StEEmcDbMaker/EEmcDbCrate.h"

#include "StEEmcUtil/EEfeeRaw/EEfeeDataBlock.h" 

//#include "EEmcHealth.h"

ClassImp(StEEmcDataMaker)

//_____________________________________________________________________
StEEmcDataMaker::StEEmcDataMaker(const char *name):StMaker(name){
  mDb=0;

}

//___________________________________________________________
StEEmcDataMaker::~StEEmcDataMaker(){

}

//__________________________________________________
//__________________________________________________
//__________________________________________________

Int_t StEEmcDataMaker::Init(){
  if(mDb==0){ 
    mDb= (StEEmcDbMaker*) GetMaker("eeDb");
  } 
  
  if(mDb==0){  
    printf("\n\nWARN %s::Init() did not found \"eeDb-maker\", all EEMC data will be ignored\n\n", GetName());
  } 

  hs[0]= new TH1F("nV-TwCr","No of valid Tw crates/eve",9,-1.5,7.5);
  hs[1]= new TH1F("nV-MaCr","No of valid MAPMT crates/eve",19,-1.5,17.5);
  
  return StMaker::Init();
}


//___________________________________________________
//___________________________________________________
//___________________________________________________

Int_t StEEmcDataMaker::InitRun  (int runNumber){
  printf("\n%s::InitRun(%d) list  DB content \n",GetName(),runNumber);
 if(mDb==0){  
    printf("\n\nWARN %s::InitRun() did not found \"eeDb-maker\", all EEMC data will be ignored\n\n", GetName());
  } else if ( mDb->valid()==0 ) {
    printf("\n\nWARN %s::InitRun()  found \"eeDb-maker\", but without any DB data, all EEMC data will be ignored\n\n", GetName());
    mDb=0;
  } else {
    mDb->print(0);
  }
  return kStOK;
}


//____________________________________________________
//____________________________________________________
//____________________________________________________
Int_t StEEmcDataMaker::Make(){
  if(mDb==0){  
    printf("WARN %s::Make() did not found \"eeDb-maker\" or no DB data for EEMC, all EEMC data will be ignored\n", GetName());
    return kStOK;
  } 
  
  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent);// fix your chain or open the right event file
  // printf("\n%s  accesing StEvent ID=%d\n",GetName(),mEvent->id());

 //::::::::::::::::: copy raw data to StEvent ::::::::::::: 
 copyRawData(mEvent);

 //::::::::::::::: assure raw data are sane  :::::::::::::::: 
 if(!headersAreSane(mEvent))   return kStOK;

 //:::::::::::::: store tower/pre/post/smd hits in StEvent
 //raw2calibrated(mEvent);
   
 return kStOK;
  }

//____________________________________________________
//____________________________________________________
//____________________________________________________
void  StEEmcDataMaker::copyRawData(StEvent* mEvent) {
  
  St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
  StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
  StEEMCReader *eeReader  = fromVictor->getEEMCReader();  
  if(!eeReader) return ;

  int icr; 

  StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection();

  if(emcC==0) { // create this collection if not existing
    emcC=new StEmcCollection();
    mEvent->setEmcCollection(emcC);
    printf(" %s::Make() has added a non existing StEmcCollection()\n", GetName());
  }
  

  StEmcRawData *raw=new  StEmcRawData;  
  emcC->setEemcRawData(raw);
  printf("%s:: copy %d EEMC raw data blocks\n",GetName(),mDb->getNCrate());

  for(icr=0;icr<mDb->getNCrate();icr++) {
    const EEmcDbCrate *crate=mDb-> getCrate(icr);
    // printf("copy EEMC raw: ");crate->print();
    raw->createBank(icr,crate->nHead,crate->nCh);
    //  printf("aa=%p bb=%p\n",eeReader->getEemcHeadBlock(crate->fiber,crate->type),eeReader->getEemcDataBlock(crate->fiber,crate->type));

    raw->setHeader(icr,eeReader->getEemcHeadBlock(crate->fiber,crate->type));
    raw->setData(icr,eeReader->getEemcDataBlock(crate->fiber,crate->type));
  }
}

//____________________________________________________
//____________________________________________________
//____________________________________________________
int  StEEmcDataMaker::headersAreSane(StEvent* mEvent) {
  int good=true; // yes as default

  StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection();

  if(emcC==0) {
    printf("%s::raw2calibrated() no emc collection, skip\n",GetName());
    return good;
  }

  StEmcRawData* raw=emcC->eemcRawData();
  assert(raw);
  
  StL0Trigger* trg=mEvent->l0Trigger(); assert(trg);
  int token=trg->triggerToken();

  EEmcHealth *health=new  EEmcHealth;
  health->setValidBlockSize(mDb->getNCrate());

  EEfeeDataBlock block; // use utility class as the work horse

  int icr;
  for(icr=0;icr<mDb->getNCrate();icr++) {
    const EEmcDbCrate *crate=mDb-> getCrate(icr);
    //printf(" EEMC raw-->pix crID=%d type=%c \n",crate->crID,crate->type);
    const  UShort_t* head=raw->header(icr);
    assert(head);
    block.clear();
    block.setHead(raw->header(icr));

    int lenCount=crate->nCh+crate->nHead;
    int errFlag=0;
    switch(crate->type) {
    case 'T': lenCount+=32; break; // one more board exist in harware
    case 'S':  errFlag=0x28; break; // sth is set wrong in the boxes
    default:;
    }

    int trigCommand=4; // physics, 9=laser/LED, 8=??
    int valid=block.isHeadValid(token,crate->crIDswitch,lenCount,trigCommand,errFlag);
    health->setValidBlock(valid,icr);
    // printf("valid=%d good=%d ",valid,good); crate->print();

    if(valid==0) { good=false; continue; }

    switch(crate->type) {
    case 'T':  health->addValidTwCr(); break;
    case 'S':  health->addValidMaCr(); break;
    default:;
    }
  }
  //  printf("eeHealth:\n");  health->print();
  hs[0]->Fill(health->getNValidTwCr());
  hs[1]->Fill(health->getNValidMaCr());

  printf("%s::rawDataSane() , good=%d\n",GetName(),good);
  return good;
}



 

// $Log: StEEmcDataMaker.cxx,v $
// Revision 1.13  2004/04/02 06:38:43  balewski
// abort on any error in any header
//
// Revision 1.12  2004/03/30 04:44:52  balewski
// raw EEMC in StEvent done
//
// Revision 1.11  2004/03/28 04:08:12  balewski
// store raw EEMC data
//
// Revision 1.9  2004/03/20 20:25:40  balewski
// fix for empty ETOW/ESMD
//
// Revision 1.8  2004/03/19 21:29:28  balewski
// new EEMC daq reader
//
// Revision 1.7  2003/09/02 17:57:54  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/08/18 21:31:42  balewski
// new adc--> energy formula, still no absolute gain
//
// Revision 1.5  2003/07/18 18:31:45  perev
// test for nonexistance of XXXReader added
//
// Revision 1.4  2003/05/01 02:19:19  balewski
// fixed empry event problem
//
// Revision 1.3  2003/04/29 16:18:39  balewski
// some ideas added
//
// Revision 1.2  2003/04/27 23:08:02  balewski
// clean up of daq-reader
//
// Revision 1.1  2003/04/25 14:15:59  jeromel
// Reshaped Jan's code
//
// Revision 1.4  2003/04/16 20:33:44  balewski
// small fixes in eemc daq reader
//
// Revision 1.3  2003/03/26 22:02:20  balewski
// fix auto db-find
//
// Revision 1.2  2003/03/26 21:27:44  balewski
// fix
//
// Revision 1.1  2003/03/25 18:30:14  balewski
// towards EEMC daq reader
//


  //old .......................

#if 0 // recording decoded data in StEvent, abandon in 2004

  int det = kEndcapEmcTowerId; 
  StDetectorId id = StDetectorId(det);
  StEmcDetector* d = new StEmcDetector(id,12);
  emcC->setDetector(d); 

  int nDrop=0;
  int nMap=0;
  int nOver=0;
  
  for(icr=0;icr<mDb->getNCrate();icr++) {
    const EEmcDbCrate *crate=mDb-> getCrate(icr);
    if(crate->crID<1 || crate->crID>6) continue; // only tower data
    
    //tmp, no header check
    
    for(int chan=0;chan<crate->nch;chan++) {
      const  StEEmcDbIndexItem1  *x=mDb->get(crate->crID,chan);
      if(x==0) {
	//printf("No EEMC mapping for crate=%3d chan=%3d\n",crate,chan);
	nDrop++;
	continue;
      }
      nMap++;
      
      int isec=x->sec-1;   //range 0-11
      int isub=x->sub-'A'; //range 0-4
      int ieta=x->eta-1;   //range 0-11
      
      if(!steemcreader) continue; // there was no data 
      int rawAdc=steemcreader->getEemcData(crate->fiber,chan,'T'); 
      
      if(rawAdc<=0) continue; // there was no data for this channel
      
      float adc=rawAdc - x->ped;
      float energy=-1.;
      
      if(rawAdc>x->thr){ // changed 8-18-03, JB
	if(x->gain>0) 
	  energy=adc/x->gain; // note: it corrects only relative gains, SF not 
	else
	  energy=-2.;
	nOver++;
      }
      
      //printf("EEMC crate=%3d chan=%3d  ADC: raw=%4d pedSub=%+.1f energy=%+10g  -->   %2.2dT%c%2.2d\n",crate,chan,rawAdc,adc,energy,x->sec,x->sub,x->eta);
      
      assert(strchr(x->name,'T')); // works only for towers
      
      StEmcRawHit* h = new StEmcRawHit(id,isec,ieta,isub,rawAdc,energy);
      d->addHit(h);
    } // end of loop over channels
  }// end of loop over crates 
    
    
    printf("%s event finished nDrop=%d nMap=%d--> nOver=%d \n",GetName(), nDrop,nMap,nOver);
    
#endif // decoding of data 



#if 0 // test of new access method  

  for(icr=0;icr<mDb->getNCrate();icr++) {
    const EEmcDbCrate *crate=mDb-> getCrate(icr);
    printf("geting data for fiber: ");crate->print();

    printf("---- HEAD ----\n");
    for(ch=0;ch<crate->nHead;ch++) {
      int val=-1;
      val=steemcreader->getEemcHead(crate->fiber,ch,crate->type); 
      printf("cr=%d ch=%d val=0x%04x\n",crate->crID,ch,val);
    }

    printf("---- DATA ----\n");
    for(ch=0;ch<crate->nch;ch++) {
      int val=-1;
      val=steemcreader->getEemcData(crate->fiber,ch,type); 
      printf("cr=%d ch=%d val=0x%04x\n",crate->crID,ch,val);
    }
    //   break;
  }
#endif


//========================= TMP
//========================= TMP
//========================= TMP


#include <cassert>

#include <TArrayS.h>

//#include "EEmcHealth.h"

ClassImp(EEmcHealth)


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------

EEmcHealth ::  EEmcHealth() {
  validBlock= new TArrayS(0);
  nValidTwCr= nValidMaCr=0;
}



//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
EEmcHealth ::  ~EEmcHealth() {
  delete validBlock;
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEmcHealth ::setValidBlockSize(int size){
  validBlock->Set(size);
  validBlock->Reset(); // set unvalid by default
}


//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEmcHealth ::setValidBlock(Short_t valid, Int_t icr){
  validBlock->AddAt(valid,icr);

}



//--------------------------------------------------
//--------------------------------------------------
//--------------------------------------------------
void EEmcHealth :: print(int flag){
  printf("EEmcHealth::print(), nBlocks=%d  Valid TwCr=%d Mapmt=%d\n",validBlock->GetSize(),nValidTwCr, nValidMaCr);
  if(flag<0) return;

  int i;
  Short_t *valid=validBlock->GetArray();
  for(i=0;i<validBlock->GetSize();i++){
    printf("i=%2d valid=%d\n",i,valid[i]);
  }
}

/*
 * $Log: StEEmcDataMaker.cxx,v $
 * Revision 1.13  2004/04/02 06:38:43  balewski
 * abort on any error in any header
 *
 */
