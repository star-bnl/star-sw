// $Id: StEEmcDataMaker.cxx,v 1.4 2003/05/01 02:19:19 balewski Exp $
// $Log: StEEmcDataMaker.cxx,v $
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

#include <iostream.h>
#include <math.h>
#include <assert.h>

#include "StEventTypes.h"
#include "StEvent.h"

#include "StEEmcDataMaker.h"

#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StEEMCReader.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/StEEmcDbIndexItem1.h"


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
    mDb->print();
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
  printf("\n%s  accesing StEvent ID=%d\n",GetName(),mEvent->id());
  
  St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
  StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
  StEEMCReader *steemcreader  = fromVictor->getEEMCReader();   assert(steemcreader);
  
  
  
  StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection();

  if(emcC==0) { // create this collection if not existing
    emcC=new StEmcCollection();
    mEvent->setEmcCollection(emcC);
    printf(" %s::Make() has added a non existing StEmcCollection()\n", GetName());
  }
  int det = kEndcapEmcTowerId; 
  StDetectorId id = StDetectorId(det);
  StEmcDetector* d = new StEmcDetector(id,12);
  emcC->setDetector(d); 

  int nDrop=0;
  int nMap=0;
  int nOver=0;
  for(int crate=3;crate<=5;crate++) 
    for(int chan=0;chan<128;chan++) {
      const  StEEmcDbIndexItem1  *x=mDb->get(crate,chan);
      if(x==0) {
	//printf("No EEMC mapping for crate=%3d chan=%3d\n",crate,chan);
	nDrop++;
	continue;
      }
      nMap++;
      
      int isec=x->sec-1;   //range 0-11
      int isub=x->sub-'A'; //range 0-4
      int ieta=x->eta-1;   //range 0-11
      
      int rawAdc=steemcreader->getTowerAdc(crate,chan);
      if(rawAdc<0) continue; // there was no data for this channel

      float adc=rawAdc - x->ped;
      float energy=-1.;
            
      if(rawAdc>x->thr){
	energy=adc*x->gain;
	nOver++;
      }
      
      //printf("EEMC crate=%3d chan=%3d  ADC: raw=%4d pedSub=%+.1f energy=%+10g  -->   %2.2dT%c%2.2d\n",crate,chan,rawAdc,adc,energy,x->sec,x->sub,x->eta);
      
      assert(strchr(x->name,'T')); // works only for towers
      
      StEmcRawHit* h = new StEmcRawHit(id,isec,ieta,isub,rawAdc,energy);
      d->addHit(h);
    }
  printf("%s event finished nDrop=%d nMap=%d--> nOver=%d\n",GetName(), nDrop,nMap,nOver);


  return kStOK;
}



