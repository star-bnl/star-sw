/****************************************************************
 * $Id: StMuEzTree.cxx,v 1.4 2007/04/27 17:07:16 mvl Exp $
 *
 * Author: Wei-Ming Zhang             KSU  Aug. 2004
 *
 *****************************************************************
 * Description:
 * Interface between ezTree and MuDst 
 *
 ****************************************************************/

#include <TArrayC.h>
 
#include "StMuEzTree.h"

#include "StEvent/StEvent.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEmcCollection.h"

#include "StEvent/StTriggerData.h"
//#include "StDaqLib/TRG/trgStructures2004.h"// tmp, untill AKio adds  trigData->getRawSize()

#include "StEvent/StTriggerDetectorCollection.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StL0Trigger.h"

#include "EztEventHeader.h"
#include "EztEmcRawData.h"
#include "EztTrigBlob.h"
#include "EztFpdBlob.h"

ClassImp(StMuEzTree)

StMuEzTree::StMuEzTree() {
  /* noop */
}

StMuEzTree::~StMuEzTree() {}

// event header cheked by JB
//---------------------------------------------------------
//---------------------------------------------------------
EztEventHeader* StMuEzTree::copyHeader(StEvent* ev){
  EztEventHeader* header = new EztEventHeader;
  header->setRunNumber(ev->runId());
  header->setEventNumber(ev->id());
  unsigned short token=ev->l0Trigger()->triggerToken();
  header->setToken(token);
  header->setTimeStamp(ev->time());
  header->setComment("Ezt-branch from StEvent");
  header->setProcessingTime(time(0)); 
  header->setStatus(0); // probably not used, JB 

  /* token in the header should be taken for real event header.
     It is used to check for corruption in the data blocks later.
     TimeStamp - event time
     ProcessingTime - when this muDst was produced
     EventNumber - real event ID
     JB */
  
  return header;
}

// trig  new ok, JB
//--------------------------------------------------------
//--------------------------------------------------------
EztTrigBlob* StMuEzTree::copyTrig(StEvent* ev){
  EztTrigBlob * trigBlob = new EztTrigBlob;
  
  StTriggerData* trigData = ev->triggerData();
  if (trigData == 0)
     return 0;
  char *rawData= trigData->getTriggerStructure();
  int rawSize=trigData->getRawSize();

  trigBlob->trgd->Set(rawSize, rawData);
  trigBlob->setVersion((UChar_t)trigData->version());
  
  //  int year =trigData->year();
  //  printf("JJJ trgSize=%d  token=%d year=%d\n",rawSize ,trigData->token(),year );

  return trigBlob;
}
 
//--------------------------------------------------------
//--------------------------------------------------------
EztEmcRawData* StMuEzTree::copyETow(StEmcRawData *inp){
  return copy(inp,0,mxETowCrate);
} 

//--------------------------------------------------------
//--------------------------------------------------------
EztEmcRawData* StMuEzTree::copyESmd(StEmcRawData *inp){
  return copy(inp,mxETowCrate,inp->getNBlocks());
}
 
//--------------------------------------------------------
//--------------------------------------------------------
EztEmcRawData* StMuEzTree::copy(StEmcRawData *inp, int ib1, int ib2){
  EztEmcRawData* raw = new EztEmcRawData;
   
  int j=0;
  for (int i = ib1; i <ib2; i++,j++) {
    if(i>=inp->getNBlocks()) break;// do not read beyond existing data
    if(inp->sizeData(i) <= 0) continue;
    raw->createBank(j,inp->sizeHeader(i),inp->sizeData(i));
    raw->setHeader(j,inp->header(i));
    raw->setData(j,inp->data(i));
  }
  return raw;
  
}


// FPD is dummy, Akio must fill it right,  JB
//--------------------------------------------------------
//--------------------------------------------------------
EztFpdBlob* StMuEzTree::copyFpd(StEvent* ev){
  EztFpdBlob * fpdBlob = new EztFpdBlob;
  unsigned short int dummData[5]={1,3,5,7,9}; // Akio, drop it
  unsigned short int *rawData= dummData; // Akio, fix it
  int rawSize=5; // Akio, fix it
  fpdBlob->smd->Set(rawSize, (short*)rawData);
  return fpdBlob;
}
 
