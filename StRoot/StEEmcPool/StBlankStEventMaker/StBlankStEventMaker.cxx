/***************************************************************************
 *
 * $Id: StBlankStEventMaker.cxx,v 1.1 2004/06/06 18:28:13 balewski Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#include "StBlankStEventMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StEvent/StEmcCollection.h"

// tracks
//#include "StEvent/StVector.h"
#include "StEvent/StGlobalTrack.h"

StBlankStEventMaker::StBlankStEventMaker(const char* name) : StMaker(name) {
  mStEvent =0;
}

StBlankStEventMaker::~StBlankStEventMaker() { 
    delete mStEvent;
    /* no=op */
}
    
 
void StBlankStEventMaker::Clear(const char*) {
    if ( mStEvent ) {
	delete mStEvent;
	mStEvent = 0;
    }
}

int StBlankStEventMaker::Make(){  ///< create a StEvent from the muDst and put it into the .data tree 
  static int run =3010006;
  run++;
  mStEvent = new StEvent();
  
  StEventInfo* info = new StEventInfo();
  mStEvent->setInfo( info );
  
  StRunInfo* runInfo = new StRunInfo();
  runInfo->setRunId(run);
  mStEvent->setRunInfo( runInfo);
  
  StEventSummary* summary = new StEventSummary();
  mStEvent->setSummary( summary);
  
  // set chain date to be the same of event date
  StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");// needed for database only
  if(!hd) { hd = new StEvtHddr();  AddData(hd); }
  hd->SetGMTime(mStEvent->time());
  hd->SetRunNumber(run);
  AddData(mStEvent);     // add StEvent to the .data tree
 
  // now CREATE the EMC stuff and put it in the StEvent
  StEmcCollection *emc=new StEmcCollection();
  mStEvent->setEmcCollection(emc);

  // now create globa tracks Collection
  StSPtrVecTrackNode         &trackNodes   = mStEvent->trackNodes();
  StTrackNode                *node;
  //vecGlobalTracks??
  // int maxId=200;
  StGlobalTrack *gtrack = 0;
  
  // add fake tracks
  int i;
  for (i=0; i<5; i++) {
    gtrack = new StGlobalTrack(); // create an empty track
    gtrack->setFlag(12+i); // put some notrivial value
    gtrack->setLength(100.+i);// put some notrivial value
    node = new StTrackNode();
    node->addTrack(gtrack);          // node<->track association
    trackNodes.push_back(node);
 }
 

  cout << "*****************" << GetName() << endl;
  return 0;
}

ClassImp(StBlankStEventMaker)
  
/***************************************************************************
 *
 */
