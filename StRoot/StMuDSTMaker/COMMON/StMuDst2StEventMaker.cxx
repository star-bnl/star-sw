/***************************************************************************
 *
 * $Id: StMuDst2StEventMaker.cxx,v 1.2 2003/03/19 18:58:04 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#include "StMuDst2StEventMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"


StMuDst2StEventMaker::StMuDst2StEventMaker(const char* self ,const char* muDstMakerName) : StMaker(muDstMakerName) {
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
}

StMuDst2StEventMaker::~StMuDst2StEventMaker() { 
  /* no=op */
}
    
int StMuDst2StEventMaker::Init(){
  return 0;
}
 
void StMuDst2StEventMaker::Clear() {
  /* no-op */
}

int StMuDst2StEventMaker::Make(){  ///< create a StEvent from the muDst and put it into the .data tree 
  mStEvent = 0; // I do not delete the StEvent, the chain is responsible for deletion. 
                // I just set the pointer to zero, so that you never pick up the old StEvent 

  if ( mMuDstMaker ) {
    mStEvent = mMuDstMaker->muDst()->createStEvent();
    if(mStEvent) {
      // set chain date to be the same of event date
      StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
      if(!hd) { hd = new StEvtHddr();  AddData(hd); }
      hd->SetGMTime(mStEvent->time());
      hd->SetRunNumber(mStEvent->runInfo()->runId());
      AddData(mStEvent);     // add StEvent to the .data tree
    }

    // print all the trigger ids
    if (mStEvent) {
	if ( mStEvent->triggerIdCollection() ) {
	    if ( mStEvent->triggerIdCollection()->l1() ) {
		cout << "l1 triggers: ";
		vector<unsigned int> v = mStEvent->triggerIdCollection()->l1()->triggerIds();	
		for ( unsigned int i=0; i<v.size(); i++) cout << v[i] << " "; 
		cout << endl;
	    }
	    if ( mStEvent->triggerIdCollection()->l2() ) {
		cout << "l2 triggers: ";
		vector<unsigned int> v = mStEvent->triggerIdCollection()->l2()->triggerIds();	
		for ( unsigned int i=0; i<v.size(); i++) cout << v[i] << " "; 
		cout << endl;
	    }
	    if ( mStEvent->triggerIdCollection()->l3() ) {
		cout << "l3 triggers: ";
		vector<unsigned int> v = mStEvent->triggerIdCollection()->l3()->triggerIds();	
		for ( unsigned int i=0; i<v.size(); i++) cout << v[i] << " "; 
		cout << endl;
	    }
	    if ( mStEvent->triggerIdCollection()->nominal() ) {
		cout << "nominal triggers: ";
		vector<unsigned int> v = mStEvent->triggerIdCollection()->nominal()->triggerIds();	
		for ( unsigned int i=0; i<v.size(); i++) cout << v[i] << " "; 
		cout << endl;
	    }
	}
    }
  }
  return 0;
}

int StMuDst2StEventMaker::Finish() { 
  return 0;
}
    

ClassImp(StMuDst2StEventMaker)

/***************************************************************************
 *
 * $Log: StMuDst2StEventMaker.cxx,v $
 * Revision 1.2  2003/03/19 18:58:04  laue
 * StMuChainMaker: updates for moved file catalog
 * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
 *
 * Revision 1.1  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.15  2002/11/08 14:18:59  laue
 *
 **************************************************************************/
