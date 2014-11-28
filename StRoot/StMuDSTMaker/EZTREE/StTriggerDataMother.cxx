/*******************************************************
 *
 * $Id: StTriggerDataMother.cxx,v 1.2 2006/09/15 02:45:13 mvl Exp $
 *
 * Author: Jan Balewski
 *******************************************************
 *
 * Description: abstract class for trigger data from all years
 *
 *******************************************************/
#include <TArrayC.h>

#include "StTriggerDataMother.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StEvent/StTriggerData2003.h"
#include "StEvent/StTriggerData2004.h"
#include "StEvent/StTriggerData2005.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
ClassImp(StTriggerDataMother)
//--------------------------------------------------------
//--------------------------------------------------------

StTriggerDataMother::~StTriggerDataMother() {
  delete fCurrent;
}

//--------------------------------------------------------
//--------------------------------------------------------
StTriggerDataMother::StTriggerDataMother(EztTrigBlob*trigBlob) {
  fCurrent=0;
  if(trigBlob==0) {
    gMessMgr->Message("","F") <<GetName()<<"(0) - no input trig data, NULL returned"<<endm;
    return;
  }

  gMessMgr->Message("","D") <<GetName()<<"-trig data  ver/dec="<<trigBlob->getVersion()<<endm;
  //  trigBlob->print(0);

  int runId=-99;
  if (StMuDst::array(0) && StMuDst::array(0)->GetEntriesFast() > 0) {
    StMuEvent *muEve = StMuDst::event();
    runId=muEve->eventInfo().runId();
  }
  else {
    cout << "ERROR in " << __PRETTY_FUNCTION__ << ": cannot get event, run numberi. Exiting" << endl;
    exit(255);
  }
  void *blob=trigBlob->trgd->GetArray();
  switch(trigBlob->getVersion()) {
  case 0x20:
    fCurrent = new StTriggerData2003( (const TrgDataType2003 *)blob, runId);
    break;
  case 0x21:
    fCurrent = new StTriggerData2004( (const TrgDataType2004 *)blob, runId);
    break;
  case 0x22:
    fCurrent = new StTriggerData2005( (const TrgDataType2005 *)blob, runId);
    break;
  default:
    gMessMgr->Message("","F") <<GetName()<<"-trig data  ver/dec="<<trigBlob->getVersion()<<" not supported"<<endm;
  }
}



/*
 * $Log: StTriggerDataMother.cxx,v $
 * Revision 1.2  2006/09/15 02:45:13  mvl
 * Modification to pass run number when unpacking triger data (StTriggerDataxxx). From Jan Balewski.
 *
 * Revision 1.1  2004/11/29 17:28:31  mvl
 * New class for trigger versioning (by Jan)
 *
 *
 ****************************************/
