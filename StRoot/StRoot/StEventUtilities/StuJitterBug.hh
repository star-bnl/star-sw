/***********************************************************************
 *
 * $Id: StuJitterBug.hh,v 1.2 2002/05/30 11:31:08 jones Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, Aug 2000
 *
 * Description:
 * This function checks the timing of the CTB event signal with
 * respect to the preceding pre-sample. If the pre-sample is 
 * greater than 1% of the event sample, or the event sample is
 * zero, the function return kTRUE and the event should be aborted.
 * Otherwise the function returns kFALSE.
 *
 * $Log: StuJitterBug.hh,v $
 * Revision 1.2  2002/05/30 11:31:08  jones
 * Changed tray and slat to UInt_t
 *
 * Revision 1.1  2000/09/28 20:12:54  jones
 * Adding StuJitterBug abort function
 *
 **********************************************************************/
#ifndef StuJitterBug_hh
#define StuJitterBug_hh

#include "StEvent/StEvent.h"
#include "StEvent/StTriggerDetectorCollection.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StMessMgr.h"

Bool_t t0JitterAbort(StEvent* event) {
  
  // Get the Trigger data

  StTriggerDetectorCollection *theTriggers = 
    event->triggerDetectorCollection();
  if( !theTriggers ) {
    gMessMgr->Error() << "t0JitterAbort finds no trigger data" << endl;
    return kFALSE;
  }
  StCtbTriggerDetector &ctb = theTriggers->ctb();
  Float_t ctb_evt = 0.;
  Float_t ctb_pre = 0.;

  // Loop over slats/trays and calc event and pre samples

  for( UInt_t slat=0; slat<ctb.numberOfSlats(); slat++ ) {
    for( UInt_t tray=0; tray<ctb.numberOfTrays(); tray++) {
      ctb_evt += ctb.mips( tray, slat, 0 );
      ctb_pre += ctb.mips( tray, slat, ctb.numberOfPreSamples() );
    }
  }

  Float_t ratio = (ctb_evt > 0.) ? ctb_pre/ctb_evt : 0.;

  if( ctb_evt <= 0. ) 
    gMessMgr->Warning() << "t0JitterAbort ctb_evt = " << ctb_evt
			<< " ctb_pre = " << ctb_pre << endl;

  // Return kTRUE if the presample is > 1% of the event sample
  // If the event sample is zero, the event is accepted

  return( (Bool_t) (ratio > 0.01) );
}

#endif
