//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowEvent.cc,v 1.1 1999/11/11 23:08:54 posk Exp $
//
// Author: Raimond Snellings and Art Poskanzer
//////////////////////////////////////////////////////////////////////
//
// Description: A subset of StEvent 
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowEvent.cc,v $
// Revision 1.1  1999/11/11 23:08:54  posk
// Rearrangement of files.
//
// Revision 1.1  1999/11/04 19:02:04  snelling
// First check in of StFlowMaker. It contains the common code from
// StFlowTagMaker and StFlowAnalysisMaker.
//
//////////////////////////////////////////////////////////////////////

#include "StFlowEvent.hh"
//#include "StFlowTrackCollection.hh"

//-----------------------------------------------------------

StFlowEvent::StFlowEvent() {
  mTrackCollection = new StFlowTrackCollection;
}

//-----------------------------------------------------------

StFlowEvent::~StFlowEvent() {
  StFlowTrackIterator iter;
  for (iter= mTrackCollection->begin(); iter!= mTrackCollection->end(); iter++){
    delete *iter;
  }
  delete mTrackCollection;
}


