/***************************************************************************
 *
 * $Id: FlowEvent.cc,v 1.1 1999/11/04 19:02:04 snelling Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer
 ***************************************************************************
 *
 * Description: part of Flow Framework: 
 *
 ***************************************************************************
 *
 * $Log: FlowEvent.cc,v $
 * Revision 1.1  1999/11/04 19:02:04  snelling
 * First check in of StFlowMaker. It contains the common code from
 * StFlowTagMaker and StFlowAnalysisMaker.
 *
 **************************************************************************/

#include "FlowEvent.hh"
//___________________
FlowEvent::FlowEvent() {
  mTrackCollection = new FlowTrackCollection;
}
//___________________
FlowEvent::~FlowEvent() {
  FlowTrackIterator iter;
  for (iter=mTrackCollection->begin();iter!=mTrackCollection->end();iter++){
    delete *iter;
  }
  delete mTrackCollection;
}
//___________________

