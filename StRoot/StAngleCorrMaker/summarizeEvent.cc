// $Id: summarizeEvent.cc,v 1.3 1999/06/27 22:45:25 fisyak Exp $
// $Log: summarizeEvent.cc,v $
// Revision 1.3  1999/06/27 22:45:25  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.2  1999/04/28 15:14:50  ogilvie
// included filling of histogram for pairs of tracks in real events
// i.e. numerator of correlation function
//
// Revision 1.2  1999/02/11 15:39:16  wenaus
// cleanup
//
//
///////////////////////////////////////////////////////////////////////////////
//
// summarizeEvent.cc
//
// Description: 
//  StEvent summary printout
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL  1/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StEvent.h"

static const char rcsid[] = "$Id: summarizeEvent.cc,v 1.3 1999/06/27 22:45:25 fisyak Exp $";

void summarizeEvent(StEvent& event)
{
  cout << "StAnalysisMaker:  Reading Event " << 
    " Type " << event.type() << " Run " << event.runNumber() << endl;
  cout << " N vertex " << event.vertexCollection()->size() << endl;
  cout << " N track " << event.trackCollection()->size() << endl;
  cout << " N TPC hit " << event.tpcHitCollection()->size() << endl;
  cout << " N FTPC hit " << event.ftpcHitCollection()->size() << endl;
  cout << " N SVT hit " << event.svtHitCollection()->size() << endl;
}
