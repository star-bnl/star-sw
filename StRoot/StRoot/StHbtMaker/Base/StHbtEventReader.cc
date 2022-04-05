/***************************************************************************
 *
 * $Id: StHbtEventReader.cc,v 1.2 2001/09/05 20:41:00 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for a StHbtEventReader             
 *   All HbtEventReaders should inherit from this.
 *   Objects of these classes are required
 *   to obtain the data and convert it somehow to the STAR StEvent object
 * 
 * A major change is that on 3sep99, the StHbtReader classes _MUST_ implement
 *   a Report() method, like the Cuts and CorrFctns.
 * Also, a StHbtReader MAY implement a WriteHbtEvent(StHbtEvent*) method.
 *
 ***************************************************************************
 *
 * $Log: StHbtEventReader.cc,v $
 * Revision 1.2  2001/09/05 20:41:00  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.1  2001/06/21 19:06:49  laue
 * Some minor structural changes (forward declarations, etc)
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StHbtMaker/Base/StHbtEventReader.hh"

#ifdef __ROOT__
ClassImp(StHbtEventReader)
#endif

StHbtString StHbtEventReader::Report(){
  StHbtString temp = "\n This is the base class StHbtEventReader reporting";
  temp += "\n---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> TrackCuts in Reader: ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> XiCuts in Reader: ";
  if (mXiCut) {
    temp += mXiCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> KinkCuts in Reader: ";
  if (mKinkCut) {
    temp += mKinkCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}

void StHbtEventReader::SetEventCut(StHbtEventCut* ecut){mEventCut=ecut;}
void StHbtEventReader::SetTrackCut(StHbtTrackCut* pcut){cout << pcut << endl; mTrackCut=pcut;}
void StHbtEventReader::SetV0Cut(StHbtV0Cut* pcut){mV0Cut=pcut;}
void StHbtEventReader::SetXiCut(StHbtXiCut* pcut){mXiCut=pcut;}
void StHbtEventReader::SetKinkCut(StHbtKinkCut* pcut){mKinkCut=pcut;}
StHbtEventCut* StHbtEventReader::EventCut(){return mEventCut;}
StHbtTrackCut* StHbtEventReader::TrackCut(){return mTrackCut;}
StHbtV0Cut*    StHbtEventReader::V0Cut(){return mV0Cut;} 
StHbtXiCut*    StHbtEventReader::XiCut(){return mXiCut;} 
StHbtKinkCut*    StHbtEventReader::KinkCut(){return mKinkCut;}


