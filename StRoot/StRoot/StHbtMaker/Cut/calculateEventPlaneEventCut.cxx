/***************************************************************************
 *
 * $Id: calculateEventPlaneEventCut.cxx,v 1.7 2004/02/20 20:30:45 magestro Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description:  Passes HbtEvent to FlowMaker to calculate the event
 *     plane.  Warning ... this will change event charateristics!
 *
 ***************************************************************************
 *
 * $Log: calculateEventPlaneEventCut.cxx,v $
 * Revision 1.7  2004/02/20 20:30:45  magestro
 * Added Vz, multiplicity event cuts
 *
 * Revision 1.6  2003/08/05 00:29:55  perev
 * warnOff
 *
 * Revision 1.5  2003/08/04 20:21:27  perev
 * warnOff
 *
 * Revision 1.4  2001/11/14 21:07:19  lisa
 * Fixed several small things (mostly discarded const) that caused fatal errors with gcc2.95.3
 *
 * Revision 1.3  2001/07/22 19:57:05  rcwells
 * Fixed switch in calculateEventPlaneEventCut
 *
 * Revision 1.2  2001/07/21 12:04:28  rcwells
 * Only calls FlowAnalysis if using HbtEvent
 *
 * Revision 1.1  2001/07/20 20:03:50  rcwells
 * Added pT weighting and moved event angle cal. to event cut
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/calculateEventPlaneEventCut.h"
#include <cstdio>
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowAnalysisMaker/StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowSelection.h"

#ifdef __ROOT__
ClassImp(calculateEventPlaneEventCut)
#endif

calculateEventPlaneEventCut::calculateEventPlaneEventCut(){
  mFlowMaker = 0;
  mFlowAnalysisMaker = 0;
  mFromHBT = 0;
  mNEventsPassed =  mNEventsFailed = 0;

  mVertZPos[0] = -1.e4;  
  mVertZPos[1] = 1.e4;
  mEventMult[0] = 0;   
  mEventMult[1] = 9999;
  
} 
//------------------------------
//calculateEventPlaneEventCut::~calculateEventPlaneEventCut(){
//  /* noop */
//}
//------------------------------
bool calculateEventPlaneEventCut::Pass(const StHbtEvent* ConstantEventIn){

  /* this next line makes it explicit that we are PURPOSELY ignoring the
     "const" nature of the StHbtEvent for this special case - mike lisa 14nov01
  */
  StHbtEvent* event = (StHbtEvent*)ConstantEventIn;

  bool goodEvent = false;

  if(event) {

    double VertexZPos = event->PrimVertPos().z();
    goodEvent = ( (VertexZPos >= mVertZPos[0]) && (VertexZPos <= mVertZPos[1]) );
    // cout << "eventCut:: VertexZPos: " << mVertZPos[0] << " < " << VertexZPos << " < " << mVertZPos[1] << endl;

    if (goodEvent) {
      int mult = event->UncorrectedNumberOfPrimaries();
      goodEvent = (goodEvent && (mult >= mEventMult[0]) && (mult <= mEventMult[1]));
      // cout << "eventCut:: mult:       " << mEventMult[0] << " < " << mult << " < " << mEventMult[1] << endl;
    }

    if (goodEvent && mFlowMaker) {
      if (mFromHBT) mFlowMaker->FillFlowEvent(event);
      if (mFlowMaker->FlowEventPointer()) {
        StFlowEvent::SetPtWgt(false);
        // First get RP for whole event
        mFlowMaker->FlowSelection()->SetSubevent(-1);
        double reactionPlane = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        cout << "Reaction Plane        " << reactionPlane << endl;
        event->SetReactionPlane(reactionPlane,0);
        // Sub event RPs
        mFlowMaker->FlowSelection()->SetSubevent(0);
        double RP1 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        mFlowMaker->FlowSelection()->SetSubevent(1);
        double RP2 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        event->SetReactionPlaneSubEventDifference(RP1-RP2,0);
        // Now with Pt Weighting
        StFlowEvent::SetPtWgt(true);
        // First get RP for whole event
        mFlowMaker->FlowSelection()->SetSubevent(-1);
        reactionPlane = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        cout << "Reaction Plane ptWgt  " << reactionPlane << endl;
        event->SetReactionPlane(reactionPlane,1);
        // Sub event RPs
        mFlowMaker->FlowSelection()->SetSubevent(0);
        RP1 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        mFlowMaker->FlowSelection()->SetSubevent(1);
        RP2 = mFlowMaker->FlowEventPointer()->Psi(mFlowMaker->FlowSelection());
        event->SetReactionPlaneSubEventDifference(RP1-RP2,1);
        // if Flow Analysis is switched on ... make correction histogram
        if (mFlowAnalysisMaker) mFlowAnalysisMaker->Make();
      }
      else {
        cout << "No flow event found" << endl;
        event->SetReactionPlane(-999,0);
        event->SetReactionPlane(-999,1);
        event->SetReactionPlaneSubEventDifference(-999,0);
        event->SetReactionPlaneSubEventDifference(-999,1);
      }
    }
  }
  else {
    cout << "Something wrong with HbtEvent" << endl;
    event->SetReactionPlane(-99,0);
    event->SetReactionPlane(-99,1);
    event->SetReactionPlaneSubEventDifference(-99,0);
    event->SetReactionPlaneSubEventDifference(-99,1);
  }
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  return (goodEvent);
}
//------------------------------
StHbtString calculateEventPlaneEventCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nNumber of events which passed:\t%ld  Number which failed:\t%ld",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

