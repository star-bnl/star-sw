/***************************************************************************
 *
 * $Id: calculateEventPlaneEventCut.h,v 1.5 2004/06/25 13:18:01 magestro Exp $
 *
 * Author: Randall Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: Passes HbtEvent to FlowMaker to calculate the event
 *     plane.  Warning ... this will change event charateristics!
 *
 ***************************************************************************
 *
 * $Log: calculateEventPlaneEventCut.h,v $
 * Revision 1.5  2004/06/25 13:18:01  magestro
 * Added implementation for class methods
 *
 * Revision 1.4  2004/02/20 20:30:45  magestro
 * Added Vz, multiplicity event cuts
 *
 * Revision 1.3  2004/02/17 17:05:37  jeromel
 * Bug fix
 *
 * Revision 1.2  2003/01/28 17:16:55  magestro
 * Removed inheritance from StMaker, caused compilation warnings
 *
 * Revision 1.1  2001/07/20 20:03:50  rcwells
 * Added pT weighting and moved event angle cal. to event cut
 *
 *
 **************************************************************************/

#ifndef calculateEventPlaneEventCut_hh
#define calculateEventPlaneEventCut_hh

#include "StMaker.h"

#include "StHbtMaker/Base/StHbtEventCut.h"
class StFlowMaker;
class StFlowEvent;
class StFlowAnalysisMaker;
class StFlowSelection;

class calculateEventPlaneEventCut : public StHbtEventCut {

public:

  calculateEventPlaneEventCut();
  calculateEventPlaneEventCut(calculateEventPlaneEventCut&);
  //~calculateEventPlaneEventCut();

  int NEventsPassed();
  int NEventsFailed();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);

  void SetFlowMaker(char* title);
  void SetFlowAnalysisMaker(char* title);
  void FillFromHBT(const int& hbt);

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  calculateEventPlaneEventCut* Clone();

private:   // 
  StFlowMaker* mFlowMaker;                 //!
  StFlowAnalysisMaker* mFlowAnalysisMaker; //!

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  int mFromHBT;
  long mNEventsPassed;
  long mNEventsFailed;

#ifdef __ROOT__
  ClassDef(calculateEventPlaneEventCut, 1)
#endif

};

inline void calculateEventPlaneEventCut::SetEventMult(const int& lo, const int& hi)
    {mEventMult[0]=lo; mEventMult[1]=hi;}
inline void calculateEventPlaneEventCut::SetVertZPos(const float& lo, const float& hi)
    {mVertZPos[0]=lo; mVertZPos[1]=hi;}

inline int  calculateEventPlaneEventCut::NEventsPassed() {return mNEventsPassed;}
inline int  calculateEventPlaneEventCut::NEventsFailed() {return mNEventsFailed;}
inline void calculateEventPlaneEventCut::SetFlowMaker(char* title){
  mFlowMaker = (StFlowMaker*)StMaker::GetChain()->GetMaker(title);
  if (!mFlowMaker) {
    cout << "No StFlowMaker found!" << endl;
    assert(0);
  }
}
inline void calculateEventPlaneEventCut::SetFlowAnalysisMaker(char* title) {
  StMaker tempMaker;
  mFlowAnalysisMaker = (StFlowAnalysisMaker*)tempMaker.GetMaker(title);
  if (!mFlowAnalysisMaker) {
    cout << "No StFlowAnalysisMaker found!" << endl;
    assert(0);
  }
}
inline void calculateEventPlaneEventCut::FillFromHBT(const int& hbt) {
  mFromHBT = hbt;
}
inline calculateEventPlaneEventCut* calculateEventPlaneEventCut::Clone() {
  calculateEventPlaneEventCut* c = new calculateEventPlaneEventCut(*this); return c;
}
inline calculateEventPlaneEventCut::calculateEventPlaneEventCut(calculateEventPlaneEventCut& c) : StHbtEventCut(c) {
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
