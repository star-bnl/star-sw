/***************************************************************************
 *
 * $Id: 
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * This is a class that holds a collection of StHbtTrackCuts
 * 
 ***************************************************************************
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtMultiTrackCut.h"
#include "StHbtMaker/Infrastructure/StHbtTrackCutCollection.hh"

#ifdef __ROOT__
ClassImp(StHbtMultiTrackCut)
#endif


// *************************************************************************
StHbtMultiTrackCut::StHbtMultiTrackCut(){
  mCutCollection = new StHbtTrackCutCollection;
}
// *************************************************************************
//StHbtMultiTrackCut::StHbtMultiTrackCut(const StHbtMultiTrackCut&) {}                // copy constructor
// *************************************************************************
StHbtMultiTrackCut::~StHbtMultiTrackCut(){
  StHbtTrackCutIterator iter;
  for (iter=mCutCollection->begin();iter!=mCutCollection->end();iter++){
    delete *iter;
  }
  delete mCutCollection;
}
// *************************************************************************
StHbtString StHbtMultiTrackCut::Report(){
  StHbtString temp;
  temp = "\n StHbtMultiTrackCut::Report() reporting ";
  temp += (int)mCutCollection->size();
  temp += " track cuts \n";
  StHbtTrackCutIterator iter;
  for (iter=mCutCollection->begin();iter!=mCutCollection->end();iter++){
    temp += (*iter)->Report();
  }
  return temp;
}
// *************************************************************************
bool StHbtMultiTrackCut::Pass(const StHbtTrack* track) {
  bool temp=0;
  StHbtTrackCutIterator iter;
  for (iter=mCutCollection->begin();iter!=mCutCollection->end();iter++){
    temp = temp ||  (*iter)->Pass(track);
  }
  return temp;
}
// *************************************************************************
void StHbtMultiTrackCut::EventBegin(const StHbtEvent* ev) {
  StHbtTrackCutIterator iter;
  for (iter=mCutCollection->begin();iter!=mCutCollection->end();iter++){
    (*iter)->EventBegin(ev);
  }
}
// *************************************************************************
void StHbtMultiTrackCut::EventEnd(const StHbtEvent* ev) {
  StHbtTrackCutIterator iter;
  for (iter=mCutCollection->begin();iter!=mCutCollection->end();iter++){
    (*iter)->EventEnd(ev);
  }
}
// *************************************************************************
void StHbtMultiTrackCut::AddTrackCut(StHbtTrackCut* cut) { 
  mCutCollection->push_back(cut); 
}
// *************************************************************************
StHbtMultiTrackCut::StHbtMultiTrackCut(const StHbtMultiTrackCut& cut ) : StHbtTrackCut() {
  StHbtMultiTrackCut();
  StHbtTrackCutIterator iter;
  StHbtTrackCut* t;
  for (iter=cut.mCutCollection->begin();iter!=cut.mCutCollection->end();iter++){
    t = (*iter)->Clone();
    if (t) mCutCollection->push_back(t);
  }
}
// *************************************************************************
StHbtMultiTrackCut* StHbtMultiTrackCut::Clone() { 
  StHbtMultiTrackCut* cut = this->Clone();
  return cut;
}



