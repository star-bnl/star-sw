/***************************************************************************
 *
 * $Id: mikesEventCut.cxx,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex           
 *
 ***************************************************************************
 *
 * $Log: mikesEventCut.cxx,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/mikesEventCut.h"
#include <cstdio>

ClassImp(mikesEventCut)

mikesEventCut::mikesEventCut(){
  mNEventsPassed =  mNEventsFailed = 0;
}
//------------------------------
//mikesEventCut::~mikesEventCut(){
//  /* noop */
//}
//------------------------------
bool mikesEventCut::Pass(const StHbtEvent* event){
  int mult =  event->Mult();
  double VertexZPos = event->PrimVertPos().z();
  cout << "mikesEventCut::Pass -- mult, VertexZPos : " << mult << VertexZPos << endl;
  bool goodEvent =
    ((mult > mEventMult[0]) && 
     (mult < mEventMult[1]) && 
     (VertexZPos > mVertZPos[0]) &&
     (VertexZPos < mVertZPos[1]));
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  return (goodEvent);
}
//------------------------------
string mikesEventCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"Multiplicity:\t %d-%d\n",mEventMult[0],mEventMult[1]);
  Stemp = Ctemp;
  sprintf(Ctemp,"Vertex Z-position:\t %E-%E\n",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of events which passed:\t%ld  Number which failed:\t%ld\n",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  return (Stemp);
}
