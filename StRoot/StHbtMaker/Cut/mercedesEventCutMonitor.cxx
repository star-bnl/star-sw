/***************************************************************************
 *
 * $Id: mercedesEventCutMonitor.cxx,v 1.1 2002/04/09 15:14:46 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega , Ohio State, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Event Cut Monitor that plots just the tracks that passed 
 * mercedesStarStandardEventCut in the multiplicity plot
 * Plots: EventMultiplicity (just # of tracks that passed the event cut),
 * ZVertexPosition, and 2D plot: EventMultiplicity vs Tracks (# of tracks that 
 * passed the cuts vs # of tracks in the event) 
 * 
 ***************************************************************************
 *
 * $Log: mercedesEventCutMonitor.cxx,v $
 * Revision 1.1  2002/04/09 15:14:46  mercedes
 * Event Cut Monitor for mercedesStarStandardEventCut
 *
 *
 **************************************************************************/





#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/mercedesEventCutMonitor.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(mercedesEventCutMonitor)
#endif

mercedesEventCutMonitor::mercedesEventCutMonitor(){
  mEventMultHisto = new StHbt1DHisto("Event Multiplicity","EventMultHisto",1000,0.,1000.);
  mEventMultHisto->SetDirectory(0);
  mZVertexPosHisto = new StHbt1DHisto("ZVertex Position","zVertexPos",1000,-50.0,50.0);
  mZVertexPosHisto->SetDirectory(0);
  mEventMultvsTracks = new StHbt2DHisto("Event Multiplicuty vs Tracks ","EventMultvsTracks",1000, 0., 1000., 1000,0.,10000.0);
  mEventMultvsTracks->SetDirectory(0);
}
//------------------------------
mercedesEventCutMonitor::~mercedesEventCutMonitor(){
  delete mEventMultHisto;
  delete mZVertexPosHisto;
  delete mEventMultvsTracks;
}

//------------------------------
void mercedesEventCutMonitor::Fill(const StHbtEvent* event){

  double VertexZPos = event->PrimVertPos().z();
  int mult = 0;
  StHbtTrack* track;
  for (StHbtTrackIterator pIter=event->TrackCollection()->begin();pIter!=event->TrackCollection()->end();pIter++){
    track = *pIter;
    if ((track->NHits()>=10)&&(track->DCAxy()<3.0)&&(fabs(track->P().pseudoRapidity())<0.5)){
      mult++;
    }
  }
  mEventMultHisto->Fill(mult++,1.);
  mZVertexPosHisto->Fill (VertexZPos,1.);
  mEventMultvsTracks->Fill(mult++,event->NumberOfTracks(),1.);
}
//------------------------------
void mercedesEventCutMonitor::Finish(){
  cout << " entries in Multiplicity  histogram : " << mEventMultHisto->Integral() << endl;
  cout << " entries in Vertex Pos.  histogram : " << mZVertexPosHisto->Integral() << endl;
}
//------------------------------
StHbtString mercedesEventCutMonitor::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp," mercedesEventCutMonitor");
  Stemp=Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

