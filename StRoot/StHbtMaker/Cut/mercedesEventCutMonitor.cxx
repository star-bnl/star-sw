/***************************************************************************
 *
 * $Id: mercedesEventCutMonitor.cxx,v 1.3 2003/01/22 16:05:11 mercedes Exp $
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
 * Revision 1.3  2003/01/22 16:05:11  mercedes
 * Minor changes
 *
 * Revision 1.2  2003/01/17 16:47:31  mercedes
 * Updated to get reference multiplicity
 *
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
  mEventMultHisto = new StHbt1DHisto("EventMultHisto","Event Multiplicity",1000,0.,1000.);
  mEventMultHisto->SetDirectory(0);
  mZVertexPosHisto = new StHbt1DHisto("ZVertexPos","zVertex Position",1000,-50.0,50.0);
  mZVertexPosHisto->SetDirectory(0);
  mEventMultvsTracks = new StHbt2DHisto("EventMultvsTracks ","Event Multiplicity vs Total Tracks",1000, 0., 1000., 1000,0.,10000.0);
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
  int mult = event->UncorrectedNumberOfPrimaries();

  mEventMultHisto->Fill(mult,1.);
  mZVertexPosHisto->Fill (VertexZPos,1.);
  mEventMultvsTracks->Fill(mult,event->NumberOfTracks(),1.);
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

