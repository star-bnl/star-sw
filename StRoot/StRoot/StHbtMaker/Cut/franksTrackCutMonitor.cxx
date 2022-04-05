#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Cut/franksTrackCutMonitor.h"
#include <cstdio>
#include <string>
#include "StLorentzVector.hh"

#ifdef __ROOT__ 
ClassImp(franksTrackCutMonitor)
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
franksTrackCutMonitor::franksTrackCutMonitor(const char* name){ // default constructor
  string s("franksTrackCutMonitor");
  string n(name);
  mDCAxy= new StHbt1DHisto( (s+n+"mDCAxy").c_str(),"Momentum (GeV/c) vs Energy loss (a.u.)",1000,0,5.); 
  mDCAxyGlobal= new StHbt1DHisto( (s+n+"mDCAxyGlobal").c_str(),"Momentum (GeV/c) vs Energy loss (a.u.)",1000,0.,5.); 
  mPvsDedx= new StHbt2DHisto( (s+n+"mPvsDedx").c_str(),"Momentum (GeV/c) vs Energy loss (a.u.)",100,-3.,3.,50,0.,5e-6); 
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
franksTrackCutMonitor::franksTrackCutMonitor( const  franksTrackCutMonitor& cutMoni)  {
  mDCAxy =new StHbt1DHisto(*(cutMoni.mDCAxy));
  mDCAxyGlobal =new StHbt1DHisto(*(cutMoni.mDCAxyGlobal));
  mPvsDedx =new StHbt2DHisto(*(cutMoni.mPvsDedx));
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
franksTrackCutMonitor::~franksTrackCutMonitor(){
  delete mDCAxy;
  delete mDCAxyGlobal;
  delete mPvsDedx ;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void  franksTrackCutMonitor::Fill(const StHbtTrack* track){
  mDCAxy->Fill( track->DCAxy(), 1.);
  mDCAxyGlobal->Fill( track->DCAxyGlobal(), 1.);
  mPvsDedx->Fill( abs(track->P())*track->Charge(), track->dEdx(), 1.);
}


