#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_Nhits_vs_NhitsDedx.h"
#include <cstdio>
#include "StLorentzVector.hh"

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_Nhits_vs_NhitsDedx)
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_Nhits_vs_NhitsDedx::trackCutMonitor_Nhits_vs_NhitsDedx(){
   trackCutMonitor_Nhits_vs_NhitsDedx("P_vs_Dedx","Momentum (GeV/c) vs Energy loss (a.u.)");
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_Nhits_vs_NhitsDedx::trackCutMonitor_Nhits_vs_NhitsDedx(const char* TitCutMoni, const char* title){
  mHisto= new StHbt2DHisto(TitCutMoni, title, 45 , -0.5, 45.5, 45 , -0.5, 45.5);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_Nhits_vs_NhitsDedx::trackCutMonitor_Nhits_vs_NhitsDedx( const  trackCutMonitor_Nhits_vs_NhitsDedx& cutMoni)  {
  mHisto = new StHbt2DHisto(*(cutMoni.mHisto));
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_Nhits_vs_NhitsDedx::~trackCutMonitor_Nhits_vs_NhitsDedx(){
  delete mHisto;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void  trackCutMonitor_Nhits_vs_NhitsDedx::Fill(const StHbtTrack* track){
  mHisto->Fill( track->NHits(), track->NHitsDedx(), 1.);
}


