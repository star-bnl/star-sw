#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_Pt.h"

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_Pt)
#endif

trackCutMonitor_Pt::trackCutMonitor_Pt(){
  mHisto = new StHbt1DHisto("Pt","Pt (GeV/c)",20,0.,2.);
  cout << " this " << this << endl;
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_Pt::trackCutMonitor_Pt(const char* TitCutMoni, const char* title,
						 int nbins, double min, double max){
  mHisto = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_Pt::~trackCutMonitor_Pt(){
  delete mHisto;
}

//------------------------------
void trackCutMonitor_Pt::Fill(const StHbtTrack* track){
  mHisto->Fill( track->P().perp(), 1.);
}


