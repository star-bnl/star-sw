#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_Y_vs_Pt.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_Y_vs_Pt)
#endif

trackCutMonitor_Y_vs_Pt::trackCutMonitor_Y_vs_Pt(){
  mHisto = new StHbt2DHisto("Y_vs_Pt","y vs Pt (GeV/c)",50,-2.,2,50,0.,2.);
  cout << " this " << this << endl;
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_Y_vs_Pt::trackCutMonitor_Y_vs_Pt(double aMass, const char* TitCutMoni, const char* title,
					       int nbins1, double min1, double max1,
					       int nbins2, double min2, double max2):mMass(aMass){
  mHisto = new StHbt2DHisto(TitCutMoni, title, nbins1 , min1, max1, nbins2 , min2, max2);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_Y_vs_Pt::trackCutMonitor_Y_vs_Pt(const char* TitCutMoni, const char* title,
					       int nbins1, double min1, double max1,
					       int nbins2, double min2, double max2):mMass(0.139){
  mHisto = new StHbt2DHisto(TitCutMoni, title, nbins1 , min1, max1, nbins2 , min2, max2);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_Y_vs_Pt::~trackCutMonitor_Y_vs_Pt(){
  delete mHisto;
}

//------------------------------
void trackCutMonitor_Y_vs_Pt::Fill(const StHbtTrack* track){
  StHbtLorentzVector L;
  L.setVect(track->P());
  L.setE(track->P().massHypothesis(mMass));
  mHisto->Fill( L.rapidity(), L.vect().perp(), 1.);
}


