#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_P_vs_Dedx.h"
#include <cstdio>
#include "StLorentzVector.hh"

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_P_vs_Dedx)
#endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::trackCutMonitor_P_vs_Dedx(){ // default constructor
   trackCutMonitor_P_vs_Dedx(0);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::trackCutMonitor_P_vs_Dedx(int charge){
   trackCutMonitor_P_vs_Dedx(charge,"P_vs_Dedx","Momentum (GeV/c) vs Energy loss (a.u.)",50,0.,1,50,0.,5e-6);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::trackCutMonitor_P_vs_Dedx(int charge, const char* TitCutMoni, const char* title,
						   int nbins1, double min1, double max1,
						   int nbins2, double min2, double max2){
  mHisto= new StHbt2DHisto(TitCutMoni, title, nbins1 , min1, max1, nbins2 , min2, max2);
  mCharge = charge;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::trackCutMonitor_P_vs_Dedx( const  trackCutMonitor_P_vs_Dedx& cutMoni)  {
  mCharge = cutMoni.mCharge;
  mHisto = new StHbt2DHisto(*(cutMoni.mHisto));
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::trackCutMonitor_P_vs_Dedx( const  trackCutMonitor_P_vs_Dedx& cutMoni, int charge) {
  mCharge = charge;
  mHisto = new StHbt2DHisto(*(cutMoni.mHisto));
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
trackCutMonitor_P_vs_Dedx::~trackCutMonitor_P_vs_Dedx(){
  delete mHisto;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void  trackCutMonitor_P_vs_Dedx::Fill(const StHbtTrack* track){
  if (track->Charge() == mCharge || mCharge == 0)
    mHisto->Fill( abs( track->P() ), track->dEdx(), 1.);
}


