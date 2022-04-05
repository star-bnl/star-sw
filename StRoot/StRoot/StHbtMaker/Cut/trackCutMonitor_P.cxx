#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_P.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_P)
#endif

trackCutMonitor_P::trackCutMonitor_P(){
  mHisto = new StHbt1DHisto("P","momentum (GeV/c)",20,0.,2.);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_P::trackCutMonitor_P(const char* TitCutMoni, const char* title,
						 int nbins, double min, double max){
  mHisto = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_P::~trackCutMonitor_P(){
  delete mHisto;
}

//------------------------------
void trackCutMonitor_P::Fill(const StHbtTrack* track){
  mHisto->Fill( abs(track->P()), 1.);
}

//------------------------------
void trackCutMonitor_P::Finish(){
  cout << " entries in histogram : " << mHisto->Integral() << endl;
}

//------------------------------
StHbtString trackCutMonitor_P::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp," report some stuff");
  Stemp=Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

