#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/trackCutMonitor_DCA.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(trackCutMonitor_DCA)
#endif

trackCutMonitor_DCA::trackCutMonitor_DCA(){
  mHisto = new StHbt1DHisto("DCA","DCA (cm)",20,0.,10.);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_DCA::trackCutMonitor_DCA(const char* TitCutMoni, const char* title,
				       int nbins, double min, double max){
  mHisto = new StHbt1DHisto(TitCutMoni, title, nbins, min, max);
  mHisto->SetDirectory(0);
}
//------------------------------
trackCutMonitor_DCA::~trackCutMonitor_DCA(){
  delete mHisto;
}
//------------------------------
void trackCutMonitor_DCA::Fill(const StHbtTrack* track){
  mHisto->Fill( track->DCAxy() );
}


