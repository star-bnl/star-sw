#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/eventCutMonitor_Mult.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(eventCutMonitor_Mult)
#endif
eventCutMonitor_Mult::eventCutMonitor_Mult(){
  mHisto = new StHbt1DHisto("Mult","multiplicity",1000,0,10000.);
  mHisto->SetDirectory(0);
}
//------------------------------
eventCutMonitor_Mult::eventCutMonitor_Mult(const char* TitCutMoni, const char* title,
						 int nbins, double min, double max){
  mHisto = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHisto->SetDirectory(0);
}
//------------------------------
eventCutMonitor_Mult::~eventCutMonitor_Mult(){
  delete mHisto;
}

//------------------------------
void eventCutMonitor_Mult::Fill(const StHbtEvent* event){
  mHisto->Fill( event->NumberOfTracks(), 1.);
}

//------------------------------
void eventCutMonitor_Mult::Finish(){
  cout << " entries in histogram : " << mHisto->Integral() << endl;
}

//------------------------------
StHbtString eventCutMonitor_Mult::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp," eventCutMonitor_Mult");
  Stemp=Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

