#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Cut/v0CutMonitor_Minv.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(v0CutMonitor_Minv)
#endif

v0CutMonitor_Minv::v0CutMonitor_Minv(){
  mHistoK0Short = new StHbt1DHisto("K0ShortMinv","invariant mass (GeV/c)",200,0.,1.);
  mHistoK0Short->SetDirectory(0);
  mHistoLambda = new StHbt1DHisto("LambdaMinv","invariant mass (GeV/c)",200,0.5,1.5);
  mHistoLambda->SetDirectory(0);
  mHistoAntiLambda = new StHbt1DHisto("AntiLambdaMinv","invariant mass (GeV/c)",200,0.5,1.5);
  mHistoAntiLambda->SetDirectory(0);
}
//------------------------------
v0CutMonitor_Minv::v0CutMonitor_Minv(const char* Titlek0,const char* Titlela,const char* Titlelab){
  mHistoK0Short = new StHbt1DHisto(Titlek0,"invariant mass (GeV/c)",200,0.,1.);
  mHistoK0Short->SetDirectory(0);
  mHistoLambda = new StHbt1DHisto(Titlela,"invariant mass (GeV/c)",200,0.5,1.5);
  mHistoLambda->SetDirectory(0);
  mHistoAntiLambda = new StHbt1DHisto(Titlelab,"invariant mass (GeV/c)",200,0.5,1.5);
  mHistoAntiLambda->SetDirectory(0);
}
//------------------------------
v0CutMonitor_Minv::v0CutMonitor_Minv(const char* TitCutMoni, const char* title,
						 int nbins, double min, double max){
  mHistoK0Short = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHistoK0Short->SetDirectory(0);
  mHistoLambda = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHistoLambda->SetDirectory(0);
  mHistoAntiLambda = new StHbt1DHisto(TitCutMoni, title, nbins , min, max);
  mHistoAntiLambda->SetDirectory(0);
}
//------------------------------
v0CutMonitor_Minv::~v0CutMonitor_Minv(){
  delete mHistoK0Short;
  delete mHistoLambda;
  delete mHistoAntiLambda;
}

//------------------------------
void v0CutMonitor_Minv::Fill(const StHbtV0* v0){
  mHistoK0Short->Fill( v0->massK0Short() , 1.);
  mHistoLambda->Fill( v0->massLambda() , 1.);
  mHistoAntiLambda->Fill( v0->massAntiLambda() , 1.);
}

//------------------------------
void v0CutMonitor_Minv::Finish(){
  cout << " entries in K0Short histogram    : " << mHistoK0Short->Integral() << endl;
  cout << " entries in Lambda histogram     : " << mHistoLambda->Integral() << endl;
  cout << " entries in AntiLambda histogram : " << mHistoAntiLambda->Integral() << endl;
}

//------------------------------
StHbtString v0CutMonitor_Minv::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp," v0CutMonitor ");
  Stemp=Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

