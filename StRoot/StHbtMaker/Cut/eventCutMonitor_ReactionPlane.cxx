#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Cut/eventCutMonitor_ReactionPlane.h"
#include <cstdio>
#include <cmath>

#ifdef __ROOT__ 
ClassImp(eventCutMonitor_ReactionPlane)
#endif
eventCutMonitor_ReactionPlane::eventCutMonitor_ReactionPlane(){
  mScaler = new StHbt1DHisto("Scaler", "Scaler", 100, -0.5, 99.5);
  mVertexX = new StHbt1DHisto("VertexX", "VertexX", 100, -2.,2.);
  mVertexY = new StHbt1DHisto("VertexY", "VertexY", 100, -2.,2.);
  mVertexZ = new StHbt1DHisto("VertexZ", "VertexZ", 100, -300.,300.);
  mReactionPlane = new StHbt1DHisto("ReactionPlane", "ReactionPlane", 36, -2.*asin(1.), 2.*asin(1.) );
  mReactionPlaneError = new StHbt1DHisto("ReactionPlaneError", "ReactionPlaneError", 36, 0., 2.*asin(1.) );
  mMultReactionPlaneError = new StHbt2DHisto("Mult_vs_ReactionPlaneError", "Mult_vs_ReactionPlaneError", 36, 0., 2.*asin(1.),100,0,5.e2);
}
//------------------------------
eventCutMonitor_ReactionPlane::eventCutMonitor_ReactionPlane(const char* title1, const char* title2,
						 int nbins, double min, double max){
  char tit1[100];
  sprintf(tit1,"%s%s_Scaler",title1,title2);
  mScaler = new StHbt1DHisto(tit1, "Scaler", 100, -0.5,99.5);
  sprintf(tit1,"%s%s_VertexX",title1,title2);
  mVertexX = new StHbt1DHisto(tit1, "VertexX", 100, -1.,1.);
  sprintf(tit1,"%s%s_VertexY",title1,title2);
  mVertexY = new StHbt1DHisto(tit1, "VertexY", 100, -1.,1.);
  sprintf(tit1,"%s%s_VertexZ",title1,title2);
  mVertexZ = new StHbt1DHisto(tit1, "VertexZ", 100, -200.,200.);
  sprintf(tit1,"%s%s_ReactionPlane",title1,title2);
  mReactionPlane = new StHbt1DHisto(tit1,"ReactionPlane", nbins, min, max);
  sprintf(tit1,"%s%s_ReactionPlaneError",title1,title2);
  mReactionPlaneError = new StHbt1DHisto(tit1,"ReactionPlaneError", nbins , min, max);
  sprintf(tit1,"%s%s_MultReactionPlaneError",title1,title2);
  mMultReactionPlaneError = new StHbt2DHisto(tit1,"MultReactionPlaneError", nbins, min, max ,100,0,1.e3);
}
//------------------------------
eventCutMonitor_ReactionPlane::~eventCutMonitor_ReactionPlane(){
  delete mScaler;
  delete mVertexX;
  delete mVertexY;
  delete mVertexZ;
  delete mReactionPlane;
  delete mReactionPlaneError;
  delete mMultReactionPlaneError;
}

//------------------------------
void eventCutMonitor_ReactionPlane::Fill(const StHbtEvent* event){
  mScaler->Fill(0.); // fill event counter
  mVertexX->Fill( event->PrimVertPos().x(), 1.);
  mVertexY->Fill( event->PrimVertPos().y(), 1.);
  mVertexZ->Fill( event->PrimVertPos().z(), 1.);
  mReactionPlane->Fill( event->ReactionPlane(), 1.);
  mReactionPlaneError->Fill( event->ReactionPlaneError(), 1.);
  mMultReactionPlaneError->Fill( event->ReactionPlaneError(), event->UncorrectedNumberOfNegativePrimaries());
}

//------------------------------
void eventCutMonitor_ReactionPlane::Finish(){
  cout << " entries in histogram mReactionPlane : " << mReactionPlane->Integral() << endl;
  cout << " entries in histogram mReactionPlaneError : " << mReactionPlaneError->Integral() << endl;
  cout << " entries in histogram mMultReactionPlaneError : " << mMultReactionPlaneError->Integral() << endl;
}

//------------------------------
StHbtString eventCutMonitor_ReactionPlane::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp," eventCutMonitor_ReactionPlane");
  Stemp=Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}

