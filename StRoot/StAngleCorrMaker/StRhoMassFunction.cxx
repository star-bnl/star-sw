#include "StRhoMassFunction.h"
#include <TH1.h>


StRhoMassFunction::~StRhoMassFunction() {}

void 
StRhoMassFunction::Fill(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist) 
{
  // calculates  angle between the momenta of two tracks
  // 0 < alphadiff < pi
  weight = 1.0;
  t1->GetMomentum(px1,py1,pz1);
  t2->GetMomentum(px2,py2,pz2);
  Double_t e1,e2;
  Double_t piMass,rhoMass;
  piMass = 0.139569;
  rhoMass = 0.493;
  e1 = sqrt(px1*px1 + py1*py1 + pz1*pz1 + piMass*piMass);
  e2 = sqrt(px2*px2 + py2*py2 + pz2*pz2 + piMass*piMass);
  correlation = sqrt(e1*e2 - px1*px2 - py1*py2 - pz1*pz2);
  hist->Fill(correlation,weight);
}


TString
StRhoMassFunction::GetName() 
{
  TString name = "rhoMass";
  return name;
}
