#include "StMassFunction.h"
#include <TH1.h>
#include <iostream.h>

StMassFunction::~StMassFunction() {}

void 
StMassFunction::Fill(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist) 
{
  weight = 1.0;
  correlation = func(t1,t2);
  hist->Fill(correlation,weight);
}

double
StMassFunction::GetCorr(StTrackForPool* t1, StTrackForPool* t2)
{
  return func(t1,t2);
}

double
StMassFunction::func(StTrackForPool* t1, StTrackForPool* t2)
{
  t1->GetMomentum(px1,py1,pz1);
  t2->GetMomentum(px2,py2,pz2);
  Double_t e1,e2;
  Double_t piMass = 0.139569;
  e1 = sqrt(px1*px1 + py1*py1 + pz1*pz1 + piMass*piMass);
  e2 = sqrt(px2*px2 + py2*py2 + pz2*pz2 + piMass*piMass);
  return  sqrt((e1+e2)*(e1+e2) - ((px1+px2)*(px1+px2)  + (py1+py2)*(py1+py2) + (pz1+pz2)*(pz1+pz2)));
}


TString
StMassFunction::GetName() 
{
  TString name = "InvariantMass";
  return name;
}
