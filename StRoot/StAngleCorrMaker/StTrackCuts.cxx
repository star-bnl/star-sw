#include "StTrackCuts.h"
#include <iostream.h>

StTrackCuts::StTrackCuts() 
{
  chargeCut = "any";
  charge = 0;
  rap_lowCut = -10.0;
  rap_upCut = 10.0;
  p_lowCut = 1.0;
  p_upCut  = 100.0;
  pt_lowCut = 0.0;
  pt_upCut  = 100.0;
  chi2_lowCut= 0.0;
  chi2_upCut= 10.0;
  nPoints_lowCut = 30.0;
  nPoints_upCut = 100.0;
}

StTrackCuts::~StTrackCuts() {}

Int_t 
StTrackCuts::TrackSatisfiesCuts(StTrackForPool* t)
{
  Double_t trap,tpt,tp,tchi2,tch,tnpoints;
  t->GetPseudoRapidity(trap);
  t->GetMomentum(tp);
  t->GetPt(tpt);
  tch = t->GetCharge();
  tchi2 = t->GetChiSquared();
  tnpoints = t->GetNTPCPoints();
  
  if (chargeCut == "specific" && charge != tch) return 0;
  if (trap < rap_lowCut || trap > rap_upCut  ) return 0;
  if (tp < p_lowCut || tp > p_upCut  ) return 0;
  if (tpt < pt_lowCut || tp > pt_upCut  ) return 0;
  if (tchi2 < chi2_lowCut || tchi2 > chi2_upCut  ) return 0;
  if (tnpoints < nPoints_lowCut || tnpoints > nPoints_upCut  ) return 0;
  
  return 1; 
}

void 
StTrackCuts:: SetMomentumCuts(Double_t lowerCut, Double_t upperCut)
{
  p_lowCut = lowerCut;
  p_upCut = upperCut;
}

void 
StTrackCuts:: SetPtCuts(Double_t lowerCut, Double_t upperCut)
{
  pt_lowCut = lowerCut;
  pt_upCut = upperCut;
}

void 
StTrackCuts:: SetTrackCharge(Double_t Cut)
{
  charge = Cut;
  chargeCut = "specific";
}

void 
StTrackCuts:: SetPseudoRapidityCuts(Double_t lowerCut, Double_t upperCut)
{
  rap_lowCut = lowerCut;
  rap_upCut = upperCut;
}

void 
StTrackCuts:: SetChi2Cuts(Double_t lowerCut, Double_t upperCut)
{
  chi2_lowCut = lowerCut;
  chi2_upCut = upperCut;
}


void 
StTrackCuts:: SetNTPCPointsCuts(Double_t lowerCut, Double_t upperCut)
{
  nPoints_lowCut = lowerCut;
  nPoints_upCut = upperCut;
}
