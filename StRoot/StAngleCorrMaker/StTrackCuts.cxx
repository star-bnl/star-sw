#include "StTrackCuts.h"

StTrackCuts::StTrackCuts() 
{
  chargeCut = "any";
  charge = 0;
  rap_lowCut = -10.0;
  rap_upCut = 10.0;
  p_lowCut = 0.0;
  p_upCut  = 100.0;
  pt_lowCut = 0.0;
  pt_upCut  = 100.0;
  rchiXY_lowCut= 0.0;
  rchiXY_upCut= 0.0;
  rchiZ_lowCut= 0.0;
  rchiZ_upCut= 0.0;
  nPoints_lowCut = 0.0;
  nPoints_upCut = 100.0;
}

StTrackCuts::~StTrackCuts() {}

Int_t 
StTrackCuts::TrackSatisfiesCuts(StTrackForPool* t)
{
  Double_t trap,tpt,tp,trchixy,trchiz,tch,tnpoints;
  t->GetPseudoRapidity(trap);
  t->GetMomentum(tp);
  t->GetPt(tpt);
  tch = t->GetCharge();
  trchixy = t->GetRChiSquaredXY();
  trchiz = t->GetRChiSquaredZ();
  tnpoints = t->GetNTPCPoints();

  if (chargeCut == "specific" && charge != tch) return 0;
  if (trap < rap_lowCut || trap > rap_upCut  ) return 0;
  if (tp < p_lowCut || tp > p_upCut  ) return 0;
  if (tpt < pt_lowCut || tp > pt_upCut  ) return 0;
  if (trchixy < rchiXY_lowCut || trchixy > rchiXY_upCut  ) return 0;
  if (trchiz < rchiZ_lowCut || trchiz > rchiZ_upCut  ) return 0;
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
StTrackCuts:: SetRChiXYCuts(Double_t lowerCut, Double_t upperCut)
{
  rchiXY_lowCut = lowerCut;
  rchiXY_upCut = upperCut;
}

void 
StTrackCuts:: SetRChiZCuts(Double_t lowerCut, Double_t upperCut)
{
  rchiZ_lowCut = lowerCut;
  rchiZ_upCut = upperCut;
}


void 
StTrackCuts:: SetNTPCPointsCuts(Double_t lowerCut, Double_t upperCut)
{
  nPoints_lowCut = lowerCut;
  nPoints_upCut = upperCut;
}
