#include "StOpeningAngle.h"

StOpeningAngle::~StOpeningAngle() {}

void 
StOpeningAngle::Fill(StTrackForPool* t1, StTrackForPool* t2, TH1D* hist) 
{
  // calculates  angle between the momenta of two tracks
  // 0 < alphadiff < pi
  weight = 1.0;
  t1->GetMomentum(px1,py1,pz1);
  t2->GetMomentum(px2,py2,pz2);
  p1=sqrt(px1*px1+py1*py1+pz1*pz1);
  p2=sqrt(px2*px2+py2*py2+pz2*pz2);
  correlation = (180./3.14159265)*acos( (px1*px2 + py1*py2 + pz1*pz2)/(p1*p2) );
  hist->Fill(correlation,weight);
}

TString 
StOpeningAngle::GetName() 
{
  TString name = "openingAngle";
  return name;
}
