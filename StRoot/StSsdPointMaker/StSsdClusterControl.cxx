#include <Stiostream.h>
#include "StSsdClusterControl.h"
//_____________________________________________________________________________
StSsdClusterControl::StSsdClusterControl()
{
  // Former scf_ctrl table
  mHighCut       = 5.0;
  mTestTolerance = 0.2;

  // Former scm_ctrl table
  mClusterTreat  = 13;
  mAdcTolerance  =  0.2;
  mMatchMean     =  0.;
  mMatchSigma    =  8.;
}
//_____________________________________________________________________________
StSsdClusterControl::~StSsdClusterControl()
{
}
//_____________________________________________________________________________
void  StSsdClusterControl::printParameters(){
  cout<<"**** **** SSD Cluster Control Parameters **** ****"<<endl;
  cout<<"**** HighCut = "<<this->getHighCut()<<"  ****"<<endl;
  cout<<"**** TestTolerance = "<<this->getTestTolerance()<<"  ****"<<endl;
  cout<<"**** ClusterTreat = "<<this->getClusterTreat()<<"  ****"<<endl;
  cout<<"**** AdcTolerance = "<<this->getAdcTolerance()<<"  ****"<<endl;
  cout<<"**** MatchMean = "<<this->getMatchMean()<<"  ****"<<endl;
  cout<<"**** MatchSigma = "<<this->getMatchSigma()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}
