#include <Stiostream.h>
#include <stdlib.h>
#include "StSsdClusterControl.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_scm_ctrl_Table.h"

StSsdClusterControl::StSsdClusterControl()
{
}
//_____________________________________________________________________________
StSsdClusterControl::~StSsdClusterControl()
{
}
//_____________________________________________________________________________
float StSsdClusterControl::getHighCut()       { return mHighCut; }
float StSsdClusterControl::getTestTolerance() { return mTestTolerance; }
long  StSsdClusterControl::getClusterTreat()  { return mClusterTreat; }
float StSsdClusterControl::getAdcTolerance()  { return mAdcTolerance; }
float StSsdClusterControl::getMatchMean()     { return mMatchMean; }
float StSsdClusterControl::getMatchSigma()    { return mMatchSigma; }

void  StSsdClusterControl::setHighCut(float highCut) {mHighCut = highCut;}
void  StSsdClusterControl::setTestTolerance(float testTolerance){mTestTolerance = testTolerance;}
void  StSsdClusterControl::setClusterTreat(long clusterTreat){mClusterTreat = clusterTreat;}
void  StSsdClusterControl::setAdcTolerance(float adcTolerance){mAdcTolerance = adcTolerance;}
void  StSsdClusterControl::setMatchMean(float matchMean){mMatchMean = matchMean;}
void  StSsdClusterControl::setMatchSigma(float matchSigma){mMatchSigma = matchSigma;}

void  StSsdClusterControl::printParameters(){
  cout<<"**** **** Control Parameters **** ****"<<endl;
  cout<<"**** HighCut = "<<this->getHighCut()<<"  ****"<<endl;
  cout<<"**** TestTolerance = "<<this->getTestTolerance()<<"  ****"<<endl;
  cout<<"**** ClusterTreat = "<<this->getClusterTreat()<<"  ****"<<endl;
  cout<<"**** AdcTolerance = "<<this->getAdcTolerance()<<"  ****"<<endl;
  cout<<"**** MatchMean = "<<this->getMatchMean()<<"  ****"<<endl;
  cout<<"**** MatchSigma = "<<this->getMatchSigma()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}
