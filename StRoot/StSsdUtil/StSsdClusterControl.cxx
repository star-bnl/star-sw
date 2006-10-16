#include <Stiostream.h>
#include "StSsdUtil/StSsdClusterControl.h"
#include "StMessMgr.h"
#include "tables/St_clusterControl_Table.h"
/*! 
Basic constructor. The members are filled in the code
*/
StSsdClusterControl::StSsdClusterControl()
{
  // Former scf_ctrl table
//   mHighCut       = 5.0;
//   mTestTolerance = 0.2;


//   // Former scm_ctrl table
//   mClusterTreat  = 13;
//   mAdcTolerance  =  0.2;
//   mMatchMean     =  0.;
//   mMatchSigma    =  8.;
}
/*!
Constructor loading the parameters from the Db table
 */
StSsdClusterControl::StSsdClusterControl(St_clusterControl *clusterCtrl)
{

  clusterControl_st *cluster = clusterCtrl->GetTable();
  if (!cluster) gMessMgr->Error() << "No clusterControl_st table available" << endm;
  else
    {
      mHighCut       = cluster[0].highCut;
      mTestTolerance = cluster[0].testTolerance;
      mClusterTreat  = cluster[0].clusterTreat;
      mAdcTolerance  = cluster[0].adcTolerance;
      mMatchMean     = cluster[0].matchMean;
      mMatchSigma    = cluster[0].matchSigma;
    }
}
/*!
The destructor deletes nothing
 */
StSsdClusterControl::~StSsdClusterControl()
{
}
/*!
Printing the major parameters
 */
void  StSsdClusterControl::printParameters(){
  cout<<"**** **** SSD Cluster Control Parameters **** ****"<<endl;
  cout<<"**** HighCut       = "<<this->getHighCut()<<"  ****"<<endl;
  cout<<"**** TestTolerance = "<<this->getTestTolerance()<<"  ****"<<endl;
  cout<<"**** ClusterTreat  = "<<this->getClusterTreat()<<"  ****"<<endl;
  cout<<"**** AdcTolerance  = "<<this->getAdcTolerance()<<"  ****"<<endl;
  cout<<"**** MatchMean     = "<<this->getMatchMean()<<"  ****"<<endl;
  cout<<"**** MatchSigma    = "<<this->getMatchSigma()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}
