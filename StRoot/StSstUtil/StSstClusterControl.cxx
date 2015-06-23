//$Id: StSstClusterControl.cxx,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstClusterControl.cxx,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein
#include <Stiostream.h>
#include "StSstUtil/StSstClusterControl.h"
#include "StMessMgr.h"
#include "tables/St_clusterControl_Table.h"
/*! 
Basic constructor. The members are filled in the code
*/
StSstClusterControl::StSstClusterControl()
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
StSstClusterControl::StSstClusterControl(St_clusterControl *clusterCtrl)
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
StSstClusterControl::~StSstClusterControl()
{
}
/*!
Printing the major parameters
 */
void  StSstClusterControl::printParameters(){
  cout<<"**** **** SSD Cluster Control Parameters **** ****"<<endl;
  cout<<"**** HighCut       = "<<this->getHighCut()<<"  ****"<<endl;
  cout<<"**** TestTolerance = "<<this->getTestTolerance()<<"  ****"<<endl;
  cout<<"**** ClusterTreat  = "<<this->getClusterTreat()<<"  ****"<<endl;
  cout<<"**** AdcTolerance  = "<<this->getAdcTolerance()<<"  ****"<<endl;
  cout<<"**** MatchMean     = "<<this->getMatchMean()<<"  ****"<<endl;
  cout<<"**** MatchSigma    = "<<this->getMatchSigma()<<"  ****"<<endl;
  cout<<"**************************************"<<endl;
}
