//$Id: StSstClusterControl.cxx,v 1.3 2016/06/06 18:48:39 bouchet Exp $
//
//$Log: StSstClusterControl.cxx,v $
//Revision 1.3  2016/06/06 18:48:39  bouchet
//coverity : UNINIT_CTOR
//
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein
#include "St_base/Stiostream.h"
#include "StSstUtil/StSstClusterControl.h"
#include "StMessMgr.h"
#include "tables/St_clusterControl_Table.h"
/*! 
Basic constructor. The members are filled in the code
*/
StSstClusterControl::StSstClusterControl(){
  mHighCut       = 0.;
  mTestTolerance = 0.;
  mClusterTreat  = 0.;
  mAdcTolerance  = 0.;
  mMatchMean     = 0.;
  mMatchSigma    = 0.;
}

/*!
Constructor loading the parameters from the Db table
 */
StSstClusterControl::StSstClusterControl(St_clusterControl *clusterCtrl){
  
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
