/***************************************************************************
 *
 * $Id: StSvtHybridCluster.hh,v 1.2 2003/09/02 17:59:07 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Hybrid Cluster Class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCluster.hh,v $
 * Revision 1.2  2003/09/02 17:59:07  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/07/06 03:50:34  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#ifndef STSVTHYBRIDCLUSTER_HH
#define STSVTHYBRIDCLUSTER_HH

#include <Stiostream.h>
#include "StSvtClusterFinder.hh"

typedef struct  StSvtClusterMemberInfo
 {
   int listAnode;
   int actualAnode;    //actual anode number
   int seq;

 } StSvtClusterMemberInfo;

#include "StSvtClassLibrary/StSvtHybridObject.hh" 

class StSvtHybridCluster:public StSvtHybridObject   //StSvtHybridObject inherits from TObject
{
  public:
     StSvtHybridCluster();
     StSvtHybridCluster(const StSvtHybridCluster& c);
     ~StSvtHybridCluster();

     int setCluster(StSvtClusterFinder* cluFinder);
     int getNumberOfClusters();
     int getNumberOfMembers(int clu);
     int getCluMemInfo(int clu, int &numOfMem, StSvtClusterMemberInfo** memInfo);

     StSvtClusterMemberInfo* getCluMemInfo(int clu);

   private:
     int numberOfClusters;
     int* numberOfMembers;
     StSvtClusterMemberInfo** memberInfo;

  ClassDef(StSvtHybridCluster,1)

};

#endif
