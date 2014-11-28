/***************************************************************************
 *
 * $Id: StSvtHybridCluster.cc,v 1.2 2000/08/21 13:06:58 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Hybrid Cluster class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCluster.cc,v $
 * Revision 1.2  2000/08/21 13:06:58  caines
 * Much improved hit finding and fitting
 *
 * Revision 1.1  2000/07/06 03:50:34  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#include "StSvtHybridCluster.hh"


ClassImp(StSvtHybridCluster)

StSvtHybridCluster::StSvtHybridCluster()
{
 numberOfClusters = 0;
 numberOfMembers = NULL;
 memberInfo = NULL;

}



StSvtHybridCluster::StSvtHybridCluster(const StSvtHybridCluster& c)
{
 numberOfClusters = c.numberOfClusters;  
 numberOfMembers =c.numberOfMembers; 
 memberInfo =c.memberInfo;
}


StSvtHybridCluster::~StSvtHybridCluster()
{
 delete[] numberOfMembers;

for(int clu = 0; clu <  numberOfClusters; clu++)
  delete[] memberInfo[clu];

 delete [] memberInfo;

}

int StSvtHybridCluster::setCluster(StSvtClusterFinder* cluFinder)
{
 
  numberOfClusters = cluFinder -> ClusterIndex();

  numberOfMembers = new int[numberOfClusters];

  memberInfo = new StSvtClusterMemberInfo*[numberOfClusters];
 
 for(int clu = 0; clu <  numberOfClusters; clu++)
  {
   numberOfMembers[clu] = cluFinder -> ClusterMembers(clu);
   
   memberInfo[clu] = new StSvtClusterMemberInfo[numberOfMembers[clu]];

   for(int mem = 0; mem < numberOfMembers[clu]; mem++)
     {
       memberInfo[clu][mem].listAnode  = cluFinder -> ClusterListAnode(clu,mem);
       int listanode = memberInfo[clu][mem].listAnode;
       memberInfo[clu][mem].actualAnode = cluFinder ->ClusterActualAnode(listanode);
       memberInfo[clu][mem].seq =  cluFinder -> ClusterSequence(clu,mem);   
    
     }
  }
 return 0;
}

int StSvtHybridCluster::getNumberOfClusters()
  { 
    return numberOfClusters;
  }


int StSvtHybridCluster::getNumberOfMembers(int clu)
  { 
    return numberOfMembers[clu];
  }

int StSvtHybridCluster::getCluMemInfo(int clu, int &numOfMem, StSvtClusterMemberInfo** memInfo)
  {
   for(int i=0; i<numberOfClusters;i++)
     if(i==clu)
       {
        numOfMem = numberOfMembers[i];
        *memInfo = NULL;
        *memInfo = memberInfo[i];
         break;
        }
 return 0; 
 }


StSvtClusterMemberInfo* StSvtHybridCluster::getCluMemInfo(int clu)
{
  if(clu >= 0 && clu <= numberOfClusters) {
    return  memberInfo[clu]; 
  }
  else{
    return NULL;
  }
}

