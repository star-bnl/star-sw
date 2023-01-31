#include "StFstCluster.h"
#include "StFstClusterCollection.h"
#include "St_base/StMessMgr.h"


StFstClusterCollection::StFstClusterCollection( int wedge ) : StObject(), mWedge( wedge )
{
}

StFstClusterCollection::~StFstClusterCollection()
{
   Clear("");
}

void StFstClusterCollection::Clear( Option_t *opt )
{
   //free memory and clear the vector
   std::vector< StFstCluster * >::iterator vecIter;

   for ( vecIter = mClusterVec.begin(); vecIter != mClusterVec.end(); ++vecIter ) {
      if (*vecIter != NULL) {
         delete *vecIter;
         *vecIter = NULL;
      }
   }

   mClusterVec.clear();
}


void StFstClusterCollection::Print(Option_t *opt) const
{
   int clusterIdx = 0;

   for (std::vector<StFstCluster*>::const_iterator it = mClusterVec.begin(); it != mClusterVec.end(); ++it, ++clusterIdx)
   {
      LOG_DEBUG << "cluster: Idx=" << clusterIdx << endm;
      (*it)->Print();
   }
}


vector<StFstCluster *> &StFstClusterCollection::getClusterVec()
{
   return mClusterVec;
};

const vector<StFstCluster *> &StFstClusterCollection::getClusterVec() const
{
   return mClusterVec;
};

size_t StFstClusterCollection::getNumClusters() const
{
   return mClusterVec.size();
};

void StFstClusterCollection::setWedge( int wedge )
{
   mWedge = wedge;
};

unsigned char StFstClusterCollection::getWedge() const
{
   return mWedge;
};

ClassImp(StFstClusterCollection);
