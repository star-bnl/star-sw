#include "StIstCluster.h"
#include "StIstClusterCollection.h"
#include "St_base/StMessMgr.h"


StIstClusterCollection::StIstClusterCollection( int ladder ) : StObject(), mLadder( ladder )
{
}

StIstClusterCollection::~StIstClusterCollection()
{
   Clear("");
}

void StIstClusterCollection::Clear( Option_t *opt )
{
   //free memory and clear the vector
   std::vector< StIstCluster * >::iterator vecIter;

   for ( vecIter = mClusterVec.begin(); vecIter != mClusterVec.end(); ++vecIter ) {
      if (*vecIter != NULL) {
         delete *vecIter;
         *vecIter = NULL;
      }
   }

   mClusterVec.clear();
}


void StIstClusterCollection::Print(Option_t *opt) const
{
   int clusterIdx = 0;

   for (std::vector<StIstCluster*>::const_iterator it = mClusterVec.begin(); it != mClusterVec.end(); ++it, ++clusterIdx)
   {
      LOG_DEBUG << "cluster: Idx=" << clusterIdx << endm;
      (*it)->Print();
   }
}


vector<StIstCluster *> &StIstClusterCollection::getClusterVec()
{
   return mClusterVec;
};

const vector<StIstCluster *> &StIstClusterCollection::getClusterVec() const
{
   return mClusterVec;
};

size_t StIstClusterCollection::getNumClusters() const
{
   return mClusterVec.size();
};

void StIstClusterCollection::setLadder( int ladder )
{
   mLadder = ladder;
};

unsigned char StIstClusterCollection::getLadder() const
{
   return mLadder;
};

ClassImp(StIstClusterCollection);
