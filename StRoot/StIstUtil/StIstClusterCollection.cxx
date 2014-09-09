/***************************************************************************
*
* $Id: StIstClusterCollection.cxx,v 1.8 2014/09/09 08:23:46 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************/

#include "StIstCluster.h"
#include "StIstClusterCollection.h"
#include "StRoot/St_base/StMessMgr.h"


//constructors
StIstClusterCollection::StIstClusterCollection( int ladder ) : StObject(), mLadder( ladder )
{
   /* nothing to do */
};

//deconstructor
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


/***************************************************************************
*
* $Log: StIstClusterCollection.cxx,v $
* Revision 1.8  2014/09/09 08:23:46  ypwang
* all unsgined char was updated to int type as Victor P. suggested
*
* Revision 1.7  2014/09/08 19:06:57  smirnovd
* Added Print() methods to print out properties of StIstCluster and StIstRawHit objects and their respective collections
*
* Revision 1.6  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.5  2014/02/14 14:51:06  ypwang
* update Clear() function, and call Clear() function in deconstructor
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/
