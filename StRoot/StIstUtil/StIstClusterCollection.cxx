/***************************************************************************
*
* $Id: StIstClusterCollection.cxx,v 1.4 2014/02/13 02:35:49 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************/

#include "StIstCluster.h"
#include "StIstClusterCollection.h"

//constructors
StIstClusterCollection::StIstClusterCollection( unsigned char ladder ) : StObject(), mLadder( ladder )
{
   /* nothing to do */
};

//deconstructor
StIstClusterCollection::~StIstClusterCollection()
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

void StIstClusterCollection::Clear( Option_t *opt )
{
   // clear the vector
   mClusterVec.clear();
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

void StIstClusterCollection::setLadder( unsigned char ladder )
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
