#include "StIstCluster.h"
#include "StIstClusterCollection.h"
#include "StRoot/St_base/StMessMgr.h"


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


/***************************************************************************
*
* $Log: StIstClusterCollection.cxx,v $
* Revision 1.8.2.1  2014/12/12 22:33:29  smirnovd
* Squashed commit of the following:
*
*     StIstDb: Modified getter for sensors transormation matrix to accept ladder and sensor id-s using human friendly numbering starting with 1. The input values outside of possible ranges will return a null pointer
*
*     Use flags to indicate DbMaker readiness
*
*     Return fatal if database tables are not found
*
*     StIstDb: Added method to access transformation matrix for a given IST ladder/sensor pair
*
*     Set class version to 0 in order to avoid IO dictionary generation by ROOT's CINT. STAR makers are not persistent
*
*     [Style] Changes in comments and user feedback only
*
*     [Minor] Coding style clean-up. Removed unconstructive comments
*
*     Do not destruct StIstDb object as the ownership is passed to the framework
*
*     Renamed printGeoHMatrices to customary Print as that what users of ROOT framework normaly expect
*
*     Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.10  2014/11/18 23:11:36  smirnovd
* [Minor] Coding style clean-up. Removed unconstructive comments
*
* Revision 1.9  2014/11/18 23:08:37  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
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
