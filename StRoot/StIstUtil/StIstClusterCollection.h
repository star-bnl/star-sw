/***************************************************************************
*
* $Id: StIstClusterCollection.h,v 1.4 2014/02/13 02:35:49 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* A collection of StIstCluster classes, and basically is a wrapper for a
* clusters vector. One instance corresponds to one ladder.
****************************************************************************/

#ifndef StIstClusterCollection_hh
#define StIstClusterCollection_hh

#include "StObject.h"
#include "StIstCluster.h"

class StIstClusterCollection : public StObject
{
public:
   //constructors
   StIstClusterCollection(unsigned char ladder = 0);

   //deconstructor
   ~StIstClusterCollection();

   vector<StIstCluster *> &getClusterVec();
   const vector<StIstCluster *> &getClusterVec() const;

   //size of internal vector
   size_t getNumClusters() const;

   //modify/access the ladder
   unsigned char getLadder() const;
   void setLadder( unsigned char ladder );

   //Clear
   void Clear( Option_t *opt = "" );

protected:
   //data members
   unsigned char mLadder;
   std::vector<StIstCluster *> mClusterVec;

private:
   ClassDef(StIstClusterCollection, 1);
};

#endif


/***************************************************************************
*
* $Log: StIstClusterCollection.h,v $
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/
