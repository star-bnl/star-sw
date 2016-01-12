#ifndef StIstClusterCollection_hh
#define StIstClusterCollection_hh

#include "StObject.h"
#include "StIstCluster.h"


/**
 * A collection of StIstCluster objects which is basically a wrapper for
 * a clusters vector. One instance corresponds to one ladder.
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstClusterCollection : public StObject
{
public:

   StIstClusterCollection(int ladder = 0);
   ~StIstClusterCollection();

   vector<StIstCluster *> &getClusterVec();
   const vector<StIstCluster *> &getClusterVec() const;

   //size of internal vector
   size_t getNumClusters() const;

   //modify/access the ladder
   unsigned char getLadder() const;
   void setLadder( int ladder );

   void Clear( Option_t *opt = "" );
   virtual void Print(Option_t *opt = "") const;

protected:

   unsigned char mLadder;
   std::vector<StIstCluster *> mClusterVec;

   ClassDef(StIstClusterCollection, 1);
};

#endif
