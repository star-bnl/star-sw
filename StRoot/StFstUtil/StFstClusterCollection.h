#ifndef StFstClusterCollection_hh
#define StFstClusterCollection_hh

#include "StObject.h"
#include "StFstCluster.h"


/**
 * A collection of StFstCluster objects which is basically a wrapper for
 * a clusters vector. One instance corresponds to one wedge.
 *
 * \author Shenghui Zhang
 * \date Sep. 2021
 */
class StFstClusterCollection : public StObject
{
public:

   StFstClusterCollection(int wedge = 0);
   ~StFstClusterCollection();

   vector<StFstCluster *> &getClusterVec();
   const vector<StFstCluster *> &getClusterVec() const;

   //size of internal vector
   size_t getNumClusters() const;

   //modify/access the wedge
   unsigned char getWedge() const;
   void setWedge( int wedge );

   void Clear( Option_t *opt = "" );
   virtual void Print(Option_t *opt = "") const;

protected:

   unsigned char mWedge;
   std::vector<StFstCluster *> mClusterVec;

   ClassDef(StFstClusterCollection, 1);
};

#endif
