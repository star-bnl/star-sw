///
// $Id: StFgtSimpleClusterAlgo.h,v 1.10 2013/02/19 18:24:04 avossen Exp $
// $Log: StFgtSimpleClusterAlgo.h,v $
// Revision 1.10  2013/02/19 18:24:04  avossen
// *** empty log message ***
//
// Revision 1.9  2012/03/08 17:43:40  avossen
// added default cluster algo, made StFgtIClusterAlgo destructor =0
//
// Revision 1.8  2012/03/07 03:57:23  avossen
// various updates
//
// Revision 1.7  2012/02/28 19:32:25  avossen
// many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
//
// Revision 1.6  2011/11/01 18:46:30  sgliske
// Updated to correspond with StEvent containers, take 2.
//
// Revision 1.5  2011/10/10 20:35:08  avossen
// fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
//  \author Anselm Vossen (avossen@indiana.edu) 
//  \class StFgtSimpleClusterAlgo
//
// Simple Clustering algorithm taking the hits from the StFgtEvent and adding Clusters
//
//
//


#ifndef STAR_StFgtSimpleClusterAlgo_HH
#define STAR_StFgtSimpleClusterAlgo_HH

#include "StFgtIClusterAlgo.h"
/**
This class implements the IClusterAlgo interface, in particular the doClustering function.
The implemented algo (simple) agregates all strips that are above threshold to clusters. It respects the fact that at the inner radius only every second P-Strip exist.
There is a cutoff on the maximum numbers of strips per cluster as a safety in case of noisy data.


Copy constructor and assignment operator omitted deliberately 
*/
class StFgtSimpleClusterAlgo :public StFgtIClusterAlgo
{

 public:
  StFgtSimpleClusterAlgo();
  ///the main function, using a collection of strips tht fired to build clusters of neighbouring strips
  virtual Int_t doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection& strips, StFgtHitCollection& clusters );
  virtual Int_t Init();
  virtual ~StFgtSimpleClusterAlgo();
 protected:
  ///migrated to A2C maker
  //  Bool_t checkPulse(StFgtHit* pClus);
  

 private:
  ClassDef(StFgtSimpleClusterAlgo,1);
};


#endif
