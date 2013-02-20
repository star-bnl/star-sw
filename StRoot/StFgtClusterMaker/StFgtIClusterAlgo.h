// $Id: StFgtIClusterAlgo.h,v 1.10 2013/02/20 01:32:27 avossen Exp $
// $Log: StFgtIClusterAlgo.h,v $
// Revision 1.10  2013/02/20 01:32:27  avossen
// added n strips before and after cluster
//
// Revision 1.9  2013/02/19 18:24:04  avossen
// *** empty log message ***
//
// Revision 1.8  2012/12/10 23:18:01  avossen
// merged cluster finder
//
// Revision 1.7  2012/03/08 17:43:40  avossen
// added default cluster algo, made StFgtIClusterAlgo destructor =0
//
// Revision 1.6  2011/11/01 18:46:30  sgliske
// Updated to correspond with StEvent containers, take 2.
//
// Revision 1.5  2011/10/03 19:39:46  avossen
// compiling version of simple cluster maker, changed PushBack->pushBack energy->charge in ClusterArray and Cluster
//
// Revision 1.4  2011/09/27 22:14:27  avossen
// cluster maker compiles
//
// Revision 1.3  2011/09/27 17:19:05  avossen
// simple cluster makers
//
// Revision 1.2  2011/08/24 14:30:44  avossen
// Continued raw maker development
//
// Revision 1.1  2011/08/23 03:05:09  avossen
// *** empty log message ***
//
//
//author Anselm Vossen
//
//abstract base class for cluster algorithm implementation
//
//
#ifndef STAR_StFgtIClusterAlgo_HH
#define STAR_StFgtIClusterAlgo_HH

#include "Stypes.h"
class StFgtStripCollection;
class StFgtHitCollection;
class StFgtCollection;
class StFgtDb;

class StFgtIClusterAlgo
{
 public:
  /**subclasses must implement this function that takes raw hits from StEvent and fills the Cluster collection
  //the input might be modified, since the clustering checks if the seeds are legitimate and adds the info to the strips if they 
  //are at the beginning or end of a cluster*/
  virtual Int_t doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection&, StFgtHitCollection& )=0;
  virtual Int_t Init()=0;
  virtual Int_t Finish()=0;
  virtual void setDb(StFgtDb* pDb)=0;

  virtual ~StFgtIClusterAlgo()=0;

 private:
  ClassDef( StFgtIClusterAlgo, 1 );  
};

#endif
