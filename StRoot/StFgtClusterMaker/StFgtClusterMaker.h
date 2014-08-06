//
// First Cluster Maker
//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.h,v 1.21 2014/08/06 11:43:09 jeromel Exp $
//   $Log: StFgtClusterMaker.h,v $
//   Revision 1.21  2014/08/06 11:43:09  jeromel
//   Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
//   Revision 1.20  2013/02/20 01:32:27  avossen
//   added n strips before and after cluster
//
//   Revision 1.19  2012/12/10 23:18:00  avossen
//   merged cluster finder
//
//   Revision 1.18  2012/07/31 21:45:25  jeromel
//   Misc reshapes
//
//   Revision 1.17  2012/03/08 17:43:40  avossen
//   added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//   Revision 1.16  2012/03/07 03:57:22  avossen
//   various updates
//
//   Revision 1.15  2012/02/28 19:32:25  avossen
//   many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
//
//   Revision 1.14  2012/01/06 17:58:39  sgliske
//   Added requested GetCVS tag
//
//   Revision 1.13  2011/11/01 18:46:30  sgliske
//   Updated to correspond with StEvent containers, take 2.
//
//   Revision 1.12  2011/10/28 14:29:43  sgliske
//   fixed CVS tags
//
//   Revision 1.11  2011/10/28 14:28:41  sgliske
//   Cleaned up prepareEnvironment (no functional change).
//   Removed old methods of getting data pointer.
//   Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
//
//   Revision 1.10  2011/10/26 20:56:50  avossen
//   use geoIds to determine if two strips are adjacent
//
//   Revision 1.9  2011/10/20 17:30:37  balewski
//   revert
//
//   Revision 1.7  2011/10/17 21:42:02  balewski
//   added tmp interface to fgt-simu-maker
//
//   Revision 1.6  2011/10/10 20:35:08  avossen
//   fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//

#ifndef STAR_StFgtClusterMaker_HH
#define STAR_StFgtClusterMaker_HH

#include "StMaker.h"

class StFgtIClusterAlgo;
class StFgtDb;
/**
The cluster maker. It uses an external algorithm which has to implement StFgtIClusterAlgo to do the actual clustering by calling doClustering with the fgt hits in StEvent.

*/
class StFgtClusterMaker : public StMaker
{
  //omitted assignment operator and copy constructor on purpose
 public:
  StFgtClusterMaker( const Char_t* name="FgtCluster");
  ~StFgtClusterMaker();
  /// Init function. Checks if there is a cluster algo and initializes the same.
  virtual Int_t Init();
  Int_t InitRun(Int_t runumber);
  virtual Int_t Finish();



  /**
     The make function. Uses the cluster algo member to do the actual clustering. Then it does some post processing using the info on the absolute position of the disk that the algo does not have.
In addition the cluster error is computed for the orthogonal direction (e.g. for r clusters in phi) from the strip length.
  */
  Int_t Make();
  ///clear function is empty at the moment
  //  virtual void Clear( Option_t *opts = "" );

  /**sets the clustering algorithm. Currently there is the simple Clustering algorithm and the max cluster algorithm. 
The simple cluster algorithm is the default one. The max cluster only selects one hit stip per plane, the one with the highest charge
   */
  Int_t setClusterAlgo(StFgtIClusterAlgo*);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtClusterMaker.h,v 1.21 2014/08/06 11:43:09 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 protected:
  StFgtIClusterAlgo* mClusterAlgoPtr;
   // pointer to the DB
   StFgtDb* mDb;
  ClassDef(StFgtClusterMaker,1);

};
#endif
