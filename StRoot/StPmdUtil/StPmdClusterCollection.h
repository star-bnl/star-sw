/*!
 * \class StPmdClusterCollection
 * \author  Subhasis Chattopadhaya
 */
/*********************************************************
 *
 * $Id: StPmdClusterCollection.h,v 1.1 2002/08/27 12:19:30 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhaya
 **********************************************************
 *
 * Description: Base class for PMD cluster collection
 *
 * $Log: StPmdClusterCollection.h,v $
 * Revision 1.1  2002/08/27 12:19:30  subhasis
 * First version
 *
 *
 *********************************************************/
#ifndef STAR_StPmdClusterCollection
#define STAR_StPmdClusterCollection

#include <TObjArray.h>
#include <TArrayI.h>
#include <TArrayF.h>
#include <TMatrix.h>
#include "StPmdDetector.h"
#include "StPmdCluster.h"


class StPmdClusterCollection : public StObject {
private:

  Int_t           mNclusters;
  TObjArray       mClusters;
 
protected:   
public: 

                  StPmdClusterCollection();  
  virtual         ~StPmdClusterCollection();

  Int_t           Nclusters() const;    //! no. of clusters
  TObjArray*      Clusters();

  void            setNclusters(Int_t);  

  virtual  void   DeleteCluster();


  virtual  void   addCluster(StPmdCluster* cluster);
  
  
  ClassDef(StPmdClusterCollection,1)// Base class for PMD cluster collection 
};

inline Int_t    StPmdClusterCollection::Nclusters() const {return mNclusters;}
inline TObjArray* StPmdClusterCollection::Clusters() {return &mClusters;}
inline void StPmdClusterCollection::setNclusters(Int_t var) {mNclusters = var;}

#endif




