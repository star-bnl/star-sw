//
// $id$
//
// $Log: StEmcPreClusterCollection.h,v $
// Revision 1.1  2000/05/15 21:24:00  subhasis
// initial version
//
// PreClusters Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay,
//         Aleksei Pavlinov , July 1999
//

#ifndef STAR_StEmcPreClusterCollection
#define STAR_StEmcPreClusterCollection

#include "TObjArray.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_TableSorter.h"
#include "StEmcPreCluster.h"
#include "St_emc_Maker/StEmcHitCollection.h"

class StEmcPreClusterCollection : public St_DataSet {
  friend class StBemcPreClusterCollection;
  friend class StBsmdePreClusterCollection;
  friend class StBsmdpPreClusterCollection;
private:
  Int_t     mDetector;
  Float_t   mEnergySeed;
  Float_t   mEnergyAdd;
  Float_t   mEnergyThresholdAll;
  Int_t     mSizeMax;
  Int_t     mNclusters;
  TObjArray mClusters;
protected:   
public: 

  StEmcPreClusterCollection();
  StEmcPreClusterCollection(const Char_t *);
  virtual ~StEmcPreClusterCollection();

  Int_t Detector() const;
  Float_t EnergySeed() const;
  Float_t EnergyAdd() const;
  Float_t EnergyThresholdAll() const;
  Int_t SizeMax() const;
  Int_t Nclusters() const;
  TObjArray* Clusters();

  void setDetector(Int_t);
  void setEnergySeed(Int_t);
  void setEnergyAdd(Int_t);
  void setEnergyThresholdAll(Int_t);
  void setSizeMax(Int_t);
  void setNclusters(Int_t);
  //  void setClusters(TObjArray&);
  void Browse(TBrowser *b);
  Int_t findClusters(StEmcHitCollection*); // Find clusters in all detectors

  virtual  void findClustersInModule(StEmcHitCollection*); // Find clusters in one module
  virtual Int_t testOnNeighbor(Int_t);
  virtual  void addPreCluster(StEmcHitCollection* , TArrayI*);
  virtual void printCluster(Int_t, StEmcPreCluster*);
  virtual void printClusters(Int_t n=5, Int_t start=5);
  virtual void printClustersAll();
  //  virtual Long_t HasData() const {return 1;}   // Non zero means it has data
  //  virtual Bool_t IsFolder() {return kFALSE;}   // KTRUE means it is directory
  ClassDef(StEmcPreClusterCollection,1)// Base class for electromagnetic calorimeter cluster collection 
};

inline StEmcPreClusterCollection::~StEmcPreClusterCollection() {/* Nobody */}
inline Int_t StEmcPreClusterCollection::Detector()   const  {return mDetector;}
inline Float_t StEmcPreClusterCollection::EnergySeed() const  {return mEnergySeed;}
inline Float_t StEmcPreClusterCollection::EnergyAdd()  const {return mEnergyAdd;}
inline Float_t StEmcPreClusterCollection::EnergyThresholdAll() const{return mEnergyThresholdAll;}
inline Int_t StEmcPreClusterCollection::Nclusters()   const {return mNclusters;}
inline Int_t StEmcPreClusterCollection::SizeMax() const {return mSizeMax;} 
inline TObjArray*  StEmcPreClusterCollection::Clusters() {return &mClusters;}

inline void StEmcPreClusterCollection::setDetector(Int_t var)  {mDetector = var;}
inline void StEmcPreClusterCollection::setEnergySeed(Int_t var){mEnergySeed = var;}
inline void StEmcPreClusterCollection::setEnergyAdd(Int_t var) {mEnergyAdd = var;}
inline void StEmcPreClusterCollection::setEnergyThresholdAll(Int_t var){mEnergyThresholdAll = var;}
inline void StEmcPreClusterCollection::setNclusters(Int_t var) {mNclusters = var;}
inline void StEmcPreClusterCollection::setSizeMax(Int_t var) {mSizeMax = var;}
//inline void StEmcPreClusterCollection::setClusters(TObjArray &var) {mClusters = var;}

#endif




