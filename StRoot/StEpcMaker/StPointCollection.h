//
// $id$
//
// $Log: StPointCollection.h,v $
// Revision 1.1  2000/05/15 21:18:33  subhasis
// initial version
//
// Pi0 Candidate Finder Maker for EMC
//
//
// Author: Subhasis Chattopadhyay, Jan 2000
//

#ifndef STAR_StPointCollection
#define STAR_StPointCollection

#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"                                                     
#endif
#include "StEpcConstants.h"                                                     
#include "StEpcCut.h"                                                     

#include <vector>
#include <algorithm>  // min() max()
#include <utility>    // pair

#if !defined(ST_NO_NAMESPACES)
using std::vector;
using namespace units;
using std::min;
using std::max; 
#endif  

#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
typedef vector<int, allocator<int> > intVector;
#else
#define StVector(T) vector<T>
typedef vector<int> intVector;
#endif

#include "TObjArray.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_TableSorter.h"
#include "StPi0Candidate.h"
#include "St_emc_Maker/StEmcHitCollection.h"
#include "StPreEclMaker/StEmcPreClusterCollection.h"
#include "StPreEclMaker/StBemcPreClusterCollection.h"
#include "StPreEclMaker/StBsmdePreClusterCollection.h"
#include "StPreEclMaker/StBsmdpPreClusterCollection.h"
#include "StEpcMaker.h"

typedef StVector(StEmcPreCluster*) StMatchVec;
typedef StVector(StEmcPreCluster*)::iterator StMatchVecIter;

#include "tables/St_dst_track_Table.h"
class StPointCollection : public St_DataSet {

private:
 TObjArray mPoints;
 TObjArray mPointsReal;
 Int_t mNPoints;
 Int_t mNPointsReal;

protected:   

public: 

  StPointCollection();
  StPointCollection(const Char_t *);
  virtual ~StPointCollection();

  Int_t NPoints() const;
  const TObjArray* Points() const;
  Int_t NPointsReal() const;
  const TObjArray* PointsReal() const;


Int_t
  findPoints(StBemcPreClusterCollection*,
             StBsmdePreClusterCollection*,
             StBsmdpPreClusterCollection*,
             StEmcHitCollection*,
             StEmcHitCollection*,
             StEmcHitCollection*,
             St_dst_track *); 

virtual Int_t
  addPoints(Float_t*);

void 
  findCandidates(StBemcPreClusterCollection*,
                 StBsmdePreClusterCollection*,
                 StBsmdpPreClusterCollection*,
                 StEmcHitCollection*,
                 StEmcHitCollection*,
                 StEmcHitCollection*); 

virtual Int_t
  GetEmcPoint(const StMatchVec,
              const StMatchVec,
              const StMatchVec,
              const FloatVector,
              const FloatVector,
              const FloatVector,
              Int_t *);

virtual Int_t
  GlobSort(const St_dst_track *) const;
 
  ClassDef(StPointCollection,1)// Base class for electromagnetic calorimeter Point collection 
};
//
inline 
 Int_t StPointCollection::NPointsReal() const {return mNPointsReal;}
inline
 const TObjArray* StPointCollection::PointsReal() const {return &mPointsReal;}
inline
 Int_t StPointCollection::NPoints() const {return mNPoints;}
inline
const TObjArray* StPointCollection::Points() const  {return &mPoints;}

#endif





