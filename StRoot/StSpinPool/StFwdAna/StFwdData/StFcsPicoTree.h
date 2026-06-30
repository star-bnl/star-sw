/*
  The purpose of this class is to represent our own *TTree* that will contain more basic "pico" level information about FCS data from the MuDSTs

  @[June 13, 2023](David Kapukchyan) > Removed dummy classes that hold data to their own separate header
  
  @[April 3, 2023](David Kapukchyan) > First instance
 */

#ifndef StFcsPicoTree_H
#define StFcsPicoTree_H

//ROOT headers
#include "TObject.h"
#include "TFile.h"
#include "TTree.h"
#include "TClonesArray.h"

#include "StFcsPico.h"

class StFcsPicoTree : public TTree
{
  
 public:
  StFcsPicoTree();
  StFcsPicoTree(const char* name, const char* title, Int_t splitlevel=99);
  StFcsPicoTree(const char* name, TFile* file);
  virtual ~StFcsPicoTree();
  //StFcsPicoTree(const StFcsPicoTree& tm) = delete;
  //StFcsPicoTree& operator=(const StFcsPicoTree& tm) = delete;

  void LoadFile(TFile* file, const char* treename);    //!< Grab the TClonesArray objects from a differently named StFcsPicoTree and load them into *this* tree  

  TClonesArray* GetHits()    const { return mHits; }
  TClonesArray* GetClusters()const { return mClusters; }
  TClonesArray* GetPoints()  const { return mPoints; }

  StFcsPicoHit* ConstructedHit(Int_t ihit);            //!< If ihit exists in #mHits then return it otherwise create one, i.e. calls mHits->ConstructedAt(ihit);
  StFcsPicoCluster* ConstructedCluster(Int_t iclus);   //!< If iclus exists in #mClusters then return it otherwise create one, i.e. calls mHits->ConstructedAt(ihit);
  StFcsPicoPoint* ConstructedPoint(Int_t ipoint);      //!< If ipoint exists in #mPoints then return it otherwise create one, i.e. calls mHits->ConstructedAt(ihit);

  void ClearAll();        //!< Clear #mHits, #mClusters, and #mPoints
  void ClearHits();       //!< Clear #mHits
  void ClearClusters();   //!< Clear #mClusters
  void ClearPoints();     //!< Clear #mPoints

  void DeleteAll();       //!< Check and delete all TClonesArrays
  void DeleteHits();      //!< Check and delete #mHits
  void DeleteClusters();  //!< Check and delete #mClusters
  void DeletePoints();    //!< Check and delete #mPoints
  
    
 protected:
  TClonesArray* mHits = 0;         //!< Array of #StFcsPicoHit
  TClonesArray* mClusters = 0;     //!< Array of #StFcsPicoCluster
  TClonesArray* mPoints = 0;       //!< Array of #StFcsPicoPoint

  void InitBranches();             //!< Set up the branch objects for *this* tree
  
  ClassDef(StFcsPicoTree,1);
};

#endif
