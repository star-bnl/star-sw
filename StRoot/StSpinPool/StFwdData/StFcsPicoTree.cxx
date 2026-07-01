#include "StFcsPicoTree.h"

ClassImp(StFcsPicoTree);

StFcsPicoTree::StFcsPicoTree():TTree()
{
  InitBranches();
}

StFcsPicoTree::StFcsPicoTree(const char* name, const char* title, Int_t splitlevel):TTree(name,title,splitlevel)
{
  InitBranches();
}

StFcsPicoTree::~StFcsPicoTree()
{
  delete mHits;
  delete mClusters;
  delete mPoints;  
}

void StFcsPicoTree::InitBranches()
{
  mHits = new TClonesArray("StFcsPicoHit",1);
  mClusters = new TClonesArray("StFcsPicoCluster",1);
  mPoints = new TClonesArray("StFcsPicoPoint",1);
  Branch("PicoHits",&mHits);
  Branch("PicoClusters",&mClusters);
  Branch("PicoPoints",&mPoints);
}

void StFcsPicoTree::LoadFile(TFile* file, const char* treename)
{
  if( GetEntriesFast()>0 ){ return; }//if tree is not empty don't copy from other tree
  
  StFcsPicoTree* othertree = (StFcsPicoTree*) file->Get(treename);
  TBranch* brhits = othertree->GetBranch("PicoHits");
  brhits->SetAddress(&mHits);
  TBranch* brclus = othertree->GetBranch("PicoClusters");
  brclus->SetAddress(&mClusters);
  TBranch* brpoints = othertree->GetBranch("PicoPoints");
  brpoints->SetAddress(&mPoints);
  CopyEntries(othertree);
}

StFcsPicoHit* StFcsPicoTree::ConstructedHit(Int_t ihit)
{
  if( mHits ){ return static_cast<StFcsPicoHit*>(mHits->ConstructedAt(ihit)); }
  else{ return 0; }
}

StFcsPicoCluster* StFcsPicoTree::ConstructedCluster(Int_t iclus)
{
  if( mClusters ){ return static_cast<StFcsPicoCluster*>(mClusters->ConstructedAt(iclus)); }
  else{ return 0; }
}

StFcsPicoPoint* StFcsPicoTree::ConstructedPoint(Int_t ipoint)
{
  if( mPoints ){ return static_cast<StFcsPicoPoint*>(mPoints->ConstructedAt(ipoint)); }
  else{ return 0; }
}

void StFcsPicoTree::ClearAll()
{
  ClearHits();
  ClearClusters();
  ClearPoints();
}

void StFcsPicoTree::ClearHits()
{
  if( mHits ){ mHits->Clear(); }
}

void StFcsPicoTree::ClearClusters()
{
  if( mClusters ){ mClusters->Clear(); }
}

void StFcsPicoTree::ClearPoints()
{
  if( mPoints ){ mPoints->Clear(); }
}

void StFcsPicoTree::DeleteAll()
{
  DeleteHits();
  DeleteClusters();
  DeletePoints();
}

void StFcsPicoTree::DeleteHits()
{
  if( mHits ){ delete mHits; mHits = 0; }
}

void StFcsPicoTree::DeleteClusters()
{
  if( mClusters ){ delete mClusters; mClusters = 0; }
}

void StFcsPicoTree::DeletePoints()
{
  if( mPoints ){ delete mPoints; mPoints = 0; }
}

