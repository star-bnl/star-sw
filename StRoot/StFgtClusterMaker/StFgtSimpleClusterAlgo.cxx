//
//  $Id
//  $Log
//
// \class StFgtSimpleClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtSimpleClusterAlgo.h"


StFgtSimpleClusterAlgo::StFgtSimpleClusterAlgo():mIsInitialized(0)
{
  //nothing else to do....
};

Int_t StFgtSimpleClusterAlgo::Init(StFgtEvent* mEvent)
{
  mIsInitialized=true;
};

Int_t StFgtSimpleClusterAlgo::doClustering(const StFgtRawHitArray& hits, StFgtClusterArray& clusters)
{





};


ClassImp(StFgtSimpleClusterAlgo);
