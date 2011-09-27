//
//  $Id
//  $Log
//
// \class StFgtSimpleClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//

#include "StFgtSimpleClusterAlgo.h"


StFgtSimpleClusterAlgo():mInitialized(false)
{
  //nothing else to do....
};

Int_t StFgtSimpleClusterAlgo::Init(StFgtEvent* mEvent)
{
  mInitialized=true;

};

Int_t StFgtSimpleClusterAlgo::doClustering()
{

};


ClassImp(StFgtSimpleClusterAlgo);
