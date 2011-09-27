//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id
//   $Log
//
//

#include "StFgtClusterMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StFgtRawMaker/StFgtCosmicMaker.h"
#include "StRoot/StFgtRawMaker/StFgtRawMaker.h"

void StFgtClusterMaker::Clear(Option_t *opts)
{

}
Int_t StFgtClusterMaker::Make()
{
  Int_t ierr = kStOk;
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;

  if( !mIsInitialized )
    {
      LOG_ERROR << "Not initialized" << endm;
      return kStFatal;
    }
  else
    {
      //invoke algo for each disc
      for(int discIdx=0;discIdx<mFgtEventPtr->getNumDiscs();discIdx++)
	{
	  StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr(discIdx);
	  if(pDisc)
	    {
	      pClusterAlgo->doClustering(pDisc->getRawHitArray(),pDisc->getClusterArray());
	    }
	}
    }

  return ierr;

};


Int_t StFgtClusterMaker::Init()
{
  Int_t ierr = kStOk;
  TObject *dataMaker = GetMaker( mFgtEventMakerName.data());
  if( !dataMaker ){
    LOG_FATAL << "::Init() could not get pointer to a maker with name '" << mFgtEventMakerName << "'" << endm;
    ierr = kStFatal;
  };
  if( !ierr )
    {
      if( dataMaker->InheritsFrom( "StFgtCosmicMaker" ) ){
	StFgtCosmicMaker* maker = static_cast< StFgtCosmicMaker* >( dataMaker );
	mFgtEventPtr = maker->getFgtEventPtr();
      } 
      else if ( dataMaker->InheritsFrom( "StFgtRawMaker" ) ){
      StFgtRawMaker* maker = static_cast< StFgtRawMaker* >( dataMaker );
      mFgtEventPtr = maker->getFgtEventPtr();
      }
    }
    
  if( !mFgtEventPtr ){
    LOG_FATAL << "::Init() could not get pointer to StFgtEvent" << endm;
    ierr = kStFatal;
  }
    
    
  return ierr;
};
  
  
StFgtClusterMaker::StFgtClusterMaker(const Char_t* rawBaseMakerName, const Char_t* name) : StMaker("fgt"),mFgtEventPtr(0),mFgtEventMakerName( rawBaseMakerName ),mIsInitialized(0)
{
  SetName(name);
};

StFgtClusterMaker::~StFgtClusterMaker()
{
	
};

    
ClassImp(StFgtClusterMaker);
    
