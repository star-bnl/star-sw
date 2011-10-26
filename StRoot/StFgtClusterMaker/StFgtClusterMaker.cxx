//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.cxx,v 1.11 2011/10/26 17:02:04 balewski Exp $
//   $Log: StFgtClusterMaker.cxx,v $
//   Revision 1.11  2011/10/26 17:02:04  balewski
//   get fgt event the proper way
//
//   Revision 1.10  2011/10/20 17:30:37  balewski
//   revert
//
//   Revision 1.7  2011/10/17 21:42:02  balewski
//   added tmp interface to fgt-simu-maker
//
//   Revision 1.6  2011/10/10 20:35:08  avossen
//   fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
//

#include "StFgtClusterMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StFgtRawMaker/StFgtCosmicMaker.h"
#include "StRoot/StFgtRawMaker/StFgtRawMaker.h"
#include "StFgtSimulator/StFgtSlowSimuMaker.h"
#include "../StEvent/StEvent.h"

void StFgtClusterMaker::Clear(Option_t *opts)
{

};
Int_t StFgtClusterMaker::Make()
{
  Int_t ierr = kStOk;
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StClusterMaker::Make()******************************************************************"<<endm;

  //  Access of FGT from  StEvent 
  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  assert(mEvent); // fix your chain

  mFgtEventPtr=mEvent->fgtEvent(); 
  assert(mFgtEventPtr);
  printf("StFgtClusterMaker::Make()-> mIsInitialized=%d,  pClusterAlgo=%p\n",mIsInitialized , pClusterAlgo);

  if( !mIsInitialized || !pClusterAlgo)
    {
      LOG_ERROR << "cluster maker not initialized" << endm;
      if(!pClusterAlgo) 
	LOG_ERROR << "no cluster maker " << endm;
      return kStFatal;
    }
  else
    {
      //invoke algo for each disc
      for(int discIdx=0;discIdx<mFgtEventPtr->getNumDiscs();discIdx++)
	{
	  StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr(discIdx);
	  printf("iD=%d %p\n",discIdx,pDisc);
	  if(pDisc)
	    {
	      cout <<"disc: " << discIdx << " has " << pDisc->getRawHitArray().getEntries() <<endl;
	      Int_t loc_ierr=pClusterAlgo->doClustering(pDisc->getRawHitArray(),pDisc->getClusterArray());
	      if(loc_ierr!=kStOk)
		{
		  LOG_WARN <<"StClusterMaker::Make(): clustering for disc " << discIdx << " returned " << loc_ierr <<endm;
		  if(loc_ierr>ierr)
		    ierr=loc_ierr;
		}
	    }
	}
    }

  return ierr;

};


Int_t StFgtClusterMaker::setClusterAlgo(StFgtIClusterAlgo* algo)
{
  pClusterAlgo=algo;
  return kStOk;
}

Int_t StFgtClusterMaker::Init()
{
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;

#if 0 // this method of data access wil not work in BFC, disabled, Jan
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
	//	cout <<" have cosmic  maker "<< endl;
      } 
      else if ( dataMaker->InheritsFrom( "StFgtRawMaker" ) ){
      StFgtRawMaker* maker = static_cast< StFgtRawMaker* >( dataMaker );
      mFgtEventPtr = maker->getFgtEventPtr();
      //      cout <<" have raw  maker "<< endl;
      }
     
    }
    

  if( !mFgtEventPtr ){
    LOG_FATAL << "::Init() could not get pointer to StFgtEvent" << endm;
    ierr = kStFatal;

  }
#endif


  mIsInitialized=true;
  return ierr;
};
  
  
StFgtClusterMaker::StFgtClusterMaker(const Char_t* rawBaseMakerName, const Char_t* name) : StMaker("fgt"),mFgtEventPtr(0),mFgtEventMakerName( rawBaseMakerName ),mIsInitialized(0),pClusterAlgo(0)
{
  SetName(name);
};

StFgtClusterMaker::~StFgtClusterMaker()
{
	
};

    
ClassImp(StFgtClusterMaker);
    
