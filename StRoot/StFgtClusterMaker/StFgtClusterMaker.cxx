//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.cxx,v 1.8 2011/10/18 01:18:50 avossen Exp $
//   $Log: StFgtClusterMaker.cxx,v $
//   Revision 1.8  2011/10/18 01:18:50  avossen
//   changed data access method to GetInputDS called from Make()
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
#include "StRoot/StEvent/StEvent.h"

void StFgtClusterMaker::Clear(Option_t *opts)
{

};

Int_t StFgtClusterMaker::PrepareEnvironment()
{
  StEvent* mEvent=0;
  mEvent=(StEvent*)GetInputDS("StEvent");
  mFgtEventPtr=NULL;
  if(mEvent)
    {
      mFgtEventPtr=mEvent->fgtEvent();
    }
  else
    {
      //in other makers we would construct a new event here, but this doesn't make sense for the cluster maker
      return kStErr;
    }
  return kStOK;
  };


Int_t StFgtClusterMaker::Make()
{
  Int_t ierr = kStOk;
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StClusterMaker::Make()******************************************************************"<<endm;
  if( !mIsInitialized || !pClusterAlgo|| !PrepareEnvironment())
    {
      LOG_ERROR << "cluster maker not initialized" << endm;
      if(!pClusterAlgo) 
	LOG_ERROR << "no cluster algo " << endm;
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
	      //	      cout <<"disc: " << discIdx << " has " << pDisc->getRawHitArray().getEntries() <<endl;
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
    
