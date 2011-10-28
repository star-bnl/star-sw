//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.cxx,v 1.15 2011/10/28 14:29:43 sgliske Exp $
//   $Log: StFgtClusterMaker.cxx,v $
//   Revision 1.15  2011/10/28 14:29:43  sgliske
//   fixed CVS tags
//
//   Revision 1.14  2011/10/28 14:28:26  sgliske
//   Cleaned up prepareEnvironment (no functional change).
//   Removed old methods of getting data pointer.
//   Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
//
//   Revision 1.13  2011/10/26 20:56:50  avossen
//   use geoIds to determine if two strips are adjacent
//
//   Revision 1.12  2011/10/26 19:32:31  balewski
//   now fgt-geom is owned by fgtDb-maker
//
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
//#include "StRoot/StFgtRawMaker/StFgtCosmicMaker.h"
//#include "StRoot/StFgtRawMaker/StFgtRawMaker.h"
//
#include "../StEvent/StEvent.h"

void StFgtClusterMaker::Clear(Option_t *opts)
{

};





Int_t StFgtClusterMaker::prepareEnvironment()
{
  Int_t ierr = kStOk;

  //StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  //assert(mEvent); // fix your chain
  StEvent* mEvent=0;
  mEvent=(StEvent*)GetInputDS("StEvent");

  if( !mEvent ){
     LOG_ERROR << "Pointer to StEvent is null" << endl;
     ierr = kStErr;
  };

  mFgtEventPtr=NULL;
  if(mEvent)
    {
      mFgtEventPtr=mEvent->fgtEvent();
    };

  // Note: only the makers StFgtRawMaker, StFgtCosmicMaker and
  // StFgtSlowSimuMaker make the StFgtEvent.  If there is not an
  // StFgtEvent in StEvent, then throw an error.

  if( !mFgtEventPtr)
    {
       LOG_ERROR << "Pointer to StFgtEvent is null when sought in '" << ClassName() << "::prepareEnviroment'" << endm;
      ierr = kStErr;
    }

  return ierr;
};



Int_t StFgtClusterMaker::Make()
{
  Int_t ierr = kStOk;
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StClusterMaker::Make()******************************************************************"<<endm;

  //  Access of FGT from  StEvent 
  //  StEvent* mEvent = (StEvent*)GetInputDS("StEvent");
  //  assert(mEvent); // fix your chain


  if( !mClusterAlgoPtr || (prepareEnvironment()!=kStOK))
    {
      if(!mClusterAlgoPtr) 
	LOG_ERROR << "no cluster maker " << endm;
      return kStFatal;
    }
  else
    {
      //invoke algo for each disc
      for(int discIdx=0;discIdx<mFgtEventPtr->getNumDiscs();discIdx++)
	{ 
	  StFgtDisc* pDisc=mFgtEventPtr->getDiscPtr(discIdx);
	  //printf("iD=%d %p\n",discIdx,pDisc);
	  if(pDisc)
	    { 
	      cout <<"disc: " << discIdx << " has " << pDisc->getRawHitArray().getEntries() <<endl;
	      Int_t loc_ierr=mClusterAlgoPtr->doClustering(pDisc->getRawHitArray(),pDisc->getClusterArray());
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
  mClusterAlgoPtr=algo;
  return kStOk;
}

Int_t StFgtClusterMaker::Init()
{
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;

  if( !mClusterAlgoPtr ){
     LOG_ERROR << "No fgt cluster algorithm specified" << endm;
     ierr = kStErr;
  };

  return ierr;
};
  
  
StFgtClusterMaker::StFgtClusterMaker( const Char_t* name ) : StMaker(name),mFgtEventPtr(0),mClusterAlgoPtr(0)
{
   /* */
};

StFgtClusterMaker::~StFgtClusterMaker()
{
	
};

    
ClassImp(StFgtClusterMaker);
    
