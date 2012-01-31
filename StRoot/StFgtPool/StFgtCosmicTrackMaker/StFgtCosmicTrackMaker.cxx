/***************************************************************************
 *
 * $Id: StFgtCosmicTrackMaker.cxx,v 1.1 2012/01/31 23:25:35 avossen Exp $
 * Author: C. K. Riley (ckriley@bnl.gov), Oct. 10 2011 
 *
 ***************************************************************************
 *
 * Description: CosmicTrackMaker for the cosmic stand
 *
 ***************************************************************************
 *
 * $Log: StFgtCosmicTrackMaker.cxx,v $
 * Revision 1.1  2012/01/31 23:25:35  avossen
 * moved StFgtCosmicTrackMaker to StFgtPool
 *
 * Revision 1.12  2011/11/25 20:24:59  ckriley
 * now will look at all possible point combinations for tracks and pick the best one
 *
 * Revision 1.11  2011/11/16 22:15:07  ckriley
 * now looks at all points on quadrants and gets best track
 *
 * Revision 1.10  2011/11/09 20:56:58  ckriley
 * working version for new containers
 *
 * Revision 1.9  2011/11/01 18:50:13  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.8  2011/10/28 18:30:09  ckriley
 * make things up-to-date
 *
 * Revision 1.7  2011/10/28 14:54:11  sgliske
 * Changed to get StFgtEvent from StEvent rather than another maker.
 * Added some sanity checks to ensure the algo has been set.
 * Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
 *
 * Revision 1.5  2011/10/20 17:13:44  ckriley
 * major update -> headers, tracks stored in StFgtEvent instead of StFgtDisc, changes to trackmaker and algorithms
 *
 *
 **************************************************************************/

#include "StFgtCosmicTrackMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtPoint.h"

#include "StFgtICosmicTrackAlgo.h"

// constructors
StFgtCosmicTrackMaker::StFgtCosmicTrackMaker(const Char_t* name ):
  StMaker( name ), eventCounter(0) {
   /* */
};

// deconstructor
StFgtCosmicTrackMaker::~StFgtCosmicTrackMaker(){
    // check if pointers were created, then make sure to delete them
};

// set algorithm
Int_t StFgtCosmicTrackMaker::setCosmicTrackAlgo(StFgtICosmicTrackAlgo* algo) {
  mCosmicTrackAlgoPtr=algo;
  return kStOk;
}

// initialize pointers to data
Int_t StFgtCosmicTrackMaker::Init(){
  Int_t ierr = kStOk;

  if( !mCosmicTrackAlgoPtr ){
     LOG_ERROR << "No algorithm yet set for StFgtCosmicTrackMaker" << endm;
     ierr = kStErr;
  };

  if( !ierr )
     mCosmicTrackAlgoPtr->Init();

  return ierr;
};

// extract points, calculate ODR line and calculate chi^2 for an event
Int_t StFgtCosmicTrackMaker::Make(){
  Int_t ierr = kStOk;

  LOG_DEBUG <<"StFgtCosmicTrackMaker::Make()******************************************************************"<<endm;

  if( !mCosmicTrackAlgoPtr ){
     LOG_ERROR << "No algorithm set for StFgtCosmicTrackMaker" << endm;
     ierr = kStErr;
  };

  StEvent* eventPtr = 0;
  eventPtr = (StEvent*)GetInputDS("StEvent");

  if( !eventPtr ) {
     LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
     ierr = kStErr;
  };

  StFgtCollection* fgtCollectionPtr = 0;

  if( eventPtr ) {
     fgtCollectionPtr=eventPtr->fgtCollection();
  };

  if( !fgtCollectionPtr) {
     LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
     ierr = kStErr;
  };

  StFgtPointCollection *pointCollectionPtr = 0;

  if( fgtCollectionPtr )
     pointCollectionPtr = fgtCollectionPtr->getPointCollection();

  if( !pointCollectionPtr && !ierr ){
     LOG_ERROR << "Error getting pointer to StFgtPointCollection from StFgtCollection" << endm;
     ierr = kStErr;
  };

  if( !ierr ){
     // heart of cosmic trackmaker
     //Int_t filledQuads = 0;

     StSPtrVecFgtPoint &pointVec = pointCollectionPtr->getPointVec();
     StSPtrVecFgtPointIterator iter;

/*   with simple cluster algo, you would expect there to be more than 3pts
     if( pointVec.size() > 3 ) {
        LOG_WARN << "too many points! " << endl;
        ierr=kStWarn;
     }
*/
     // check to see if at least one point on each disc
     Bool_t haveDisc[3];
     Int_t count[3];
     for(Int_t j=0; j<3; j++) {
       haveDisc[j]=false;
       count[j]=0;
     }
     for( iter = pointVec.begin(); iter != pointVec.end(); ++iter ){
        StFgtPoint *hit = *iter;
        Int_t discId = hit->getDisc();
        if(discId<3) {haveDisc[discId]=true; count[discId]++;}
     }
     if(!haveDisc[0] || !haveDisc[1] || !haveDisc[2]) return ierr;

/*   ONLY 3 QUADS OF 6 HAVE POINTS, SO NOT ALL WILL HAVE POINTS
     if( filledQuads != 0x07 ){
        LOG_ERROR << "not every quadrant has one point." << endl;
        ierr = kStErr;
     }
*/
     if( ierr < kStErr )
        mCosmicTrackAlgoPtr->makeCosmicTracks(*pointCollectionPtr, mTrackVec, eventCounter); 
  };

  ++eventCounter;

  return ierr;
};

// finish
Int_t StFgtCosmicTrackMaker::Finish(){
  cout << "StFgtCosmicTrackMaker::Finish()" << endl;
  return kStOk;
};

// clear function
void StFgtCosmicTrackMaker::Clear( Option_t *opt ){
   mTrackVec.clear();
};


ClassImp(StFgtCosmicTrackMaker);

