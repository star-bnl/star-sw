//
// StHeavyTagMaker class for Spectra Reconstruction Tags                  //
//

#include "StHeavyTagMaker.h"
#include "tables/St_HeavyTag_Table.h"

#include "StChain.h"
#include "StEvent.h"
#include "StMessMgr.h"
#include "StEventUtilities/StuRefMult.hh"

// prototypes for helper functions
float largestPairMass(StEvent*);
int countTrackTowerMatches(StEvent*);

ClassImp(StHeavyTagMaker)

StHeavyTagMaker::StHeavyTagMaker(const char *name,const char* title)
    : StMaker(name,title),
      mTagTable(0),
      mEvent(0)
{
    //  StHeavyTagMaker constructor
    // mass of Cquark (from PDG:  1 < m_c < 1.4 GeV) 
    // mass of Bquark (from PDG:  4 < m_b < 4.5 GeV)
    // 3d threshold, set to 7 GeV so each threshold is in steps of ~3 GeV
    mMassThres[0] = 1.2;
    mMassThres[1] = 4.25;
    mMassThres[3] = 7.0;
}

StHeavyTagMaker::~StHeavyTagMaker(){
  // StHeavyTagMaker destructor
}

Int_t StHeavyTagMaker::Init(){
 //  
    return StMaker::Init();
}

Int_t StHeavyTagMaker::Make(){


 //  Make - this method is called in loop for each event


  // Create a data set and add the table to it.
  mTagTable = new HeavyTag_st;
  mEvent = (StEvent*) GetInputDS("StEvent");
  if (!mEvent) return kStOk; // if no event, there is nothing to do.

  // fill tag here
  fillTag();
  
  // after tag is filled, add it to the data area of the maker
  St_HeavyTag* heavyTag = new St_HeavyTag("HeavyTag",1);
  AddData(heavyTag);
  heavyTag->AddAt(mTagTable,0); // AddAt takes a HeavyTag_st*
  
  delete mTagTable;
  mTagTable=0;
  // heavyTag is not deleted, control is passed to Maker by adding it to its
  // dataset branch.
  return kStOk;
}
void StHeavyTagMaker::fillTag() {

    // Count the tracks with p>thres for 3 different
    // thresholds

    // Initialize the track counters
    int tracksGtMass1 = 0;
    int tracksGtMass2 = 0;
    int tracksGtMass3 = 0;
       
    // now count up the number of interesting tracks
    // We just check for the momentum above the c and b quark masses
    // using some loose, but sensible cuts:
    // flag>0
    // tpc fit points>15
    // |eta|<1.5
    const_StPrimaryTrackIterator itr;
    StPrimaryTrack *track;
    float momentum;
    if (mEvent->primaryVertex()){
	const StSPtrVecPrimaryTrack& tracks = mEvent->primaryVertex()->daughters();
	for (itr=tracks.begin();itr != tracks.end(); itr++){
	    track = *itr;
	    if (!track) continue;
	    if (track->flag()<=0)  continue;
	    if (track->fitTraits().numberOfFitPoints(kTpcId)<15) continue;
	    StThreeVectorF mom = track->geometry()->momentum();
	    if (fabs(mom.pseudoRapidity())>1.5) continue;
	    momentum = mom.mag();
	    // check for momentum above mass thresholds
	    if (momentum<mMassThres[0]) continue;        // step out if p<thres1
	    ++tracksGtMass1;                             // increase count for thres1
	    if (momentum<mMassThres[1]) continue;        // step out if p<thres2
	    ++tracksGtMass2;                             // increase count for thres2
	    if (momentum>mMassThres[2]) ++tracksGtMass3; // count if p>thres3
	}  // loop over tracks
    } // check for primary vertex

    
    
    mTagTable->mNumberOfTracksAboveThres[0] = tracksGtMass1;
    mTagTable->mNumberOfTracksAboveThres[1] = tracksGtMass2;
    mTagTable->mNumberOfTracksAboveThres[2] = tracksGtMass3;
    mTagTable->mNumberOfTrackToTowerMatches = countTrackTowerMatches(mEvent);
    mTagTable->mLargestPairMass = largestPairMass(mEvent);

    return;
}

HeavyTag_st* StHeavyTagMaker::tag()
{
    return mTagTable;
}






