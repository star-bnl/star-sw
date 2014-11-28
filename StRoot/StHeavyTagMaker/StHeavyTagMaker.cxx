/* $Id: StHeavyTagMaker.cxx,v 1.4 2007/04/28 17:56:16 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, July 2004
 ***************************************************************************
 *
 * Description:   Maker to fill the Heavy Flavor Tags
 * 
 * StHeavyTagMaker class for Heavy Flavor Reconstruction Tags
 * The tags that are currently filled are:
 * 1) The number of tracks above 3 different mass thresholds
 * -m_charm  = 1.25 GeV/c^2
 * -m_beauty = 4.2  GeV/c^2
 *             7    GeV/c^2
 * 2) The number of tracks above p>2 GeV/c that point to a tower
 *    with ADC-30>360, which should roughly correspond to towers above
 *    Et=3 GeV.  The mean pedestal is 30 adc counts, and 360 roughly
 *    corresponds to the 3 GeV, based on a sample of 10 events in
 *    the AuAu62 run, which gives 0.0083 GeV/adc
 *
 * 3) The largest pair invariant mass in the event, assuming the electron mass.
 *
 ***************************************************************************
 *
 * $Log: StHeavyTagMaker.cxx,v $
 * Revision 1.4  2007/04/28 17:56:16  perev
 * Redundant StChain.h removed
 *
 * Revision 1.3  2004/08/26 03:24:10  jeromel
 * Fixed wrong indexing
 *
 * Revision 1.2  2004/07/29 23:06:11  calderon
 * Changed adc cut to match towers to 360 ADC counts,
 * and documented the origin.
 * Added Description to cxx file.
 * Removed unnecessary static_cast for StDetectorId
 *
 **************************************************************************/

#include "StHeavyTagMaker.h"
#include "tables/St_HeavyTag_Table.h"

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
    mMassThres[2] = 7.0;
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

    if (Debug()>0) {
	cout << "Heavy Flavor Tags for Run " << mEvent->runId() << " Event: " << mEvent->id() << endl;
	cout << "Tracks above m_c  : " << mTagTable->mNumberOfTracksAboveThres[0] << endl;
	cout << "Tracks above m_b  : " << mTagTable->mNumberOfTracksAboveThres[1] << endl;
	cout << "Tracks above 7GeV : " << mTagTable->mNumberOfTracksAboveThres[2] << endl;
	cout << "Track-Tower Matches " << mTagTable->mNumberOfTrackToTowerMatches << endl;
	cout << "Largest Inv. Mass : " << mTagTable->mLargestPairMass  << endl;

    }
    return;
}

HeavyTag_st* StHeavyTagMaker::tag()
{
    return mTagTable;
}






