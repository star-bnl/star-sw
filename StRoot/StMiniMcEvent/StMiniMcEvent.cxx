//
// $Id: StMiniMcEvent.cxx,v 1.5 2004/01/26 13:58:18 calderon Exp $
//

#include "StMiniMcEvent.h"
#include "StMiniMcPair.h"
#include "StContamPair.h"

#include "Stiostream.h"

Int_t StMiniMcEvent::mSFirst=1; 
ClassImp(StMiniMcEvent)

//___________________

StMiniMcEvent::StMiniMcEvent() :
    mEventId(0),
    mRunId(0),
    mOriginMult(0),
    mCentralMult(0),
    mCentrality(0),
    mNUncorrectedNegativePrimaries(0),
    mNUncorrectedPrimaries(0),
    mNFtpcWUncorrectedPrimaries(0),
    mNFtpcEUncorrectedPrimaries(0),
    mMcMult(0),
    mNMcNch(0),      
    mNMcFtpcWNch(0),
    mNMcFtpcENch(0),
    mNMcHminus(0),
    mNMcGlobal(0),
    mNMcGoodGlobal20(0),
    mNRcGlobal(0),
    mNRcGoodGlobal20(0),
    mVertexX(999),
    mVertexY(999),
    mVertexZ(999),
    mMcVertexX(999),
    mMcVertexY(999),
    mMcVertexZ(999),
    mMagField(0),
    mCenterOfMassEnergy(0),
    mBackgroundRate(0),
    mBeamMassNumberEast(0),
    mBeamMassNumberWest(0),
    mCtb(0),
    mZdcE(0),
    mZdcW(0),
    mNMcTrack(0),
    mNMatchedPair(0),
    mNMergedPair(0),
    mNSplitPair(0),
    mNGhostPair(0),
    mNContamPair(0),
    mNMatGlobPair(0)
{
  cout << "###StMiniMcEvent::StMiniMcEvent()" << endl;
  //
  // TClonesArray only created on the heap
  // the first time a StMiniMcEvent object is constructed
  //
  
  //
  // initialize the size of the track array
  //
  const Int_t nMcTrack      = 6000;
  const Int_t nMatchedPair = 6000;
  const Int_t nMergedPair  = 500;
  const Int_t nSplitPair   = 500;
  const Int_t nGhostPair   = 500;
  const Int_t nContamPair  = 500;
  const Int_t nMatGlobPair = 6000;
  if(mSFirst){ 
    cout << "\tCreating the clones arrays" << endl;
    
    mMcTracks      = new TClonesArray("StTinyMcTrack",nMcTrack);
    mMatchedPairs = new TClonesArray("StMiniMcPair",nMatchedPair);
    mMergedPairs  = new TClonesArray("StMiniMcPair",nMergedPair);
    mSplitPairs   = new TClonesArray("StMiniMcPair",nSplitPair);

    mGhostPairs  = new TClonesArray("StMiniMcPair",nGhostPair);
    mContamPairs = new TClonesArray("StContamPair",nContamPair); 
    mMatGlobPairs = new TClonesArray("StMiniMcPair",nMatGlobPair);

  }

  mSFirst = 0; // flag so we dont create any more arrays;

}

//__________________

StMiniMcEvent::~StMiniMcEvent()
{
}

//_________________

void 
StMiniMcEvent::addMcTrack(StTinyMcTrack* trk)
{
  TClonesArray &tracks = *mMcTracks;
  new(tracks[mNMcTrack++]) StTinyMcTrack(*trk);
}

//________________

void 
StMiniMcEvent::addTrackPair(StMiniMcPair* pair,Category category)
{
  TClonesArray *tracks;
  Int_t *nPair;

  // special case for contamination pairs (make it a new function?)
  // downcast..
  //
  StContamPair* contamPair = 0;
  switch(category){

  case MATCHED:
    tracks = mMatchedPairs; nPair = &mNMatchedPair;
    break;
  case MERGED:
    tracks = mMergedPairs;  nPair = &mNMergedPair;
    break;
  case SPLIT:
    tracks = mSplitPairs;   nPair = &mNSplitPair;
    break;
  case GHOST:
    tracks = mGhostPairs;  nPair = &mNGhostPair;
    break;  
  case CONTAM:
    tracks = mContamPairs; nPair = &mNContamPair;
    contamPair = dynamic_cast<StContamPair*>(pair);
    break;
  case MATGLOB:
    tracks = mMatGlobPairs; nPair = &mNMatGlobPair;
    break;
  default:
    cout << "****ERROR!****" << endl 
	 << "WRONG CATEGORY " 
	 << " IN StMiniMcEvent::addPair()" << endl;
    std::exit(-1);
  }

  if(contamPair){
    new((*tracks)[(*nPair)++]) StContamPair(*contamPair); 
  }
  else{
    new((*tracks)[(*nPair)++]) StMiniMcPair(*pair);
  }
}


TClonesArray* 
StMiniMcEvent::tracks(Category category)
{
  switch(category){
  case MC:
    return mMcTracks; 
  case MATCHED:
    return mMatchedPairs;
  case MERGED:
    return mMergedPairs;
  case SPLIT:
    return mSplitPairs;
  case GHOST:
    return mGhostPairs;
  case CONTAM:
    return mContamPairs;
  case MATGLOB:
    return mMatGlobPairs;
  default:
    cout << "****WRONG CATEGORY****"<< endl
	 << "StMiniMcEvent::tracks()" << endl;
    return 0;
  }

}

//_________________
//
// clear all the track arrays for the next event
//

void
StMiniMcEvent::Clear(Option_t *option)
{
  mMcTracks->Clear(); 
  mMatchedPairs->Clear();
  mMergedPairs->Clear();
  mSplitPairs->Clear();
  mGhostPairs->Clear();
  mContamPairs->Clear();
  mMatGlobPairs->Clear();
  
  mNMcTrack = mNMatchedPair = mNMergedPair 
    = mNSplitPair = mNGhostPair = mNContamPair = mNMatGlobPair = 0;
  
}

//__________________
//
// $Log $
//

  
