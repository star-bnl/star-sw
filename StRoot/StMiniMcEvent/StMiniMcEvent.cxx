
#include "StMiniMcEvent.h"
#include <iostream>

Int_t StMiniMcEvent::mSFirst=1; 
ClassImp(StMiniMcEvent)

//___________________

StMiniMcEvent::StMiniMcEvent()
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

  if(mSFirst){ 
    cout << "\tCreating the clones arrays" << endl;
    
    mMcTracks      = new TClonesArray("StTinyMcTrack",nMcTrack);
    mMatchedPairs = new TClonesArray("StMiniMcPair",nMatchedPair);
    mMergedPairs  = new TClonesArray("StMiniMcPair",nMergedPair);
    mSplitPairs   = new TClonesArray("StMiniMcPair",nSplitPair);

    mGhostPairs  = new TClonesArray("StMiniMcPair",nGhostPair);
    mContamPairs = new TClonesArray("StContamPair",nContamPair); 

  }

  mSFirst = 0; // flag so we dont create any more arrays;

  mNMcTrack=mNMatchedPair=mNMergedPair=mNSplitPair=mNGhostPair=mNContamPair=0;

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

  mNMcTrack = mNMatchedPair = mNMergedPair 
    = mNSplitPair = mNGhostPair = mNContamPair = 0;
}

//__________________

void
StMiniMcEvent::setCentrality(Int_t nTrack)
{//               0   1   2   3   4   5   6   7  8 
  Int_t cent[] = {20,100,180,270,360,460,560,660,870};
  if (nTrack < cent[0])       { mCentrality = 0; }
  else if (nTrack < cent[1])  { mCentrality = 1; }
  else if (nTrack < cent[2])  { mCentrality = 2; }
  else if (nTrack < cent[3])  { mCentrality = 3; }
  else if (nTrack < cent[4])  { mCentrality = 4; }
  else if (nTrack < cent[5])  { mCentrality = 5; }
  else if (nTrack < cent[6])  { mCentrality = 6; }
  else if (nTrack < cent[7])  { mCentrality = 7; }
  else if (nTrack < cent[8])  { mCentrality = 8; }
  else                        { mCentrality = 9; }  
}


  
