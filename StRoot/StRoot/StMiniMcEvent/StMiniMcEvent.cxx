//
// $Id: StMiniMcEvent.cxx,v 1.9 2012/03/15 23:37:20 perev Exp $
//

#include "StMiniMcEvent.h"
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
    mNUncorrectedGlobals(0),
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
  const Int_t nMcTrack     = 6000;
  const Int_t nMatchedPair = 6000;
  const Int_t nMergedPair  = 500;
  const Int_t nSplitPair   = 500;
  const Int_t nGhostPair   = 500;
  const Int_t nContamPair  = 500;
  const Int_t nMatGlobPair = 6000;
  if(mSFirst){ 
    cout << "\tCreating the clones arrays" << endl;
    mSFirst = 0; // flag so we dont create any more arrays;
  }
  mMcTracks     = new TClonesArray("StTinyMcTrack",nMcTrack);
  mMatchedPairs = new TClonesArray("StMiniMcPair",nMatchedPair);
  mMergedPairs  = new TClonesArray("StMiniMcPair",nMergedPair);
  mSplitPairs   = new TClonesArray("StMiniMcPair",nSplitPair);
  
  mGhostPairs   = new TClonesArray("StMiniMcPair",nGhostPair);
  mContamPairs  = new TClonesArray("StContamPair",nContamPair); 
  mMatGlobPairs = new TClonesArray("StMiniMcPair",nMatGlobPair);
  memset (mVertexCovMatrix, 0, 6*sizeof(Float_t));
}

//__________________

StMiniMcEvent::~StMiniMcEvent()
{
}

//_________________


StTinyMcTrack *StMiniMcEvent::addMcTrack(StTinyMcTrack* trk)
{
  TClonesArray &tracks = *mMcTracks;
  if (trk) 
    return new(tracks[mNMcTrack++]) StTinyMcTrack(*trk);
  else 
    return new(tracks[mNMcTrack++]) StTinyMcTrack();
}
//________________________________________________________________________________
StContamPair *StMiniMcEvent::addContamPair(StContamPair* trk)
{
  TClonesArray &tracks = *mContamPairs;
  if (trk) 
    return new(tracks[mNContamPair++]) StContamPair(*trk);
  else 
    return new(tracks[mNContamPair++]) StContamPair();
}

//________________

StMiniMcPair* StMiniMcEvent::addTrackPair(StMiniMcPair* pair,Category category)
{
  TClonesArray *tracks;
  Int_t *nPair;

  // special case for contamination pairs (make it a new function?)
  // downcast..
  //
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
  case MATGLOB:
    tracks = mMatGlobPairs; nPair = &mNMatGlobPair;
    break;
  default:
    Fatal("StMiniMcEvent", "WRONG CATEGORY  IN StMiniMcEvent::addPair()");
//    std::exit(-1);
    break;
  }

  if (pair) 
    return new((*tracks)[(*nPair)++]) StMiniMcPair(*pair);
  else 
    return new((*tracks)[(*nPair)++]) StMiniMcPair();
}

#if 0
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
#endif
TClonesArray* 
StMiniMcEvent::tracks(Category category) const
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
  
  mNMcTrack = mNMatchedPair = mNMergedPair = mNSplitPair 
            = mNGhostPair   = mNContamPair = mNMatGlobPair = 0;
  
}
#define PrMinMc(A) cout << #A":" << m ## A << "\t"
//________________________________________________________________________________
void StMiniMcEvent::Print(Option_t *option) const {
  cout << "StMiniMcEvent\t";  PrMinMc(RunId);PrMinMc(EventId);
  PrMinMc(OriginMult);  // StEvent::primaryVertex(0)->numberOfDaughters()
  PrMinMc(CentralMult); // reco, primary trk, flag>0, |eta|<0.75 
  PrMinMc(Centrality);  // centrality bin, Nch cuts, P02gd, 2k2
  cout << endl;
  PrMinMc(NUncorrectedNegativePrimaries); // from StuRefMult
  PrMinMc(NUncorrectedPrimaries); // from StuRefMult
  PrMinMc(NFtpcWUncorrectedPrimaries); // reco, primaries, flag>0, glTrk->helix->dist(vtx)<3, prim.pt<3, gl.fitPts>=5, 2.8 < eta < 3.8
  PrMinMc(NFtpcEUncorrectedPrimaries); // reco, primaries, flag>0, glTrk->helix->dist(vtx)<3, prim.pt<3, gl.fitPts>=5,-2.8 > eta >-3.8
  cout << endl;
  PrMinMc(McMult);      // embedding: n mc tracks; (StMcEvent::numberOfPrimaryTracks()  simulation: same as mOriginMult 
  PrMinMc(NMcNch);      // mc, primary, charge!=0, |eta|<0.5
  PrMinMc(NMcFtpcWNch); // mc, primary, charge!=0, 2.8 < eta < 3.8
  PrMinMc(NMcFtpcENch); // mc, primary, charge!=0,-2.8 > eta >-3.8
  PrMinMc(NMcHminus);   // mc, primary, charg  <0, |eta|<0.5
  cout << endl;
  PrMinMc(NMcGlobal);             // mc, primary, |eta|<4
  PrMinMc(NMcGoodGlobal20);	// mc, primary, |eta|<4, MC TPC hits >=20

  PrMinMc(NRcGlobal);     	// reco, primaries flag > 0
  PrMinMc(NRcGoodGlobal20); 	// reco, primaries flag > 0, 20 fit hits
  cout << endl;
  PrMinMc(McMult); PrMinMc(NMcNch); PrMinMc(NMcFtpcWNch); PrMinMc(NMcFtpcENch); PrMinMc(NMcHminus); cout << endl;
  PrMinMc(MagField); 
  cout << "Vertex X/Y/Z = " << mVertexX << "/" << mVertexY << "/" <<  mVertexZ
       << "\tMc Vertex X/Y/Z = " << mMcVertexX << "/" <<mMcVertexY << "/" <<mMcVertexZ << endl;

  PrMinMc(CenterOfMassEnergy);
  PrMinMc(BackgroundRate);
  PrMinMc(BeamMassNumberEast);
  PrMinMc(BeamMassNumberWest); cout << endl;

  PrMinMc(Ctb);
  PrMinMc(ZdcE);
  PrMinMc(ZdcW); cout << endl;
  PrMinMc(NMcTrack);     
  PrMinMc(NMatchedPair);
  PrMinMc(NMergedPair);
  PrMinMc(NSplitPair);
  PrMinMc(NGhostPair);
  PrMinMc(NContamPair);
  PrMinMc(NMatGlobPair); cout << endl;
  StTinyMcTrack *mctrak = 0;
  StMiniMcPair  *mcpair = 0;
  StTinyRcTrack *rctrak = 0;
  Category Kase[] = {MC,MATCHED,MERGED,SPLIT,CONTAM,GHOST,MATGLOB};
  for (Int_t K = MC; K <= MATGLOB; K++) {
    TClonesArray*  Tracks = ( TClonesArray* ) tracks(Kase[K]);
    Int_t N = Tracks->GetEntriesFast();
    for (int l = 0; l < N; l++) {
      switch (K) {
      case MC:
	mctrak  = (StTinyMcTrack *) Tracks->At(l);
	if (! l) {
	  cout << "McTracks ====================================" << endl;
	  mctrak->Print("desc");
	}
	mctrak->Print();
	break;
      default:
	mcpair  = (StMiniMcPair  *) Tracks->At(l);
	mctrak  = (StTinyMcTrack *) mcpair;
	rctrak  = (StTinyRcTrack *) mcpair;
	if (! l) {
	  if (K == MATCHED)  cout << "MATCHED ======================" << endl;
	  if (K == MERGED)   cout << "MERGED ======================" << endl;  
	  if (K == SPLIT)    cout << "SPLIT ======================" << endl;   
	  if (K == CONTAM)   cout << "CONTAM ======================" << endl;  
	  if (K == GHOST)    cout << "GHOST ======================" << endl;   
	  if (K == MATGLOB)  cout << "MATGLOB ======================" << endl;
	  mctrak->Print("desc"); rctrak->Print("desc");
	}
	mctrak->Print("");  rctrak->Print();
	break;
      }
    }
  }
}
#undef PrMinMc
//__________________
//
// $Log $
//

  
