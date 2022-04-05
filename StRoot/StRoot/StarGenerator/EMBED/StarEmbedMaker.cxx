#include "StarEmbedMaker.h"

#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StFixedVertexFinder.h"

//_____________________________________________________________________________
StarEmbedMaker::StarEmbedMaker() :  StarPrimaryMaker(), mFzdInput( true ), mFilename(),  mFile(0),  mTree(0),  mCurrentEntry(0), mRun(0),  mEvent(0), mVertexX(-9E9), mVertexY(-9E9), mVertexZ(-9E9), mSigmaX(-9E9), mSigmaY(-9E9), mSigmaZ(-9E9) {
}
//_____________________________________________________________________________
StarEmbedMaker::~StarEmbedMaker() {

}
//_____________________________________________________________________________
void StarEmbedMaker::SetInputFile(const char* filename){ 
  mFilename = filename; 
  mFile     = TFile::Open(mFilename.c_str());                                    assert(mFile);
  mTree     = dynamic_cast<TTree*>( mFile->Get("Tag") );                         assert(mTree);
  mTree->SetBranchAddress( "EvtHddr.mRunNumber",       &mRun   );
  mTree->SetBranchAddress( "EvtHddr.mEventNumber",     &mEvent );
  mTree->SetBranchAddress( "GlobalTag.primaryVertexX", &mVertexX );
  mTree->SetBranchAddress( "GlobalTag.primaryVertexY", &mVertexY );
  mTree->SetBranchAddress( "GlobalTag.primaryVertexZ", &mVertexZ );
  mTree->SetBranchAddress( "GlobalTag.sigmaPVX", &mSigmaX );
  mTree->SetBranchAddress( "GlobalTag.sigmaPVY", &mSigmaY );
  mTree->SetBranchAddress( "GlobalTag.sigmaPVZ", &mSigmaZ );
  mRun = mEvent = -1;
  mVertexX = mVertexY = mVertexZ = mSigmaX = mSigmaY = mSigmaZ = -9E9;
}
//_____________________________________________________________________________
void StarEmbedMaker::Clear(const Option_t* opts )
{
  mRun = mEvent = -1;
  mVertexX = mVertexY = mVertexZ = mSigmaX = mSigmaY = mSigmaZ = -9E9;
  StarPrimaryMaker::Clear(opts);
}
//_____________________________________________________________________________
// And now for the business end of this maker.  We will be setting up the event
// for embedding.  In this initial release, we will be reading in events from
// the FZD file (so event generation and simulation are already done).  We only
// need to read in the vertex from the tags file and setup the vertex finder
// to use it as the fixed vertex.
//
// This initial version will be limited to the HFT embedding jobs.  We will
// plan to read in from the FZD file.  But 
//
// Future versions will support simulation within the embedding job.
//
int StarEmbedMaker::Make()
{

  mTree -> GetEntry( mCurrentEntry++ );

  SetVertex( mVertexX, mVertexY, mVertexZ );
  SetSigma ( mSigmaX,  mSigmaY,  mSigmaZ  ); // smears the simulated vertex by the real vtx error... should be an option

  int result = kStOK;
  if ( 0 == mFzdInput ) {

    result = StarPrimaryMaker::Make();

  }

  // Now we set the vertex for the vertex finder
  auto* vertexMaker  = dynamic_cast<StGenericVertexMaker *>( GetMaker("GenericVertex") );       assert(vertexMaker);
  auto* vertexFinder = dynamic_cast<StFixedVertexFinder  *>( vertexMaker->GetGenericFinder() ); assert(vertexFinder);

  vertexFinder->SetVertexPosition( mVertexX, mVertexY, mVertexZ );
  vertexFinder->SetVertexError   ( mSigmaX,  mSigmaY,  mSigmaZ );

  return result;
}
// 
//_____________________________________________________________________________
