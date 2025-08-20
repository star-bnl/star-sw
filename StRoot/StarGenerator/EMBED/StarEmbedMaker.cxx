#include "StarEmbedMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"

//#include "StGenericVertexMaker/StGenericVertexMaker.h"
//#include "StGenericVertexMaker/StFixedVertexFinder.h"

namespace {
vector<int> triglist(const string &s, const string &sep_chars)
{
  string::size_type prev_pos = 0, pos = 0;
  vector<int> result;

  if ( s.length() > 1 ) {
    while ((pos = s.find_first_of(sep_chars, pos)) != string::npos) {
      result.push_back(  std::stoi( s.substr(prev_pos, pos - prev_pos))  );
      pos += 1;
      prev_pos = pos;
    }
    result.push_back( std::stoi(s.substr(prev_pos)));
  }

  return result;
}
}

//_____________________________________________________________________________
StarEmbedMaker::StarEmbedMaker(const char* name) :  StarPrimaryMaker(), mFzdInput(false), mFilename(),  mFile(0),  mTree(0),  mCurrentEntry(0), mRun(0),  mEvent(0), mVertexX(-9E9), mVertexY(-9E9), mVertexZ(-9E9), mSigmaX(-9E9), mSigmaY(-9E9), mSigmaZ(-9E9) {
  SetName(name);
  SetAttr("minMult",double(5.0));
  SetAttr("skipmode", int(1));
  SetAttr("reqtrig", int(1));
  SetAttr("vpdvzcut", int(0));  // TODO VPD vz cut
  SetAttr("pvrankcut", int(0)); // TODO PV rank cut
}
//_____________________________________________________________________________
StarEmbedMaker::~StarEmbedMaker() {

}
//_____________________________________________________________________________
int StarEmbedMaker::Init() {

  if ( SAttr("tags") ) SetInputFile( SAttr("tags") );

  StarPrimaryMaker::Init();
  
  return kStOK;
}
//_____________________________________________________________________________
void StarEmbedMaker::SetInputFile(const char* filename){ 
  mFilename = filename; 
  LOG_INFO << mFilename << endm;
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
  mTree->SetBranchAddress( "GlobalTag.TriggerId",  mTriggerId );
  mTree->SetBranchAddress( "GlobalTag.uncorrectedNumberOfPrimaries",&mNumberOfPrimaries);
  mTree->SetBranchAddress( "GlobalTag.primaryVertexFlag",&mPrimaryVertexFlag);
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

  // List of concrete generators
  const     std::vector<std::string> geners = { "StarKine" };

  StEvtHddr* EvtHddr = (StEvtHddr*) GetDataSet("EvtHddr");
  if (! EvtHddr) {
    LOG_ERROR << "StarEmbedMaker::Make EvtHddr has not been found" << endm;
    return kStErr;
  }

  // Get the tags file entry from the current run and event number
  int valid = mTree->Draw("Entry$",Form("(mRunNumber==%i) && (mEventNumber==%i)",EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),"goff");
  if ( 1 != valid ) {
    LOG_ERROR << "StarEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	      << " has been found in tag file" << valid << " times" <<  endm;
    return kStErr;
  }

  mCurrentEntry = (int)mTree->GetV1()[0];

  LOG_INFO << "StarEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	   << " has been found in tag file at entry " << mCurrentEntry << endm;

  // Read the entry from the TTree.
  mTree -> GetEntry( mCurrentEntry++ );

  bool skipmode = IAttr("skipmode");
  bool reqtrig  = IAttr("reqtrig");


  LOG_INFO << "StarEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber()
	   << "/" << EvtHddr->GetEventNumber() 
	   << " has been found with uncorrectedNumberOfPrimaries = " << mNumberOfPrimaries
	   << " and primaryVertexFlag = "                            << mPrimaryVertexFlag
	   <<  endm; 

  if ( 0==mNumberOfPrimaries || mPrimaryVertexFlag ) {
    LOG_ERROR << "StarEmbedMaker::Make reject this event (no primaries||novertex)" << endm;
    return kStErr;
  }



  // "More Tree" selections... TBD


  if ( skipmode ) {
  
    if ( fabs(mVertexX)<0.1E-7 && fabs(mVertexY)<0.1E-7 && fabs(mVertexZ)<0.1E-7 ) {
      LOG_INFO << "Skip on no primary vertex" << endm;
      return kStSKIP;
    }

    const double vr2 = mVertexX*mVertexX + mVertexY*mVertexY;
    double vrcut = DAttr("vrmax");
    double vzmin = DAttr("vzmin");
    double vzmax = DAttr("vzmax");
    if ( vr2 > vrcut*vrcut || mVertexZ < vzmin || mVertexZ > vzmax ) {
      LOG_INFO << "StarEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	       << " has tags with vertex at (" << mVertexX << "," << mVertexY << "," << mVertexZ
	       << "), vr = " << sqrt(vr2)
	       << " - out of Vz or Vr range, skipping." << endm;
      return kStSKIP;
    }

    LOG_INFO << "StarEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << mVertexX << "," << mVertexY << "," << mVertexZ
	     << "), vr = " << sqrt(vr2)
	     << " - within requested Vz and Vr range." << endm;
  
    bool passTriggerRequirement = true;
    if ( SAttr("triggers") ) {
      passTriggerRequirement = false;
      std::map<unsigned int, unsigned int> fired;
      for ( int i=0;i<32;i++ ) { 
	int trig = mTriggerId[i]; 
	if ( trig ) {
	  fired[trig]++;
	  LOG_INFO << "... trigger id " << trig << " in this event ..." <<endm;
	}
      }      
      std::vector<int> reqtrgs = triglist( SAttr("triggers"), " " );
      for ( int trg : reqtrgs ) {
	bool go = fired[trg];
	if (go) {
	  passTriggerRequirement = true;
	  LOG_INFO << "Got trigger id " << trg << endl;
	}
      }
    }
    if ( 0==passTriggerRequirement && reqtrig ) {
      std::string trigs;
      for ( int i=0;i<32;i++ ) { 
	int trig = mTriggerId[i]; 
	if ( trig ) { trigs += trig; trigs += " "; }
      }
      LOG_INFO << "StarEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	       << " did not find trigger id(s) " 
	       << endm;
      return kStSKIP;
    }
    
  }

  // VPD z cut

  // Primary vertex rank cut

  // 



  //
  // Set event-by-event quantities
  //
  if ( SAttr("eventmult" ) ) {
    for ( auto g : geners ) {
      auto* mk = Maker( g.c_str() );
      if ( mk ) {
	double eventmult=DAttr("eventmult");
	int ntrack = std::max( eventmult * mNumberOfPrimaries, DAttr("minMult") );
	mk->SetAttr("ntrack", ntrack );
	LOG_INFO << "eventmult=" << eventmult << endm;
	LOG_INFO << "numberOfPrimaries = " << mNumberOfPrimaries << endm;
	LOG_INFO << "minMult = " << DAttr("minMult") << endm;
	LOG_INFO << "Setting ntrack=" << ntrack << endm;
      }
    }
  }


  LOG_INFO << "Setting vertex to " << mVertexX << " " << mVertexY << " " << mVertexZ << endm;
  LOG_INFO << "Setting vsigma to " << mSigmaX << " " << mSigmaY << " " << mSigmaZ << endm;

  LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	   << " has tags with vertex errors of (" << mSigmaX << " " << mSigmaY << " " << mSigmaZ 
	   << ")" << endm;

  StGenericVertexMaker * vmaker = (StGenericVertexMaker*) GetMaker("GenericVertex");
  StGenericVertexFinder * vfinder = (vmaker ? vmaker->GetGenericFinder() : 0);
  if((vfinder) && (vfinder->IsFixed())){
    vfinder->SetVertexPosition(mVertexX,mVertexY,mVertexZ);
    vfinder->SetVertexError(mSigmaX,mSigmaY,mSigmaZ);
  }
  else {
    LOG_WARN << "StPrepEmbedMaker::Make  a fixed vertex finder is not in the chain, vertex position and errors are not set!" << endm;
  }

  SetVertex( mVertexX, mVertexY, mVertexZ );
  SetSigma ( mSigmaX,  mSigmaY,  mSigmaZ  ); // smears the simulated vertex by the real vtx error... should be an option


  LOG_INFO << "StarEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber()
	   << "/" << EvtHddr->GetEventNumber() 
	   << " has been ACCEPTED with uncorrectedNumberOfPrimaries = " << mNumberOfPrimaries
	   << " and primaryVertexFlag = "                            << mPrimaryVertexFlag
	   <<  endm; 


  int result = kStOK;
  if ( 0 == mFzdInput ) {

    result = StarPrimaryMaker::Make();

  }

  mPrimaryEvent->Print();

  // Now we set the vertex for the vertex finder
  //auto* vertexMaker  = dynamic_cast<StGenericVertexMaker *>( GetMaker("GenericVertex") );       assert(vertexMaker);
  //auto* vertexFinder = dynamic_cast<StFixedVertexFinder  *>( vertexMaker->GetGenericFinder() ); assert(vertexFinder);
  //vertexFinder->SetVertexPosition( mVertexX, mVertexY, mVertexZ );
  //vertexFinder->SetVertexError   ( mSigmaX,  mSigmaY,  mSigmaZ );

  return result;
}
// 
//_____________________________________________________________________________

