//M.L. Miller
//MIT Software
//6/04

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>


//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//StJetMaker
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/StJets.h"
#include "StJetMaker/StJetReader.h"
#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"

ClassImp(StJetReader);

StJetReader::StJetReader(const char* name)
  : StMaker(name)
  , mFile(0)
  , mTree(0)
  , mSkimFile(0)
  , mSkimTree(0)
  , mValid(false)
  , mSkimEvent(0)
  , mOfstream(0)
{
  LOG_INFO << "StJetReader::StJetReader()" << endm;
}

StJetReader::~StJetReader()
{
  LOG_INFO << "StJetReader::~StJetReader()" << endm;
}

Int_t StJetReader::Init()
{
  return kStOk;
}

void StJetReader::InitFile(const char* file)
{
  LOG_INFO << "StJetReader::InitFile()" << endm;

  LOG_INFO << "open file:\t" << file << "\tfor reading" << endm;
  mFile = TFile::Open(file);
  assert(mFile);

  LOG_INFO << "recover tree" << endm;
  mTree = dynamic_cast<TTree*>(mFile->Get("jet"));
  assert(mTree);

  // Build index table to access jets by run number and event number
  int nentries = mTree->BuildIndex("mRunNumber","mEventNumber");
  if (nentries < 0) {
    LOG_WARN << "Could not build jet tree index: nentries=" << nentries << endm;
  }
  else {
    LOG_INFO << "Built jet tree index: nentries=" << nentries << endm;
  }
	
  LOG_INFO << "\tset tree pointer" << endm;
  LOG_INFO << "Number of entries in tree:\t" << mTree->GetEntriesFast() << endm;

  LOG_INFO << "\tGet Branches" << endm;
  TObjArray* branches = mTree->GetListOfBranches();
  if (!branches) {
    LOG_ERROR << "StJetReader::InitFile().  Null branches" << endm;
    abort();
  }

  LOG_INFO << "\tLoop on branches" << endm;

  for (int i=0; i<branches->GetEntriesFast(); ++i) {
    TBranch* branch = dynamic_cast<TBranch*>(branches->At(i));
    if (!branch) {
      LOG_ERROR << "StJetReader::InitFile().  Null branch" << endm;
      abort();
    }

    LOG_INFO << "\t--- Found branch:\t" << branch->GetName() << endm;
  }

  if (0) {
    string jetCheck(file);
    jetCheck += ".read.txt";
    mOfstream = new ofstream(jetCheck.c_str());
  }

  LOG_INFO << "\tfinished!" <<endm;

  return;
}

void StJetReader::InitJetSkimFile(const char* file)
{
  LOG_INFO << "StJetReader::InitJetSkimFile(const char* file)" << endm;
  LOG_INFO << "open file:\t" << file << "\tfor reading" << endm;

  mSkimFile = TFile::Open(file);
  assert(mSkimFile);

  mSkimTree = dynamic_cast<TTree*>(mSkimFile->Get("jetSkimTree"));
  assert(mSkimTree);

  // Build index table to access skim events by run number and event number
  int nentries = mSkimTree->BuildIndex("mRunId","mEventId");
  if (nentries < 0) {
    LOG_WARN << "Could not build skim tree index: nentries=" << nentries << endm;
  }
  else {
    LOG_INFO << "Built skim tree index: nentries=" << nentries << endm;
  }

  mSkimEvent = new StJetSkimEvent;

  //set branch status
  mSkimTree->SetBranchStatus("skimEventBranch", 1);
  mSkimTree->SetBranchAddress("skimEventBranch", &mSkimEvent);

  LOG_INFO << "successfully recovered tree from file." << endm;
}

void StJetReader::InitTree(TTree* tree)
{
  LOG_INFO << "StJetReader::InitTree()" << endm;

  LOG_INFO << "\tset tree pointer" << endm;
  LOG_INFO << "Number of entries in tree:\t" << tree->GetEntriesFast() << endm;

  TList* friendList = tree->GetListOfFriends();
  TTree* t=0;
  for (int j=0; j<friendList->GetSize(); ++j) {
    TFriendElement* fr = static_cast<TFriendElement*>( friendList->At(j) );
    string tree_name( fr->GetTreeName() );
    if (tree_name == "jet") {
      t = fr->GetTree();
      break;
    }
  }
  assert(t);

  LOG_INFO << "\tGet Branches" << endm;
  TObjArray* branches = t->GetListOfBranches();
  if (!branches) {
    LOG_ERROR << "StJetReader::InitFile().  Null branches" << endm;
    abort();
  }

  LOG_INFO << "\tLoop on branches" << endm;

  for (int i=0; i<branches->GetEntriesFast(); ++i) {
    TBranch* branch = dynamic_cast<TBranch*>(branches->At(i));
    if (!branch) {
      LOG_INFO << "StJetReader::InitFile().  Null branch" << endm;
      abort();
    }

    LOG_INFO << "\t--- Found branch:\t" << branch->GetName() << endm;
  }

  LOG_INFO << "\tfinished!" << endm;

  return;
}

int StJetReader::preparedForDualRead()
{
  LOG_INFO << "StJetReader::preparedForDualRead()" << endm;
  int jetStatus = mTree != 0;
  int skimStatus = mSkimTree != 0;
  LOG_INFO << "jet tree status:\t" << jetStatus << endm;
  LOG_INFO << "skim tree status:\t" << skimStatus << endm;
  assert(mTree);
  assert(mSkimTree);

  int nJetEvents = mTree->GetEntriesFast();
  int nSkimEvents = mSkimTree->GetEntriesFast();
  LOG_INFO << "nJetEvents:\t" << nJetEvents << endm;
  LOG_INFO << "nSkimEvents:\t" << nSkimEvents << endm;
  assert(nJetEvents == nSkimEvents);
  assert(mSkimEvent);

  LOG_INFO << "Congratulations, you are ready to process events!" << endm;
  mValid = true;
  return 1;
}

Int_t StJetReader::Make()
{
  if (mTree && mTree->GetEntryWithIndex(GetRunNumber(),GetEventNumber()) < 0) {
    LOG_WARN << Form("Could not find jet tree entry for run=%d event=%d",GetRunNumber(),GetEventNumber()) << endm;
    return kStWarn;
  }

  if (mSkimTree && mSkimTree->GetEntryWithIndex(GetRunNumber(),GetEventNumber()) < 0) {
    LOG_WARN << Form("Could not find skim tree entry for run=%d event=%d",GetRunNumber(),GetEventNumber()) << endm;
    return kStWarn;
  }

  return kStOk;
}

Int_t StJetReader::Finish()
{
  if (mOfstream) {
    delete mOfstream;
    mOfstream=0;
  }
	
  return kStOk;
}

void dumpProtojetToStream(int event, ostream& os, StJets* stjets);

string idString(TrackToJetIndex* t)
{
  string idstring;
  StDetectorId mDetId = t->detectorId();
  if (mDetId==kTpcId) {
    idstring = "kTpcId";
  }
  else if (mDetId==kBarrelEmcTowerId) {
    idstring = "kBarrelEmcTowerId";
  }
  else if (mDetId==kEndcapEmcTowerId) {
    idstring = "kEndcapEmcTowerId";
  }
  else {
    idstring = "kUnknown";
  }
  return idstring;
}

//nice check to verify that the jet 4-mom is equal to the _vector_ sum of it's part
bool verifyJet(StJets* stjets, int ijet)
{
  TClonesArray& jets = *(stjets->jets());
  StJet* pj = static_cast<StJet*>(jets[ijet]);
    
    
  StThreeVectorD j3(pj->Px(), pj->Py(), pj->Pz());
  StLorentzVectorD j4(pj->E(),j3 );
	
  //LOG_INFO <<"\tjet:\t"<<ijet<<"\t4mom:\t"<<j4<<endm;
	
  StLorentzVectorD jetMom(0., 0., 0., 0.);
    
  typedef vector<TLorentzVector*> FourpList;
  FourpList particles = stjets->particles(ijet);
    
  for (FourpList::iterator it=particles.begin(); it!=particles.end(); ++it) {
    TLorentzVector* v = *it;
    StThreeVectorD v3(v->Px(), v->Py(), v->Pz() );
    StLorentzVectorD v4(v->E(), v3 );
    jetMom += v4;
    //LOG_INFO <<"\t\t4p:\t"<<v4<<endm;
  }
  //LOG_INFO <<"\t\t\tcheck:\t"<<jetMom<<endm;
  StLorentzVectorD diff = j4-jetMom;
  if (abs(diff)>1.e-6) { //they have to be the same to 1 eV
    LOG_INFO << "verifyJet.  assert will fail for jet:\t" << ijet << "\t4p:\t" << j4 <<"\tcompared to sum_particles:\t" << jetMom << endm;
    return false;
  }
  else {
    return true;
  }
}


void StJetReader::exampleFastAna()
{
  LOG_INFO << "StJetReader::exampleEventAna()" << endm;
	
  StJetSkimEvent* skEv = skimEvent();
	    
  //basic information:
  LOG_INFO << "fill/run/event:\t" << skEv->fill() << "\t" << skEv->runId() << "\t" << skEv->eventId() << endm;
  LOG_INFO << "fileName:\t" << skEv->mudstFileName().GetString() << endm;
	
  //some spin info:
  LOG_INFO << "bx48:\t" << skEv->bx48() << "\tspinBits:\t" << skEv->spinBits() << endm;
	
  //some event info:
  LOG_INFO << "Ebbc:\t" << skEv->eBbc() << "\tWbbc:\t" << skEv->wBbc() << "\tbbcTimeBin:\t" << skEv->bbcTimeBin() << endm;
	
  //best vertex info:
  const float* bestPos = skEv->bestVert()->position();
  LOG_INFO << "best Vertex (x,y,z):\t" << bestPos[0] << "\t" << bestPos[1] << "\t" << bestPos[2] << endm;
	
  //trigger info:
  const TClonesArray* trigs = skEv->triggers();
  assert(trigs);
  int nTrigs = trigs->GetEntriesFast();
  for (int i=0; i<nTrigs; ++i) {
    StJetSkimTrig* trig = static_cast<StJetSkimTrig*>( trigs->At(i) );
    assert(trig);
    if (trig->didFire() != 0) {
      //LOG_INFO << "\tTriggerd:\t" << trig->trigId() << "\twith prescale:\t" << trig->prescale() << endm;
    }
    if (trig->shouldFireL2() == 1) {
      LOG_INFO << "\tshTrigger L2: " << trig->trigId() << endm;
      const int* l2temp = trig->L2ResultEmulated();
      for (int ii = 0; ii < 9; ++ii) {
	LOG_INFO << "SimL2--- " << ii << "\t" << l2temp[ii] << endm;
      }
    }
  }
		
  //L2 Info:
  LOG_INFO << "\n--- Non-zero L2 Results:" << endm;
  const int* l2temp = skEv->L2Result();
  for (int ii=0; ii<9; ii++) {
    if (l2temp[ii]!=0)  LOG_INFO << "DatL2--- " << ii << "\t" << l2temp[ii] << endm;
  }

  //vertex info:
  const TClonesArray* verts = skEv->vertices();
  assert(verts);
  int nVerts = verts->GetEntriesFast();
  for (int i=0; i<nVerts; ++i) {
    StJetSkimVert* vert = static_cast<StJetSkimVert*>( verts->At(i) );
    assert(vert);
    const float* vertPos = vert->position();
    LOG_INFO << "vert:\t" << i << "\t at z:\t" << vertPos[2] << "\tranking:\t"
	     << vert->ranking() << "\tnTracks:\t" << vert->nTracksUsed() << endm;
  }

  //And then access to jets from different algorithms...
  LOG_INFO << "\nLoop on jet branches" << endm;
  for (int i = 0; i < numberOfBranches(); ++i) {
    StJets* stjets = getStJets(i);
    int nJets = stjets->nJets();
    LOG_INFO << "Found\t" << nJets << "\tjets from:\t" << branchName(i) << endm;

    //first make sure that we have the same run/event:
    assert(stjets->runId()==skEv->runId());
    assert(stjets->eventId()==skEv->eventId());

    TClonesArray* jets = stjets->jets();
		
    for(int ijet=0; ijet<nJets; ++ijet){ 

      //loop on jets
      StJet* j = dynamic_cast<StJet*>( jets->At(ijet) );
      assert(j);
      //assert(verifyJet(stjets, ijet));
			
      LOG_INFO <<"jet:\t"<<ijet<<"\tEjet:\t"<<j->E()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<"\tdetEta:\t"<<j->detEta()<<endm;
			
      //look at 4-momenta in the jet:
      typedef vector<TLorentzVector*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
			
      for (TrackToJetVec::iterator partIt=particles.begin(); partIt!=particles.end(); ++partIt) {
	const TLorentzVector* theParticle = *partIt;
	LOG_INFO <<"\tparticle \t pt:\t"<<theParticle->Pt()<<"\teta:\t"<<theParticle->Eta()<<"\tphi:\t"<<theParticle->Phi()<<endm;
      }
    }
  }	
}

void StJetReader::exampleEventAna()
{
  LOG_INFO <<"StJetReader::exampleEventAna()"<<endm;
	
  LOG_INFO <<"nPrimary:\t"<<StMuDst::numberOfPrimaryTracks() << endm;

  for (int i = 0; i < numberOfBranches(); ++i) {
    StJets* stjets = getStJets(i);
    int nJets = stjets->nJets();
    LOG_INFO <<"Found\t"<<nJets<<"\tjets from:\t"<<branchName(i)<<endm;
		
    TClonesArray* jets = stjets->jets();
		
    //Dylan, here's a nice check...
    if (0) {
      if (stjets->nJets()>0) {
	dumpProtojetToStream(StMuDst::event()->eventId(), *mOfstream, stjets);
      }
    }
		
    for(int ijet=0; ijet<nJets; ++ijet){ 
			
      //loop on jets
      StJet* j = dynamic_cast<StJet*>( jets->At(ijet) );
      assert(j);
      assert(verifyJet(stjets, ijet));
			
      LOG_INFO <<"jet:\t"<<ijet<<"\tEjet:\t"<<j->E()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endm;
			
      //look at 4-momenta in the jet:
      typedef vector<TLorentzVector*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
    }
  }
}

int StJetReader::numberOfBranches() const
{
  return mTree->GetNbranches();
}

const char* StJetReader::branchName(int i) const
{
  TBranch* branch = (TBranch*)mTree->GetListOfBranches()->At(i);
  return branch ? branch->GetName() : 0;
}

StJets* StJetReader::getStJets(int i) const
{
  TBranch* branch = (TBranch*)mTree->GetListOfBranches()->At(i);
  return branch ? *(StJets**)branch->GetAddress() : 0;
}

StJets* StJetReader::getStJets(const char* bname) const
{
  TBranch* branch = mTree->GetBranch(bname);
  return branch ? *(StJets**)branch->GetAddress() : 0;
}
