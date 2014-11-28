//M.L. Miller
//MIT Software
//6/04

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>
#include <math.h>

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"

//STAR
#include "StMessMgr.h"

//StMCAsymMaker
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"
#include "StSpinPool/StMCAsymMaker/StPythiaEvent.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StJetMaker
#include "StJetMaker/StJetSimuUtil/StJetSimuReader.h"

// StSpinPool
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"

ClassImp(StJetSimuReader)


bool verifyJet(StJets* stjets, int ijet);

void dumpProtojetToStream(int event, ostream& os, StJets* stjets);

string idString(TrackToJetIndex* t);


StJetSimuReader::StJetSimuReader(const char* name, StMuDstMaker* uDstMaker)
    : StMaker(name), mFile(0), mTree(0), mSkimFile(0), mSkimTree(0),
      mDstMaker(uDstMaker), mCounter(0), mValid(false), mSkimEvent(0), mOfstream(0)
{
  LOG_DEBUG <<"StJetSimuReader::StJetSimuReader()"<<endm;
  
}

StJetSimuReader::~StJetSimuReader()
{
  LOG_DEBUG <<"StJetSimuReader::~StJetSimuReader()"<<endm;
}


Int_t StJetSimuReader::Init()
{
  LOG_DEBUG <<"StJetSimuReader::Init()"<<endm;
  return kStOk;
}

void StJetSimuReader::InitFile(const char* file)
{
    LOG_DEBUG <<"StJetSimuReader::InitFile()"<<endm;

    LOG_DEBUG <<"open file:\t"<<file<<"\tfor reading"<<endm;
    mFile = new TFile(file,"READ");
    assert(mFile);

    LOG_DEBUG <<"recover jet tree"<<endm;
    TObject* tree = mFile->Get("jet");
    TTree* t = dynamic_cast<TTree*>(tree);
    assert(t);
    mTree = t;

    LOG_DEBUG <<"\tset tree pointer"<<endm;
    LOG_DEBUG <<"Number of entries in jet tree:\t"<<t->GetEntries();
    
    LOG_DEBUG <<"\tGet Jet Branches"<<endm;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {LOG_DEBUG <<"StJetSimuReader::InitFile().  Null branches"<<endm; abort();}

    LOG_DEBUG <<"\tLoop on branches"<<endm;
    
    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {LOG_DEBUG <<"StJetSimuReader::InitFile().  Null branch"<<endm; abort();}
	string bname( branch->GetName() );
	LOG_DEBUG <<"\t--- Found branch:\t"<<bname<<endm;
	
	if ( (bname.find("Cone")!=bname.npos) || (bname.find("cone")!=bname.npos) ) {
	  LOG_DEBUG <<"\t\tcreate StJets object for branch:\t"<<bname<<endm;
	  
	  //create StJets object here, put in map:
	  StJets* jets = new StJets();
	  jets->Clear();
	  mStJetsMap[bname] = jets;
	  LOG_DEBUG <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endm;
	  t->SetBranchStatus(bname.c_str(), 1);
	  t->SetBranchAddress(bname.c_str(), &jets);
          branch->SetAutoDelete(true);
	}
    }

    if (0) {
      string jetCheck(file);
      jetCheck += ".read.txt";
      mOfstream = new ofstream(jetCheck.c_str());
    }
    
    LOG_DEBUG <<"\tfinished!"<<endm;
    
    return ;
}

void StJetSimuReader::InitJetSkimFile(const char* file)
{
  LOG_DEBUG <<" StJetSimuReader::InitJetSkimFile(const char* file)"<<endm;
  LOG_DEBUG <<"open file:\t"<<file<<"\tfor reading"<<endm;
  
  mSkimFile = new TFile(file,"READ");
  assert(mSkimFile);
  
  TObject* t = mSkimFile->Get("jetSkimTree");
  assert(t);
  
  mSkimTree = dynamic_cast<TTree*>(t);
  assert(mSkimTree);
  
  mSkimEvent = new StJetSkimEvent();
  
  //set branch status
  mSkimTree->SetBranchStatus("skimEventBranch", 1);
  mSkimTree->SetBranchAddress("skimEventBranch", &mSkimEvent);
  
  LOG_DEBUG <<"successfully recovered tree from file."<<endm;
}


void StJetSimuReader::InitTree(TTree* tree)
{
    LOG_DEBUG <<"StJetSimuReader::InitTree()"<<endm;
    LOG_DEBUG <<"\tset tree pointer"<<endm;
    LOG_DEBUG <<"Number of entries in tree:\t"<<tree->GetEntries();

    TList* friendmist = tree->GetListOfFriends();
    TTree* t=0;
    for (int j=0; j<friendmist->GetSize()+1; ++j) {
	TFriendElement* fr = static_cast<TFriendElement*>( friendmist->At(j) );
	string tree_name( fr->GetTreeName() );
	if (tree_name == "jet") {
	    t = fr->GetTree();
	    break;
	}
    }
    assert(t);
    
    LOG_DEBUG <<"\tGet Branches"<<endm;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {LOG_DEBUG <<"StJetSimuReader::InitFile().  Null branches"<<endm; abort();}

    LOG_DEBUG <<"\tLoop on branches"<<endm;

    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {LOG_DEBUG <<"StJetSimuReader::InitFile().  Null branch"<<endm; abort();}
	string bname( branch->GetName() );
	LOG_DEBUG <<"\t--- Found branch:\t"<<bname<<endm;
	
	if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
	    LOG_DEBUG <<"\t\tcreate StJets object for branch:\t"<<bname<<endm;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    LOG_DEBUG <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endm;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    LOG_DEBUG <<"\tfinished!"<<endm;

    return ;
}


int StJetSimuReader::preparedForDualRead()
{
  LOG_DEBUG <<"StJetSimuReader::preparedForDualRead()"<<endm;
  int jetStatus = (mTree) ? 1 : 0;
  int skimStatus = (mSkimTree) ? 1 : 0;
  LOG_DEBUG <<"jet tree status:\t"<<jetStatus<<endm;
  LOG_DEBUG <<"skim tree status:\t"<<skimStatus<<endm;
  assert(mTree);
  assert(mSkimTree);
  
  int nJetEvents = mTree->GetEntries();
  int nSkimEvents = mSkimTree->GetEntries();
  LOG_DEBUG <<"nJetEvents:\t"<<nJetEvents<<endm;
  LOG_DEBUG <<"nSkimEvents:\t"<<nSkimEvents<<endm;
  assert(nJetEvents==nSkimEvents);
  assert(mSkimEvent);
  
  LOG_DEBUG <<"Congratulations, you are ready to process events!"<<endm;
  mValid = true;
  return 1;
}


Int_t StJetSimuReader::Make()
{
  int status = mTree->GetEntry(mCounter);
  if (status<0) {
    LOG_DEBUG <<"StJetSimuReader::getEvent(). ERROR:\tstatus<0.  return null"<<endm;
  }
  
  if (mSkimTree) {
    status = mSkimTree->GetEntry(mCounter);
  }
  
  mCounter++;
  
  return kStOk;
}

Int_t StJetSimuReader::Finish()
{
    if (mOfstream) {
	delete mOfstream;
	mOfstream=0;
    }

    return kStOk;
}

void StJetSimuReader::exampleSimuAna()
{
  LOG_DEBUG <<"StJetSimuReader::exampleSimuAna()"<<endm;
  
  StJetSkimEvent* skEv = skimEvent();
  
  //basic information:
  LOG_DEBUG <<"fill/run/event:\t"<<skEv->fill()<<"\t"<<skEv->runId()<<"\t"<<skEv->eventId()<<endm;
  LOG_DEBUG <<"fileName:\t"<<skEv->mudstFileName().GetString()<<endm;
  
  //some event info:
  LOG_DEBUG <<"Ebbc:\t"<<skEv->eBbc()<<"\tWbbc:\t"<<skEv->wBbc()<<"\tbbcTimeBin:\t"<<skEv->bbcTimeBin()<<endm;
  
  //best vertex info:
  const float* bestPos = skEv->bestVert()->position();
  LOG_DEBUG <<"best Vertex (x,y,z):\t"<<bestPos[0]<<"\t"<<bestPos[1]<<"\t"<<bestPos[2]<<endm;
  
  //pythia event inf
  const StPythiaEvent *pyEv=skEv->mcEvent();
  LOG_DEBUG <<" subprocess Id="<<pyEv->processId()<<endm;
  LOG_DEBUG <<" s =" <<pyEv->s()<<endm;
  LOG_DEBUG <<" t =" <<pyEv->t()<<endm;
  LOG_DEBUG <<" u =" <<pyEv->u()<<endm;
  LOG_DEBUG <<" x1 =" <<pyEv->x1()<<endm;
  LOG_DEBUG <<" x2 =" <<pyEv->x2()<<endm;
  LOG_DEBUG <<" Q2 =" <<pyEv->Q2()<<endm;
  LOG_DEBUG <<" Partonic pT="<<pyEv->pt()<<endm;
  LOG_DEBUG <<" Partonic cos(th)="<<pyEv->cosTheta()<<endm;
  LOG_DEBUG <<" Partonic Asym="<<pyEv->partonALL()<<endm;
  LOG_DEBUG <<" Polarzied PDF for x1="<<pyEv->dF1(StPythiaEvent::STD)<<endm;
  LOG_DEBUG <<" Polarzied PDF for x2="<<pyEv->dF2(StPythiaEvent::STD)<<endm;
  LOG_DEBUG <<" UnPolarzied PDF for x1="<<pyEv->f1(StPythiaEvent::STD)<<endm;
  LOG_DEBUG <<" UnPolarzied PDF for x2="<<pyEv->f1(StPythiaEvent::STD)<<endm;
  LOG_DEBUG <<" Weighting =(dF1*dF2*partonALL)/(f1*f2)="<<pyEv->ALL(StPythiaEvent::STD)<<endm;
  
  //trigger info:
  const TClonesArray* trigs = skEv->triggers();
  assert(trigs);
  int nTrigs = trigs->GetLast()+1;
  for (int i=0; i<nTrigs; ++i) {
    StJetSkimTrig* trig = static_cast<StJetSkimTrig*>( (*trigs)[i] );
    assert(trig);
    if (trig->shouldFire() != 0) {
      LOG_DEBUG <<"\tTriggerd:\t"<<trig->trigId()<<endm;
    }
  }
		
  //GET PARTICLE JETS
   for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {

    LOG_DEBUG <<"Found\t"<<(*it).first<<endm;    
    if ((*it).first!="PythiaConeR04") continue;

    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    
    //first make sure that we have the same run/event:
    assert(stjets->runId()==skEv->runId());
    assert(stjets->eventId()==skEv->eventId());
    
    TClonesArray* jets = stjets->jets();

    for (int ijet=0;ijet<nJets;ijet++){
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));	    
      LOG_DEBUG<<"%PYpT="<<j->Pt()<<" PYeta="<<j->Eta()<<" PYphi="<<j->Phi()<<endm;
    }

  }

  //GET DETECTOR JETS
  for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {

    LOG_DEBUG <<"Found\t"<<(*it).first<<endm;
    if ((*it).first!="MkConeR04") continue;


    StJets* stjets = (*it).second;
    int nJets = stjets->nJets();
    LOG_DEBUG <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endm;
       
    //first make sure that we have the same run/event:
    assert(stjets->runId()==skEv->runId());
    assert(stjets->eventId()==skEv->eventId());
    TClonesArray* jets = stjets->jets();
    
    for(int ijet=0; ijet<nJets; ++ijet){ 
      StJet* j = static_cast<StJet*>( (*jets)[ijet] );
      assert(j);
      assert(verifyJet(stjets, ijet));
      LOG_DEBUG <<"jet:\t"<<ijet<<"\tEjet:\t"<<j->E()<<"\tEta:\t"<<j->Eta()<<"\tPhi:\t"<<j->Phi()<<endm;
      
      //look at 4-momenta in the jet:
      typedef vector<TLorentzVector*> TrackToJetVec;
      TrackToJetVec particles = stjets->particles(ijet);
      for (TrackToJetVec::iterator it=particles.begin(); it!=particles.end(); ++it) {
	TLorentzVector* t2j = (*it); 
	assert(t2j);
	if (dynamic_cast<TrackToJetIndex*>(t2j)) {LOG_DEBUG<<"TPC track pT="<<t2j->Pt()<<endm;}
	if (dynamic_cast<TowerToJetIndex*>(t2j)) {LOG_DEBUG<<"TOWER track eT="<<t2j->Et()<<endm;}
      }
    }
  }
}






