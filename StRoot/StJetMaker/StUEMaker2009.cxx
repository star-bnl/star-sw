//
// Grant Webb
// Brookhaven National Laboratory
// 30 July 2015
//

#include <list>
#include "TFile.h"
#include "TTree.h"
#include "StAnaPars.h"
#include "StjTPCMuDst.h"
//#include "StjTPCRandomMuDst.h"
#include "StjBEMCMuDst.h"
#include "StjEEMCMuDst.h"
#include "StjMCMuDst.h"
#include "StjTPCNull.h"
#include "StjBEMCNull.h"
#include "StjEEMCNull.h"
#include "StjAbstractTowerEnergyCorrectionForTracks.h"
#include "StjeTrackListToStMuTrackFourVecList.h"
#include "StjeTowerEnergyListToStMuTrackFourVecList.h"
#include "StjMCParticleToStMuTrackFourVec.h"
//#include "StJetFinder/StJetFinder.h"
#include "StSpinPool/StJetEvent/StJetEventTypes.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StJetMaker2012.h"
#include "StJetSkimEventMaker.h"
#include "StUEMaker2009.h"

ClassImp(StUEMaker2009);

void StUEMaker2009::Clear(Option_t* option)
{
  for (size_t iBranch = 0; iBranch < mUeBranches.size(); ++iBranch) {
    StUeBranch* uebranch = mUeBranches[iBranch];
    uebranch->event->Clear(option);
  }
  
  StMaker::Clear(option);
}

int StUEMaker2009::Init()
{
  assert(!mFileName.IsNull());

  mFile = TFile::Open(mFileName,"recreate");
  assert(mFile);

  mTree = new TTree("ue","ueTree");

  for (size_t iBranch = 0; iBranch < mUeBranches.size(); ++iBranch) {
    StUeBranch* uebranch = mUeBranches[iBranch];
    // jetbranch->jetfinder->Init();
    mTree->Branch(uebranch->name + "_" + uebranch->jetname,"StUeEvent",&uebranch->event);
  }

  mTree->BranchRef();
  mJetMaker  = dynamic_cast<StJetMaker2012*>( GetMakerInheritsFrom("StJetMaker2012") );
  mSkimMaker = dynamic_cast<StJetSkimEventMaker*>( GetMakerInheritsFrom("StJetSkimEventMaker") );
  // StEEmcDb* eemcDb = (StEEmcDb*)GetDataSet("StEEmcDb");
  // eemcDb->setThreshold(3);
  return StMaker::Init();
}

int StUEMaker2009::Make()
{
  for ( size_t iBranch = 0; iBranch < mUeBranches.size(); ++iBranch){
    StUeBranch* uebranch = mUeBranches[iBranch];
    StJetEvent * jetEvent = mJetMaker->event(uebranch->jetname); 
    StJetVertex * jetvertex = jetEvent->vertex();
    StJetCandidate *leadingjet = findLeadingJet(jetvertex); 
    
    uebranch->event->mRunId = GetRunNumber();
    uebranch->event->mEventId = GetEventNumber();
    uebranch->event->mDatime = GetDateTime();
    uebranch->event->mLeadingJetpt = leadingjet->pt();
    
    if(uebranch->anapars->useTpc){
      
      StjTPCMuDst tpc;
      // Save vertex index
      //      int savedVertexIndex = tpc.currentVertexIndex();
      
      // Keep track of number of good vertices
      int nvertices = 0;

      // Vertex loop
      for (int iVertex = 0; iVertex < tpc.numberOfVertices(); ++iVertex) {
	tpc.setVertexIndex(iVertex);
	
	// Get TPC vertex and tracks
	StjPrimaryVertex vertex = tpc.getVertex();
	
	// Skip pile-up vertex
	if (vertex.ranking() <= 0) continue;
	if(leadingjet->pt() == 0) continue; // no jet in the event
	if(leadingjet->vertex()->position().Z() !=  vertex.position().Z()) continue; // Check that the vertex in the jet and the event are the same
	
	// Found good vertex
	++nvertices;
	
	StjTrackList trackList = tpc.getTrackList();
	if (uebranch->anapars->changeTracks) trackList = (*uebranch->anapars->changeTracks)(trackList);
	trackList = uebranch->anapars->tpcCuts()(trackList);
	trackList = uebranch->anapars->trackRegion()(trackList,leadingjet,uebranch->name);
	addTracks(trackList,uebranch->event);
	
	StjTowerEnergyList bemcEnergyList;
	if (uebranch->anapars->useBemc) {
	  StjBEMCMuDst bemc;
	  if (uebranch->anapars->changeTowers) (*uebranch->anapars->changeTowers)(bemcEnergyList);
	  bemcEnergyList = uebranch->anapars->bemcCuts()(bemc.getEnergyList());
	}
	StjTowerEnergyList energyList; 
	//Apply hadronic correction to towers
	energyList = uebranch->anapars->correctTowerEnergyForTracks()(bemcEnergyList,trackList);
	energyList = uebranch->anapars->towerRegion()(energyList,leadingjet,uebranch->name);
	addTowers(energyList,uebranch->event);

	StJetVertex* uevertex = uebranch->event->newVertex();
	copyVertex(vertex, uevertex);
	
      } // End of vertex Loop
    } // End of useTpc
    if (uebranch->anapars->useMonteCarlo) {
      StjMCMuDst mc(this);
      StjPrimaryVertex mcvertex = mc.getMCVertex();
      StjMCParticleList mcparticles = uebranch->anapars->mcCuts()(mc.getMCParticleList());
      mcparticles = uebranch->anapars->particleRegion()(mcparticles,leadingjet,uebranch->name);
      addParticles(mcparticles,uebranch->event);
      StJetVertex* uevertex = uebranch->event->newVertex();
      copyVertex(mcvertex, uevertex);
    }
  } // End of Loop over UE Branches

  mTree->Fill();
  return kStOk;
}

int StUEMaker2009::Finish()
{
  mFile->Write();
  mFile->Close();

  return kStOk;
}

void StUEMaker2009::addBranch(const char* name, StAnaPars* anapars, const char* jetname)// StJetPars* jetpars)
{
  mUeBranches.push_back(new StUeBranch(name,anapars,jetname));
}


void StUEMaker2009::setUeFile(const char* filename)
{
  mFileName = filename;
}

StUeEvent* StUEMaker2009::event(const char* branchname)
{
  TBranch* branch = mTree->GetBranch(branchname);
  if (branch) return *(StUeEvent**)branch->GetAddress();
  return 0;
}

StJetCandidate* StUEMaker2009::findLeadingJet(StJetVertex *vertex)
{
  // Find highest pt jet
  StJetCandidate * leadingjet = new StJetCandidate(); 
  StJetCandidate * jets = new StJetCandidate();
  for (int iJet = 0; iJet < vertex->numberOfJets(); ++iJet){
    jets = (StJetCandidate*) vertex->jet(iJet);
    if(leadingjet->pt() < jets->pt()){
      leadingjet = jets;
    } 
  } // End of loop over jets

  return leadingjet;
}


void StUEMaker2009::copyVertex(const StjPrimaryVertex& vertex, StJetVertex* jetvertex)
{
  jetvertex->mPosition              = vertex.position();
  jetvertex->mPosError              = vertex.posError();
  jetvertex->mVertexFinderId        = vertex.vertexFinderId();
  jetvertex->mRanking               = vertex.ranking();
  jetvertex->mNTracksUsed           = vertex.nTracksUsed();
  jetvertex->mNBTOFMatch            = vertex.nBTOFMatch();
  jetvertex->mNCTBMatch             = vertex.nCTBMatch();
  jetvertex->mNBEMCMatch            = vertex.nBEMCMatch();
  jetvertex->mNEEMCMatch            = vertex.nEEMCMatch();
  jetvertex->mNCrossCentralMembrane = vertex.nCrossCentralMembrane();
  jetvertex->mSumTrackPt            = vertex.sumTrackPt();
  jetvertex->mMeanDip               = vertex.meanDip();
  jetvertex->mChiSquared            = vertex.chiSquared();
  jetvertex->mRefMultPos            = vertex.refMultPos();
  jetvertex->mRefMultNeg            = vertex.refMultNeg();
  jetvertex->mRefMultFtpcWest       = vertex.refMultFtpcWest();
  jetvertex->mRefMultFtpcEast       = vertex.refMultFtpcEast();
}

void StUEMaker2009::copyTrack(const StjTrack& t, StJetTrack* track)
{
  track->mId             = t.id;
  track->mDetectorId     = t.detectorId;
  track->mFlag           = t.flag;
  track->mCharge         = t.charge;
  track->mNHits          = t.nHits;
  track->mNHitsFit       = t.nHitsFit;
  track->mNHitsPoss      = t.nHitsPoss;
  track->mNHitsDedx      = t.nHitsDedx;
  track->mDedx           = t.dEdx;
  track->mBeta           = t.beta;
  track->mFirstPoint     = t.firstPoint;
  track->mLastPoint      = t.lastPoint;
  track->mExitTowerId    = t.exitTowerId;
  track->mExitDetectorId = t.exitDetectorId;
  track->mDca.SetXYZ(t.dcaX,t.dcaY,t.dcaZ);
  track->mDcaD           = t.dcaD;
  track->mChi2           = t.chi2;
  track->mChi2Prob       = t.chi2prob;
  track->mPt             = t.pt;
  track->mEta            = t.eta;
  track->mPhi            = t.phi;
  track->mNSigmaPion     = t.nSigmaPion;
  track->mNSigmaKaon     = t.nSigmaKaon;
  track->mNSigmaProton   = t.nSigmaProton;
  track->mNSigmaElectron = t.nSigmaElectron;
}

void StUEMaker2009::copyTower(const StjTowerEnergy& t, StJetTower* tower)
{
  tower->mId         = t.towerId;
  tower->mDetectorId = t.detectorId;
  tower->mAdc        = t.adc;
  tower->mPedestal   = t.pedestal;
  tower->mRms        = t.rms;
  tower->mStatus     = t.status;
  tower->mEta        = t.towerEta;
  tower->mPhi        = t.towerPhi;
  
  Float_t energy = t.energy;
  tower->mPt         = energy/TMath::CosH(t.towerEta);   
}

void StUEMaker2009::copyParticle(const StjMCParticle& t, StJetParticle* particle)
{
  particle->mId     = t.mcparticleId;
  particle->mPt     = t.pt;
  particle->mEta    = t.eta;
  particle->mPhi    = t.phi;
  particle->mM      = t.m;
  particle->mE      = t.e;
  particle->mPdg    = t.pdg;
  particle->mStatus = t.status;
  particle->mFirstMother   = t.firstMotherId;
  particle->mLastMother    = t.lastMotherId;
  particle->mFirstDaughter = t.firstDaughterId;
  particle->mLastDaughter  = t.lastDaughterId;
}

void StUEMaker2009::addTracks(const StjTrackList& trackList, StUeEvent* event){

  for (StjTrackList::const_iterator iTrack = trackList.begin(); iTrack != trackList.end(); ++iTrack) {
    StJetTrack* ueTrack = event->newTrack();
    StjTrack track = *iTrack;
    copyTrack(track, ueTrack);
  }// End of track Loop

}

void StUEMaker2009::addTowers(const StjTowerEnergyList& towerList, StUeEvent* event){

  for (StjTowerEnergyList::const_iterator iTower = towerList.begin(); iTower != towerList.end(); ++iTower) {
   
    StJetTower* ueTower = event->newTower();
    StjTowerEnergy tower = *iTower;
    copyTower(tower, ueTower);
  }// End of tower Loop
}

void StUEMaker2009::addParticles(const StjMCParticleList& mcParticleList, StUeEvent* event){

  for (StjMCParticleList::const_iterator iParticle = mcParticleList.begin(); iParticle != mcParticleList.end(); ++iParticle) {

    StJetParticle* ueParticle = event->newParticle();
    StjMCParticle mcparticle = *iParticle;
    copyParticle(mcparticle, ueParticle);
  }// End of particle Loop
}
