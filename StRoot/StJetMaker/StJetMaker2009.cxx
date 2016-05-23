//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 May 2010
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
#include "StJetFinder/StJetFinder.h"
#include "StSpinPool/StJetEvent/StJetEventTypes.h"
#include "StMuTrackFourVec.h"
#include "StMuTrackEmu.h"
#include "StMuTowerEmu.h"
#include "StMcTrackEmu.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/database/StEEmcDb.h"

#include "StJetMaker2009.h"

ClassImp(StJetMaker2009);

void StJetMaker2009::Clear(Option_t* option)
{
  for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
    StJetBranch* jetbranch = mJetBranches[iBranch];
    jetbranch->event->Clear(option);
  }

  StMaker::Clear(option);
}

int StJetMaker2009::Init()
{
  assert(!mFileName.IsNull());

  mFile = TFile::Open(mFileName,"recreate");
  assert(mFile);

  mTree = new TTree("jet","jetTree");

  for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
    StJetBranch* jetbranch = mJetBranches[iBranch];
    jetbranch->jetfinder->Init();
    mTree->Branch(jetbranch->name,"StJetEvent",&jetbranch->event);
  }

  mTree->BranchRef();

  StEEmcDb* eemcDb = (StEEmcDb*)GetDataSet("StEEmcDb");
  eemcDb->setThreshold(3);

  return StMaker::Init();
}

int StJetMaker2009::Make()
{
  // Loop over jet branches
  for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
    StJetBranch* jetbranch = mJetBranches[iBranch];

    // Fill header
    jetbranch->event->mRunId = GetRunNumber();
    jetbranch->event->mEventId = GetEventNumber();
    jetbranch->event->mDatime = GetDateTime();

    if (jetbranch->anapars->useTpc) {
//       StjTPCRandomMuDst tpc((StMuDstMaker*)0,
// 			    jetbranch->anapars->randomSelectorProb,
// 			    jetbranch->anapars->randomSelectorAt,
// 			    jetbranch->anapars->randomSelectorSeed);
      StjTPCMuDst tpc;

      // Save vertex index
      int savedVertexIndex = tpc.currentVertexIndex();

      // Keep track of number of good vertices
      int nvertices = 0;

      // Vertex loop
      for (int iVertex = 0; iVertex < tpc.numberOfVertices(); ++iVertex) {
	tpc.setVertexIndex(iVertex);

	// Get TPC vertex and tracks
	StjPrimaryVertex vertex = tpc.getVertex();

	// Skip pile-up vertex
	if (vertex.ranking() <= 0) continue;

	// Found good vertex
	++nvertices;
	StjTrackList trackList = tpc.getTrackList();
	if (jetbranch->anapars->changeTracks) trackList = (*jetbranch->anapars->changeTracks)(trackList);
	trackList = jetbranch->anapars->tpcCuts()(trackList);

	// Get BEMC towers
	StjTowerEnergyList bemcEnergyList;

	if (jetbranch->anapars->useBemc) {
	  StjBEMCMuDst bemc;
	  bemcEnergyList = bemc.getEnergyList();
	  if (jetbranch->anapars->changeTowers) (*jetbranch->anapars->changeTowers)(bemcEnergyList);
	  bemcEnergyList = jetbranch->anapars->bemcCuts()(bemcEnergyList);
	}

	// Get EEMC towers
	StjTowerEnergyList eemcEnergyList;

	if (jetbranch->anapars->useEemc) {
	  StjEEMCMuDst eemc;
	  eemcEnergyList = eemc.getEnergyList();
	  if (jetbranch->anapars->changeTowers) (*jetbranch->anapars->changeTowers)(eemcEnergyList);
	  eemcEnergyList = jetbranch->anapars->eemcCuts()(eemcEnergyList);
	}

	// Merge BEMC and EEMC towers
	StjTowerEnergyList energyList;

	copy(bemcEnergyList.begin(),bemcEnergyList.end(),back_inserter(energyList));
	copy(eemcEnergyList.begin(),eemcEnergyList.end(),back_inserter(energyList));

	// Apply hadronic correction to towers
	energyList = jetbranch->anapars->correctTowerEnergyForTracks()(energyList,trackList);

	// Convert tracks and towers to Lorentz vectors
	FourList tpc4pList = StjeTrackListToStMuTrackFourVecList()(trackList);
	FourList energy4pList = StjeTowerEnergyListToStMuTrackFourVecList()(energyList);

	// Merge tracks and towers
	StProtoJet::FourVecList particles; // vector<const AbstractFourVec*>

	copy(tpc4pList.begin(),tpc4pList.end(),back_inserter(particles));
	copy(energy4pList.begin(),energy4pList.end(),back_inserter(particles));

	// Run jet finder
	StJetFinder::JetList protojets;	// list<StProtoJet>
	jetbranch->jetfinder->findJets(protojets,particles);

	// Filter jets
	protojets = jetbranch->anapars->jetCuts()(protojets);

	// Add vertex
	StJetVertex* jetvertex = jetbranch->event->newVertex();
	copyVertex(vertex,jetvertex);

	// Add jets
	for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet)
	  addJet(*iProtoJet,jetbranch->event,jetvertex);

	// Clean up particles
	for (StProtoJet::FourVecList::const_iterator i = particles.begin(); i != particles.end(); ++i)
	  delete *i;
      }	// End vertex loop

      // No good vertex was found. Use (0,0,0) for vertex and EMC-only jets.
      // Useful for beam-gas background studies.
      if (!nvertices) {
	// Get BEMC towers
	StjTowerEnergyList bemcEnergyList;

	if (jetbranch->anapars->useBemc) {
	  StjBEMCMuDst bemc;
	  bemc.setVertex(0,0,0);
	  bemcEnergyList = bemc.getEnergyList();
	  if (jetbranch->anapars->changeTowers) (*jetbranch->anapars->changeTowers)(bemcEnergyList);
	  bemcEnergyList = jetbranch->anapars->bemcCuts()(bemcEnergyList);
	}

	// Get EEMC towers
	StjTowerEnergyList eemcEnergyList_temp, eemcEnergyList;

	if (jetbranch->anapars->useEemc) {
	  StjEEMCMuDst eemc;
	  eemcEnergyList = eemc.getEnergyList();
	  if (jetbranch->anapars->changeTowers) (*jetbranch->anapars->changeTowers)(eemcEnergyList);
	  eemcEnergyList = jetbranch->anapars->eemcCuts()(eemcEnergyList);
	  
	}

	// Merge BEMC and EEMC towers
	StjTowerEnergyList energyList;

	copy(bemcEnergyList.begin(),bemcEnergyList.end(),back_inserter(energyList));
	copy(eemcEnergyList.begin(),eemcEnergyList.end(),back_inserter(energyList));

	// Convert towers to Lorentz vectors
	FourList energy4pList = StjeTowerEnergyListToStMuTrackFourVecList()(energyList);

	// Merge tracks and towers
	StProtoJet::FourVecList particles; // vector<const AbstractFourVec*>

	copy(energy4pList.begin(),energy4pList.end(),back_inserter(particles));

	// Run jet finder
	StJetFinder::JetList protojets;	// list<StProtoJet>
	jetbranch->jetfinder->findJets(protojets,particles);

	// Filter jets
	protojets = jetbranch->anapars->jetCuts()(protojets);

	// Add dummy vertex (0,0,0)
	StJetVertex* jetvertex = jetbranch->event->newVertex();
	jetvertex->mPosition.SetXYZ(0,0,0);

	// Add jets
	for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet)
	  addJet(*iProtoJet,jetbranch->event,jetvertex);

	// Clean up particles
	for (StProtoJet::FourVecList::const_iterator i = particles.begin(); i != particles.end(); ++i)
	  delete *i;
      }

      // Restore vertex index
      tpc.setVertexIndex(savedVertexIndex);

    } // End useTpc

    if (jetbranch->anapars->useMonteCarlo) {
      StjMCMuDst mc(this);
      StjPrimaryVertex mcvertex = mc.getMCVertex();
      StjMCParticleList mcparticles = jetbranch->anapars->mcCuts()(mc.getMCParticleList());
      StProtoJet::FourVecList particles; // vector<const AbstractFourVec*>
      transform(mcparticles.begin(),mcparticles.end(),back_inserter(particles),StjMCParticleToStMuTrackFourVec());

      // Run jet finder
      StJetFinder::JetList protojets;	// list<StProtoJet>
      jetbranch->jetfinder->findJets(protojets,particles);

      // Filter jets
      protojets = jetbranch->anapars->jetCuts()(protojets);

      // Add vertex
      StJetVertex* jetvertex = jetbranch->event->newVertex();
      copyVertex(mcvertex,jetvertex);

      // Add jets
      for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet)
	addJet(*iProtoJet,jetbranch->event,jetvertex);

      // Clean up particles
      for (StProtoJet::FourVecList::const_iterator i = particles.begin(); i != particles.end(); ++i)
	delete *i;
    } // End useMonteCarlo

  } // End loop over jet branches

  mTree->Fill();

  return kStOk;
}

int StJetMaker2009::Finish()
{
  mFile->Write();
  mFile->Close();

  return kStOk;
}

// Setters

void StJetMaker2009::addBranch(const char* name, StAnaPars* anapars, StJetPars* jetpars)
{
  mJetBranches.push_back(new StJetBranch(name,anapars,jetpars));
}

void StJetMaker2009::setJetFile(const char* filename)
{
  mFileName = filename;
}

// Getters

TTree* StJetMaker2009::tree()
{
  return mTree;
}

StJetEvent* StJetMaker2009::event(const char* branchname)
{
  TBranch* branch = mTree->GetBranch(branchname);
  if (branch) return *(StJetEvent**)branch->GetAddress();
  return 0;
}

void StJetMaker2009::addJet(const StProtoJet& protojet, StJetEvent* event, StJetVertex* vertex)
{
  StJetCandidate* jet = event->newJet(vertex->position(),TLorentzVector(protojet.px(),protojet.py(),protojet.pz(),protojet.e()), static_cast<float>(protojet.area()), static_cast<float>(protojet.areaError()));
  vertex->addJet(jet);
  jet->setVertex(vertex);

  // Add jet particles
  const StProtoJet::FourVecList& particles = protojet.list();
  for (StProtoJet::FourVecList::const_iterator iParticle = particles.begin(); iParticle != particles.end(); ++iParticle) {
    const StMuTrackFourVec* particle = dynamic_cast<const StMuTrackFourVec*>(*iParticle);

    if (const StMuTrackEmu* t = particle->track()) {
      StJetTrack* track = event->newTrack();
      copyTrack(t,track);
      jet->addTrack(track)->setJet(jet);
    }

    if (const StMuTowerEmu* t = particle->tower()) {
      StJetTower* tower = event->newTower();
      copyTower(t,tower);
      jet->addTower(tower)->setJet(jet);
    }

    if (const StMcTrackEmu* t = particle->mctrack()) {
      StJetParticle* part = event->newParticle();
      copyParticle(t,part);
      jet->addParticle(part)->setJet(jet);
    }
  } // End add jet particles

  // Calculate neutral fraction
  float sumTowerPt = jet->sumTowerPt();
  float sumTrackPt = jet->sumTrackPt();
  jet->mRt = sumTowerPt/(sumTowerPt+sumTrackPt);
}

void StJetMaker2009::copyVertex(const StjPrimaryVertex& vertex, StJetVertex* jetvertex)
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

void StJetMaker2009::copyTrack(const StMuTrackEmu* t, StJetTrack* track)
{
  track->mId             = t->id();
  track->mDetectorId     = t->detectorId();
  track->mFlag           = t->flag();
  track->mCharge         = t->charge();
  track->mNHits          = t->nHits();
  track->mNHitsFit       = t->nHitsFit();
  track->mNHitsPoss      = t->nHitsPoss();
  track->mNHitsDedx      = t->nHitsDedx();
  track->mDedx           = t->dEdx();
  track->mBeta           = t->beta();
  track->mFirstPoint     = t->firstPoint();
  track->mLastPoint      = t->lastPoint();
  track->mExitTowerId    = t->exitTowerId();
  track->mExitDetectorId = t->exitDetectorId();
  track->mExitPoint.SetPtEtaPhi(t->bemcRadius(),t->etaext(),t->phiext());
  track->mDca.SetXYZ(t->dcaX(),t->dcaY(),t->dcaZ());
  track->mDcaD           = t->dcaD();
  track->mChi2           = t->chi2();
  track->mChi2Prob       = t->chi2prob();
  TVector3 mom(t->px(),t->py(),t->pz());
  track->mPt             = mom.Pt();
  track->mEta            = mom.Eta();
  track->mPhi            = mom.Phi();
  track->mNSigmaPion     = t->nSigmaPion();
  track->mNSigmaKaon     = t->nSigmaKaon();
  track->mNSigmaProton   = t->nSigmaProton();
  track->mNSigmaElectron = t->nSigmaElectron();
}

void StJetMaker2009::copyTower(const StMuTowerEmu* t, StJetTower* tower)
{
  tower->mId         = t->id();
  tower->mDetectorId = t->detectorId();
  tower->mAdc        = t->adc();
  tower->mPedestal   = t->pedestal();
  tower->mRms        = t->rms();
  tower->mStatus     = t->status();
  TVector3 mom(t->px(),t->py(),t->pz());
  tower->mPt  = mom.Pt();
  tower->mEta = mom.Eta();
  tower->mPhi = mom.Phi();
}

void StJetMaker2009::copyParticle(const StMcTrackEmu* t, StJetParticle* particle)
{
  particle->mId     = t->id();
  particle->mPt     = t->pt();
  particle->mEta    = t->eta();
  particle->mPhi    = t->phi();
  particle->mM      = t->m();
  particle->mE      = t->e();
  particle->mPdg    = t->pdg();
  particle->mStatus = t->status();
  particle->mFirstMother   = t->firstMother();
  particle->mLastMother    = t->lastMother();
  particle->mFirstDaughter = t->firstDaughter();
  particle->mLastDaughter  = t->lastDaughter();
}
