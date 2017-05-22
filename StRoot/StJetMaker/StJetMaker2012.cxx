//******************************
//2012 version of Jet Maker 
//with off-axis cone underlying 
//-event study included the unde-
//rlying event will be saved in
//the tree called "ue". The Mak-
//-er is derivatived from the
//older 2009 version.
//
//Author: Zilong Chang
//user name: zchang
//Cyclotron Institute
//Texas A&M University
//******************************
#include "StThreeVector.hh"
#include "TFile.h"
#include "TTree.h"
#include "StAnaPars.h"
#include "StjTPCMuDst.h"
//#include "StjTPCRandomMuDst.h"
#include "StjBEMCMuDst.h"
#include "StjEEMCMuDst.h"
#include "StjFMSMuDst.h"
#include "StjMCMuDst.h"
#include "StjTPCNull.h"
#include "StjBEMCNull.h"
#include "StjEEMCNull.h"
#include "StjFMSNull.h"
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

#include "StSpinPool/StUeEvent/StUeOffAxisConesEvent.h"
#include "StSpinPool/StUeEvent/StUeVertex.h"
#include "StSpinPool/StUeEvent/StUeOffAxisConesJet.h"
#include "StSpinPool/StUeEvent/StUeOffAxisCones.h"

#include "StJetMaker2012.h"

ClassImp(StJetMaker2012);

void StJetMaker2012::Clear(Option_t* option)
{
  StJetMaker2009::Clear(option);
  for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ++ueBranch){
      StJetUeBranch* jetuebranch = mJetUeBranches[ueBranch];
    for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
      (jetuebranch->eventUe).at(iBranch)->Clear(option);
      //      jetbranch->event->Clear(option);
    }
  }
  StJetMaker2009::Clear(option);
}

int StJetMaker2012::Init()
{
  assert(!mFileNameUe.IsNull());
  mFileUe = TFile::Open(mFileNameUe,"recreate");
  assert(mFileUe);

  mTreeUe = new TTree("ue","ueTree");

  for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ++ueBranch){
    StJetUeBranch* jetuebranch = mJetUeBranches[ueBranch];
    for (size_t iBranch = 0; iBranch < mJetBranches.size(); ++iBranch) {
      StJetBranch* jetbranch = mJetBranches[iBranch];
      TString branchname(jetbranch->name);
      branchname.Append(jetuebranch->name);
      mTreeUe->Branch(branchname, "StUeOffAxisConesEvent",&((jetuebranch->eventUe)[iBranch]));
      cout<<jetbranch->name<<" initialized!"<<endl;
    }
  }
  
  mTreeUe->BranchRef();

  return StJetMaker2009::Init();
  //return kStOk;
}

int StJetMaker2012::Make()
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

	// Get FMS towers                                                                                                 
	StjTowerEnergyList fmsEnergyList;

	if (jetbranch->anapars->useFms) {
	  StjFMSMuDst fms;
	  fmsEnergyList = fms.getEnergyList();
	}

	// Merge BEMC and EEMC towers
	StjTowerEnergyList energyList;

	copy(bemcEnergyList.begin(),bemcEnergyList.end(),back_inserter(energyList));
	copy(eemcEnergyList.begin(),eemcEnergyList.end(),back_inserter(energyList));
	copy(fmsEnergyList.begin(),fmsEnergyList.end(),back_inserter(energyList));

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
	//Add UE vertex
	for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ueBranch++){
	  ((mJetUeBranches[ueBranch]->eventUe).at(iBranch))->setEventId(GetEventNumber());
	  StUeVertex *uevertex = (mJetUeBranches[ueBranch]->eventUe).at(iBranch)->newVertex();
	  uevertex->setPosition(vertex.position());
	  uevertex->setRanking(vertex.ranking());
	}
	
	// Add jets
	for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet){
	  addJet(*iProtoJet,jetbranch->event,jetvertex);
	  //UE jets
	  StJetCandidate* currentjet = jetbranch->event->jet(jetbranch->event->numberOfJets()-1);
	  for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ueBranch++){
	    StJetUeBranch *jetuebranch = mJetUeBranches.at(ueBranch);
	    double radius = jetuebranch->uePars->coneRadius();
	    double uedensity = addJetUe(particles, (jetuebranch->eventUe).at(iBranch), *iProtoJet, radius);
	    currentjet->setUeDensity(jetuebranch->name, uedensity);
	  }
	}
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

	// Get FMS towers                                                                                                 
	StjTowerEnergyList fmsEnergyList;
	if (jetbranch->anapars->useFms) {
	  StjFMSMuDst fms;
	  fmsEnergyList = fms.getEnergyList();
	}

	// Merge BEMC and EEMC towers
	StjTowerEnergyList energyList;

	copy(bemcEnergyList.begin(),bemcEnergyList.end(),back_inserter(energyList));
	copy(eemcEnergyList.begin(),eemcEnergyList.end(),back_inserter(energyList));
	copy(fmsEnergyList.begin(),fmsEnergyList.end(),back_inserter(energyList));

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
      //Add UE vertex
      for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ueBranch++){
	((mJetUeBranches[ueBranch]->eventUe).at(iBranch))->setEventId(GetEventNumber());       
	StUeVertex *uevertex = (mJetUeBranches[ueBranch]->eventUe).at(iBranch)->newVertex();
	uevertex->setPosition(mcvertex.position());
	uevertex->setRanking(mcvertex.ranking());
      }

      // Add jets
      for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet){
	addJet(*iProtoJet,jetbranch->event,jetvertex);
	//add UE jets

	StJetCandidate* currentjet = jetbranch->event->jet(jetbranch->event->numberOfJets()-1);//pointer to the current jet
	for(size_t ueBranch = 0; ueBranch < mJetUeBranches.size(); ueBranch++){
	  StJetUeBranch *jetuebranch = mJetUeBranches.at(ueBranch);
	  double radius = jetuebranch->uePars->coneRadius();
	  double uedensity = addJetUe(particles, (jetuebranch->eventUe).at(iBranch), *iProtoJet, radius);
	  currentjet->setUeDensity(jetuebranch->name, uedensity);
	}
      }

      // Clean up particles
      for (StProtoJet::FourVecList::const_iterator i = particles.begin(); i != particles.end(); ++i)
	delete *i;
    } // End useMonteCarlo

  } // End loop over jet branches
  mTree->Fill();

  mTreeUe->Fill();

  return kStOk;
}

int StJetMaker2012::Finish()
{
    mFileUe->Write();
    mFileUe->Close();

    return  StJetMaker2009::Finish();
}

// Setters

void StJetMaker2012::addUeBranch(const char* name, StOffAxisConesPars* pars)
{
  StJetUeBranch *ue = new StJetUeBranch(name, pars);
  for(size_t iBranch = 0; iBranch < mJetBranches.size(); iBranch++)
    (ue->eventUe).push_back(new StUeOffAxisConesEvent);
  mJetUeBranches.push_back(ue);

}

void StJetMaker2012::setJetFileUe(const char* filename)
{
  mFileNameUe = filename;
}
TTree* StJetMaker2012::treeUe()
{
  return mTreeUe;
}

StUeOffAxisConesEvent* StJetMaker2012::eventUe(const char* branchname)
{
  TBranch* branch = mTreeUe->GetBranch(branchname);
  if (branch) return *(StUeOffAxisConesEvent**)branch->GetAddress();
  return 0;
}

double StJetMaker2012::addJetUe(StProtoJet::FourVecList particles, StUeOffAxisConesEvent *ueEvent, const StProtoJet &jet, double radius)
{

  double jpt = jet.pt();
  double jeta = jet.eta();
  double jphi = jet.phi();
  double je = jet.e();
  double jarea = jet.area();

  const int Ncone = 2;
  StUeOffAxisCones *cones[Ncone];
  const double PI = 3.14159;
  
  double pt[Ncone];
  int number[Ncone];
  double cone_eta[Ncone];
  double cone_phi[Ncone];
  for(int ii = 0; ii < Ncone; ii++){
    cone_eta[ii] = jeta;
    cone_phi[ii] = jphi + (PI/2.)*(2*ii-1);
    cones[ii] = ueEvent->newUeOffAxisCones();
    pt[ii] = 0.;
    number[ii] = 0;
  }
  cout<<"calculating UE from particles size: "<<particles.size()<<endl;
  for(size_t ii = 0; ii < particles.size(); ii++){
    StMuTrackFourVec * vect = (StMuTrackFourVec*)particles.at(ii);                           
    //const AbstractFourVec * vect = particles.at(ii);
    //cout<<"particle pt: "<<p_pt<<endl;                                                   
    StThreeVector<double> mom(vect->px(), vect->py(), vect->pz());
    double p_pt = mom.perp();
    double p_eta = vect->eta();
    double p_phi = vect->phi();
    //    cout<<"p_pt = "<<p_pt<<" p_eta = "<<p_eta<<" p_phi = "<<p_phi<<endl;                 

    int index = -1;
    double dR = DeltaR(p_eta, p_phi, cone_eta[0], cone_phi[0]);
    if(dR < radius){
      pt[0] += p_pt;
      number[0]++;
      index = 0;
    }else{
      dR = DeltaR(p_eta, p_phi, cone_eta[1], cone_phi[1]);
      if(dR < radius){
        pt[1] += p_pt;
        number[1]++;
	index = 1;
      }
    }
    if(index == 0 || index == 1){
      //track
      if (const StMuTrackEmu* t = vect->track()) {
	StJetTrack* track = ueEvent->newTrack();
	copyTrack(t,track);
	cones[index]->addTrack(track);
      }
      //tower
      if (const StMuTowerEmu* t = vect->tower()) {
	StJetTower* tower = ueEvent->newTower();
	//copyTower(t, vertex, tower);
	copyTower(t, tower);
	cones[index]->addTower(tower);
      }
      //particle
      if (const StMcTrackEmu* t = vect->mctrack()) {
	StJetParticle* part = ueEvent->newParticle();
	copyParticle(t,part);
	cones[index]->addParticle(part);
      }      
    }
  }

  double density = 0.;
  for(int ii = 0; ii < Ncone; ii++){
    cones[ii]->setPt(pt[ii]);
    cones[ii]->setEtaPhi(cone_eta[ii], cone_phi[ii]);
    cones[ii]->setRadius(radius);
    cones[ii]->setMult(number[ii]);
    density += pt[ii]/(PI*radius*radius);
  }
  density = density/(Ncone+0.);

  StUeOffAxisConesJet *jcones = ueEvent->newUeOffAxisConesJet();
  jcones->setPt(jpt); 
  jcones->setEta(jeta); 
  jcones->setPhi(jphi); 
  jcones->setE(je);
  jcones->setArea(jarea);
  jcones->setDensity(density);
  cout<<"add new jet to underlying event"<<endl;
  jcones->addCone(cones[0]);
  jcones->addCone(cones[1]);
  StUeVertex *ueVertex = ueEvent->lastVertex();
  ueVertex->addUeJet(jcones);

  return density;
}
double StJetMaker2012::DeltaR(double etaA, double phiA, double etaB, double phiB)
{
  const double PI = 3.14159;

  double deta = etaA - etaB;
  double dphi = phiA - phiB;

  if(dphi > PI) dphi -= 2.*PI;
  if(dphi < -1.*PI) dphi += 2.*PI;

  double dR = TMath::Sqrt(deta*deta+dphi*dphi);
  //  cout<<etaA<<" "<<phiA<<" "<<etaB<<" "<<phiB<<" "<<dR<<"\n";                            
  return dR;
}
