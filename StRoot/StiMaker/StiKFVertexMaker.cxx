// $Id: StiKFVertexMaker.cxx,v 2.7 2015/12/20 01:06:39 fisyak Exp $
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include <map>
using std::map;
#include "TMath.h"
#include "TH1.h"
#include "TCanvas.h"
#include "StDcaGeometry.h"
#include "StXiVertex.h"
#include "StV0Vertex.h"
#include "KFParticle/KFParticle.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFPTrack.h"
#include "KFParticle/KFParticleFinder.h"
#include "StKFParticleAnalysisMaker/StKFParticleInterface.h"
#include "StKFParticleAnalysisMaker/StKFParticlePerformanceInterface.h"
#include "StKFVertexMaker/StAnneling.h"
#include "StKFVertexMaker/StKFTrack.h"
#include "StKFVertexMaker/StKFVertex.h"
#include "StKFVertexMaker/StKFVerticesCollection.h"
#include "TDirectory.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "SystemOfUnits.h"
#include "StiKFVertexMaker.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "Sti/StiHit.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiVertexFinder.h"
#include "Sti/StiDefaultToolkit.h"
#include "StiStEventFiller.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
#include "Sti/StiToolkit.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "TMVA/Reader.h"
#include "StTMVARank/TMVAdata.h"
#include "StTMVARank/TMVArank.h"
#include "StTMVARank/StTMVARanking.h"
using namespace TMVA;
ClassImp(StiKFVertexMaker);
#ifdef StTrackMassFit_hh
#endif
#define PrP(A,B)                    {LOG_INFO << "StiKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP(A,B)  if (Debug() > 1) {LOG_INFO << "StiKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP2(A,B) if (Debug() > 2) {LOG_INFO << "StiKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrParticle2(A) if (Debug() > 2) {cout << "StiKFVertexMaker::" << (#A)  << endl; PrintParticles();}
//________________________________________________________________________________
KFParticle *StiKFVertexMaker::AddTrackAt(const StiKalmanTrackNode *tNode, Int_t kg) {
  if (! tNode) return 0;
  Double_t xyzp[6], CovXyzp[21];
  tNode->getXYZ(xyzp,CovXyzp);
  Float_t xyzF[6], CovXyzF[21];
  TCL::ucopy(xyzp,xyzF,6);
  TCL::ucopy(CovXyzp,CovXyzF,21);
  static KFPTrack track;
  track.SetParameters(xyzF);
  track.SetCovarianceMatrix(CovXyzF);
  track.SetNDF(1);
  //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
  track.SetId(kg);
  Int_t q   = 1;
  Int_t pdg = 211;
  if (tNode->getCharge() < 0) {
    q = -1;
    pdg = -211;
  } 
  track.SetCharge(q);
  KFParticle *particle = new KFParticle(track, pdg);
  particle->SetId(kg);
  fParticles->AddAtAndExpand(particle, kg);
  return particle;
}
//________________________________________________________________________________
StPrimaryTrack *StiKFVertexMaker::FitTrack2Vertex(StKFVertex *V, StKFTrack*   track) {
  StPrimaryTrack* pTrack = 0;
  const KFParticle   &P = track->Particle();
  Int_t kg = P.Id();
  PrPP2(FitTrack2Vertex, *V);
  if (Debug() > 2) {
    const KFParticle   *PO = track->OrigParticle();
    const KFParticle *PS[2] = {PO, &P};
    for (Int_t m = 0; m < 2; m++) {
      if (! m) cout << "Original";
      else     cout << "Fitted  ";
      static const Char_t *names[6] = {"x","y","z","px","py","pz"};
      for (Int_t j = 0; j < 6; j++) {
	cout << Form(" %2s: %8.3f +/- %8.3f",names[j], 
		     PS[m]->GetParameter(j), 
		     PS[m]->GetCovariance(j,j) > 0 ? TMath::Sqrt(PS[m]->GetCovariance(j,j)) : -13);
      }
      cout << endl;
    }
  }
  StTrackNode *node = fTrackNodeMap[kg];
  if (! node) {
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  const StiKalmanTrack* kTrackC = (*StiStEventFiller::Node2TrackMap())[node];
  if (! kTrackC) {
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  StiKalmanTrack* kTrack = StiToolkit::instance()->getTrackFactory()->getInstance();
  *kTrack = *kTrackC;
  StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
  if (! gTrack) {
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  // Replace dca node by a primary vertex
  StiKalmanTrackNode *tNode = kTrack->getInnerMostNode();
  if (! tNode || tNode->getDetector()) {// Track has to be fitted to a vertex or to be Dca 
    BFactory::Free(kTrack);
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  if (Debug() > 1) {
    TRVector Pdca(6);
    TRSymMatrix covPdca(6);
    tNode->getXYZ(Pdca.GetArray(),covPdca.GetArray());
    PrPP2(FitTrack2Vertex,Pdca); PrPP2(FitTrack2Vertex,covPdca);
    KFParticle PKdca;
    PKdca.Create(Pdca.GetArray(),covPdca.GetArray(),P.GetQ(),P.GetMass());
    PrPP2(FitTrack2Vertex,PKdca);
  }
  //      StiHit localVertex = *Vertex;
  //      localVertex.rotate(tNode->getAlpha());
  //      tNode->setHit(&localVertex);
  // subtruct track from vertex and refit it
  KFVertex cVert(V->Vertex()); PrPP2(FitTrack2Vertex,cVert); // current vertex
  KFParticle PF(P); PrPP2(FitTrack2Vertex,PF   );
#if 0 /* Don't substract track from vertex */
  Float_t CovF[6] = {(Float_t ) 16.*cVert.GetCovariance(0,0),
		     (Float_t ) 16.*cVert.GetCovariance(0,1), (Float_t ) 16.*cVert.GetCovariance(1,1),
		     (Float_t ) 16.*cVert.GetCovariance(0,2), (Float_t ) 16.*cVert.GetCovariance(1,2), (Float_t ) 16.*cVert.GetCovariance(2,2)};
  if (cVert.NDF() > 2) {
    PF.SubtractFromParticle(cVert); PrPP2(FitTrack2Vertex,cVert);//  PrPP2(FitTrack2Vertex,PF   ); 
    TCL::ucopy(&cVert.Covariance(0), CovF, 6);
  }
#else
  Float_t CovF[6] = {(Float_t ) cVert.GetCovariance(0,0),
		     (Float_t ) cVert.GetCovariance(0,1), (Float_t ) cVert.GetCovariance(1,1),
		     (Float_t ) cVert.GetCovariance(0,2), (Float_t ) cVert.GetCovariance(1,2), (Float_t ) cVert.GetCovariance(2,2)};
#endif
  StiHit *Vertex = StiToolkit::instance()->getHitFactory()->getInstance();
  Vertex->setGlobal(0, 0, cVert.X(), cVert.Y(), cVert.Z(), 0);
  Vertex->setError(CovF);
  // Remove nodes between vertex and beam line
  Double_t R = TMath::Sqrt(cVert.GetX()*cVert.GetX() + cVert.GetY()*cVert.GetY());
  StiKalmanTrackNode *lastNode = 0;
  Int_t fail = 0;
  while ((lastNode = kTrack->getLastNode())) {
    const StiDetector *tdet = lastNode->getDetector();
    if ( tdet) { // remove Dca node or nodes below the vertex
      const StiPlacement* pl = tdet->getPlacement();
      if (pl->getNormalRadius() > R) break;
#if 1
      // Check for hits at radius less than Vertex
      if (lastNode->getHit()) {fail = 1; break;}
#endif
    }
    kTrack->removeLastNode();
  }
  if (kTrack->getPointCount() < 5 || ! kTrack->getFirstNode() || ! kTrack->getLastNode()) fail = 1;
  if (fail) {
    BFactory::Free(kTrack);
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  StiKalmanTrackNode *extended = (StiKalmanTrackNode*) kTrack->extendToVertex(Vertex);
  kTrack->reduce();
  if (extended) {
    if (Debug() > 2) {
      TRVector Pext(8,extended->fitPars().A()); PrPP2(FitTrack2Vertex,Pext);
      TRSymMatrix CovExt(6,extended->fitErrs().G()); PrPP2(FitTrack2Vertex,CovExt);
    }
    if (extended && !extended->isValid())      {extended=0;}
    if (extended && extended->getChi2()>1000)  {extended=0;}
  } 
  if (! extended) {
    BFactory::Free(kTrack);
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  kTrack->setPrimary(V->ID());
  if (Debug() > 2) {
    TRVector POext(6,extended->fitPars().A()); PrPP2(FitTrack2Vertex,POext);
    TRSymMatrix CovO(6,extended->fitErrs().G()); PrPP2(FitTrack2Vertex,CovO);
  }
  if (cVert.NDF() > 2) { //refit with vertex only if this is possible
    kTrack->add(extended,kOutsideIn);
    extended->setUntouched();
  }
  Int_t status = kTrack->refit(); // refit with primary vertex
  if (! status && cVert.NDF() <= 2) {
    extended = (StiKalmanTrackNode*) kTrack->extendToVertex(Vertex);
    if (extended) kTrack->add(extended,kOutsideIn);
    status |= (kTrack->getInnerMostHitNode(3) != extended);
  }
  kTrack->reduce();
  if (status || kTrack->getChi2() >= 100) {
    //    kTrack->removeLastNode();
    BFactory::Free(kTrack);    
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack; // failed to refit
  }
  if (Debug() > 2) {
    TRSymMatrix CovF(6,extended->fitErrs().G()); PrPP2(FitTrack2Vertex,CovF);
    const KFParticle   *PO = track->OrigParticle(); PrPP2(FitTrack2Vertex,*PO); PrPP2(FitTrack2Vertex,P);
    TRVector Pxyz(6);
    TRSymMatrix covPxyz(8);
    extended->getXYZ(Pxyz.GetArray(),covPxyz.GetArray());
    PrPP2(FitTrack2Vertex,Pxyz); PrPP2(FitTrack2Vertex,covPxyz);
    KFParticle Pext;
    Pext.Create(Pxyz.GetArray(),covPxyz.GetArray(),P.GetQ(),P.GetMass());
    PrPP2(FitTrack2Vertex,Pext);
  }
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  StiStEventFiller::instance()->fillDetectorInfo(detInfo,kTrack,kFALSE); //3d argument used to increase/not increase the refCount. MCBS oct 04.
  //      StiStEventFiller::instance()->fillPulls(kTrack,1); 
  pTrack = new StPrimaryTrack();
  node->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
  pTrack->setKey( gTrack->key());
  pTrack->setFlagExtension( gTrack->flagExtension());
  pTrack->setIdTruth(gTrack->idTruth(),gTrack->qaTruth());
  StiStEventFiller::instance()->fillTrack(pTrack,kTrack, detInfo);
  // set up relationships between objects
  StSPtrVecTrackDetectorInfo& detInfoVec = pEvent->trackDetectorInfo(); 
  detInfoVec.push_back(detInfo);
  UpdateParticleAtVertex(kTrack, &track->Particle());
  BFactory::Free(kTrack);
  return pTrack;
}
//________________________________________________________________________________
Int_t StiKFVertexMaker::Make() {
  pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StiKFVertexMaker::fit: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  StKFVertex::ResetTotalNoVertices();
  // add Fixed Primary vertex if any
  if (StiToolkit::instance()->getVertexFinder() && (IAttr("VFFV") || IAttr("VFMCE"))) {
    StiToolkit::instance()->getVertexFinder()->fit(pEvent);
    const std::vector<StiHit*> *vertexes = StiToolkit::instance()->getVertexFinder()->result();
    if (vertexes) StKFVertex::ResetTotalNoVertices(vertexes->size());
    UInt_t NoPV = pEvent->numberOfPrimaryVertices();
    for (UInt_t ipv = 0; ipv < NoPV; ipv++) {
      StPrimaryVertex *Vp = pEvent->primaryVertex(ipv);
      if (Vp && ! Vp->key()) {
	Vp->setKey(ipv+1);
      }
      Vp->setIdTruth();
      KFParticle KVx;
      KVx.Initialize();
      KVx.SetId(Vp->key());
      TCL::ucopy(Vp->position().xyz(), &KVx.Parameter(0), 3);
      TCL::ucopy(Vp->covariance(), &KVx.Covariance(0), 6);
      KVx.NDF() = 1;
      KVx.SetIdTruth(Vp->idTruth(),Vp->qaTruth());
      // copy Point fit as MassFit
      StTrackMassFit *pf = new StTrackMassFit(KVx.Id(),&KVx);
      PrPP(Make,*pf);
      Vp->setParent(pf);
    }
  }
  Double_t bField = 0;
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();
  KFParticle::SetField(bField);
  MakeParticles();
  if (fNGoodGlobals < 1) return kStOK;
  PrParticle2(Afer MakeParticles);
  Fit();
  PrParticle2(After Fit);
  if (! fgcVertices) return kStOK;
  if (fgcVertices->IsEmpty()) {
    SafeDelete(fgcVertices); 
    return kStOK;
  }
  ReFitToVertex();
  PrParticle2(After ReFitToVertex);
  ClearParentIDs();
  fgcVertices->UniqueTracks2VertexAssociation();
  
#ifdef  __V0__
#ifdef  __UseMakeV0__
  // Loop for V0
  UInt_t NoPV = pEvent->numberOfPrimaryVertices();
  for (UInt_t ipv = 0; ipv < NoPV; ipv++) {
    StPrimaryVertex *V0 = pEvent->primaryVertex(ipv);
    if (! V0) continue;
    UInt_t NoTracks = V0->numberOfDaughters();
    if (NoTracks != 2) continue;
    StTrackMassFit *pf = V0->parentMF();
    assert(pf);
    if (pf->kfParticle()->GetQ()) continue;
    MakeV0(V0);
  }
#else /* !  __UseMakeV0__ */
  ParticleFinder();
#endif /*  __UseMakeV0__ */
#endif /* __V0__ */
  SafeDelete(fgcVertices);
  return kStOK;
}
//________________________________________________________________________________
void StiKFVertexMaker::ReFitToVertex() {
  //  Int_t NoVertices = fgcVertices->Vertices()->GetSize();
  for (Int_t l = fgcVertices->Vertices()->GetSize() - 1; l >= 0; l--) {
    StPrimaryVertex *primV = 0;
    StKFVertex *V = (StKFVertex *) fgcVertices->Vertices()->At(l);
    if (! V) continue;
    //    Bool_t ok = kTRUE;
    Int_t NoTracks = V->NoTracks();
    KFVertex     &KVx = V->Vertex();
    // Store vertex
    primV = new StPrimaryVertex;
    if (! FillVertex(&KVx,primV)) {
      SafeDelete(primV);
      delete fgcVertices->Vertices()->Remove(V);
      continue;
    }
    primV->setRanking(333);
    primV->setNumTracksUsedInFinder(NoTracks);
    primV->setVertexFinderId(KFVertexFinder);
    PrPP(ReFitToVertex,KVx);
    PrPP(ReFitToVertex,*primV);
    Bool_t beam = kFALSE;
    StiHit *Vertex = StiToolkit::instance()->getHitFactory()->getInstance();
    Vertex->setGlobal(0, 0, KVx.X(), KVx.Y(), KVx.Z(), 0);
    Vertex->setError(primV->covariance());
    
    TArrayI indexT(NoTracks); Int_t *indexes = indexT.GetArray();
    TArrayI IdT(NoTracks);    Int_t *Ids     = IdT.GetArray();
    TIter next(&V->Tracks());
    StKFTrack **tracks = new StKFTrack*[NoTracks]; memset(tracks, 0, NoTracks*sizeof(StKFTrack*));
    StPrimaryTrack **pTracks = new StPrimaryTrack*[NoTracks]; memset(pTracks, 0, NoTracks*sizeof(StPrimaryTrack*));
    StTrackNode **nodes = new StTrackNode*[NoTracks]; memset(nodes, 0, NoTracks*sizeof(StTrackNode*));
    StKFTrack* track = 0;
    Int_t itk = 0;
    while ((track = (StKFTrack*) next())) {
      tracks[itk] = track;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.Id();
      Ids[itk] = kg;
      itk++;
    }
    TMath::Sort(NoTracks,Ids,indexes,0);
    for (Int_t i = 0; i < NoTracks; i++) {
      Int_t itk = indexes[i];
      StKFTrack*   track = tracks[itk];
      if (! track) continue;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.Id();
      if (kg <= 0) {
	assert(!beam);
	beam = kTRUE;
	continue;
      }
      nodes[itk] = fTrackNodeMap[kg];
      if (! nodes[itk]) {
	nodes[itk] = new StTrackNode;
	StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
	trNodeVec.push_back(nodes[itk]);
	fTrackNodeMap[kg] = nodes[itk];
      }
      if (P.GetQ()) {
	pTracks[itk] =  FitTrack2Vertex(V, track);
#if 1 /* remove tracks which fails fit */
	if (! pTracks[itk]) {
	  delete V->Remove((StKFTrack*) track);
	  tracks[itk] = 0;
	}
#endif
      }
    }
    if (beam ) primV->setBeamConstrained();
    //..... add vertex to the list
    UInt_t NoPrTracks = 0;
    for (Int_t i = 0; i < NoTracks; i++) {if (pTracks[i]) NoPrTracks++;}
    UInt_t NoPrTracksB = NoPrTracks;
#ifdef  __UseMakeV0__
    if (beam) NoPrTracksB++;
    if (NoPrTracksB < 2) 
#else
    if (beam) NoPrTracksB += 2;
    if (NoPrTracksB <= 2) 
#endif
      {
      for (Int_t i = 0; i < NoTracks; i++) {
	StPrimaryTrack *t = pTracks[i];
	if (! t) continue;
	StTrackNode *n = t->node();
	n->removeTrack(t);
	pTracks[i] = 0;
      }
      PrPP(ReFitToVertex SafeDelete,*primV);
      SafeDelete(primV);
      delete fgcVertices->Vertices()->Remove(V);
    } else {
      // copy Point fit as MassFit
      StTrackMassFit *pf = new StTrackMassFit(KVx.Id(),&KVx);
      PrPP(ReFitToVertex,*pf);
      primV->setParent(pf);
      StTrackNode *nodepf = new StTrackNode;
      nodepf->addTrack(pf);
      Int_t kgp = KVx.Id();
      fTrackNodeMap[kgp] = nodepf;
      StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
      trNodeVec.push_back(nodepf);
      for (Int_t i = 0; i < NoTracks; i++) {
	if (! tracks[i]) continue;
	if (! nodes[i]) continue;
	StPrimaryTrack *t = pTracks[i];
	if (t) {
	  primV->addDaughter(t);
	  // Done in FitTrack2Vertex    nodes[i]->addTrack(t);
	}
	PrPP(ReFitToVertex,tracks[i]->Particle());
	StTrackMassFit *mf = new StTrackMassFit(tracks[i]->Id(),&tracks[i]->Particle());
	PrPP(ReFitToVertex,*mf);
	primV->addMassFit(mf);
	nodes[i]->addTrack(mf);
      }
      primV->setTrackNumbers();
      CalculateRank(primV);
      pEvent->addPrimaryVertex(primV,orderByRanking);
      //      PrintPrimVertices();
      fVertices->AddLast(new KFVertex(KVx)); //<<<<<<<<<<<<<<<< ????????
    }
    delete [] tracks;   
    delete [] pTracks;
    delete [] nodes;
  }
}
//________________________________________________________________________________
void StiKFVertexMaker::UpdateParticleAtVertex(StiKalmanTrack *kTrack,KFParticle *particle) {
  StiKalmanTrackNode *extended = 0;
  if (kTrack) extended = kTrack->getInnerMostHitNode(3);
  if (! extended) {
    if (StKFVertex::Debug() > 2) {
      cout << "StiKFVertexMaker::UpdateParticleAtVertex extention to InnerMostNdode failed" << endl;
    }
    particle->NDF() = -1;
    particle->Chi2() = -1;;
    return;
  }
  TRVector Pxyz(6);
  TRSymMatrix covPxyz(6);
  extended->getXYZ(Pxyz.GetArray(),covPxyz.GetArray());
  PrPP(UpdateParticleAtVertex before,*particle);
#if 0
  particle->Create(Pxyz.GetArray(),covPxyz.GetArray(),particle->Q(),(Float_t) TDatabasePDG::Instance()->GetParticle(kTrack->pdgId())->Mass());
#else
  particle->Create(Pxyz.GetArray(),covPxyz.GetArray(),particle->Q(),(Float_t) TDatabasePDG::Instance()->GetParticle(particle->GetPDG())->Mass());
#endif
  //  particle->SetPDG(kTrack->pdgId());
  particle->NDF() = 2;
  particle->Chi2() = extended->getChi2();
  PrPP(UpdateParticleAtVertex  after,*particle);
}
//________________________________________________________________________________
// $Log: StiKFVertexMaker.cxx,v $
