// $Id: StKFVertexMaker.cxx,v 2.5 2013/04/08 19:21:41 fisyak Exp $
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
#include "StTrackMassFit.h"
#include "StXiVertex.h"
#include "StV0Vertex.h"
#include "KFParticle.h"
#include "KFVertex.h"
#include "MTrack.h"
#include "VVertex.h"
#include "TH1K.h"
#include "StAnneling.h"
#include "StKFTrack.h"
#include "StKFVertex.h"
#include "StKFVerticesCollection.h"
#include "TDirectory.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "SystemOfUnits.h"
#include "StKFVertexMaker.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "Sti/StiHit.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "StiStEventFiller.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
#include "Sti/StiToolkit.h"
#include "TArrayI.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TArrayF.h"
ClassImp(StKFVertexMaker);
StKFVerticesCollection *StKFVertexMaker::fcVertices = 0;
#define PrPP(A,B)  if (Debug() > 1) {LOG_INFO << "StKFVertex::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP2(A,B) if (Debug() > 2) {LOG_INFO << "StKFVertex::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
static map<Int_t,StTrackNode*> TrackNodeMap;
//________________________________________________________________________________
StKFVertexMaker::~StKFVertexMaker() {
  SafeDelete(fVtxM);
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    SafeDelete(fVtxKs[pass]);
    SafeDelete(fVtxKs[pass]);
  }
  SafeDelete(fSpectrum);
  SafeDelete(func);
  SafeDelete(fminBrent);
  delete [] fVerticesPass; fVerticesPass = 0;
  //  SafeDelete(fParticles);
}
//________________________________________________________________________________
void StKFVertexMaker::Clear(Option_t *option) {
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    fVtxKs[pass]->Reset();
    fVtxKs[pass]->SetMaximum();
    fVtxs[pass]->Reset();
    fVtxs[pass]->SetMaximum();
  }
  fVtx = fVtxs[0]; // << switch between types    Vtx = fVtxKs[0];
  fVtxM->Reset();
  fcVertices = 0;
  //  fParticles->Clear("C");
}
//________________________________________________________________________________
StKFVertexMaker::StKFVertexMaker(const char *name) : StMaker(name),
						     fNzBins(2500),  fNPasses(2), fSpectrum(0), fzWindow(2), 
						     fVtxM(0), fVerticesPass(0), fTempLog(2), fminBrent(0), func(0),
						     mBeamLine(kFALSE), fc1(0) , pEvent(0)
{
  Int_t npeaks = 100;
  Double_t zmin = -250;
  Double_t zmax = 250;
  //  StKFVertex::_debug = 1;
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    fVtxs[pass] = new TH1F(Form("Vtx%1i",pass),Form("z-dca distribution for pass = %1i",pass),fNzBins,zmin,zmax);
    fVtxs[pass]->SetDirectory(0);
    if (pass)  fVtxs[pass]->SetLineColor(5);
    fVtxs[pass]->SetDefaultSumw2();
    fVtxs[pass]->SetStats(0);
    fVtxKs[pass] = new TH1K(Form("VtxK%1i",pass),Form("z-dca distribution for pass = %1i",pass),fNzBins,zmin,zmax);
    fVtxKs[pass]->SetDirectory(0);
    fVtxKs[pass]->SetStats(0);
    fVtxKs[pass]->SetLineColor(2);
  }
  fVtxM = new TH1F("VtxM","MuDst reconstructed multiplicities versus Z",fNzBins,zmin,zmax);
  fVtxM->SetDirectory(0);
  fSpectrum = new TSpectrum(2*npeaks);
  func = new ROOT::Math::Functor1D(&StKFVertexMaker::AnnelingFcn);
  fminBrent = new ROOT::Math::GSLMinimizer1D();
  fVerticesPass = new StKFVerticesCollection *[fNPasses+1];
  memset (fVerticesPass, 0, (fNPasses+1)*sizeof(StKFVerticesCollection *));
  fParticles = new TObjArray(); // TClonesArray("KFParticle");
  fParticles->SetOwner(kTRUE);
  TObjectSet *part = new TObjectSet("KFTracks");
  part->SetObject(fParticles);
  AddData(part);
  TObjectSet *vert = new TObjectSet("KFVertices");
  vert->SetObject(new TObjArray());
  AddData(vert);
  mVertexOrderMethod = orderByRanking; // change ordering by ranking
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::Init(){
  mBeamLine = IAttr("beamLine");
  return StMaker::Init();
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddBeamTrack() {
  St_vertexSeedC* vSeed = St_vertexSeedC::instance();
  Double_t x0   = vSeed->x0()  ; Double_t err_x0   = vSeed->err_x0();  
  Double_t y0   = vSeed->y0()  ; Double_t err_y0   = vSeed->err_y0();
  Double_t dxdz = vSeed->dxdz(); Double_t err_dxdz = vSeed->err_dxdz();
  Double_t dydz = vSeed->dydz(); Double_t err_dydz = vSeed->err_dydz();
  Double_t weight = vSeed->weight();
  if (err_x0 < 0.010) err_x0 = 0.010;
  if (err_y0 < 0.010) err_y0 = 0.010;
  static Bool_t firstTime = kTRUE;
  if (firstTime) {
    firstTime = kFALSE;
    LOG_INFO << "BeamLine Constraint: weight =  " << weight << endm;
    LOG_INFO << "x(z) = (" << x0 << " +- " << err_x0 << ") + (" << dxdz << " +- " << err_dxdz << ") * z" << endm;
    LOG_INFO << "y(z) = (" << y0 << " +- " << err_y0 << ") + (" << dydz << " +- " << err_dydz << ") * z" << endm;
  }
  static Double_t pZ = 1000;
  static MTrack track;
  Double_t xyzP[6] = {     x0,      y0, 0.,
			   pZ*dxdz, pZ*dydz, pZ};
  Double_t CovXyzP[21] = {
    err_x0*err_x0,
    0            ,err_y0*err_y0,
    0            ,0              , 0,
    0            ,0              , 0, (err_dxdz*pZ)*(err_dxdz*pZ),
    0            ,0              , 0,                             0, (err_dydz*pZ)*(err_dydz*pZ)
  };
  track.SetParameters(xyzP);
  track.SetCovarianceMatrix(CovXyzP);
  track.SetNDF(1);
  track.SetID(0);
  track.SetCharge(1);
  //  KFParticle *beam = new (Particles()[0]) KFParticle(track, 2212);
  KFParticle *beam = new KFParticle(track, 2212);
  fParticles->AddAtAndExpand(beam, 0);
  return beam;
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::MakeParticles() {
  if (mBeamLine) AddBeamTrack();
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  Int_t NGoodGlobals = 0;
  TrackNodeMap.clear();
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    if (gTrack->flag()     <   0) continue;     // Bad fit
    if (gTrack->flag()     > 700) continue;     // FTPC
    if (gTrack->flag()%100 == 11) continue;     // Short track pointing to EEMC
    if ((gTrack->isWestTpcOnly() || gTrack->isEastTpcOnly()) && gTrack->isPostXTrack()) continue; // wrong TPC side track
    Int_t kg = gTrack->key();
    TrackNodeMap[kg] = node;
    KFParticle *particle = AddTrackAt(gTrack);
    if (! particle) continue;
    if (Debug() > 2) {
      LOG_INFO << Form("particle: %4i/%4i ",NGoodGlobals,kg) << *particle << endm;
      LOG_INFO << "Add to map[" << kg << "] node = " << TrackNodeMap[kg] << endm;
    }
    NGoodGlobals++;
  }
  return NGoodGlobals;
}
//________________________________________________________________________________
Int_t StKFVertexMaker::Make() {
  pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StKFVertexMaker::fit: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  Double_t bField = 0;
  if (pEvent->runInfo() && ! IAttr("laserIT")) bField = pEvent->runInfo()->magneticField();
  KFParticle::SetField(bField);
  Int_t NGoodGlobals = MakeParticles();
  if (NGoodGlobals < 2) return 0;
  StKFVertex::ResetTotalNoVertices();
  Fit();
  if (! Vertices()) return 0;
  TObjectSet *vert = (TObjectSet *) GetDataSet("KFVertices");
  assert(vert);
  TObjArray *verticies = (TObjArray *) vert->GetObject();
  assert(verticies);
  Int_t Nvtx = Vertices()->NoVertices();
  for (Int_t i = 0; i < Nvtx; i++) {
    StKFVertex *V = Vertices()->Vertex(i);
    if (! V) continue;
    verticies->AddAtAndExpand(new KFVertex(V->Vertex()),i);
  }
  // Loop for V0
  for (Int_t l = 0; l < Nvtx; l++) {
    StKFVertex *V = Vertices()->Vertex(l);
    if (! V) continue;
    if (V->Q() != 0)  continue; 
    Int_t NoTracks = V->NoTracks();
    if (NoTracks != 2) continue;
    MakeV0(V);
  }
  for (Int_t l = 0; l < Nvtx; l++) {
    StKFVertex *V = Vertices()->Vertex(l);
    if (! V) continue;
    Int_t NoTracks = V->NoTracks();
    if (NoTracks <= 2) continue;
    PrPP(Make,*V);
    if (Debug() > 2) V->PrintW();
    // Store vertex
    StPrimaryVertex *primV  = new StPrimaryVertex;
    StThreeVectorF XVertex(&V->Vertex().X());
    primV->setKey(V->ID());
    primV->setPosition(XVertex);
    primV->setChiSquared(V->Vertex().Chi2()/V->Vertex().GetNDF());  
    primV->setProbChiSquared(TMath::Prob(V->Vertex().GetChi2(),V->Vertex().GetNDF()));
    Float_t cov[6];
    TCL::ucopy(&V->Vertex().Covariance(0),cov,6);
    primV->setCovariantMatrix(cov); 
    primV->setVertexFinderId(KFVertexFinder);
    primV->setFlag(1); // Set default values
    primV->setRanking(333);
    primV->setNumTracksUsedInFinder(V->NoTracks());
    Bool_t beam = kFALSE;
    Double_t Pars[6];
    TCL::ucopy(&V->Vertex().X(), Pars, 6);
    Double_t Cov[21];
    TCL::ucopy(&V->Vertex().Covariance(0), Cov, 21);
    StiHit *Vertex = StiToolkit::instance()->getHitFactory()->getInstance();
    Vertex->setGlobal(0, 0, V->Vertex().X(), V->Vertex().Y(), V->Vertex().Z(), 0);
    Vertex->setError(cov);
    TArrayI indexT(NoTracks); Int_t *indexes = indexT.GetArray();
    TArrayI IdT(NoTracks);    Int_t *Ids     = IdT.GetArray();
    for (Int_t itk = 0; itk < NoTracks; itk++) {
      Ids[itk] = 999999;
      const StKFTrack*   track = V->Track(itk);
      if (! track) continue;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.GetID();
      Ids[itk] = kg;
    }
    TMath::Sort(NoTracks,Ids,indexes,0);
    for (Int_t i = 0; i < NoTracks; i++) {
      Int_t itk = indexes[i];
      const StKFTrack*   track = V->Track(itk);
      if (! track) continue;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.GetID();
      if (kg == 0) {
	assert(!beam);
	beam = kTRUE;
	continue;
      }
      StTrack *pTrack = 0;
      if (P.GetQ()) {
	pTrack =  FitTrack2Vertex(V, track);
	if (! pTrack) continue;
	Int_t kg = P.GetID();
	StTrackNode *node = TrackNodeMap[kg];
	if (! node) continue;
	StiKalmanTrack* kTrack = (*StiStEventFiller::Node2TrackMap())[node];
	if (! kTrack) continue;
	UpdateParticleAtVertex(kTrack, track->OrigParticle());
      } else {
	pTrack = new StTrackMassFit(V->ID(),massFitAtVx,new KFParticle(P));
      }
      primV->addDaughter(pTrack);
    }
    if (beam ) primV->setBeamConstrained();
    //..... add vertex to the list
    if (primV->numberOfDaughters() < 1) {
      SafeDelete(primV);
    } else {
      primV->setTrackNumbers();
      calculateRank(primV);
      pEvent->addPrimaryVertex(primV,orderByRanking);
    }
  }
  return kStOK;
}
//________________________________________________________________________________
void StKFVertexMaker::calculateRank(StPrimaryVertex *primV) {    
  // Calculation of veretx ranks to select 'best' (i.e. triggered)  vertex
  // Simpilfied version (w/o weighting)
  Float_t rank = primV->probChiSquared();
  static Float_t Wveto = 1;
  static Float_t Wmatch = 4;
  if (primV->isBeamConstrained()) rank += Wmatch;
  rank -= Wveto*primV->numPostXTracks();
  rank += Wmatch*primV->numTracksWithPromptHit();
  rank += Wmatch*primV->numTracksCrossingCentralMembrane();
  rank += Wmatch*primV->numMatchesWithCTB()
    -     Wveto*primV->numNotMatchesWithCTB();
  rank += Wmatch*primV->numMatchesWithBTOF() 
    -     Wveto*primV->numNotMatchesWithBTOF();
  rank += Wmatch*(primV->numMatchesWithBEMC() + primV->numMatchesWithEEMC());
  rank -= Wveto*(primV->numNotMatchesWithBEMC() + primV->numNotMatchesWithEEMC());
  if (primV->numTracksTpcWestOnly() > 0 && primV->numTracksTpcEastOnly() > 0) 
    rank += Wmatch*TMath::Min(primV->numTracksTpcWestOnly(),primV->numTracksTpcEastOnly());
  primV->setRanking(rank); 
  if (Debug()) primV->Print();
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StDcaGeometry *dca, Int_t kg) {
  if (! dca) return 0;
  Double_t xyzp[6], CovXyzp[21];
  dca->GetXYZ(xyzp,CovXyzp);
  static MTrack track;
  track.SetParameters(xyzp);
  track.SetCovarianceMatrix(CovXyzp);
  track.SetNDF(1);
  //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
  track.SetID(kg);
  Int_t q   = 1;
  Int_t pdg = 211;
  if (dca->charge() < 0) {
    q = -1;
    pdg = -211;
  } 
  track.SetCharge(q);
  //  KFParticle *particle = new (Particles()[kg]) KFParticle(track, pdg);
  KFParticle *particle = new KFParticle(track, pdg);
  particle->SetID(kg);
  fParticles->AddAtAndExpand(particle, kg);
  return particle;
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StGlobalTrack *gTrack) {
  if (! gTrack) return 0;
  const StDcaGeometry* dca = gTrack->dcaGeometry();
  if (! dca) return 0;
  Int_t kg = gTrack->key();
  KFParticle *particle = AddTrackAt(dca,kg);
  if (particle) {
    particle->SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
    particle->SetIdParentMcVx(gTrack->idParentVx());
    UShort_t nfitp = gTrack->fitTraits().numberOfFitPoints();
    Double_t chi2OverNdf = gTrack->fitTraits().chi2(0);
    particle->NDF()  = 2*nfitp - 5;
    particle->Chi2() = chi2OverNdf*particle->NDF();
  }
  return particle;
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StiKalmanTrackNode *tNode, Int_t kg) {
  if (! tNode) return 0;
  Double_t xyzp[6], CovXyzp[21];
  tNode->getXYZ(xyzp,CovXyzp);
  static MTrack track;
  track.SetParameters(xyzp);
  track.SetCovarianceMatrix(CovXyzp);
  track.SetNDF(1);
  //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
  track.SetID(kg);
  Int_t q   = 1;
  Int_t pdg = 211;
  if (tNode->getCharge() < 0) {
    q = -1;
    pdg = -211;
  } 
  track.SetCharge(q);
  //  KFParticle *particle = new (Particles()[kg]) KFParticle(track, pdg);
  KFParticle *particle = new KFParticle(track, pdg);
  particle->SetID(kg);
  fParticles->AddAtAndExpand(particle, kg);
  return particle;
}
//________________________________________________________________________________
StTrack *StKFVertexMaker::FitTrack2Vertex(StKFVertex *V, const StKFTrack*   track) {
  const KFParticle   &P = track->Particle();
  Int_t kg = P.GetID();
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
  StTrackNode *node = TrackNodeMap[kg];
  if (! node) {
    return 0;
  }
  StiKalmanTrack* kTrack = (*StiStEventFiller::Node2TrackMap())[node];
  assert(kTrack);
  StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
  assert(gTrack);
  // Replace dca node by a primary vertex
  StiKalmanTrackNode *tNode = kTrack->getInnerMostNode();
  if (! tNode->isDca()) {
    return 0;
  }
  if (Debug() > 1) {
    TRVector Pdca(6);
    TRSymMatrix covPdca(6);
    tNode->getXYZ(Pdca.GetArray(),covPdca.GetArray());
    PrPP2(FitTrack2Vertex,Pdca); PrPP2(FitTrack2Vertex,covPdca);
    KFParticle PKdca;
    PKdca.Initialize(Pdca.GetArray(),covPdca.GetArray(),P.GetQ(),P.GetMass());
    PrPP2(FitTrack2Vertex,PKdca);
  }
  //      StiHit localVertex = *Vertex;
  //      localVertex.rotate(tNode->getAlpha());
  //      tNode->setHit(&localVertex);
  // subtruct track from vertex and refit it
  KFVertex cVert(V->Vertex()); PrPP2(FitTrack2Vertex,cVert); // current vertex
  KFParticle PF(P); PrPP2(FitTrack2Vertex,PF   );
  Float_t CovF[6] = {16.*cVert.GetCovariance(0,0),
		     16.*cVert.GetCovariance(0,1), 16.*cVert.GetCovariance(1,1),
		     16.*cVert.GetCovariance(0,2), 16.*cVert.GetCovariance(1,2), 16.*cVert.GetCovariance(2,2)};
  if (cVert.NDF() > 2) {
    PF.SubtractFromParticle(cVert); PrPP2(FitTrack2Vertex,cVert);//  PrPP2(FitTrack2Vertex,PF   ); 
    TCL::ucopy(&cVert.Covariance(0), CovF, 6);
  }
  StiHit *Vertex = StiToolkit::instance()->getHitFactory()->getInstance();
  Vertex->setGlobal(0, 0, cVert.X(), cVert.Y(), cVert.Z(), 0);
  Vertex->setError(CovF);
  // Remove nodes between vertex and beam line
  Double_t R = TMath::Sqrt(cVert.GetX()*cVert.GetX() + cVert.GetY()*cVert.GetY());
  StiKalmanTrackNode *lastNode = 0;
  Int_t fail = 0;
  while ((lastNode = kTrack->getLastNode())) {
    if (lastNode->getHit()) {fail = 1; break;}
    const StiDetector *tdet = lastNode->getDetector();
    if (! tdet) {kTrack->removeLastNode(); continue;}
    const StiPlacement* pl = tdet->getPlacement();
    if (pl->getNormalRadius() > R) break;
    kTrack->removeLastNode();
  }
  if (fail) {
    return 0;
  }
  StiKalmanTrackNode *extended = (StiKalmanTrackNode*) kTrack->extendToVertex(Vertex);
  kTrack->reduce();
  if (extended) {
    if (Debug() > 2) {
      TRVector Pext(8,extended->fitPars().A()); PrPP2(FitTrack2Vertex,Pext);
      TRSymMatrix CovExt(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovExt);
    }
    if (extended && !extended->isValid())      {extended=0;}
    if (extended && extended->getChi2()>1000)  {extended=0;}
  } 
  if (! extended) {
    return 0;
  }
  kTrack->setPrimary(V->ID());
  if (Debug() > 2) {
    TRVector POext(6,extended->fitPars().A()); PrPP2(FitTrack2Vertex,POext);
    TRSymMatrix CovO(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovO);
  }
  if (cVert.NDF() > 2) { //refit with vertex only if this is possible
    kTrack->add(extended,kOutsideIn);
    extended->setUntouched();
  }
  Int_t status = kTrack->refit(); // refit with primary vertex
  if (! status && cVert.NDF() <= 2) {
    extended = (StiKalmanTrackNode*) kTrack->extendToVertex(Vertex);
    kTrack->add(extended,kOutsideIn);
  }
  status |= (kTrack->getInnerMostHitNode(3) != extended);
  kTrack->reduce();
  if (status) {
    kTrack->removeLastNode();
    return 0; // failed to refit
  }
  if (Debug() > 2) {
    TRSymMatrix CovF(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovF);
    const KFParticle   *PO = track->OrigParticle(); PrPP2(FitTrack2Vertex,*PO); PrPP2(FitTrack2Vertex,P);
    TRVector Pxyz(6);
    TRSymMatrix covPxyz(8);
    extended->getXYZ(Pxyz.GetArray(),covPxyz.GetArray());
    PrPP2(FitTrack2Vertex,Pxyz); PrPP2(FitTrack2Vertex,covPxyz);
    KFParticle Pext;
    Pext.Initialize(Pxyz.GetArray(),covPxyz.GetArray(),P.GetQ(),P.GetMass());
    PrPP2(FitTrack2Vertex,Pext);
  }
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
  StiStEventFiller::instance()->fillDetectorInfo(detInfo,kTrack,kFALSE); //3d argument used to increase/not increase the refCount. MCBS oct 04.
  //      StiStEventFiller::instance()->fillPulls(kTrack,1); 
  StPrimaryTrack* pTrack = new StPrimaryTrack;
  node->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
  pTrack->setKey( gTrack->key());
  pTrack->setFlagExtension( gTrack->flagExtension());
  StiStEventFiller::instance()->fillTrack(pTrack,kTrack, detInfo);
  // set up relationships between objects
  StSPtrVecTrackDetectorInfo& detInfoVec = pEvent->trackDetectorInfo(); 
  detInfoVec.push_back(detInfo);
  return pTrack;
}
//________________________________________________________________________________
void StKFVertexMaker::Fit() {
  if (Debug() > 3)  StKFVertex::SetDebug(Debug());
  fcVertices = 0;
  for (Int_t i = 0; i < fNPasses+1; i++) {
    SafeDelete(fVerticesPass[i]);
  }
  Int_t LastGlobal = Particles().GetLast();
  Double_t TempLog = fTempLog; // default Temperature Log
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    Int_t nAccepted = 0;
    Double_t dZ = fVtxs[pass]->GetBinWidth(1);
    for (Int_t k = 0; k <= LastGlobal; k++) {
      KFParticle *particle = (KFParticle *) Particles()[k];
      if (! particle) continue;
      Double_t pT;
      Double_t dpT;
      particle->GetPt(pT,dpT);
      Double_t offset = 0.5*particle->GetPz()/pT;
      Double_t SigmaZ = TMath::Sqrt(particle->Covariance(2,2) + offset*offset);
      SigmaZ += dZ;
      Double_t Z = particle->GetZ();
      fVtxKs[pass]->Fill(Z);
      Int_t bin1 = fVtxs[pass]->FindBin(Z - 5*SigmaZ);
      if (bin1 < 1) bin1 = 1;
      Int_t bin2 = fVtxs[pass]->FindBin(Z + 5*SigmaZ);
      if (bin2 > fNzBins) bin2 = fNzBins;
      Double_t z = fVtxs[pass]->GetBinCenter(bin1);
      for (Int_t bin = bin1; bin <= bin2; bin++, z += dZ) {
	fVtxs[pass]->Fill(z,(TMath::Erfc((z - Z - fzWindow)/SigmaZ) - TMath::Erfc((z - Z + fzWindow)/SigmaZ))/2.);
      }
      nAccepted++;
    }
    Double_t F = fVtxKs[pass]->GetEntries();
    if (F < 1) continue;
    fVtxKs[pass]->SetNormFactor(F/dZ);
    fVtx = fVtxs[0]; // << switch between types    Vtx = fVtxKs[0];
    TString opt("new");
    if (! Canvas()) opt = "goff";
    Int_t nfound = fSpectrum->Search(fVtx,3,opt,TMath::Min(0.1,5./LastGlobal));
    if (! nfound) continue;
    if (Canvas()) {
      Canvas()->cd();
      fVtxs[0]->Draw(); fVtxKs[0]->Draw("same");
      fVtxM->Draw("same");
      if (pass)    fVtx->Draw("same");
      Canvas()->Update();
    }
    if (StKFVertex::Debug() > 1) {
      LOG_INFO << "Found " << nfound 
	       << " candidate peaks to fit with " << LastGlobal
	       << " good globals from with " <<  nAccepted  << " accepted" << endm;
    }
    Double_t *zOfPeaks = new Double_t[nfound];
    Int_t npeaks = 0;
#if ROOT_VERSION_CODE > 336641 /* ROOT_VERSION(5,35,1) */
    Double_t *xpeaks = fSpectrum->GetPositionX();
#else
    Float_t *xpeaks = fSpectrum->GetPositionX();
#endif
    for (Int_t p = 0; p < nfound; p++) {
#if ROOT_VERSION_CODE > 336641 /* ROOT_VERSION(5,35,1) */
      Double_t xp = xpeaks[p];
#else
      Float_t xp = xpeaks[p];
#endif
      Int_t bin = fVtx->GetXaxis()->FindBin(xp);
      Double_t yp = fVtx->GetBinContent(bin);
      Double_t ep = fVtx->GetBinError(bin);
      if (yp-1.25*ep < 0) continue;
      zOfPeaks[npeaks] = xp;
      npeaks++;
    }
    if (StKFVertex::Debug() > 1) {
      LOG_INFO << "Found " << npeaks << " useful peaks to fit" << endm;
    }
    if (! npeaks) {delete [] zOfPeaks;  zOfPeaks = 0; continue; }
    if (fVerticesPass[pass]) {SafeDelete(fVerticesPass[pass]);}
    fVerticesPass[pass] = new StKFVerticesCollection(npeaks, zOfPeaks);
    delete [] zOfPeaks; zOfPeaks = 0;
    fcVertices = fVerticesPass[pass];
    if (! fcVertices) continue;
    fcVertices->DoTrack2VertexAssociation(Particles());
    if (! fcVertices->NoVertices())                         continue;
    if (AnnelingFcn(TMath::Exp(-TempLog)) <= 0) continue;
    if (! fcVertices->NoVertices())                         continue;
    fcVertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
    //       fcVertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
    // 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
    // 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
  }
  if (fVerticesPass[0] || fVerticesPass[1]) {
    if (! fVerticesPass[0] && fVerticesPass[1]) {
      fVerticesPass[0] = fVerticesPass[1];
      fVerticesPass[1] = 0;
    } 
    if (fVerticesPass[0] && fVerticesPass[1]) {
      *fVerticesPass[0] += *fVerticesPass[1];
      SafeDelete(fVerticesPass[1]);
    }
    fVerticesPass[0]->MergeDuplicatedVertices();
  }
  if (fVerticesPass[0]) {
    if (fNPasses > 1 && Canvas()) {
      Canvas()->cd();
      fVtxs[1]->Draw("same"); 
      Canvas()->Update();
    }
  }
  //  if (! fcVertices->NoVertices()) return;
  // Double_t Temperature = TMath::Exp(TempLog);
  TempLog = 5;
  Double_t Temperature = TMath::Exp(TempLog);
  // secondary vertices
  Int_t pass = fNPasses;
  if (fVerticesPass[pass]) {SafeDelete(fVerticesPass[pass]);}
  fVerticesPass[pass] = new StKFVerticesCollection();
  fcVertices = fVerticesPass[pass];
  StAnneling::SetTemperature(Temperature);
  for (Int_t k = 1; k < LastGlobal; k++) {
    KFParticle *particleK = (KFParticle *) Particles()[k];
    if (! particleK) continue;
    if (particleK->GetParentID()) continue;
    StKFVertex *vtx = 0;
    PrPP(Fit,*particleK);
    KFParticle particleV;
    for (Int_t l = k+1; l <= LastGlobal; l++) {
      KFParticle *particleL = (KFParticle *) Particles()[l];
      if (! particleL) continue;
      if (particleL->GetParentID()) continue;
      PrPP(Fit,*particleL);
      // Check consistency with others
      if (! vtx) {
	const KFParticle *vDaughters[2] = {particleK, particleL};
	KFParticle tempV;
	tempV.Construct(vDaughters,2);	PrPP2(Fit,tempV);
	Double_t prob = TMath::Prob(tempV.GetChi2(),tempV.GetNDF());
	if (prob < 1e-5) continue;
	particleV = tempV;	PrPP2(Fit,particleV);
	// Create new Vertex 
	vtx = new StKFVertex();
	vtx->AddTrack(new StKFTrack(k,particleK));
      } else {
	KFParticle tempV = particleV;
	tempV += *particleL;	PrPP2(Fit,tempV);
	Double_t prob = TMath::Prob(tempV.GetChi2(),tempV.GetNDF());
	if (prob < 1e-5) continue;
      }
      vtx->AddTrack(new StKFTrack(l,particleL));
    }
    if (! vtx) continue;
    vtx->Fit();
    PrPP(Fit,*vtx);
    Int_t N = vtx->NoTracks();
    if (! N) {SafeDelete(vtx); continue;}
    Double_t X = vtx->Vertex().X();
    Double_t Y = vtx->Vertex().Y();
    Double_t R = TMath::Sqrt(X*X + Y*Y);
    if (R > 200 ) {SafeDelete(vtx); continue;}
    Double_t prob = TMath::Prob(vtx->Vertex().GetChi2(),vtx->Vertex().GetNDF());
    if (N > 2 || prob > 1.e-5) {// Allow V2 to share tracks
      for (Int_t i = 0; i < N; i++) {
	KFParticle *particle = vtx->Track(i)->OrigParticle();;
	particle->SetParentID(vtx->ID());
      }
    }
    fcVertices->AddVertex(vtx);
  }
  if (StKFVertex::Debug() > 1) {
    LOG_INFO << "Candidate for secondary vertices: " << fcVertices->NoVertices() << endm;
  }
  if ( fcVertices->NoVertices() ) {
    //       fcVertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
    // 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
    // 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
    if (fVerticesPass[0]) {*fVerticesPass[0] += *fVerticesPass[fNPasses]; SafeDelete(fVerticesPass[fNPasses]);}
    else                  { fVerticesPass[0]  =  fVerticesPass[fNPasses]; fVerticesPass[fNPasses] = 0;}
  }
  // end of loop for secondary vertices
  fcVertices = fVerticesPass[0];
  if (! fVerticesPass[0]) return;
  fVerticesPass[0]->Compress();
  if (! fVerticesPass[0]->NoVertices()) {SafeDelete(fVerticesPass[0]); return;}
  fVerticesPass[0]->MergeDuplicatedVertices();
  fminBrent->SetFunction(*func,TMath::Exp(-0.5*(TempLog)),TMath::Exp(-TempLog),1);
  if (! fminBrent->Minimize(10,0.1,0.1)) {
    LOG_WARN << "Temperature fit has failed" << endm;
    Temperature = 1;
  } else {
    Temperature = 1./fminBrent->XMinimum();
  }
  StAnneling::SetTemperature(Temperature);
  fVerticesPass[0]->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
  fVerticesPass[0]->Fit(29,Canvas(),fVtx);
  if (Canvas()) Canvas()->Update();
}
//________________________________________________________________________________
Double_t StKFVertexMaker::AnnelingFcn(Double_t TInv) {
  if (! fcVertices) return 0;
  Double_t Temperature = 1./TInv;
  StAnneling::SetTemperature(Temperature);
  Double_t Chi2 =  fcVertices->Fit();
  if (StKFVertex::Debug()) 
    LOG_INFO << "StKFVertexMaker::AnnelingFcn\tTemperature = " << Temperature << " Chi2 = " << Chi2 << endm;
  return Chi2;
}
//________________________________________________________________________________
void StKFVertexMaker::UpdateParticleAtVertex(StiKalmanTrack *kTrack, KFParticle *particle) {
  StiKalmanTrackNode *extended = kTrack->getInnerMostHitNode(3);
  if (! extended) return;
  TRVector Pxyz(6);
  TRSymMatrix covPxyz(6);
  extended->getXYZ(Pxyz.GetArray(),covPxyz.GetArray());
  PrPP(MakeV0 before,*particle);
  particle->Initialize(Pxyz.GetArray(),covPxyz.GetArray(),particle->Q(),TDatabasePDG::Instance()->GetParticle(kTrack->pdgId())->Mass(), kTrack->pdgId());
  particle->NDF() = 2;
  particle->Chi2() = extended->getChi2();
  PrPP(MakeV0  after,*particle);
}
//________________________________________________________________________________
Bool_t StKFVertexMaker::MakeV0(StKFVertex *V) {
  static const Double_t probCut = 1e-5;
  PrPP(MakeV0,*V);
  Bool_t ok = kTRUE;
  Int_t NoTracks = V->NoTracks();
  if (NoTracks != 2) return kFALSE;
  if (V->Q() != 0)   return kFALSE;
  enum {NoV0types = 4};  // gamma K0s  Lambda, LambdaBar
  Int_t pdgV0[NoV0types]   = {  22, 310, 3122, -3122};  // geantI: {gamma    1,K0s   16, Lambda 18,AntiLambda 26}
  Int_t pdgD[2][NoV0types] = {{-11, 211, 2212,   211},  //         {postiron 2,pion+  8, Proton 14,pion+       8}
			      { 11,-211, -211, -2212}}; //         {electron 3,pion-  9, pion-   9,AntiProton 15}
  KFVertex *V0s[NoV0types]; memset(V0s, 0, NoV0types*sizeof(KFVertex *));
  
  Int_t ip = 0, in = 1;
  if (V->Track(0)->OrigParticle()->Q() < 0) {ip = 1; in = 0;}
  StKFTrack  *tracks[2] = {V->Track(ip),  V->Track(in)};
  KFParticle *particles[2] = {tracks[0]->OrigParticle(), tracks[1]->OrigParticle()};
  PrPP(MakeV0,*particles[0]);
  PrPP(MakeV0,*particles[1]);
  KFParticle pos, neg;
  KFParticle *vDaughters[2] = {&pos, &neg}; // 0 -> positive, 1 -> negative
  StTrack *trks[2] = {0,0};
  StV0Vertex *V0Vx = 0;
  StTrackNode* trackNode = 0;
  Int_t kg = Particles().GetLast() + 1; // new track
  Int_t flag = 0;
  for (Int_t l = 0; l < NoV0types; l++) {
    for (Int_t k = 0; k < 2; k++) {
      vDaughters[k]->Initialize(
				particles[k]->Parameters(), 
				particles[k]->CovarianceMatrix(), 
				particles[k]->Q(), 
				TDatabasePDG::Instance()->GetParticle(pdgD[k][l])->Mass());
      vDaughters[k]->SetID(particles[k]->GetID());
      vDaughters[k]->SetParentID(particles[k]->GetParentID());
    }
    PrPP(MakeV0,pos);
    PrPP(MakeV0,neg);
    // 2c-Fit
    KFVertex &V0 = V->Vertex();
    V0.Construct((const KFParticle **) vDaughters,NoTracks,0,TDatabasePDG::Instance()->GetParticle(pdgV0[l])->Mass(),0);
    PrPP(MakeV0,V0);
    Double_t prob = TMath::Prob(V0.GetChi2(),V0.GetNDF());
    if (prob < probCut) continue;
    V0.SetPDG(pdgV0[l]);
    Particles().AddAtAndExpand(&V0,kg);
    SafeDelete(V0s[l]); V0s[l] = new KFVertex(V0); PrPP(MakeV0,*V0s[l]);
    V0s[l]->SetID(V->ID());
    flag |= 1 << l;
    // Refit tracks with mass hyp.
    for (Int_t k = 0; k < 2; k++) {
      Int_t kgk = vDaughters[k]->GetID();
      StTrackNode *node = TrackNodeMap[kgk];
      if (! node) continue;
      StiKalmanTrack* kTrack = (*StiStEventFiller::Node2TrackMap())[node];
      if (! kTrack) continue;
      kTrack->setPDG(pdgD[k][l]);
      StTrack *pTrack =  FitTrack2Vertex(V, tracks[k]);
      if (pTrack) {
	UpdateParticleAtVertex(kTrack,vDaughters[k]);
      }
      kTrack->setPDG(211); // Reset back PDG to pion
    }    
    KFVertex V01(V0);
    V01.Construct((const KFParticle **) vDaughters,NoTracks,0,TDatabasePDG::Instance()->GetParticle(pdgV0[l])->Mass(),0);
    PrPP(MakeV0,V01);
    Double_t prob1 = TMath::Prob(V01.GetChi2(),V01.GetNDF());
    if (prob1 > probCut) {
      SafeDelete(V0s[l]); V0s[l] = new KFVertex(V01); V0s[l]->SetID(V->ID()); PrPP(MakeV0,*V0s[l]);
    }
  }
  // Try to find the best parent vertex
  Int_t  lBest = -1;
  Double_t probBest = -1;
  StKFVertex *VpBest = 0;
  Int_t Nvtx = Vertices()->NoVertices();
  for (Int_t m = 0; m < Nvtx; m++) {
    StKFVertex *Vp = Vertices()->Vertex(m);
    if (! Vp) continue;
    if (Vp == V) continue;
    PrPP(Make,*Vp);
    KFVertex Parent = Vp->Vertex(); PrPP(MakeV0,Parent);
    for (Int_t l = 0; l < NoV0types; l++) {
      if (! V0s[l]) continue;
#if 0
      KFVertex V02;
      V02.Construct((const KFParticle **) vDaughters,NoTracks,(const KFParticle*) &Parent,TDatabasePDG::Instance()->GetParticle(pdgV0[l])->Mass(),0);
#else
      KFVertex V02(*V0s[l]);
      V02.SetProductionVertex(Parent);
#endif
      PrPP(MakeV0,V02);
      Double_t prob2 = TMath::Prob(V02.GetChi2(),V02.GetNDF());
      if (prob2 < probCut) continue;
      SafeDelete(V0s[l]); V0s[l] = new KFVertex(V02); V0s[l]->SetID(V->ID()); V0s[l]->SetParentID(Vp->ID());  PrPP(MakeV0,*V0s[l]);
      if (prob2 > probBest) { 
	probBest = prob2; 
	lBest = l;
	VpBest = Vp;
      }
    }
  }
  // Fill StV0Vertex
  for (Int_t l = 0; l < NoV0types; l++) {
    if (! V0s[l]) continue;
    PrPP(MakeV0,*V0s[l]);
    V0Vx = new StV0Vertex(); 
    StThreeVectorF XVertex(V0s[l]->X(),V0s[l]->Y(),V0s[l]->Z());
    V0Vx->setKey(V->ID());
    V0Vx->setPosition(XVertex);
    V0Vx->setChiSquared(V0s[l]->Chi2()/V0s[l]->GetNDF());  
    V0Vx->setProbChiSquared(TMath::Prob(V0s[l]->GetChi2(),V0s[l]->GetNDF()));
    Float_t cov[6];
    TCL::ucopy(&V0s[l]->Covariance(0),cov,6);
    V0Vx->setCovariantMatrix(cov); 
    V0Vx->setFlag(flag); // Set default values, will use for kinematical ambiguities
    StSPtrVecV0Vertex& v0Vertices = pEvent->v0Vertices();
    v0Vertices.push_back(V0Vx);
    V0s[l]->SetID(V->ID());
    new (Particles()[kg]) KFParticle(*V0s[l]);
    // V0 track 
    StTrackMassFit* gTrack = new StTrackMassFit(V->ID(),massFitAtVx,new KFVertex(*V0s[l]));
    V0Vx->setParent(gTrack);
    gTrack->setKey(kg);
    PrPP(MakeV0,*gTrack);
    if (! trackNode) {
      trackNode = new StTrackNode;
      StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
      trNodeVec.push_back(trackNode);
    }
    trackNode->addTrack(gTrack);
    TrackNodeMap[kg] = trackNode;
    trks[0] = TrackNodeMap[neg.GetID()] ? TrackNodeMap[pos.GetID()]->track(global) : 0;
    trks[1] = TrackNodeMap[pos.GetID()] ? TrackNodeMap[pos.GetID()]->track(global) : 0;
    assert (trks[0] && trks[1]);
    gTrack->setEndVertex(V0Vx);
    V0Vx->setPosition(StThreeVectorF(V0s[l]->GetX(), V0s[l]->GetY(), V0s[l]->GetZ()));;
    V0Vx->addDaughter(trks[negative]);
    V0Vx->addDaughter(trks[positive]);
    V0Vx->setDcaDaughterToPrimaryVertex(positive,trks[negative]->impactParameter());
    V0Vx->setDcaDaughterToPrimaryVertex(negative,trks[positive]->impactParameter());
    //3VectorF vs 3VectorD???
    V0Vx->setMomentumOfDaughter(positive,StThreeVectorF(pos.GetPx(),pos.GetPy(),pos.GetPz()));
    V0Vx->setMomentumOfDaughter(negative,StThreeVectorF(neg.GetPx(),neg.GetPy(),neg.GetPz()));
    PrPP(MakeV0, *V0Vx);
  }
  if (probBest >= probCut) {
    StKFTrack *V0track = new StKFTrack(kg,V0s[lBest]);
    KFVertex  Vertex = VpBest->Vertex();
    PrPP(MakeV0 before fit,Vertex);
    Vertex.AddDaughter(*V0s[lBest]);
    PrPP(MakeV0 after fit ,Vertex);
    Double_t prob = TMath::Prob(Vertex.GetChi2(),Vertex.GetNDF());
    if (prob >= probCut) {
      PrPP(MakeV0 before,VpBest->Vertex());
      VpBest->Vertex() = Vertex;
      PrPP(MakeV0 after,VpBest->Vertex());
      VpBest->AddTrack(V0track);
      PrPP(MakeV0,*VpBest); 
      //    VpBest->Fit(); 
    }
  }
  return ok;
}  
//________________________________________________________________________________
// $Log: StKFVertexMaker.cxx,v $
// Revision 2.5  2013/04/08 19:21:41  fisyak
// Adjust for new KFParticle
//
// Revision 2.4  2013/01/28 21:51:17  fisyak
// Correct ranking
//
// Revision 2.3  2013/01/17 15:57:25  fisyak
// Add handles for debugging
//
// Revision 2.2  2012/09/16 21:38:42  fisyak
// use of Tpc West Only and East Only tracks, clean up
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.5  2012/04/13 14:42:58  fisyak
// Freeze
//
// Revision 1.4  2012/03/29 23:35:47  fisyak
// Fix problem with multiple beam tracks
//
// Revision 1.3  2012/03/26 23:42:35  fisyak
// Add beam constrain
//
// Revision 1.2  2012/02/20 22:38:34  fisyak
// Freeze before go for ranking
//
// Revision 1.1  2012/02/18 23:20:52  fisyak
// Rename StKFVertexFitter => StKFVertexMaker
//
// Revision 1.3  2012/02/07 19:38:26  fisyak
// Repackage
//
