// $Id: StKFVertexMaker.cxx,v 2.7 2015/12/20 01:06:39 fisyak Exp $
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
#include "Sti/StiVertexFinder.h"
#include "StiDefaultToolkit.h"
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
ClassImp(StKFVertexMaker);
#ifdef StTrackMassFit_hh
#define __MakeV0__
#endif
StKFVerticesCollection *StKFVertexMaker::fgcVertices = 0;
#define PrPP(A,B)  if (Debug() > 1) {LOG_INFO << "StKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP2(A,B) if (Debug() > 2) {LOG_INFO << "StKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
static map<Int_t,StTrackNode*> TrackNodeMap;
/* Bookkeeping: 
   kg =  gTrack->key(); 
   TObjArray *fParticles; // KF particles
   fParticles[kg], kg = 0 -> beam line, kg > 0 kg == gTrack->key();
   StKFVerticesCollection *fgcVertices;  // current vertex collection
   fVertices        
   MakeV0::
   
*/
//________________________________________________________________________________
StKFVertexMaker::StKFVertexMaker(const char *name) : StMaker(name),
						     fParticles(0), fVertices(0),
						     fPass(0), fNzBins(2500),  fNPasses(2), fSpectrum(0), fzWindow(2), 
						     fTempLog(2), fminBrent(0), func(0),
						     mBeamLine(kFALSE), fVertexZPlot(0), fStack(0), fc1(0) , pEvent(0)
{
  Int_t npeaks = 250;
  Double_t zmin = -250;
  Double_t zmax = 250;
  //  StKFVertex::_debug = 1;
  
  fStack = new THStack("Stack","z_dca distribution");
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    fVertexZPlots[pass] = new TH1F(Form("VertexZPlot%1i",pass),Form("z-dca distribution for pass = %1i",pass),fNzBins,zmin,zmax);
    fVertexZPlots[pass]->SetDirectory(0);
    if (pass)  fVertexZPlots[pass]->SetLineColor(pass+1);
    fVertexZPlots[pass]->SetDefaultSumw2();
    fVertexZPlots[pass]->SetStats(0);
    fStack->Add(fVertexZPlots[pass]);
  }
  fSpectrum = new TSpectrum(npeaks);
  func = new ROOT::Math::Functor1D(&StKFVertexMaker::AnnelingFcn);
  fminBrent = new ROOT::Math::GSLMinimizer1D();
  mVertexOrderMethod = orderByRanking; // change ordering by ranking
}
//________________________________________________________________________________
StKFVertexMaker::~StKFVertexMaker() {
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    SafeDelete(fVertexZPlots[pass]);
  }
  SafeDelete(fStack);
  SafeDelete(fSpectrum);
  SafeDelete(func);
  SafeDelete(fminBrent);
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddBeamTrack() {
  St_vertexSeedC* vSeed = St_vertexSeedC::instance();
  Float_t x0   = vSeed->x0()  ; Float_t err_x0   = vSeed->err_x0();  
  Float_t y0   = vSeed->y0()  ; Float_t err_y0   = vSeed->err_y0();
  Float_t dxdz = vSeed->dxdz(); Float_t err_dxdz = vSeed->err_dxdz();
  Float_t dydz = vSeed->dydz(); Float_t err_dydz = vSeed->err_dydz();
  Float_t weight = vSeed->weight();
  if (err_x0 < 0.010) err_x0 = 0.010;
  if (err_y0 < 0.010) err_y0 = 0.010;
  static Bool_t firstTime = kTRUE;
  if (firstTime) {
    firstTime = kFALSE;
    LOG_INFO << "BeamLine Constraint: weight =  " << weight << endm;
    LOG_INFO << "x(z) = (" << x0 << " +- " << err_x0 << ") + (" << dxdz << " +- " << err_dxdz << ") * z" << endm;
    LOG_INFO << "y(z) = (" << y0 << " +- " << err_y0 << ") + (" << dydz << " +- " << err_dydz << ") * z" << endm;
  }
  static Float_t pZ = 1000;
  static KFPTrack track;
  Float_t xyzPF[6] = {     x0,      y0, 0.,
			   pZ*dxdz, pZ*dydz, pZ};
  Float_t dZ = 10;
  Float_t CovXyzPF[21] = {
    err_x0*err_x0,
    0            ,err_y0*err_y0,
    0            ,0              , dZ*dZ,
    0            ,0              , 0, (err_dxdz*pZ)*(err_dxdz*pZ),
    0            ,0              , 0,                             0, 1
  };
  track.SetParameters(xyzPF);
  track.SetCovarianceMatrix(CovXyzPF);
  track.SetNDF(1);
  track.SetId(0);
  track.SetCharge(1);
  KFParticle *beam = new KFParticle(track, 2212);
  beam->SetId(0);
  fParticles->AddAtAndExpand(beam, 0);
  if (Debug()) {
    LOG_INFO << Form("particle: beam      ") << *beam << endm;
  }
  return beam;
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StDcaGeometry *dca, Int_t kg) {
  if (! dca) return 0;
  KFParticle *particle =  new KFParticle(dca->Particle(kg));
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
Double_t StKFVertexMaker::AnnelingFcn(Double_t TInv) {
  if (! fgcVertices) return 0;
  Double_t Temperature = 1./TInv;
  StAnneling::SetTemperature(Temperature);
  Double_t Chi2 =  fgcVertices->Fit();
  LOG_INFO << "StKFVertexMaker::AnnelingFcn\tTemperature = " << Temperature << " Chi2 = " << Chi2 
	   << " with " << fgcVertices->NoVertices() << " vertices" << endm;
  return Chi2;
}
//________________________________________________________________________________
void StKFVertexMaker::CalculateRank(StPrimaryVertex *primV) {    
  static St_TMVArank *tmvaRank = 0;
  if (! tmvaRank) {
    tmvaRank = (St_TMVArank *) GetDataBase("rank/TMVArank4KFV");
    assert(tmvaRank);
    TMVArank_st *tmva = tmvaRank->GetTable();
    if (TString(tmva->XmlFile) != "") {
      new StTMVARanking(tmva->ActiveVars,tmva->XmlFile,tmva->Method);
    }
  }
  if (StTMVARanking::Reader()) TMVARank(primV);
  else               SimpleMindedRank(primV);
}
//________________________________________________________________________________
void StKFVertexMaker::TMVARank(StPrimaryVertex *primV) {    
  Float_t rank = StTMVARanking::TMVARank(primV);
  primV->setRanking(rank); 
  if (Debug()) primV->Print(Form("Rank:#V[%3i]",primV->key()));
}
//________________________________________________________________________________
void StKFVertexMaker::SimpleMindedRank(StPrimaryVertex *primV) {    
  Float_t rank = StTMVARanking::SimpleMindedRank(primV);
  primV->setRanking(rank); 
  if (Debug()) primV->Print(Form("Rank:#V[%3i]",primV->key()));
}
//________________________________________________________________________________
void StKFVertexMaker::Clear(Option_t *option) {
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    fVertexZPlots[pass]->Reset();
    fVertexZPlots[pass]->SetMaximum();
  }
  fVertexZPlot = fVertexZPlots[0]; 
  SafeDelete(fgcVertices);
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::Init(){
  mBeamLine = IAttr("beamLine");
  return StMaker::Init();
}
//________________________________________________________________________________
void StKFVertexMaker::Fit() {
  if (Debug())  StKFVertex::SetDebug(Debug());
  SafeDelete(fgcVertices);
  PrimaryVertices();
  SecondaryVertices();
  if (! fgcVertices) return;
  if ( fgcVertices->IsEmpty()) {SafeDelete(fgcVertices); return;}
  //  fgcVertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
  if (! fgcVertices) return;
  if ( fgcVertices->IsEmpty()) {SafeDelete(fgcVertices); return;}
  fVertexZPlot = fVertexZPlots[0];
  fgcVertices->Fit(29,Canvas(),fVertexZPlot);
  PrPP2(After final fit, *fgcVertices);
}
//________________________________________________________________________________
StPrimaryTrack *StKFVertexMaker::FitTrack2Vertex(StKFVertex *V, StKFTrack*   track) {
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
  StTrackNode *node = TrackNodeMap[kg];
  if (! node) {
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
  StiKalmanTrack* kTrack = StiToolkit::instance()->getTrackFactory()->getInstance();
  const StiKalmanTrack* kTrackC = (*StiStEventFiller::Node2TrackMap())[node];
  if (! kTrackC) {
    UpdateParticleAtVertex(0, &track->Particle());
    return pTrack;
  }
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
  Float_t CovF[6] = {(Float_t ) 16.*cVert.GetCovariance(0,0),
		     (Float_t ) 16.*cVert.GetCovariance(0,1), (Float_t ) 16.*cVert.GetCovariance(1,1),
		     (Float_t ) 16.*cVert.GetCovariance(0,2), (Float_t ) 16.*cVert.GetCovariance(1,2), (Float_t ) 16.*cVert.GetCovariance(2,2)};
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
    const StiDetector *tdet = lastNode->getDetector();
    if ( tdet) { // remove Dca node or nodes below the vertex
      const StiPlacement* pl = tdet->getPlacement();
      if (pl->getNormalRadius() > R) break;
#if 0
      // Check for hits at radius less than Vertex
      if (lastNode->getHit()) {fail = 1; break;}
#endif
    }
    kTrack->removeLastNode();
  }
  if (! kTrack->getFirstNode() || ! kTrack->getLastNode()) fail = 1;
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
      TRSymMatrix CovExt(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovExt);
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
    TRSymMatrix CovO(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovO);
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
    TRSymMatrix CovF(6,extended->fitErrs().A); PrPP2(FitTrack2Vertex,CovF);
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
Int_t StKFVertexMaker::Make() {
  pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StKFVertexMaker::fit: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  StKFVertex::ResetTotalNoVertices();
  // add Vixed Primary vertex if any
  if (StiToolkit::instance()->getVertexFinder() && (IAttr("VFFV") || IAttr("VFMCE"))) {
    StiToolkit::instance()->getVertexFinder()->fit(pEvent);
    const std::vector<StiHit*> *vertexes = StiToolkit::instance()->getVertexFinder()->result();
    if (vertexes) StKFVertex::ResetTotalNoVertices(vertexes->size());
  }
  Double_t bField = 0;
#if 0
  if (pEvent->runInfo() && ! IAttr("laserIT")) bField = pEvent->runInfo()->magneticField();
#else
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();
#endif
  KFParticle::SetField(bField);
  MakeParticles();
  if (fNGoodGlobals < 1) return kStOK;
  Fit();
  if (! fgcVertices) return kStOK;
  if (fgcVertices->IsEmpty()) {
    SafeDelete(fgcVertices); 
    return kStOK;
  }
  ReFitToVertex();
  
#ifdef  __MakeV0__
  // Loop for V0
  UInt_t NoPV = pEvent->numberOfPrimaryVertices();
  for (Int_t ipv = 0; ipv < NoPV; ipv++) {
    StPrimaryVertex *V0 = pEvent->primaryVertex(ipv);
    if (! V0) continue;
    UInt_t NoTracks = V0->numberOfDaughters();
    if (NoTracks != 2) continue;
    StTrackMassFit *pf = V0->parentMF();
    assert(pf);
    if (pf->kfParticle()->GetQ()) continue;
    MakeV0(V0);
  }
#endif /* __MakeV0__ */
  SafeDelete(fgcVertices);
  return kStOK;
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::MakeParticles() {
  fParticles = new TObjArray(); // TClonesArray("KFParticle");
  fParticles->SetOwner(kTRUE);
  TObjectSet *part = new TObjectSet("KFTracks");
  part->SetObject(fParticles);
  AddData(part);
  TObjectSet *vert = new TObjectSet("KFVertices");
  fVertices = new TObjArray();
  fVertices->SetOwner(kFALSE);
  vert->SetObject(fVertices);
  AddData(vert);
  fNGoodGlobals = 0;
  if (mBeamLine) {AddBeamTrack();}
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  TrackNodeMap.clear();
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    Int_t kg = gTrack->key();
    fParticles->AddAtAndExpand(0, kg);
    if (gTrack->bad())            continue;
    if (gTrack->flag()     <   0) continue;     // Bad fit
    if (gTrack->flag()     > 700) continue;     // FTPC
    if (gTrack->flag()%100 == 11) continue;     // Short track pointing to EEMC
    //!    if ((gTrack->isWestTpcOnly() || gTrack->isEastTpcOnly()) && gTrack->isPostXTrack()) continue; // wrong TPC side track
    TrackNodeMap[kg] = node;
    KFParticle *particle = AddTrackAt(gTrack);
    if (! particle) continue;
    if (Debug() > 2) {
      LOG_INFO << Form("particle: %4i/%4i ",fNGoodGlobals,kg) << *particle << endm;
      //      LOG_INFO << "Add to map[" << kg << "] node = " << TrackNodeMap[kg] << endm;
    }
    fNGoodGlobals++;
  }
  return fNGoodGlobals;
}
//________________________________________________________________________________
Bool_t StKFVertexMaker::MakeV0(StPrimaryVertex *Vtx) {
  Bool_t ok = kTRUE;
  static const Double_t probCut = 1e-5;
#ifdef  __MakeV0__
  PrPP(MakeV0,*Vtx->parentMF()->kfParticle());
  KFVertex V(*Vtx->parentMF()->kfParticle());
  PrPP(MakeV0,V);
  StTrackNode* trackNode = Vtx->parentMF()->node();
  Int_t NoTracks =  Vtx->numberOfDaughters();
  if (NoTracks != 2) return kFALSE;
  if (V.GetQ() != 0) return kFALSE;
  enum {NoV0types = 4};   // gamma  K0s Lambda, LambdaBar
  Int_t pdgV0[NoV0types]   = {  22, 310, 3122, -3122};  // geantI: {gamma    1,K0s   16, Lambda 18,AntiLambda 26}
  Int_t pdgD[2][NoV0types] = {{ 11,-211, -211, -2212},  //         {electron 3,pion-  9, pion-   9,AntiProton 15}
                              {-11, 211, 2212,   211}}; //         {positron 2,pion+  8, Proton 14,pion+       8}
			      
  KFVertex *V0s[NoV0types]; memset(V0s, 0, NoV0types*sizeof(KFVertex *));   // 1c-fit
  KFVertex *V03s[NoV0types]; memset(V03s, 0, NoV0types*sizeof(KFVertex *)); // 3c-fit
  
  StTrackMassFit *mfs[2] = {Vtx->massFit(0), Vtx->massFit(1)};
  assert( mfs[0] && mfs[1]);
  KFParticle *particles[2];
  if (mfs[negative]->kfParticle()->GetQ() < 0) {
    particles[negative] = mfs[negative]->kfParticle();
    particles[positive] = mfs[positive]->kfParticle();
  } else {
    particles[positive] = mfs[negative]->kfParticle();
    particles[negative] = mfs[positive]->kfParticle();
  }
  //  Int_t ip = 0, in = 1;
  //  PrPP(MakeV0,particles[0]);
  //  PrPP(MakeV0,particles[1]);
  StTrack *trks[2] = {0,0};
  StTrack *ptrks[2] = {0,0};
  StTrackNode *Nodes[2] = {TrackNodeMap[particles[negative]->Id()], TrackNodeMap[particles[positive]->Id()]};
  for (Int_t m = 0; m < 2; m++) {
    if (! Nodes[m]) {
      cout << "Lost node for track " << *particles[m] << endl;
    }
  }
  if (! Nodes[0] || ! Nodes[1]) return ok;
  if (! Nodes[0] || ! Nodes[1]) return ok;
  for (Int_t m = 0; m < 2; m++) {
    trks[m] = Nodes[m]->track(global);
    ptrks[m] = Nodes[m]->track(primary); 
    if (! trks[m] || !  ptrks[m]) {
      cout << "Lost global/primary for track " << *particles[m] << endl;
      cout << *Nodes[m] << endl;
    }
  }
  if (! trks[0] || ! trks[1]) return ok;
  if (! ptrks[0] || ! ptrks[1]) return ok;
  StV0Vertex *V0Vx = 0;
  Int_t kg = fParticles->GetLast() + 1; // new track
  Int_t kgp = kg; // position in fParticle array
  Int_t flag = 0;
  for (Int_t l = 0; l < NoV0types; l++) {
    KFParticle pos = *particles[positive];
    KFParticle neg = *particles[negative];
    KFParticle *vDaughters[2] = {&neg, &pos};; // 0 -> negative, 1 -> positive
    for (Int_t k = 0; k < 2; k++) {
      vDaughters[k]->Create(
			    ((KFParticle *) particles[k])->Parameters(), 
			    ((KFParticle *) particles[k])->CovarianceMatrix(), 
			    particles[k]->Q(), 
			    TDatabasePDG::Instance()->GetParticle(pdgD[k][l])->Mass());
      vDaughters[k]->SetPDG(pdgD[k][l]);
      vDaughters[k]->SetId(particles[k]->Id());
      vDaughters[k]->SetParentID(particles[k]->GetParentID());
    }
    PrPP(MakeV0,pos);
    PrPP(MakeV0,neg);
    // 2c-Fit => Global V0 tracks
    KFParticle V0(V);
    V0.Construct((const KFParticle **) vDaughters,NoTracks,0,TDatabasePDG::Instance()->GetParticle(pdgV0[l])->Mass(),0);
    PrPP(MakeV0,V0);
    Double_t prob = TMath::Prob(V0.GetChi2(),V0.GetNDF());
    if (prob < probCut) continue;
    V0.SetPDG(pdgV0[l]);
    //    V0.SetId(kg);
    SafeDelete(V0s[l]); V0s[l] = new KFVertex(V0); PrPP(MakeV0,*V0s[l]);
    flag |= 1 << l;
    if (! V0s[l]) continue;
    StTrackMassFit *V0track = 0;
    // V0 track 
    V0track = new StTrackMassFit(V0s[l]->Id(),V0s[l]); 
    if (! trackNode) {
      trackNode = new StTrackNode;
      StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
      trNodeVec.push_back(trackNode);
      kgp++;
      TrackNodeMap[kgp] = trackNode;
    }
    trackNode->addTrack(V0track);
    UInt_t NoPV = pEvent->numberOfPrimaryVertices();
    for (Int_t ipv = 0; ipv <= NoPV; ipv++) {
      StPrimaryVertex *Vp = 0; 
      if (ipv > 0) {
	Vp = pEvent->primaryVertex(ipv-1);
	if (! Vp) continue;
      }
      if (Vp) {
	if (Vp == Vtx) continue;
	PrPP(Make,*Vp);
	KFVertex Parent;
	if (Vp->parentMF() && Vp->parentMF()->kfParticle()) {
	  Parent = *Vp->parentMF()->kfParticle(); 
	} else {
	  Float_t Param[6] = {Vp->position().x(), Vp->position().y(), Vp->position().z(), 0, 0, 0};
	  Float_t Cov[21] = {Vp->positionError().x()*Vp->positionError().x(),
			     0, Vp->positionError().y()*Vp->positionError().y(),
			     0, 0, Vp->positionError().z()*Vp->positionError().z(),
			     0, 0, 0, 999.,
			     0, 0, 0, 0, 999.,
			     0, 0, 0, 0, 0, 999.};
	  Int_t   charge = 0;
	  Float_t mass = 0;
	  Parent.Create(Param, Cov, charge, mass);
	}
	PrPP(MakeV0,Parent);
	KFVertex V02(*V0s[l]);
	V02.SetProductionVertex(Parent);
	PrPP(MakeV0,V02);
	Double_t prob2 = TMath::Prob(V02.GetChi2(),V02.GetNDF());
	if (prob2 < probCut) continue;
	SafeDelete(V03s[l]); 
	V03s[l] = new KFVertex(V02); 
	//      V03s[l]->SetId(kg); 
	V03s[l]->SetParentID(Vp->key());  PrPP(MakeV0,*V03s[l]);
#if 0
	// Refit primary vertex
	PrPP(MakeV0 before fit,Parent);
	Parent.AddDaughter(*V03s[l]);
	PrPP(MakeV0 after fit ,Parent);
	Double_t prob3 = TMath::Prob(Parent.GetChi2(),Parent.GetNDF());
	if (prob3 < probCut) continue;
#endif
	// V0 track 
	V0track = new StTrackMassFit(V03s[l]->Id(),V03s[l]); 
	V0track->kfParticle()->SetParentID(Vp->key());
	V0track->setEndVertex(Vtx);
#if 0
	PrPP(MakeV0 before,*Vp->parentMF());
	*Vp->parentMF() = StTrackMassFit(Parent.Id(),((KFParticle *) &Parent));
	PrPP(MakeV0 after,*VpBest->parentMF());
#endif
	Vp->addMassFit(V0track);
	PrPP(MakeV0,*Vp); 
	//    VpBest->Fit(); 
	SafeDelete(V0s[l]); 
	V0s[l] = V03s[l]; 
	V03s[l] = 0;
	if (! V0s[l]) continue;
	fVertices->AddLast(V0s[l]); 
	PrPP(MakeV0,*V0track);
	V0s[l]->SetId(V.Id());
	if (! trackNode) continue;
	trackNode->addTrack(V0track);
      } // fit to PV
      //      if (Vp) V0s[l]->TransportToDecayVertex();
      PrPP(MakeV0,*V0s[l]);
#define __MakeV0Vertex__
#ifdef __MakeV0Vertex__
      // Fill StV0Vertex
      V0Vx = new StV0Vertex(); 
      V0Vx->setParent(V0track);
      StThreeVectorF XVertex(V0s[l]->X(),V0s[l]->Y(),V0s[l]->Z());
      V0Vx->setKey(V.Id());
      V0Vx->setPosition(XVertex);
      V0Vx->setChiSquared(V0s[l]->Chi2()/V0s[l]->GetNDF());  
      V0Vx->setProbChiSquared(TMath::Prob(V0s[l]->GetChi2(),V0s[l]->GetNDF()));
      Float_t cov[6];
      TCL::ucopy(&V0s[l]->Covariance(0),cov,6);
      V0Vx->setCovariantMatrix(cov); 
      V0Vx->setFlag(flag); // Set default values, will use for kinematical ambiguities
      StSPtrVecV0Vertex& v0Vertices = pEvent->v0Vertices();
      v0Vertices.push_back(V0Vx);
      V0track->setEndVertex(V0Vx);
      V0Vx->setPosition(StThreeVectorF(V0s[l]->GetX(), V0s[l]->GetY(), V0s[l]->GetZ()));;
      V0Vx->addDaughter(new StPrimaryTrack(*((StPrimaryTrack *)Nodes[negative]->track(primary))));
      V0Vx->addDaughter(new StPrimaryTrack(*((StPrimaryTrack *)Nodes[positive]->track(primary))));
      V0Vx->setDcaDaughterToPrimaryVertex(positive,trks[negative]->impactParameter());
      V0Vx->setDcaDaughterToPrimaryVertex(negative,trks[positive]->impactParameter());
      //3VectorF vs 3VectorD???
      V0Vx->setMomentumOfDaughter(positive,StThreeVectorF(pos.GetPx(),pos.GetPy(),pos.GetPz()));
      V0Vx->setMomentumOfDaughter(negative,StThreeVectorF(neg.GetPx(),neg.GetPy(),neg.GetPz()));
      PrPP(MakeV0, *V0Vx);
#endif
    } // end loop over primary vertices
  } // end loop over NoV0types
#endif /*  __MakeV0__ */
  return ok;
}  
//________________________________________________________________________________
void StKFVertexMaker::PrimaryVertices() {
  Double_t TempLog = fTempLog; // default Temperature Log
  StKFVerticesCollection *allVertices = 0;
  Int_t LastGlobal = fParticles->GetLast();
  TArrayI Parents(LastGlobal+1);
  Int_t *parents = Parents.GetArray();
  memset(parents, 0, (LastGlobal+1)*sizeof(Int_t));
  for (fPass = 0; fPass < fNPasses; fPass++) {
    fVertexZPlot = fVertexZPlots[fPass]; 
    fgcVertices = PrimaryVertexSeeds(parents);
    if (! fgcVertices) break;;
    fgcVertices->DoTrack2VertexAssociation(*fParticles, parents);
    if (! fgcVertices->NoVertices()) {
      SafeDelete(fgcVertices);
      break;
    }
    if (AnnelingFcn(TMath::Exp(-TempLog)) <= 0) continue;
    if (fgcVertices->Vertices()->IsEmpty()) {
      SafeDelete(fgcVertices);
      continue;
    }
    fgcVertices->SetParents(parents);
    if (! allVertices) {allVertices = fgcVertices; fgcVertices = 0;}
    else {
      *allVertices += *fgcVertices;
      SafeDelete(fgcVertices);
    }
  }
  fgcVertices = allVertices;
  allVertices = 0;
  if (! fgcVertices) return ;
  if ( fgcVertices->IsEmpty()) {
    SafeDelete(fgcVertices); 
    return;
  }
  if (! fgcVertices) return;
  if ( fgcVertices->IsEmpty()) {SafeDelete(fgcVertices); return;}
  // Temperature => 1.
  Int_t NT = 5;
  Double_t dT = fTempLog/(NT-1);
  for (Int_t i = 0; i < NT; i++) {
    Double_t TempLog = fTempLog - i*dT;
    Double_t Temperature = TMath::Exp(TempLog);
    StAnneling::SetTemperature(Temperature);
    Double_t Chi2 =  fgcVertices->Fit();
    LOG_INFO << "StKFVertexMaker::AnnelingFcn\tTemperature = " << Temperature << " Chi2 = " << Chi2 
	     << " with " << fgcVertices->NoVertices() << " vertices" << endm;
  }
  fgcVertices->UniqueTracks2VertexAssociation();
}
//________________________________________________________________________________
StKFVerticesCollection *StKFVertexMaker::PrimaryVertexSeeds(Int_t *Parents) {
  Int_t LastGlobal = fParticles->GetLast();
  Int_t nAccepted = 0;
  Double_t dZ = fVertexZPlot->GetBinWidth(1);
  StKFVerticesCollection *VertexSeeds = 0;
  for (Int_t k = 1; k <= LastGlobal; k++) {
    KFParticle *particle = (KFParticle *) (*fParticles)[k];
    if (! particle) continue;
    //    PrPP2(Check,*particle);
    if (Parents[k]) continue;
    Float_t pT;
    Float_t dpT;
    particle->GetPt(pT,dpT);
    if (pT < 1e-4) continue;
    Double_t offset = 0.5*particle->GetPz()/pT;
    Double_t SigmaZ = TMath::Sqrt(particle->Covariance(2,2) + offset*offset);
    SigmaZ += dZ;
    Double_t Z = particle->GetZ();
    Int_t bin1 = fVertexZPlot->FindBin(Z - 5*SigmaZ);
    if (bin1 < 1) bin1 = 1;
    Int_t bin2 = fVertexZPlot->FindBin(Z + 5*SigmaZ);
    if (bin2 > fNzBins) bin2 = fNzBins;
    Double_t z = fVertexZPlot->GetBinCenter(bin1);
    for (Int_t bin = bin1; bin <= bin2; bin++, z += dZ) {
      fVertexZPlot->Fill(z,((TMath::Erfc((z - Z - fzWindow)/SigmaZ) - TMath::Erfc((z - Z + fzWindow)/SigmaZ)))/2.);
    }
    nAccepted++;
  }
  Double_t F = fVertexZPlot->GetEntries();
  if (F < 1) return VertexSeeds;
  TString opt("new");
  if (! Canvas()) opt = "goff";
  Int_t nfound = fSpectrum->Search(fVertexZPlot,-1,opt,TMath::Min(0.1,5./LastGlobal));
  if (! nfound) return VertexSeeds;
  if (Canvas()) {
    if (fStack) {
      Canvas()->cd();
      fStack->Draw("nostack");
      Canvas()->Update();
    }
  }
  if (StKFVertex::Debug()) {
    LOG_INFO << "Found " << nfound 
	     << " candidate peaks to fit from " << LastGlobal
	     << " good globals with " <<  nAccepted  << " accepted tracks" << endm;
  }
  Double_t *zOfPeaks = new Double_t[nfound];
  Int_t npeaks = 0;
  Float_t *xpeaks = fSpectrum->GetPositionX();
  Float_t xp = 0;
  for (Int_t p = 0; p < nfound; p++) {
    xp = xpeaks[p];
    Int_t bin = fVertexZPlot->GetXaxis()->FindBin(xp);
    Double_t yp = fVertexZPlot->GetBinContent(bin);
    Double_t ep = fVertexZPlot->GetBinError(bin);
    if (yp-1.25*ep < 0) continue;
    zOfPeaks[npeaks] = xp;
    npeaks++;
  }
  if (StKFVertex::Debug()) {
    LOG_INFO << "Found " << npeaks << " useful peaks to fit" << endm;
  }
  if (! npeaks) {delete [] zOfPeaks;  zOfPeaks = 0; return VertexSeeds; }
  St_vertexSeedC *vSeed = 0;
  if (mBeamLine) vSeed = St_vertexSeedC::instance();
  VertexSeeds = new StKFVerticesCollection(npeaks, zOfPeaks, 1.5, 1, vSeed);
  delete [] zOfPeaks; zOfPeaks = 0;
  return VertexSeeds;
}
//________________________________________________________________________________
void StKFVertexMaker::ReFitToVertex() {
  Int_t NoVertices = fgcVertices->Vertices()->GetSize();
  for (Int_t l = fgcVertices->Vertices()->GetSize() - 1; l >= 0; l--) {
    StKFVertex *V = (StKFVertex *) fgcVertices->Vertices()->At(l);
    if (! V) continue;
    Bool_t ok = kTRUE;
    Int_t NoTracks = V->NoTracks();
    KFVertex     &KVx = V->Vertex();
    if (NoTracks <= 1) ok = kFALSE;
    else {
      if (KVx.Covariance(0) < 0 ||
	  KVx.Covariance(2) < 0 ||
	  KVx.Covariance(5) < 0) ok = kFALSE;
    }
    if (! ok) {
      delete fgcVertices->Vertices()->Remove(V);
      continue;
    }
    // Store vertex
    StPrimaryVertex *primV  = new StPrimaryVertex;
    StThreeVectorF XVertex(&KVx.X());
    primV->setKey(V->ID());
    primV->setPosition(XVertex);
    primV->setChiSquared(KVx.Chi2()/KVx.GetNDF());  
    primV->setProbChiSquared(TMath::Prob(KVx.GetChi2(),KVx.GetNDF()));
    primV->setIdTruth(KVx.IdTruth(), KVx.QaTruth());
    Float_t cov[6];
    TCL::ucopy(&KVx.Covariance(0), cov, 6);
    primV->setCovariantMatrix(cov); 
    primV->setVertexFinderId(KFVertexFinder);
    primV->setFlag(1); // Set default values
    primV->setRanking(333);
    primV->setNumTracksUsedInFinder(V->NoTracks());
    Bool_t beam = kFALSE;
    StiHit *Vertex = StiToolkit::instance()->getHitFactory()->getInstance();
    Vertex->setGlobal(0, 0, KVx.X(), KVx.Y(), KVx.Z(), 0);
    Vertex->setError(cov);

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
      nodes[itk] = TrackNodeMap[kg];
      if (! nodes[itk]) {
	nodes[itk] = new StTrackNode;
	StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
	trNodeVec.push_back(nodes[itk]);
	TrackNodeMap[kg] = nodes[itk];
      }
      if (P.GetQ()) {
	pTracks[itk] =  FitTrack2Vertex(V, track);
#if 0
	if (! pTracks[itk]) {
	  delete V->Remove((StKFTrack*) track);
	}
#endif
      }
    }
    if (beam ) primV->setBeamConstrained();
    //..... add vertex to the list
    UInt_t NoPrTracks = 0;
    for (Int_t i = 0; i < NoTracks; i++) {if (pTracks[i]) NoPrTracks++;}
    UInt_t NoPrTracksB = NoPrTracks;
    if (beam) NoPrTracksB++;
    if (NoPrTracksB < 2) {
      for (UInt_t i = 0; i < NoTracks; i++) {
	StPrimaryTrack *t = pTracks[i];
	if (! t) continue;
	StTrackNode *n = t->node();
	n->removeTrack(t);
	pTracks[i] = 0;
      }
      SafeDelete(primV);
      delete fgcVertices->Vertices()->Remove(V);
    } else {
      // copy Point fit as MassFit
      StTrackMassFit *pf = new StTrackMassFit(KVx.Id(),&KVx);
      PrPP(ReFitToVertex,*pf);
      primV->setParent(pf);
      StTrackNode *nodepf = new StTrackNode;
      nodepf->addTrack(pf);
      StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
      trNodeVec.push_back(nodepf);
      for (UInt_t i = 0; i < NoTracks; i++) {
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
      fVertices->AddLast(new KFVertex(KVx)); //<<<<<<<<<<<<<<<< ????????
    }
    delete [] tracks;   
    delete [] pTracks;
    delete [] nodes;
  }
}
//________________________________________________________________________________
void StKFVertexMaker::SecondaryVertices() {
  static const Float_t MinimumDistance = 5.0; 
  Int_t LastGlobal = fParticles->GetLast();
  Double_t TempLog = 0;
  Double_t Temperature = TMath::Exp(TempLog);
  StAnneling::SetTemperature(Temperature);
  // secondary vertices
  fPass = fNPasses;
  StKFVerticesCollection *SecondaryVertices = new StKFVerticesCollection();
  StKFTrack *track = 0;
  for (Int_t k = 1; k < LastGlobal; k++) {
    KFParticle *particleK = (KFParticle *) (*fParticles)[k];
    if (! particleK) continue;
    //    if (Parents[k]) continue;
    if (particleK->GetParentID()) continue;
    StKFVertex *vtx = 0;
    PrPP2(Fit,*particleK);
    KFVertex particleV;
    for (Int_t l = k+1; l <= LastGlobal; l++) {
      KFParticle *particleL = (KFParticle *) (*fParticles)[l];
      if (! particleL) continue;
      //      if (Parents[l]) continue;
      if (particleL->GetParentID()) continue;
      PrPP2(Fit,*particleL);
      // Check consistency with others
      if (! vtx) {
	Float_t distance = particleL->GetDistanceFromParticle(*particleK);
	if (distance > MinimumDistance) continue;
	const KFParticle *vDaughters[2] = {particleK, particleL};
	KFVertex tempV;
	tempV.Construct(vDaughters,2); PrPP2(Fit,tempV);
	Double_t prob = TMath::Prob(tempV.GetChi2(),tempV.GetNDF());
	if (prob < 1e-5) continue;
	particleV = tempV;	PrPP2(Fit,particleV);
	// Create new Vertex 
	vtx = new StKFVertex(); PrPP2(newvtx, *vtx);
	particleK->SetId(k);
	track = new  StKFTrack(particleK); 
	track->SetChi2(1.);
	vtx->AddTrack(track);
      } else {
	Float_t xyzV[3] = {particleV.GetX(), particleV.GetY(), particleV.GetZ()};
	Float_t distance = particleL->GetDistanceFromVertex(xyzV);
	if (distance > MinimumDistance) continue;
	KFVertex tempV = particleV;
	tempV += *particleL;   PrPP2(Fit,tempV);
	Double_t prob = TMath::Prob(tempV.GetChi2(),tempV.GetNDF());
	if (prob < 1e-5) continue;
	particleV = tempV;
      }
      particleL->SetId(l);
      track = new  StKFTrack(particleL);
      track->SetChi2(1.);
      vtx->AddTrack(track);
    }
    if (! vtx) continue; 
    Int_t ID = vtx->Vertex().Id();
    KFVertex &V = vtx->Vertex();
    V = particleV;  
    vtx->Vertex().SetId(ID);
    vtx->UpdateVertex2TrackChi2(); PrPP(Initiate,*vtx);
    vtx->Fit();

    PrPP(Fit,*vtx);
    Int_t N = vtx->NoTracks();
    if (N <= 1) {SafeDelete(vtx); continue;}
    Double_t X = vtx->Vertex().X();
    Double_t Y = vtx->Vertex().Y();
    Double_t R = TMath::Sqrt(X*X + Y*Y);
    if (R > 200 ) {SafeDelete(vtx); continue;}
    SecondaryVertices->AddVertex(vtx);
#if 0 /* ??? */
    Double_t prob = TMath::Prob(vtx->Vertex().GetChi2(),vtx->Vertex().GetNDF());
    if (N > 2 || prob > 1.e-5) {// Allow V2 to share tracks
      TIter next(&vtx->Tracks());
      StKFTrack *Track = 0;
      while ((Track = (StKFTrack *) next())) {
	KFParticle *particle = (KFParticle *) Track->OrigParticle();;
	Int_t k = Track->K();
      }
    }
#endif 
  }
  Int_t No = SecondaryVertices->NoVertices();
  if ( No ) {
    LOG_INFO << "Candidates for secondary vertices: " << No << endm;
    SecondaryVertices->UpdateWeights();
    SecondaryVertices->UniqueTracks2VertexAssociation();
    //       SecondaryVertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
    // 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
    // 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
    if (fgcVertices) {*fgcVertices += *SecondaryVertices;}
    else             { fgcVertices  =  SecondaryVertices;  SecondaryVertices = 0;}
  }
  SafeDelete(SecondaryVertices);
  // end of loop for secondary vertices
}
//________________________________________________________________________________
void StKFVertexMaker::UpdateParticleAtVertex(StiKalmanTrack *kTrack,KFParticle *particle) {
  StiKalmanTrackNode *extended = 0;
  if (kTrack) extended = kTrack->getInnerMostHitNode(3);
  if (! extended) {
    if (StKFVertex::Debug() > 2) {
      cout << "StKFVertexMaker::UpdateParticleAtVertex extention to InnerMostNdode failed" << endl;
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
// $Log: StKFVertexMaker.cxx,v $
// Revision 2.7  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.7  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 1.3  2014/01/14 14:49:17  fisyak
// Freeze
//
// Revision 1.2  2013/10/16 13:19:15  fisyak
// Add beam line position to PV guess, add Z error in beam track, relax requirements on vertex seed
//
// Revision 1.1.1.1  2013/08/13 22:20:41  fisyak
// Save m version
//
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
