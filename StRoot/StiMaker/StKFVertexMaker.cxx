// $Id: StKFVertexMaker.cxx,v 2.8 2018/04/10 11:32:09 smirnovd Exp $
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
#include "KFParticle.h"
#include "KFVertex.h"
#include "MTrack.h"
#include "VVertex.h"
#include "TH1K.h"
#include "StAnneling.h"
#include "StKFEvent.h"
#include "StKFTrack.h"
#include "StKFVertex.h"
#include "StKFVerticesCollection.h"
#include "StVertexP.h"
#include "StVertexT.h"
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
#include "Sti/StiToolkit.h"
#include "TArrayI.h"

StKFVerticesCollection *StKFVertexMaker::fcVertices = 0;
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
  SafeDelete(fParticles);
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
  fParticles->Clear("C");
}
//________________________________________________________________________________
StKFVertexMaker::StKFVertexMaker(const char *name) : StMaker(name),
						     fNzBins(2500),  fNPasses(2), fSpectrum(0), fzWindow(2), 
						     fVtxM(0), fVerticesPass(0), fTempLog(2), fminBrent(0), func(0),
						     mBeamLine(kFALSE), fc1(0)
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
  fParticles = new TObjArray();
  fParticles->SetOwner(kTRUE);
  mVertexOrderMethod = orderByRanking; // change ordering by ranking
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::Init(){
  mBeamLine = IAttr("beamLine");
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StKFVertexMaker::InitRun(Int_t runumber){
   return StMaker::InitRun(runumber);
}
//________________________________________________________________________________
Int_t StKFVertexMaker::Make() {
  StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StKFVertexMaker::fit: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  Double_t bField = 0;
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();
  KFParticle::SetField(bField);
  if (mBeamLine) {
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
    KFParticle *beam = new KFParticle(track, 321);
    fParticles->AddAt(beam, 0);
  }
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  Int_t NGoodGlobals = 0;
  map<Int_t,StTrackNode*> TrackNodeMap;
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    const StDcaGeometry* dca = gTrack->dcaGeometry();
    if (! dca) continue;
    if (gTrack->flag()     <   0) continue;     // Bad fit
    if (gTrack->flag()     > 700) continue;     // FTPC
    if (gTrack->flag()%100 == 11) continue;     // Short track pointing to EEMC
    if ((gTrack->isWestTpcOnly() || gTrack->isEastTpcOnly()) && gTrack->isPostXTrack()) continue; // wrong TPC side track
    Int_t kg = gTrack->key();
    TrackNodeMap[kg] = node;
    KFParticle *particle = AddTrackAt(dca,kg);
    if (Debug() > 1) {
      if (Debug() > 2) {LOG_INFO << Form("particle: %4i/%4i ",NGoodGlobals,kg) << *particle << endm;}
      LOG_INFO << "Add to map[" << kg << "] node = " << TrackNodeMap[kg] << endm;
    }
    NGoodGlobals++;
  }
  if (NGoodGlobals < 2) return 0;
  Fit();
  if (! Vertices()) return 0;
  //
  //  In case there are no tracks left we better quit
  //
  StSPtrVecTrackDetectorInfo& detInfoVec = pEvent->trackDetectorInfo();
  Int_t Nvtx = Vertices()->NoVertices();
  for (Int_t l = 0; l < Nvtx; l++) {
    const StKFVertex *V = Vertices()->Vertex(l);
    if (! V) continue;
    if (Debug() > 2) V->PrintW();
    // Store vertex
    StPrimaryVertex *primV  = new StPrimaryVertex;
    StThreeVectorF XVertex(&V->Vertex().X());
    primV->setPosition(XVertex);
    primV->setChiSquared(V->Vertex().Chi2()/V->Vertex().GetNDF());  
    primV->setProbChiSquared(TMath::Prob(V->Vertex().GetChi2(),V->Vertex().GetNDF()));
    Float_t cov[6];
    TCL::ucopy(&V->Vertex().Covariance(0),cov,6);
    primV->setCovariantMatrix(cov); 
    primV->setVertexFinderId(KFVertexFinder);
    primV->setFlag(1); // was not set earlier by this vertex finder ?? Jan
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
    Int_t NoTracks = V->NoTracks();
    TArrayI indexT(NoTracks); Int_t *indexes = indexT.GetArray();
    TArrayI IdT(NoTracks);    Int_t *Ids     = IdT.GetArray();
    for (Int_t itk = 0; itk < NoTracks; itk++) {
      Ids[itk] = 999999;
      const StKFTrack*   track = V->Track(itk);
      if (! track) continue;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.GetID()%100000;
      Ids[itk] = kg;
    }
    TMath::Sort(NoTracks,Ids,indexes,0);
    for (Int_t i = 0; i < NoTracks; i++) {
      Int_t itk = indexes[i];
      const StKFTrack*   track = V->Track(itk);
      if (! track) continue;
      const KFParticle   &P = track->Particle();
      Int_t kg = P.GetID()%100000;
      if (kg == 0) {
	assert(!beam);
	beam = kTRUE;
	continue;
      }
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
      node = TrackNodeMap[kg];
      if (! node) {
	LOG_INFO << "Missing node in map[" << kg << "] node = " << TrackNodeMap[kg] << endm;
	assert(node);
      }
      StiKalmanTrack* kTrack = (*StiStEventFiller::Node2TrackMap())[node];
      assert(kTrack);
      StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
      assert(gTrack);
#ifdef ADD_NEW_NODE
      // Replace dca node by a primary vertex
      StiKalmanTrackNode *tNode = kTrack->getInnerMostNode();
      if (! tNode->isDca()) continue;
#if 1
      //      tNode->print("XYZEPIJK");
#endif
      tNode->rotate(-tNode->getAlpha());
      //      StiHit localVertex = *Vertex;
      //      localVertex.rotate(tNode->getAlpha());
      //      tNode->setHit(&localVertex);
      tNode->setHit(Vertex);
      tNode->setDetector(0);
      Double_t Phi, dPhi;
      P.GetPhi(Phi,dPhi);
      Double_t pT, dpT;
      P.GetPt(pT,dpT);
      StiNodePars &FP = tNode->fitPars();
      FP.x()     = P.GetX();  
      FP.y()     = P.GetY();         //  local Y-coordinate of this track (reference plane)           		     
      FP.z()     = P.GetZ();         //  local Z-coordinate of this track (reference plane)			     
      FP.eta()   = Phi - tNode->getAlpha(); //  (signed curvature)*(local Xc of helix axis - X current point on track)
      FP.ptin()  = - P.GetQ()/pT; //  signed invert pt [sign = sign(-qB)]					     
      FP.tanl()  = P.GetPz()/pT; //  tangent of the track momentum dip angle
      //      FP.curv()  = FP.hz()/FP.ptin(); //  signed curvature [sign = sign(-qB)]					     
      FP.ready();
      //      FP.hz()    = 0; //  Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)                  
      Double_t pzpT3 = - P.GetPz()/(pT*pT*pT);
      Double_t f[6*6] = {
	/*            x, y, z,                                   pX,                                   pY,     pZ  */
	/* x    */    1, 0, 0,                                    0,                                    0,      0,
	/* y    */    0, 1, 0,                                    0,                                    0,      0,
	/* z    */    0, 0, 1,                                    0,                                    0,      0,
	/* eta  */    0, 0, 0,                    P.GetPy()/(pT*pT),                   -P.GetPx()/(pT*pT),      0,
	/* q/pT */    0, 0, 0, -FP.ptin()*P.GetPx()/(pT*pT), -FP.ptin()*P.GetPy()/(pT*pT),      0,
	/* tanL */    0, 0, 0,                      pzpT3*P.GetPx(),                      pzpT3*P.GetPy(),  1./pT};
      TRMatrix F(6,6,f); if (Debug()) {LOG_INFO << "F\t" << F << endm;}
      TRSymMatrix CovP(6,&((KFParticle *)&P)->Covariance(0)); if (Debug()) {LOG_INFO << "CovP\t" << CovP << endm;}
      TRSymMatrix Covi(F,TRArray::kAxSxAT,CovP); if (Debug()) {LOG_INFO << "Covi\t" << Covi << endm;}
      StiNodeErrs  &FE = tNode->fitErrs(); 
      TCL::ucopy(Covi.GetArray(), FE.A, 21);
      tNode->setReady();
#if 0
      //      tNode->print("XYZEPIJK");
      StiKalmanTrackNode *test = kTrack->getInnerMostHitNode();
      assert(test == tNode);
      //      Int_t status = kTrack->refit(); // refit with primary vertex
      Int_t status = kTrack->fit(kInsideOut);
      if (status) continue; // failed to refit
#endif
      kTrack->setPrimary(l+1);
#else  /* ! ADD_NEW_NODE */
      StiKalmanTrackNode *extended = (StiKalmanTrackNode*) kTrack->extendToVertex(Vertex);
      if (extended) {
        kTrack->add(extended,kOutsideIn);
        if (extended && !extended->isValid()) 		extended=0;
        if (extended && extended->getChi2()>1000) 	extended=0;
      }
      kTrack->reduce();
      if (! extended) continue;
      //?      kTrack->add(extended,kOutsideIn);
      kTrack->setPrimary(l+1);
      extended->setUntouched();
      Int_t ifail = kTrack->refit();
      ifail |= (kTrack->getInnerMostHitNode(3)!=extended);
      kTrack->reduce();
      // something is wrong. It is not a primary
      if (ifail) { 
	kTrack->removeLastNode(); 
	kTrack->setPrimary(0); 
	continue;
      }
#endif /* ADD_NEW_NODE */
      //________________________________________________________________________________
      StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
      StiStEventFiller::instance()->fillDetectorInfo(detInfo,kTrack,kFALSE); //3d argument used to increase/not increase the refCount. MCBS oct 04.
      //      StiStEventFiller::instance()->fillPulls(kTrack,1); 
      StPrimaryTrack* pTrack = new StPrimaryTrack;
      node->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
      pTrack->setKey( gTrack->key());
      pTrack->setFlagExtension( gTrack->flagExtension());
      StiStEventFiller::instance()->fillTrack(pTrack,kTrack, detInfo);
      // set up relationships between objects
      detInfoVec.push_back(detInfo);
      primV->addDaughter(pTrack);
      //________________________________________________________________________________      
    }
    if (beam ) primV->setBeamConstrained();
    //..... add vertex to the list
    if (primV->numberOfDaughters() < 1) {
      delete primV;
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
  // It is unclear why the next line was uncommented but apparently it is not
  // used. So, either this line or the semicolumn at the end of the preceeding
  // line should be removed
  //-     Wveto*(primV->numNotMatchesWithBEMC() + primV->numNotMatchesWithEEMC());
  if (primV->numTracksTpcWestOnly() > 0 && primV->numTracksTpcEastOnly() > 0) 
    rank += Wmatch*TMath::Min(primV->numTracksTpcWestOnly(),primV->numTracksTpcEastOnly());
  primV->setRanking(rank); 
  if (Debug()) primV->Print();
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StDcaGeometry *dca, Int_t kg) {
  fParticles->AddAtAndExpand (0,kg);
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
  KFParticle *particle = new KFParticle(track, pdg);
  particle->SetID(kg);
  fParticles->AddAt(particle,kg);
  return particle;
}
//________________________________________________________________________________
void StKFVertexMaker::Fit() {
  if (Debug() != 2)  StKFVertex::SetDebug(Debug());
  fcVertices = 0;
  for (Int_t i = 0; i < fNPasses+1; i++) {
    SafeDelete(fVerticesPass[i]);
  }
  Int_t NGoodGlobals = Particles().GetLast();
  
  Double_t TempLog = fTempLog; // default Temperature Log
  for (Int_t pass = 0; pass < fNPasses; pass++) {
    Int_t nAccepted = 0;
    Double_t dZ = fVtxs[pass]->GetBinWidth(1);
    for (Int_t k = 0; k < NGoodGlobals; k++) {
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
    Int_t nfound = fSpectrum->Search(fVtx,3,opt,TMath::Min(0.1,5./NGoodGlobals));
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
	   << " candidate peaks to fit with " << NGoodGlobals
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
    if (! npeaks) {delete [] zOfPeaks; break; }
    if (fVerticesPass[pass]) {delete fVerticesPass[pass]; fVerticesPass[pass] = 0;}
    fVerticesPass[pass] = new StKFVerticesCollection(npeaks, zOfPeaks);
    delete [] zOfPeaks;
    fcVertices = fVerticesPass[pass];
    fcVertices->DoTrack2VertexAssociation(Particles());
    if (! fcVertices->NoVertices())                         continue;
    if (AnnelingFcn(TMath::Exp(-TempLog)) <= 0) continue;
    if (! fcVertices->NoVertices())                         continue;
    fcVertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
    //       fcVertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
    // 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
    // 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
  }
  if (! fVerticesPass[0]) return;
  if (fNPasses > 1 && Canvas()) {
    Canvas()->cd();
    fVtxs[1]->Draw("same"); 
    Canvas()->Update();
  }
  Int_t N1 = fVerticesPass[0]->NoVertices();
  if (! N1) return;
  if (fVerticesPass[1]) {
    *fVerticesPass[0] += *fVerticesPass[1];
  }
  fcVertices = fVerticesPass[0];
  fcVertices->MergeDuplicatedVertices();
  if (! fcVertices->NoVertices()) return;
  // Double_t Temperature = TMath::Exp(TempLog);
  TempLog = 5;
  Double_t Temperature = TMath::Exp(TempLog);
#if 1  
  // secondary vertices
  Int_t pass = fNPasses;
  if (fVerticesPass[pass]) {delete fVerticesPass[pass]; fVerticesPass[pass] = 0;}
  fVerticesPass[pass] = new StKFVerticesCollection();
  fcVertices = fVerticesPass[pass];
  StAnneling::SetTemperature(Temperature);
  for (Int_t k = 1; k < NGoodGlobals; k++) {
    KFParticle *particleK = (KFParticle *) Particles()[k];
    if (! particleK) continue;
    if (particleK->GetID() > 100000) continue;
    StKFVertex *vtx = 0;
    for (Int_t l = k+1; l < NGoodGlobals; l++) {
      KFParticle *particleL = (KFParticle *) Particles()[l];
      if (! particleL) continue;
      if (particleL->GetID() > 100000) continue;
      Double_t dist = particleK->GetDistanceFromParticle(*particleL);
      if (dist > 5.0) continue;
      if (! vtx) {
	vtx = new StKFVertex(fcVertices->NoVertices() + 1);
	vtx->AddTrack(new StKFTrack(k,particleK));
      }
      vtx->AddTrack(new StKFTrack(l,particleL));
    }
    if (! vtx) continue;
    vtx->Fit();
    Int_t N = vtx->NoTracks();
    if (! N) {delete vtx; vtx = 0; continue;}
    Double_t X = vtx->Vertex().X();
    Double_t Y = vtx->Vertex().Y();
    Double_t R = TMath::Sqrt(X*X + Y*Y);
    if (R > 200 ) {delete vtx; vtx = 0; continue;}
    Double_t prob = TMath::Prob(vtx->Vertex().GetChi2(),vtx->Vertex().GetNDF());
    if (N > 2 || prob > 1.e-3) {// Allow V2 to share tracks
      for (Int_t i = 0; i < N; i++) {
	KFParticle *particle = vtx->Track(i)->OrigParticle();;
	Int_t ID = particle->GetID()%100000 + 100000*vtx->ID();;
	particle->SetID(ID);
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
    *fVerticesPass[0] += *fVerticesPass[fNPasses];
  }
  // end of loop for secondary vertices
#endif
  fcVertices = fVerticesPass[0];
  fcVertices->Compress();
  if (! fcVertices->NoVertices()) return;
  fcVertices->MergeDuplicatedVertices();
  fminBrent->SetFunction(*func,TMath::Exp(-0.5*(TempLog)),TMath::Exp(-TempLog),1);
  if (! fminBrent->Minimize(10,0.1,0.1)) {
    LOG_WARN << "Temperature fit has failed" << endm;
    Temperature = 1;
  } else {
    Temperature = 1./fminBrent->XMinimum();
  }
  StAnneling::SetTemperature(Temperature);
  fcVertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
  fcVertices->Fit(29,Canvas(),fVtx);
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
// $Log: StKFVertexMaker.cxx,v $
// Revision 2.8  2018/04/10 11:32:09  smirnovd
// Minor corrections across multiple files
//
// - Remove ClassImp macro
// - Change white space
// - Correct windows newlines to unix
// - Remove unused debugging
// - Correct StTpcRTSHitMaker header guard
// - Remove unused preprocessor directives in StiCA
// - Minor changes in status and debug print out
// - Remove using std namespace from StiKalmanTrackFinder
// - Remove includes for unused headers
//
// Revision 2.7  2016/06/08 23:32:46  smirnovd
// Integration of StiCA
//
// This is a squashed commit with all changes combined. To see individual
// modifications check out the ds-StiCA_2016 branch in star-sti repository.
// Alternatively, one can explore the StiCA_2016 branch in the STAR's CVS
// repository.
//
// Revision 2.6  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
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
