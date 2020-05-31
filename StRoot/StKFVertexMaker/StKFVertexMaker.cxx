// $Id: StKFVertexMaker.cxx,v 2.7 2015/12/20 01:06:39 fisyak Exp $
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TMath.h"
#include "TH1.h"
#include "TEnv.h"
#include "TCanvas.h"
#include "StDcaGeometry.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFPTrack.h"

#include "StAnneling.h"
#include "StKFTrack.h"
#include "StKFVertex.h"
#include "StKFVerticesCollection.h"
#include "TDirectory.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTrackMassFit.h"
#include "Stypes.h"
#include "SystemOfUnits.h"
#include "StKFVertexMaker.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
#include "TDatabasePDG.h"
#include "TParticlePDG.h"
#include "TArrayF.h"
#include "TArrayI.h"
#include "TMVA/Reader.h"
#include "StTMVARank/TMVAdata.h"
#include "StTMVARank/TMVArank.h"
#include "StTMVARank/StTMVARanking.h"
#include "StarMagField/StarMagField.h"
#include "StGenericVertexMaker/StGenericVertexFinder.h"
#include "StGenericVertexMaker/StFixedVertexFinder.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
using namespace TMVA;
ClassImp(StKFVertexMaker);
#ifdef StTrackMassFit_hh
#endif
StKFVerticesCollection *StKFVertexMaker::fgcVertices = 0;
#define PrP(A,B)                    {LOG_INFO << "StKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP(A,B)  if (Debug() > 1) {LOG_INFO << "StKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrPP2(A,B) if (Debug() > 2) {LOG_INFO << "StKFVertexMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#define PrParticle2(A) if (Debug() > 2) {cout << "StKFVertexMaker::" << (#A)  << endl; PrintParticles();}
Double_t StKFVertexMaker::fgProbCut = 1e-5;
map<Int_t,StTrackNode*> StKFVertexMaker::fTrackNodeMap;
/* Bookkeeping: 
   kg =  gTrack->key(); 
   TObjArray *fParticles; // KF particles
   fParticles[kg], kg = 0 -> beam line, kg > 0 kg == gTrack->key();
   StKFVerticesCollection *fgcVertices;  // current vertex collection
   fVertices        
   
*/
//________________________________________________________________________________
StKFVertexMaker::StKFVertexMaker(const Char_t *name) : StMaker(name),
						     fParticles(0), fVertices(0),
						     fPass(0), fNzBins(2500),  fNPasses(2), fSpectrum(0), fzWindow(2), 
#if 1
						     fTempLog(2), fminBrent(0), func(0),
#else
						     fTempLog(0), fminBrent(0), func(0),
#endif
						     fVertexZPlot(0), fStack(0), mBeamLine(kFALSE), fc1(0) , pEvent(0)
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
  if (fLastGlobalId < kg) fLastGlobalId = kg;
  return particle;
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const KFParticle *particleO, Int_t kg) {
  if (! particleO) return 0;
  KFParticle *particle =  new KFParticle(*particleO);
  fParticles->AddAtAndExpand(particle, kg);
  if (fLastGlobalId < kg) fLastGlobalId = kg;
  return particle;
}
//________________________________________________________________________________
KFParticle *StKFVertexMaker::AddTrackAt(const StGlobalTrack *gTrack) {
  if (! gTrack) return 0;
  Int_t kg = gTrack->key();
  KFParticle *particle = 0;
  const StTrackNode *node = gTrack->node();
  if (! node) return 0;
  StTrackMassFit *mf = (StTrackMassFit *) node->track(massFit,0);
  if (mf) {
    mf->setLength(gTrack->length());
    particle = AddTrackAt(mf->kfParticle(),kg);
  } 
  if (! particle) {
    const StDcaGeometry* dca = gTrack->dcaGeometry();
    if (! dca) return 0;
    particle = AddTrackAt(dca,kg);
    if (particle) {
      particle->SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
      particle->SetIdParentMcVx(gTrack->idParentVx());
      UShort_t nfitp = gTrack->fitTraits().numberOfFitPoints();
      Double_t chi2OverNdf = gTrack->fitTraits().chi2(0);
      particle->NDF()  = 2*nfitp - 5;
      particle->Chi2() = chi2OverNdf*particle->NDF();
    }
  }
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
void StKFVertexMaker::ClearParentIDs() {
  UInt_t N = fParticles->GetSize();
  for (UInt_t k = 0; k < N; k++) {
    KFParticle *particle = (KFParticle *) fParticles->UncheckedAt(k);
    if (particle) {
      PrPP2(ClearParentIDs before, *particle);
      particle->SetParentID();
      PrPP2(ClearParentIDs  after, *particle);
    }
  }
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
#if 0
  SecondaryVertices();
#endif
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
Int_t StKFVertexMaker::Make() {
  pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (! pEvent) {
    LOG_WARN << "StKFVertexMaker::fit: no StEvent " << endm;
    return kStOK;        // if no event, we're done
  }
  StKFVertex::ResetTotalNoVertices();
  Double_t bField = 0;
  if (pEvent->runInfo()) bField = pEvent->runInfo()->magneticField();
  KFParticle::SetField(bField);
  MakeParticles();
  if (fNGoodGlobals < 1) return kStOK;
  // add Fixed Primary vertex if any
  if (IAttr("VFFV") || IAttr("VFMCE") || IAttr("VFMinuitX") ) {
    StGenericVertexFinder *mGVF = 0;
    StGenericVertexMaker* gvm = (StGenericVertexMaker*)StMaker::GetChain()->GetMaker("GenericVertex");
    if (gvm) mGVF = gvm->GetGenericFinder();
    if (mGVF) {
      if (IAttr("VFFV") || IAttr("VFMCE")) {
	((StFixedVertexFinder *)mGVF)->SetVertexError(gEnv->GetValue("FixedSigmaX", 0.0176),
						      gEnv->GetValue("FixedSigmaY", 0.0176),
						      gEnv->GetValue("FixedSigmaZ", 0.0176));
      } 
      if (IAttr("VFMinuitX")) {
	fgcVertices = new StKFVerticesCollection();
	mGVF->fit(pEvent);
	mGVF->FillStEvent(pEvent);
	UInt_t NoPV = pEvent->numberOfPrimaryVertices();
	if (! NoPV) return kStOK;
	for (UInt_t iv = 0; iv < NoPV; iv++) {
	  StPrimaryVertex *primV = pEvent->primaryVertex(iv);
	  if (! primV) continue;
	  primV->setRanking(primV->numTracksUsedInFinder()); 
	}
	pEvent->sortVerticiesByRank();
	for (UInt_t iv = 0; iv < NoPV; iv++) {
	  StPrimaryVertex *primV = pEvent->primaryVertex(iv);
	  if (primV && ! primV->key()) {
	    primV->setKey(iv+1);
	  }
	  primV->setIdTruth();
	  StKFVertex KVx;
	  KVx.Initialize();
	  KVx.SetId(primV->key());
	  TCL::ucopy(primV->position().xyz(), &KVx.Parameter(0), 3);
	  TCL::ucopy(primV->covariance(), &KVx.Covariance(0), 6);
	  KVx.NDF() = 1;
	  KVx.SetIdTruth(primV->idTruth(),primV->qaTruth());
	  // copy Point fit as MassFit
	  StTrackMassFit *pf = new StTrackMassFit(KVx.Id(),&KVx);
	  PrPP(Make,*pf);
	  primV->setParent(pf);
	  fgcVertices->AddVertex(&KVx);
	  StTrackNode *nodepf = new StTrackNode;
	  nodepf->addTrack(pf);
	  StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
	  trNodeVec.push_back(nodepf);
	}
	ReFitToStVertex();
	pEvent->sortVerticiesByRank();
      }
    }
  } else {
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
    fgcVertices->Print();
  }
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
  fLastGlobalId = 0;
  if (mBeamLine) {AddBeamTrack();}
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node=0;
  fTrackNodeMap.clear();
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; 
    if (!node) continue;
    StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    Int_t kg = gTrack->key();
    //    fParticles->AddAtAndExpand(0, kg);
    if (gTrack->bad())            continue;
    if (gTrack->flag()     <   0) continue;     // Bad fit
    if (gTrack->flag()     > 700) continue;     // FTPC
    if (gTrack->flag()%100 == 11) continue;     // Short track pointing to EEMC
    //!    if ((gTrack->isWestTpcOnly() || gTrack->isEastTpcOnly()) && gTrack->isPostXTrack()) continue; // wrong TPC side track
    fTrackNodeMap[kg] = node;
    KFParticle *particle = AddTrackAt(gTrack);
    if (! particle) continue;
    if (Debug() > 2) {
      LOG_INFO << Form("particle: %4i/%4i ",fNGoodGlobals,kg) << *particle << endm;
      //      LOG_INFO << "Add to map[" << kg << "] node = " << fTrackNodeMap[kg] << endm;
    }
    fNGoodGlobals++;
  }
  return fNGoodGlobals;
}
//________________________________________________________________________________
Bool_t StKFVertexMaker::FillVertex(const KFParticle *KVx, StVertex *primV) {
  StThreeVectorF XVertex(&KVx->X());
  primV->setKey(KVx->Id());
  primV->setPosition(XVertex);
  primV->setChiSquared(KVx->Chi2()/KVx->GetNDF());  
  primV->setProbChiSquared(TMath::Prob(KVx->GetChi2(),KVx->GetNDF()));
  primV->setIdTruth(KVx->IdTruth(), KVx->QaTruth());
  primV->setCovariantMatrix(&(((KFParticle *) KVx)->Covariance(0))); 
  primV->setFlag(1); // Set default values
  return kTRUE;
}
//________________________________________________________________________________
void StKFVertexMaker::PrintParticles() {
  UInt_t noTrack = fParticles->GetSize();
  for (UInt_t k = 0; k < noTrack; k++) {
    KFParticle *particle = (KFParticle *) (*fParticles)[k];
    if (! particle) continue;
    particle->Print("");
  }
}
//________________________________________________________________________________
void StKFVertexMaker::PrimaryVertices() {
  Double_t TempLog = fTempLog; // default Temperature Log
  StKFVerticesCollection *allVertices = 0;
  Int_t LastGlobal = fParticles->GetLast();
  TArrayI Parents(LastGlobal+1);
  Int_t *parents = Parents.GetArray();
  //  memset(parents, 0, (LastGlobal+1)*sizeof(Int_t));
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
#if 1
  Int_t NT = 5;
  Double_t dT = fTempLog/(NT-1);
#else
  Int_t NT = 1;
  Double_t dT = 0;
#endif
  for (Int_t i = 0; i < NT; i++) {
    Double_t TempLog = fTempLog - i*dT;
    Double_t Temperature = TMath::Exp(TempLog);
    StAnneling::SetTemperature(Temperature);
    Double_t Chi2 =  fgcVertices->Fit();
    LOG_INFO << "StKFVertexMaker::AnnelingFcn\tTemperature = " << Temperature << " Chi2 = " << Chi2 
	     << " with " << fgcVertices->NoVertices() << " vertices" << endm;
  }
  ClearParentIDs();
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
      fVertexZPlot->Fill(z,(TMath::Erfc((z - Z - fzWindow)/SigmaZ) - TMath::Erfc((z - Z + fzWindow)/SigmaZ))/2.);
    }
    nAccepted++;
  }
  Double_t F = fVertexZPlot->GetEntries();
  if (F < 1) return VertexSeeds;
  TString opt("new");
  if (! Canvas()) opt = "goff";
  Int_t nfound = fSpectrum->Search(fVertexZPlot,-1,opt,TMath::Min(0.1,5./nAccepted));
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
	     << " candidate peaks to fit from " << fNGoodGlobals
	     << " good globals with " <<  nAccepted  << " accepted tracks" << endm;
  }
  Double_t *zOfPeaks = new Double_t[nfound];
  Int_t npeaks = 0;
#if  ROOT_VERSION_CODE < 395523
  Float_t *xpeaks = fSpectrum->GetPositionX();
  Float_t xp = 0;
#else
  Double_t *xpeaks = fSpectrum->GetPositionX();
  Double_t xp = 0;
#endif
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
void StKFVertexMaker::ResetDaughterIds(KFParticle *particle, vector<KFParticle> &particles) {
  const std::vector<int> OldDaughterIds = particle->DaughterIds();
  particle->CleanDaughtersId();
  UInt_t N = OldDaughterIds.size();
  UInt_t NO = particles.size();
  Int_t  ID = -1;
  for (UInt_t i = 0; i < N; i++) {
    UInt_t j = (UInt_t) OldDaughterIds[i];
    if (j < NO) ID = particles[j].DaughterIds()[0];
    else        ID = j;
    particle->AddDaughterId(ID);
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
	if (prob < fgProbCut) continue;
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
	if (prob < fgProbCut) continue;
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
    if (N > 2 || prob > fgProbCut) {// Allow V2 to share tracks
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
void StKFVertexMaker::SimpleMindedRank(StPrimaryVertex *primV) {    
  Float_t rank = StTMVARanking::SimpleMindedRank(primV);
  primV->setRanking(rank); 
  if (Debug()) primV->Print(Form("Rank:#V[%3i]",primV->key()));
}
//________________________________________________________________________________
void StKFVertexMaker::TMVARank(StPrimaryVertex *primV) {    
  Float_t rank = StTMVARanking::TMVARank(primV);
  primV->setRanking(rank); 
  if (Debug()) primV->Print(Form("Rank:#V[%3i]",primV->key()));
}
//________________________________________________________________________________
void StKFVertexMaker::PrintPrimVertices() {
  StPrimaryVertex *pVertex = 0;
  for (Int_t ipr=0;(pVertex=pEvent->primaryVertex(ipr));ipr++) {
    Int_t key = pVertex->key();
    if (key <= 0)  pVertex->setKey(ipr);
    cout << ipr << "\t" <<  *pVertex << endl;
  }// end prim vtx    
}
//________________________________________________________________________________
void StKFVertexMaker::ReFitToVertex() {
  //  Int_t NoVertices = fgcVertices->Vertices()->GetSize();
  for (Int_t l = fgcVertices->Vertices()->GetSize() - 1; l >= 0; l--) {
    StPrimaryVertex *primV = 0;
    StKFVertex *V = (StKFVertex *) fgcVertices->Vertices()->At(l);
    if (! V) continue;
    //    Bool_t ok = kTRUE;
    Int_t NoTracks = V->NoTracks();
    // Ignore vertices with <= 2 no. of tracks
    if (NoTracks <= 2) continue;
    KFVertex     &KVx = V->Vertex();
    // Store vertex
    primV = new StPrimaryVertex;
    FillVertex(&KVx,primV);
    primV->setRanking(333);
    primV->setNumTracksUsedInFinder(NoTracks);
    primV->setVertexFinderId(KFVertexFinder);
    PrPP(ReFitToVertex,KVx);
    PrPP(ReFitToVertex,*primV);
    Bool_t beam = kFALSE;
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
	pTracks[itk] =  FitTrack2Vertex(V, track, primV);
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
    if (beam) NoPrTracksB += 2;
    if (NoPrTracksB < 0) { // 2)       {
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
#if 0
      nodepf->Print();
#endif      
      Int_t kgp = KVx.Id();
      fTrackNodeMap[kgp] = nodepf;
      StSPtrVecTrackNode& trNodeVec = pEvent->trackNodes(); 
      trNodeVec.push_back(nodepf);
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
#if 1
//________________________________________________________________________________
void StKFVertexMaker::ReFitToStVertex() {
  Int_t NoPV = pEvent->numberOfPrimaryVertices();
  StSPtrVecTrackNode& nodes = pEvent->trackNodes();
  Int_t NoTracks = nodes.size();
  Bool_t beam = kFALSE;
  for (Int_t it = 0; it < NoTracks; it++) {
    StTrackNode *node = nodes[it]; 
    if (! node) continue;
    StGlobalTrack  *gTrack = dynamic_cast<StGlobalTrack *>(node->track(global));
    if (! gTrack) continue;
    StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    if (pTrack) continue;
    StTrackMassFit *mf = static_cast<StTrackMassFit*>(node->track(massFit));
    if (! mf) continue;
    KFParticle *T = mf->kfParticle();
    Int_t kg = T->Id();
    if (kg <= 0) {
      assert(!beam);
      beam = kTRUE;
      continue;
    }
  }
  for (Int_t iv = 0; iv < NoPV; iv++) {
    StPrimaryVertex *primV = pEvent->primaryVertex(iv);
    if (! primV) continue;
    StTrackMassFit *pf = primV->parentMF();
    if (! pf) continue;
    KFParticle *V = pf->kfParticle();
    if (! V) continue;
    for (Int_t itk = 0; itk < NoTracks; itk++) {
      StTrackNode *node = nodes[itk]; 
      if (! node) continue;
      StGlobalTrack  *gTrack = dynamic_cast<StGlobalTrack *>(node->track(global));
      if (! gTrack) continue;
      StPrimaryTrack *pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
      if (pTrack) continue;
      StTrackMassFit *mf = static_cast<StTrackMassFit*>(node->track(massFit));
      if (! mf) continue;
      KFParticle *T = mf->kfParticle();
      if (! T) continue;
      KFParticle P = *T;
      FitTrack2Vertex(*V, P, primV);
    }
    if (beam ) primV->setBeamConstrained();
    primV->setTrackNumbers();
    CalculateRank(primV);
#if 0
    UInt_t NoTracksP = primV->numberOfDaughters();
    if (NoTracksP < 1) { // 2)       {
      for (UInt_t i = 0; i < NoTracksP; i++) {
	StTrack *t = primV->daughter(i);
	if (! t) continue;
	StTrackNode *n = t->node();
	n->removeTrack(t);
      }
      PrPP(ReFitToVertex SafeDelete,*primV);
      SafeDelete(primV);
      delete fgcVertices->Vertices()->Remove(V);
    }
#endif
  }
}
#endif
//________________________________________________________________________________
StPrimaryTrack *StKFVertexMaker::FitTrack2Vertex(StKFVertex *V, StKFTrack*   track, StPrimaryVertex *primV) {
  if (! V || ! track || ! primV) return 0;
  KFParticle &P = track->Particle();
  const KFVertex &VKF = V->Vertex();
  StPrimaryTrack *pTrack = FitTrack2Vertex(VKF, P, primV);
  if (pTrack) {
    const KFParticle   *PO = track->OrigParticle();
    if (Debug() > 2 && PO) {
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
  }
  return pTrack;
}
//________________________________________________________________________________
StPrimaryTrack *StKFVertexMaker::FitTrack2Vertex(const KFParticle &Vtx, KFParticle& P, StPrimaryVertex *primV) {
  StPrimaryTrack* pTrack = 0;
  Float_t chi2 = P.GetChi2();
  // Calculate Chi2 deviation from vertex
  Float_t chi2Vx =  P.GetDistanceFromParticle(Vtx);
  if (chi2Vx > StAnneling::Chi2Cut()) return pTrack;
  P.SetProductionVertex(Vtx);
  Float_t chi2AtVx = P.GetChi2();
  Float_t dChi2AtVx = chi2AtVx - chi2;
  if (dChi2AtVx > StAnneling::Chi2Cut()) return pTrack;
  Int_t kg = P.Id();
  PrPP2(FitTrack2Vertex, Vtx);
  StTrackNode *node = fTrackNodeMap[kg];
  if (! node) {
    return pTrack;
  }
  StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
  if (! gTrack) {
    return pTrack;
  }
#if 0
  node->Print(); //<<<<<<<<<<<<<<<<<<<<<<
#endif
  pTrack = new StPrimaryTrack();
  *pTrack = *gTrack;
  primV->addDaughter(pTrack);
  PrPP(FitTrack2Vertex,P);
  node->addTrack(pTrack);  // StTrackNode::addTrack() calls track->setNode(this);
#if 0  
  node->Print("");
#endif
  StTrackMassFit *mf = new StTrackMassFit(kg,&P);
  PrPP(FitTrack2Vertex,*mf);
  primV->addMassFit(mf);
#if 0
  primV->Print();
  for (UInt_t i = 0; i < primV->numberOfDaughters(); i++) {
    primV->daughter(i)->Print();
  }
  for (UInt_t i = 0; i < primV->numberOfMassFits(); i++) {
    primV->MassFit(i)->Print();
  }
#endif
  node->addTrack(mf);
#if 0
  node->Print("");
#endif
  pTrack->setKey( gTrack->key());
  pTrack->setFlagExtension( gTrack->flagExtension());
  pTrack->setIdTruth(gTrack->idTruth(),gTrack->qaTruth());
  pTrack->fitTraits().setChi2(chi2AtVx-chi2,1);
  StThreeVectorF origin(P.GetX(),P.GetY(),P.GetZ());
  StThreeVectorF field;
  StarMagField::Instance()->BField(origin.xyz(), field.xyz());
  static const Double_t EC = 2.99792458e-4;
  StThreeVectorF p(P.GetPx(), P.GetPy(), P.GetPz());
  Double_t hz = EC*field.z();
  Double_t qovepT = P.GetQ()/P.GetPt();
  Double_t curvature = - hz*qovepT;
  Double_t helicity = (curvature < 0) ? -1 : 1;
  StTrackGeometry* geometry = new StHelixModel(P.GetQ(),
					       p.phi(),
					       fabs(curvature), 
					       TMath::PiOver2() - p.theta(),
					       origin, 
					       p,
					       helicity);
  pTrack->setGeometry(geometry);
  return pTrack;
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
