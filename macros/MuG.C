/* Global Alignment
   root.exe lMuDst1.C MuG.C+
   root.exe lMuDst1.C MuG.root
*/
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StarRoot/TPolynomial.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "Names.h"
#include "StarRoot/KFVertex.h"
#include "StarRoot/KFParticle.h"
#include "StarRoot/MTrack.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StMagF/StMagFMaker.h"
//#include "StBichsel/Bichsel.h"
#include "TArrayI.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif
#endif
StMuDstMaker* maker = 0;
enum TrackMatchType {kPositive, kNegative, kTotalSigns};                                     // switch between charges
struct PlotName_t {
  TrackMatchType    k;
  const Char_t *Name;
  const Char_t *Title;
};
struct VarName_t {
  const Char_t *Name;
  const Char_t *Title;
  Int_t nx;
  Double_t xmin, xmax;
  Int_t ny;
  Double_t ymin, ymax;
  Int_t nz;
  Double_t zmin, zmax;
  Double_t  min,  max; // min and max for plots
};
const Char_t *NameCharge[kTotalSigns] = {"Pos", "Neg"};
const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};
//________________________________________________________________________________
static Int_t _debug = 0;
void SetDebug(Int_t k) {_debug = k;}
Int_t Debug() {return _debug;}
//________________________________________________________________________________
//________________________________________________________________________________
Bool_t Accept(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  //  if (! gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 10) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Int_t SectorNumber(Float_t x, Float_t y, Float_t z) {
  Double_t phi = TMath::RadToDeg()*TMath::ATan2(y,x) + 180.;
  Int_t iphi = TMath::Nint(phi/30.);
  Int_t Sector;
//   Sector = ( ( 30 - (int)(phi/15.) )%24 ) / 2 ;
//   if ( z < 0 ) Sector = 24 - Sector     ;  // Note that order of these two if statements is important
//   else if ( Sector == 0 ) Sector = 12   ;
  if (z > 0) {
    Sector = 3 - iphi;
    if (Sector <=  0) Sector += 12;
  } else {
    Sector = 15 + iphi;
    if (Sector > 24) Sector -= 12;
  }
  return Sector;
}
//________________________________________________________________________________
Int_t SectorNumber(const StThreeVectorF &position) {
  return SectorNumber(position.x(), position.y(), position.z());
}
//________________________________________________________________________________
Double_t Chi2Vx(StMuPrimaryVertex *VtxH, StMuPrimaryVertex *Vtx) {
  if (! VtxH || ! Vtx) return -999.;
  StThreeVectorF xyzH = VtxH->position();
  StThreeVectorF xyz  = Vtx->position();
  StThreeVectorF ExyzH = VtxH->posError();
  StThreeVectorF Exyz  = Vtx->posError();
  StThreeVectorF  diff  = xyzH - xyz;
  StThreeVectorF  pull(diff.x()/TMath::Sqrt(ExyzH.x()*ExyzH.x() + Exyz.x()*Exyz.x()),
		       diff.y()/TMath::Sqrt(ExyzH.y()*ExyzH.y() + Exyz.y()*Exyz.y()),
		       diff.z()/TMath::Sqrt(ExyzH.z()*ExyzH.z() + Exyz.z()*Exyz.z()));
  Double_t Chisq = pull.mag2();
  return Chisq;
}
//________________________________________________________________________________
void MuG(Long64_t nevent = 9999999,
	 //	  const char* file="/star/rcf/test/dev/trs_sl302.ittf/Wed/year_2011/pp500_pileup/rcf10100_90_200evts_Wplus_enu.MuDst.root",
	 //	const char* file="MuDstSel.lis",
	 const char* file="*.MuDst.root",
	 const char* filter="st:MuDst.root",
	 const  char* outFile="Mu.root") {
  TFile *fOut = new TFile(outFile,"recreate");
  TH2F *dZ = new TH2F("dZ","dZ (W - E)/2 versus Z",100,-200,200,500,-5,5);
  TH2F *dX = new TH2F("dX","dX (W - E)/2 versus Z",100,-200,200,400,-1.,1.);
  TH2F *dY = new TH2F("dY","dY (W - E)/2 versus Z",100,-200,200,400,-1.,1.);
  TH2F *X = new TH2F("X","X (W + E)/2 versus Z",100,-200,200,100,-1.,1.);
  TH2F *Y = new TH2F("Y","Y (W + E)/2 versus Z",100,-200,200,100,-1.,1.);
  TH2F *Zchisq = new TH2F("Zchisq","chisq between the highest rank vertex and this one", 100,-200,200,500,0,500);
  TH2F *dXS = new TH2F("dXS","dX versus sector",24,0.5,24.5,500,-2.5,2.5);
  TH2F *dYS = new TH2F("dYS","dY versus sector",24,0.5,24.5,500,-2.5,2.5);
  TH2F *dZS = new TH2F("dZS","dZ versus sector",24,0.5,24.5,500,-2.5,2.5);
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,filter,1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {"MuEvent"
				    ,"PrimaryVertices"
				    ,"PrimaryTracks"
				    ,"GlobalTracks"
				    ,"CovPrimTrack"
				    ,"CovGlobTrack"
#if 0
				    ,"StStMuMcVertex"
				    ,"StStMuMcTrack"
#endif
				    ,"KFTracks"
				    ,"KFVertices"
  };
  new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb"); 
  new StMagFMaker();
  new StTpcDbMaker;
  StChain *chain = (StChain *)  StMaker::GetTopChain();
  chain->SetDebug(0);
  StMaker::lsMakers(chain);
  chain->Init();
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  TChain *tree = maker->chain();
  if (! tree) return;
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
  
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (chain->MakeEvent()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    Double_t Bz = muEvent->magneticField();
    KFParticle::SetField(Bz);
    // cout << " #" << ev; 
    //    Int_t referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
    // cout << " refMult= "<< referenceMultiplicity;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
#if 1
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovPrimTrack     = mu->covPrimTrack();          if (Debug()) {cout << "\tCovPrimTrack " << CovPrimTrack->GetEntriesFast();}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
#endif
    TClonesArray *KFTracks = mu->KFTracks();
    Int_t NoKFTracks = KFTracks->GetEntriesFast();                if (Debug()) {cout << "\tKFTracks " << NoKFTracks;}
    TClonesArray *KFVertices = mu->KFVertices();
    Int_t NoKFVertices = KFVertices->GetEntriesFast();            if (Debug()) {cout << "\tKFVertices " << NoKFVertices;}
    if (Debug()) {cout << endl;}
    if (NoPrimaryVertices <2) continue;
    TString WE("");
    StMuPrimaryVertex *VtxW = 0;
    StMuPrimaryVertex *VtxE = 0;
    StMuPrimaryVertex *VtxH = 0;
    vector<KFParticle> particles;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (! Vtx) continue;
      if (Vtx->position().perp() > 1.0) continue;
      if (Vtx->noTracks() < 10) continue;
      if (!VtxH) VtxH = Vtx;
      else if (VtxH) {
	Double_t chi2 = Chi2Vx(VtxH, Vtx);
	Zchisq->Fill((VtxH->position().z()+Vtx->position().z())/2, Chi2Vx(VtxH, Vtx));
	if (chi2 > 250) continue;
      }
      TArrayI Sec(24); Int_t *sec = Sec.GetArray();
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	if (! pTrack) continue;
	if (pTrack->vertexIndex() != l) continue;
	Int_t kg = pTrack->index2Global();
	if (kg < 0 || kg > NoGlobalTracks) continue;
	StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
	if (! gTrack) continue;
	Short_t id = gTrack->id();
	if (Debug()) cout << *gTrack << endl;
	Int_t kgc = gTrack->index2Cov();
	StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
	if (! dcaG) continue;
	if (Debug()) cout << dcaG << endl;
	Double_t xyzp[6], CovXyzp[21];
	dcaG->GetXYZ(xyzp,CovXyzp);
	// Create GL particle
	static MTrack track;
	track.SetParameters(xyzp);
	track.SetCovarianceMatrix(CovXyzp);
	track.SetNDF(1);
	//    track.SetChi2(GlobalTracks_mChiSqXY[k]);
	track.SetID(id);
	Int_t q   = 1;
	Int_t pdg = 211;
	if (dcaG->charge() < 0) {
	  q = -1;
	  pdg = -211;
	} 
	track.SetCharge(q);
	
	KFParticle particle(track, pdg);
	particle.SetID(id);
	particle.SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
#if 0	
	// Find Particle on MuDST
	Int_t idd = id;
	KFParticle *tk = 0;
	while (idd >= 0) {
	  tk = (KFParticle *) KFTracks->UncheckedAt(idd);
	  if (tk) {
	    if (tk->GetID() == id) break;
	  }
	  idd--;
	}
	if (! tk) continue;
	if (Debug()) cout << *tk << endl;
#endif
	Int_t sector = SectorNumber(gTrack->firstPoint());
	sec[sector-1] += 1;
	particle.SetIdParentMcVx(sector);
// 	StThreeVectorF W = 0.5*(gTrack->firstPoint() + gTrack->lastPoint());
// 	particle.SetVtxGuess(W.x(), W.y(), W.z());
	if (Debug()) cout << particle << endl;
	particles.push_back(particle);
      }
      WE = "";
      //      for (Int_t i = 1; i <= 24; i++) {if (sec[i-1] > 2) { WE += i; WE += ":"; WE += sec[i-1]; WE += "|";}}
      if (Vtx->nTpcWestOnly() && Vtx->nTpcWestOnly() > 3*Vtx->nTpcEastOnly()) {
	WE += " West"; 
	if (! VtxW) VtxW = Vtx;
	else  
	  if (VtxW->noTracks() < Vtx->noTracks()) VtxW = Vtx;
      }
      if (Vtx->nTpcEastOnly() && Vtx->nTpcEastOnly() > 3*Vtx->nTpcWestOnly()) {
	WE += " East";
	if (! VtxE) VtxE = Vtx;
	else  
	  if (VtxE->noTracks() < Vtx->noTracks()) VtxE = Vtx;
      }
      if (Debug()) {cout << l << "\t" << *Vtx << WE.Data() << endl;}
    }
    if (VtxW && VtxE) {
      if (Debug()) {
	cout << "West\t" << *VtxW << endl;
	cout << "East\t" << *VtxE << endl;
      }
      StThreeVectorF sum = 0.5*(VtxW->position() + VtxE->position());
      StThreeVectorF dif = 0.5*(VtxW->position() - VtxE->position());
      if (TMath::Abs(dif.z()) > 2.5) continue;
      dX->Fill(sum.z(),dif.x());
      dY->Fill(sum.z(),dif.y());
      dZ->Fill(sum.z(),dif.z());
      X->Fill(sum.z(),sum.x());
      Y->Fill(sum.z(),sum.y());
    }
    UInt_t npart = particles.size();
    if (npart < 2) continue;
    // Refit a primary vertex
    KFVertex     Vertex;
    const Double_t par[6] = {VtxH->position().x(),VtxH->position().y(),VtxH->position().z(), 0, 0, 1000};
    const Double_t cov[21] = {VtxH->posError().x()*VtxH->posError().x(),
			      0, VtxH->posError().y()*VtxH->posError().y(),
			      0, 0, VtxH->posError().z()*VtxH->posError().z(),
			      0, 0, 0, 0,
			      0, 0, 0, 0, 0,
			      0, 0, 0, 0, 0, 0};
#if 0
    Vertex.SetBeamConstraint(VtxH->position().x(),VtxH->position().y(),VtxH->position().z(),
			    VtxH->posError().x(),VtxH->posError().y(),VtxH->posError().z());
#endif
    Vertex.Initialize(par,cov, 0, 0, 0);
    if (Debug()) cout << "Main Vertex:" << Vertex << endl;
    for (UInt_t i = 0; i < npart; i++) {
      KFParticle particle = particles[i];          if (Debug()) cout << "Orig. particle:" << particle << endl;
      Int_t sector = particle.IdParentMcVx();
      if (sector < 1 || sector > 24) {
	continue;
      }
      Double_t ds = particle.GetDStoPoint(par);    if (Debug()) cout << "s = " << ds << endl;
      particle.TransportToDS( ds );                if (Debug()) cout << " particle " << particle << endl;
      StThreeVectorD vx = VtxH->position();        if (Debug()) cout << "Vx:" << vx << endl;
      const KFParticleBase *pb = (const KFParticleBase *) &particle;
      const Double_t *pars = pb->GetParameter();
      StThreeVectorD x(pars);   if (Debug()) cout << "x :" <<  x << endl;
      StThreeVectorD p(pars+3); if (Debug()) cout << "p :" <<  p << endl;
      StThreeVectorD vS, xS, pS; // in sector CS
      StTpcDb::instance()->SupS2Glob(sector).MasterToLocal(vx.xyz(),  vS.xyz());    if (Debug()) cout << " VS:" <<  vS << endl;
      StTpcDb::instance()->SupS2Glob(sector).MasterToLocal( x.xyz(),  xS.xyz());    if (Debug()) cout << " xS:" <<  xS << endl;
      StTpcDb::instance()->SupS2Glob(sector).MasterToLocalVect( p.xyz(), pS.xyz()); if (Debug()) cout << " pS:" << pS << endl;
      StThreeVectorD d = xS - vS;                                                  if (Debug()) cout << "d: " << d << endl;
      dXS->Fill(sector,d.x());
      dYS->Fill(sector,d.y());
      dZS->Fill(sector,d.z());
#if 0
      /*
	v    x0, y0, z0,        alpha,        beta,        gamma
	dX [ 1.,  0., 0.,          0.,  zW - ds*nz, - yW - ds*ny]
	dY [ 0.,  1., 0., -zW - ds*nz,          0.,   xW + ds*nx] 
	dZ [ 0.,  0., 1.,  yW + ds*ny, -xW - ds*nx,            0]
      */
      //      StThreeVectorD W(particle.VtxGuss());
      StThreeVectorD p(particle.GetParameter()+3);
      StThreeVectorD n = p.unit();
      TRMatrix A(3,6,
		 //                 0    1    2                    3               4                  5
		 //                dx   dy   dz                alpha            beta              gamma
		 /* dX 0*/	   1.,  0.,  0.,                  0., W.z()-ds*n.z(), -W.y() - ds*n.y(),
		 /* dY 1*/         0.,  1., 
		 /* dZ 2*/
#endif
      
    }
  }
  if (fOut) fOut->Write();
}
