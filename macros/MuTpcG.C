/* Global Alignment
   root.exe lMuDst.C MuTpcG.C+
   root.exe lMuDst.C MuTpcG.root
   .L MuTpcG.C+
   Draw();
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
#include "TCernLib.h"
#include "TEnv.h"
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
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"
#include "KFParticle/KFPTrack.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StMagF/StMagFMaker.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TGeoMatrix.h"
#include "TMinuit.h"
#include "TMinuitMinimizer.h"
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
struct PlotName_t {
  const Char_t *Name;
  const Char_t *Title;
  Int_t nx;
  Double_t xmin, xmax;
  Int_t nz;
  Double_t zmin, zmax;
  Bool_t flag;
};
enum {NPlots = 18};
const  PlotName_t plotNameD[NPlots] = {// plots for drift
  {"dXdx0"    ,"dX  => x0",    100, -0.05,  0.0, 500, -1.000, 1.000, kTRUE},
  {"dXdy0"    ,"dX  => y0",    100,  -0.2,  0.2, 500, -1.000, 1.000, kTRUE},
  {"dXdz0"    ,"dX  => z0",    100,  -0.1,  0.5, 500, -1.000, 1.000, kTRUE},
#ifndef __USE_ZGG__
  {"dXdalpha" ,"dX  => alpha", 100, -50.0, 50.0, 500, -1.000, 1.000, kTRUE},
#else
  {"dXdalpha" ,"dX  => alpha", 100,  -0.5,  0.5, 500, -1.000, 1.000, kTRUE},
#endif
  {"dXdbeta"  ,"dX  => beta",  100,  -0.2,  2.0, 500, -1.000, 1.000, kFALSE},
#ifndef __USE_ZGG__
  {"dXdgamma" ,"dX  => gamma", 100,  -0.3,  0.3, 500, -1.000, 1.000, kFALSE},
#else
  {"dXdgamma" ,"dX  => gamma", 100,  -0.5,  0.5, 500, -1.000, 1.000, kFALSE},
#endif
  {"dYdx0"    ,"dY  => x0",    100,  -0.4,  0.4, 500, -1.000, 1.000, kTRUE},
  {"dYdy0"    ,"dY  => y0",    100, -1.05, -1.0, 500, -1.000, 1.000, kTRUE},
  {"dYdz0"    ,"dY  => z0",    100, -0.05, 0.05, 500, -1.000, 1.000, kFALSE},
#ifndef __USE_ZGG__
  {"dYdalpha" ,"dY  => alpha", 100, -400.,-200., 500, -1.000, 1.000, kTRUE},
  {"dYdbeta"  ,"dY  => beta",  100, -100.0,100., 500, -1.000, 1.000, kTRUE},
#else
  {"dYdalpha" ,"dY  => alpha", 100,  -10., -10., 500, -1.000, 1.000, kTRUE},
  {"dYdbeta"  ,"dY  => beta",  100,  -0.2,  0.2, 500, -1.000, 1.000, kTRUE},
#endif
  {"dYdgamma" ,"dY  => gamma", 100,  -0.6,  0.6, 500, -1.000, 1.000, kFALSE},

  {"dZdx0"    ,"dZ  => x0",    100,  -0.1,  1.7, 500, -1.000, 1.000, kTRUE},
  {"dZdy0"    ,"dZ  => y0",    100,  -0.1,  0.1, 500, -1.000, 1.000, kTRUE},
  {"dZdz0"    ,"dZ  => z0",    100, -1.05, -1.0, 500, -1.000, 1.000, kTRUE},
#ifndef __USE_ZGG__
  {"dZdalpha" ,"dZ  => alpha", 100, -100., 100., 500, -1.000, 1.000, kTRUE},
  {"dZdbeta"  ,"dZ  => beta",  100, -320.,  20., 500, -1.000, 1.000, kTRUE},
  {"dZdgamma" ,"dZ  => gamma", 100,  -0.1,  0.1, 500, -1.000, 1.000, kFALSE}
#else
  {"dZdalpha" ,"dZ  => alpha", 100,  -1.6,  1.6, 500, -1.000, 1.000, kTRUE},
  {"dZdbeta"  ,"dZ  => beta",  100,  -2.0,  2.0, 500, -1.000, 1.000, kTRUE},
  {"dZdgamma" ,"dZ  => gamma", 100,  -0.2,  0.1, 500, -1.000, 1.000, kFALSE}
#endif
};
const Char_t* NCharge[3] = {"","P","N"};
const Char_t* TCharge[3] = {"","+","-"};
struct Val_t {
  Double_t val;
  Double_t valError;
  Int_t    iFlag;
};
//________________________________________________________________________________
static Int_t _debug = 0;
#define PrPP(B)  if (_debug)   {cout << (#B) << " = \t" << (B) << endl;}
#define PrPP2(B) if (_debug>1) {cout << (#B) << " = \t" << (B) << endl;}
#define PrPP3(B) if (_debug>2) {cout << (#B) << " = \t" << (B) << endl;}
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
  if (  gTrack->pt() < 0.5) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  if (TMath::Abs(Vtx->position().z()) > 250) return kFALSE;
  if (Vtx->position().perp() > 10.0)         return kFALSE;
  if (Vtx->noTracks() < 10)                 return kFALSE;
  //  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Int_t SectorNumber(Float_t x, Float_t y, Float_t z) {
  StThreeVectorD glob(x,y,z);
  StThreeVectorD tpc;
  StTpcDb::instance()->Tpc2GlobalMatrix().MasterToLocal(glob.xyz(),tpc.xyz());
  Double_t phi = TMath::RadToDeg()*TMath::ATan2(tpc.y(),tpc.x());
  Int_t iphi = TMath::Nint(phi/30.);
  Int_t Sector;
  if (tpc.z() > 0) {
    Sector = 3 - iphi;
    if (Sector <=  0) Sector += 12;
  } else {
    Sector = 21 + iphi;
    if (Sector > 24) Sector -= 12;
  }
  return Sector;
}
//________________________________________________________________________________
Int_t SectorNumber(const StThreeVectorF &position) {
  return SectorNumber(position.x(), position.y(), position.z());
}
//________________________________________________________________________________
Int_t SectorNumber(const StThreeVectorD &position) {
  return SectorNumber(position.x(), position.y(), position.z());
}
//________________________________________________________________________________
void Glob2SupS(Int_t sector, Double_t master[3], Double_t local[3]) {
  // SupS2Glob = Tpc2Glob * Swap * Half * R_ideal (Ideal Phi) * dR ( StTpcSuperSectorPosition )
  StTpcDb::instance()->SupS2Glob(sector).MasterToLocal(master,local);
  //#define __USE_ZGG__
#ifdef __USE_ZGG__
  local[2] += 208.707;
#endif
}
//________________________________________________________________________________
void Glob2SupSVect(Int_t sector, Double_t master[3], Double_t local[3]) {
  StTpcDb::instance()->SupS2Glob(sector).MasterToLocalVect(master,local);
}
//________________________________________________________________________________
Double_t Chi2Vx(StMuPrimaryVertex *VtxH, StMuPrimaryVertex *Vtx) {
  if (! VtxH || ! Vtx) return 99999.;
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
void Process1Event(StMuDst* mu = 0, Long64_t ev = 0) {
  static TH2F *dZ = 0,  *dX, *dY, *X, *Y,    *Zchisq, *dZT;
  static TH3F *dXS,   *dYS,   *dZS, *dXYS, *dXTpcS,   *dYTpcS,   *dZTpcS;
  static TH3F *dXSPhi,   *dYSPhi,   *dZSPhi, *dXSDip,   *dYSDip,   *dZSDip;
  static TH3F ***plots3D;
  static TH1D *LSF[3][24];
  static TH1D *Chi2T[3];
  static TH1D *NPART;
  const static Int_t tZero= 19950101;
  const static TDatime t0(tZero,0);
  const static Int_t timeOffSet = t0.Convert();
  const Int_t nZ = 500;
#ifdef __FIXED_TARGET__
  const Double_t Zmin = 200;
  const Double_t Zmax = 250;
#else
  const Double_t Zmin = -25;
  const Double_t Zmax =  25;
#endif  
  if (! dZ) {
    NPART = new TH1D("npart","no accepted particles",500,0,5000);
    dZ = new TH2F("dZ","dZ (W - E)/2 versus Z",nZ,Zmin,Zmax,1600,-2.,2.);
    const static Int_t tMin = 20140410;
    const static Int_t tMax = 20140411;
    TDatime t1(tMin,0); // min Time and
    TDatime t2(tMax,0); // max 
    
    UInt_t i1 = t1.Convert() - timeOffSet;
    UInt_t i2 = t2.Convert() - timeOffSet;
    Int_t Nt = (i2 - i1)/(360); // each 10 mins 
    dZT = new TH2F("dZT","dZ (W - E)/2 versus date",Nt,i1,i2,1600,-2.,2.);
    
    dX = new TH2F("dX","dX (W - E)/2 versus Z",nZ,Zmin,Zmax,400,-1.,1.);
    dY = new TH2F("dY","dY (W - E)/2 versus Z",nZ,Zmin,Zmax,400,-1.,1.);
    X = new TH2F("X","X (W + E)/2 versus Z",nZ,Zmin,Zmax,100,-1.,1.);
    Y = new TH2F("Y","Y (W + E)/2 versus Z",nZ,Zmin,Zmax,100,-1.,1.);
    Zchisq = new TH2F("Zchisq","chisq between the highest rank vertex and this one", 100,-25,25,500,0,500);
    dXS = new TH3F("dXS","dX in SCS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-0.05,0.05);
    dYS = new TH3F("dYS","dY in SCS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-1.0,1.0);
    dZS = new TH3F("dZS","dZ in SCS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-0.5,0.5);
    dXTpcS = new TH3F("dXTpcS","dX in Tpc CS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-0.05,0.05);
    dYTpcS = new TH3F("dYTpcS","dY in Tpc CS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-0.5,0.5);
    dZTpcS = new TH3F("dZTpcS","dZ in Tpc CS versus sector and Z",24,0.5,24.5,nZ,Zmin,Zmax,500,-0.5,0.5);
    dXYS   = new TH3F("dXYS","X and Y versus sector",24,0.5,24.5,100,-1.0,1.0,100,-1.0,1.0);
    dXSPhi = new TH3F("dXSPhi","dX in SCS versus sector and Phi",24,0.5,24.5,100,-0.5,0.5,500,-1.0,1.0);
    dYSPhi = new TH3F("dYSPhi","dY in SCS versus sector and Phi",24,0.5,24.5,100,-0.5,0.5,500,-1.0,1.0);
    dZSPhi = new TH3F("dZSPhi","dZ in SCS versus sector and Phi",24,0.5,24.5,100,-0.5,0.5,500,-1.0,1.0);
    dXSDip = new TH3F("dXSDip","dX in SCS versus sector and Dip",24,0.5,24.5,100,-0.2,1.2,500,-1.0,1.0);
    dYSDip = new TH3F("dYSDip","dY in SCS versus sector and Dip",24,0.5,24.5,100,-0.2,1.2,500,-1.0,1.0);
    dZSDip = new TH3F("dZSDip","dZ in SCS versus sector and Dip",24,0.5,24.5,100,-0.2,1.2,500,-1.0,1.0);
    plots3D = new TH3F**[3];
    for (Int_t charge = 0; charge < 3; charge++) {
      plots3D[charge] = new TH3F*[NPlots];
      for (Int_t sec = 1; sec <= 24; sec++) 
	LSF[charge][sec-1] = new TH1D(Form("LSF_%02i%s",sec,NCharge[charge]),
				 Form("Matrix and right part for Least Squared Fit for sector = %02i %s",sec,TCharge[charge]),
				 28,0,28.);
      for (Int_t i = 0; i < NPlots; i++) {
	TString Name(plotNameD[i].Name);   Name  += NCharge[charge];
	TString Title(plotNameD[i].Title); Title += TCharge[charge];
#if 1
	plots3D[charge][i] = new TH3F(Name, Title, 24, 0.5, 24.5, 
			      plotNameD[i].nx, plotNameD[i].xmin, plotNameD[i].xmax,
			      plotNameD[i].nz, plotNameD[i].zmin, plotNameD[i].zmax);
#else
	plots3D[charge][i] = new TH3F(Name, Title, 24, 0.5, 24.5, 
			      plotNameD[i].nx, 0, 0,
			      plotNameD[i].nz, plotNameD[i].zmin, plotNameD[i].zmax);
#endif
      }
      Chi2T[charge] = new TH1D(Form("Chi2T%s",NCharge[charge]), Form("Chi2 for %s",TCharge[charge]),2500,0.,2.5e4);
    }
  }
  if (! mu) return;
  Int_t date = maker->GetDateTime().Convert() - timeOffSet;
  if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
  StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
  Double_t Bz = muEvent->magneticField();
  KFParticle::SetField(Bz);
  //  KFParticle::SetField(-Bz);
  // cout << " #" << ev; 
  //    Int_t referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
  // cout << " refMult= "<< referenceMultiplicity;
  TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
  Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  PrPP(NoPrimaryVertices);
  TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
  Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      PrPP( NoPrimaryTracks);
  TClonesArray *GlobalTracks     = mu->array(muGlobal);  
  Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        PrPP( NoGlobalTracks);
  TClonesArray *CovPrimTrack     = mu->covPrimTrack();          PrPP( CovPrimTrack->GetEntriesFast());
  TClonesArray *CovGlobTrack     = mu->covGlobTrack();          PrPP( CovGlobTrack->GetEntriesFast());
  TClonesArray *KFTracks = mu->KFTracks();
  Int_t NoKFTracks = KFTracks->GetEntriesFast();                PrPP( NoKFTracks);
  TClonesArray *KFVertices = mu->KFVertices();
  Int_t NoKFVertices = KFVertices->GetEntriesFast();            PrPP( NoKFVertices);
  if (NoPrimaryVertices < 1) return;
  StMuPrimaryVertex *VtxH = 0;
  vector<KFParticle> particles[3]; // 0 => All, 1 => West, 2 => East
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {
    StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
    if (! AcceptVX(Vtx)) continue;
    if (VtxH && Vtx->noTracks() < VtxH->noTracks()) continue;
    VtxH = Vtx;
  }
  if (! VtxH) return;
  StThreeVectorD VGlob(VtxH->position().xyz());                 PrPP(VGlob);
  const TGeoHMatrix &Tpc2Global = StTpcDb::instance()->Tpc2GlobalMatrix();
  StThreeVectorD VTpc;                                
  Tpc2Global.MasterToLocal(VGlob.xyz(), VTpc.xyz());            PrPP(VTpc);
  for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
    StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
    if (! gTrack) continue;
    if (! Accept(gTrack)) continue;
    Int_t sector = SectorNumber(gTrack->firstPoint()); PrPP3(sector);
    if (SectorNumber(gTrack->lastPoint()) != sector) continue;
    PrPP3(*gTrack);
    Short_t id = gTrack->id();
    Int_t kgc = gTrack->index2Cov();
    if (kgc < 0) continue;
    StDcaGeometry *dcaG = (StDcaGeometry *) CovGlobTrack->UncheckedAt(kgc);
    if (! dcaG) continue;
    PrPP3(*dcaG);
    Double_t xyzp[6], CovXyzp[21];
    dcaG->GetXYZ(xyzp,CovXyzp);
    THelixTrack     thelixK =  dcaG->thelix();
    Double_t dca[2], ermx[3];
    thelixK.Dca(VGlob.xyz(),dca[0],dca[1],ermx,2);
    if (TMath::Abs(dca[0]) > 2 || TMath::Abs(dca[1]) > 5) continue;
   // Create GL particle
    static KFPTrack track;
    track.SetParameters(xyzp);
    track.SetCovarianceMatrix(CovXyzp);
    track.SetNDF(1);
    //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
    track.SetId(id);
    Int_t q   = 1;
    Int_t pdg = 211;
    if (dcaG->charge() < 0) {
      q = -1;
      pdg = -211;
    } 
    track.SetCharge(q);
    
    KFParticle particle(track, pdg);
    particle.SetId(id);
    particle.SetIdTruth(gTrack->idTruth(),gTrack->qaTruth());
#if 0
    StThreeVectorD firstPoint(gTrack->firstPoint()); PrPP3(firstPoint);
    StThreeVectorD firstPointTpc;
    Tpc2Global.MasterToLocal(firstPoint.xyz(), firstPointTpc.xyz()); PrPP3(firstPointTpc);
#endif
    particle.SetIdParentMcVx(sector); PrPP3(particle);
    particle.S() = dcaG->curvature();
    particles[0].push_back(particle);
    if (sector <= 12) particles[1].push_back(particle);
    else              particles[2].push_back(particle);
  }
  UInt_t npart = particles[0].size();
  NPART->Fill(npart);
  if (npart < 10) return;
  // Refit a primary vertex
  KFVertex     Vertex[3];
  const Double_t par[6] = {VGlob.x(),VGlob.y(),VGlob.z(), 0, 0, 1000};
  const Double_t cov[21] = {1,
			    0, 1,
			    0, 0, 100,
			    0, 0,   0, 100,
			    0, 0,   0,   0, 100,
			    0, 0,   0,   0,   0, 100};
  static Double_t Chi2Cut = 10;
  for (UInt_t ivx = 0; ivx < 3; ivx++) {
    Vertex[ivx].Create(par,cov, 0, 0);
    Vertex[ivx].SetId(ivx+1);
    UInt_t N = particles[ivx].size();
    if (N < 3) continue;
    TArrayC Flag(N);
    KFParticle **parts = new KFParticle*[N];
    for (UInt_t i = 0; i < N; i++) {
      parts[i] = &particles[ivx][i];
    }
#ifdef __NEW__
    //    Vertex[ivx].SetVtxGuess
#endif /* __NEW__ */
    Vertex[ivx].ConstructPrimaryVertex((const KFParticle **) parts, N, 
				       (Bool_t*) Flag.GetArray(), Chi2Cut/2);
    PrPP(Vertex[ivx]);
    delete [] parts;
  }
  if (Vertex[1].GetChi2() > 0 && Vertex[2].GetChi2() > 0) {
    StThreeVectorD W(Vertex[1].GetX(), Vertex[1].GetY(), Vertex[1].GetZ());
    StThreeVectorD E(Vertex[2].GetX(), Vertex[2].GetY(), Vertex[2].GetZ());
    StThreeVectorD WTpc, ETpc;
    Tpc2Global.MasterToLocal(W.xyz(),WTpc.xyz()); PrPP(WTpc);
    Tpc2Global.MasterToLocal(E.xyz(),ETpc.xyz()); PrPP(ETpc);
    StThreeVectorD sum = 0.5*(WTpc+ETpc);
    StThreeVectorD dif = 0.5*(WTpc-ETpc);
    if (TMath::Abs(dif.z()) < 25) {
      dX->Fill(sum.z(),dif.x());
      dY->Fill(sum.z(),dif.y());
      dZ->Fill(sum.z(),dif.z());
      dZT->Fill(date,dif.z());
      X->Fill(sum.z(),sum.x());
      Y->Fill(sum.z(),sum.y());
    }
  }
  KFParticleBase *vb = (KFParticleBase *) &Vertex[0];
  const Float_t &Cov = vb->Covariance(0,0);
  TRSymMatrix CVx(3,&Cov); PrPP(CVx);
  for (UInt_t i = 0; i < npart; i++) {
    KFParticle particle = particles[0][i];          if (Debug() > 2) {cout << "Orig."; PrPP(particle);}
    Int_t sector = particle.IdParentMcVx();
    if (sector < 1 || sector > 24) {
      continue;
    }
    StThreeVectorD vxS;
#if 0
    const TGeoHMatrix &SupS2Tpc =  StTpcDb::instance()->SupS2Tpc(sector);
    TGeoHMatrix SupS2Glob = Tpc2Global * SupS2Tpc;
    TRMatrix RTR(3, 3, SupS2Glob.GetRotationMatrix());  PrPP3(RTR);
#else
    TRMatrix RTR(3, 3, StTpcDb::instance()->SupS2Glob(sector).GetRotationMatrix());  PrPP3(RTR);
#endif
    Glob2SupS(sector,VGlob.xyz(), vxS.xyz()); PrPP(vxS);
    StThreeVectorF vx(Vertex[0].GetX(), Vertex[0].GetY(), Vertex[0].GetZ()); PrPP(vx);
    TArrayF dsdr(6);
    Float_t ds = particle.GetDStoPoint(vx.xyz(),dsdr.GetArray());    PrPP3(ds);
    particle.TransportToDS( ds,dsdr.GetArray() );                     if (Debug() > 2) {cout << "Trans."; PrPP(particle);}
    KFParticleBase *pb = (KFParticleBase *) &particle;
    const Float_t *pars = &pb->Parameter(0);
    TRSymMatrix C(3,&pb->Covariance(0)); PrPP(C);
    C += CVx;   PrPP(C);
    //??    TRSymMatrix CL(RTR,TRArray::kAxSxAT,C); PrPP(CL);
    TRSymMatrix CL(RTR,TRArray::kATxSxA,C); PrPP(CL);
    TRSymMatrix GC(C,TRArray::kInvertedA); PrPP(GC);
    if (! GC.IsValid()) {
      continue;
    }
    StThreeVectorD x(pars);   PrPP2(x);
    StThreeVectorD p(pars+3); PrPP2(p);
    StThreeVectorD vS, xS, pS; // in sector CS
    //    StTpcDb::instance()->SupS2Tpc(sector).MasterToLocal(vx.xyz(),  vS.xyz());  PrPP2(vS);
    StThreeVectorD vxD = vx;
    Glob2SupS(sector,vxD.xyz(), vS.xyz());    PrPP2(vS);
    Glob2SupS(sector,x.xyz(),  xS.xyz());     PrPP2(xS);
    Glob2SupSVect(sector, p.xyz(), pS.xyz()); PrPP2(pS);
    StThreeVectorD d = xS - vS; if (Debug() > 1) cout << "sector:" << sector << "\td: " << d << endl;
    if (d.perp() > 1.0) continue;
    StThreeVectorD n = pS.unit();
    Double_t Phi = TMath::ATan2(pS.y(),pS.x());
    Double_t Dip = TMath::ATan2(pS.z(),pS.perp());
    dXS->Fill(sector,x.z(),d.x());
    dYS->Fill(sector,x.z(),d.y());
    dZS->Fill(sector,x.z(),d.z());

    dXSPhi->Fill(sector,Phi,d.x());
    dYSPhi->Fill(sector,Phi,d.y());
    dZSPhi->Fill(sector,Phi,d.z());
    dXSDip->Fill(sector,Dip,d.x());
    dYSDip->Fill(sector,Dip,d.y());
    dZSDip->Fill(sector,Dip,d.z());
    StThreeVectorD xTpc, vxTpc;
    Tpc2Global.MasterToLocal(x.xyz(),xTpc.xyz());
    Tpc2Global.MasterToLocal(vxD.xyz(),vxTpc.xyz());
    StThreeVectorD dTpc = xTpc - vxTpc;
    dXTpcS->Fill(sector,x.z(),dTpc.x());
    dYTpcS->Fill(sector,x.z(),dTpc.y());
    dZTpcS->Fill(sector,x.z(),dTpc.z());
    dXYS->Fill(sector,d.x(),d.y());
    Double_t nX = n.x();
    Double_t nY = n.y();
    Double_t nZ = n.z();
    Double_t xM = d.x();
    Double_t yM = d.y();
    Double_t zM = d.z();
    Double_t xT = xS.x();
    Double_t yT = xS.y();
    Double_t zT = xS.z();
    Double_t xV = vS.x();
    Double_t yV = vS.y();
    Double_t zV = vS.z();
    Double_t tY = nY/nX;
    Double_t tZ = nZ/nX;// + 0.01;
#ifdef __2DCASE__
    TRVector mX(2, d.y(), d.z());  PrPP2(mX);
    TRMatrix A(2,6,//   x0  y0  z0           alpha                        beta                        gamma
	       /* dY */ tY,-1., 0., zT+tZ*xV-tZ*xT,        tY*(zT+ tZ*(xV-xT)),-tY*yT-tY*tY*xV-xV+tY*tY*xT,
	       /* dZ */ tZ, 0.,-1.,-yT-tY*xV+tY*xT,tZ*zT+(tZ*tZ+1)*xV-tZ*tZ*xT,      tZ*(tY*(xT-xV)-yT)   );
    PrPP2(A);
    TRMatrix V(NPlots,2,
	       mX(0)  ,  A(0,0), // "dYdx0"    ,"dY vs tY  => x0",      
	       //	       mX(0)  ,  A(0,1), // "dYdy0"    ,"dY vs -1  => y0",      
	       //	       mX(0)  ,  A(0,2), // "dYdz0"    ,"dY vs  0  => z0",      
	       mX(0)  ,  A(0,3), // "dYdalpha" ,"dY vs zT+tZ*xV-tZ*xT  => alpha",   
	       mX(0)  ,  A(0,4), // "dYdbeta"  ,"dY vs tY*(zT+ tZ*(xV-xT)) => beta",    
	       mX(0)  ,  A(0,5), // "dYdgamma" ,"dY vs -tY*yT-tY*tY*xV-xV+tY*tY*xT => gamma",   

	       mX(1)  ,  A(1,0), // "dZdx0"    ,"dZ  vs tZ => x0",      
	       //	       mX(1)  ,  A(1,1), // "dZdy0"    ,"dZ  vs  0 => y0",      
	       //	       mX(1)  ,  A(1,2), // "dZdz0"    ,"dZ  vs -1 => z0",      
	       mX(1)  ,  A(1,3), // "dZdalpha" ,"dZ  vs -yT-tY*xV+tY*xT => alpha",   
	       mX(1)  ,  A(1,4), // "dZdbeta"  ,"dZ  vs tZ*zT+(tZ*tZ+1)*xV-tZ*tZ*xT => beta",      
	       mX(1)  ,  A(1,5)  // "dZdgamma" ,"dZ  vs tZ*(tY*(xT-xV)-yT) => gamma",             
  	       ); PrPP2(V);
#else /* 3D case */
    TRVector mX(3, d.xyz());  PrPP2(mX);
    TRMatrix A(3,6,//            x0       y0       z0                                                           alpha                                                       beta                                                     gamma
	       /* dX */-tZ*tZ-tY*tY,      tY,      tZ,                                                    tZ*yV-tY*zV,(1-tZ*tZ)*zV+(-tY*tY-1)*zT-tY*tZ*yV+tY*tZ*yT-2*tZ*xV+tZ*xT,tY*tZ*zV-tY*tZ*zT+(tY*tY-1)*yV+(tZ*tZ+1)*yT+2*tY*xV-tY*xT,      
	       /* dY */          tY,-tZ*tZ-1,   tY*tZ,  (tZ*tZ-tY*tY)*zV+(tY*tY+1)*zT+2*tY*tZ*yV-tY*tZ*yT+tZ*xV-tZ*xT,                                            tY*zV-tY*tZ*xV,-tZ*zV+tZ*zT-2*tY*yV+tY*yT+(tY*tY-1)*xV+(-tZ*tZ-tY*tY)*xT,       
	       /* dZ */          tZ,   tY*tZ,-tY*tY-1,-2*tY*tZ*zV+tY*tZ*zT+(tZ*tZ-tY*tY)*yV+(-tZ*tZ-1)*yT-tY*xV+tY*xT,   2*tZ*zV-tZ*zT+tY*yV-tY*yT+(1-tZ*tZ)*xV+(tZ*tZ+tY*tY)*xT,                                           tY*tZ*xV-tZ*yV);
    PrPP2(A);
    TRMatrix V(NPlots,2,
	       mX(0), A(0,0),
	       mX(0), A(0,1),
	       mX(0), A(0,2),
	       mX(0), A(0,3),
	       mX(0), A(0,4),
	       mX(0), A(0,5),

	       mX(1), A(1,0),
	       mX(1), A(1,1),
	       mX(1), A(1,2),
	       mX(1), A(1,3),
	       mX(1), A(1,4),
	       mX(1), A(1,5),

	       mX(2), A(2,0),
	       mX(2), A(2,1),
	       mX(2), A(2,2),
	       mX(2), A(2,3),
	       mX(2), A(2,4),
	       mX(2), A(2,5)
  	       ); PrPP2(V);
#endif

    //#define __USE_COV__
#ifdef __USE_COV__
    /* 0 
       1 2 
       3 4 5 */
#ifdef __2DCASE__
    TRSymMatrix G(2,    
		  GC(2), 
		  GC(4), GC(5));     PrPP2(G);
#else
    TRSymMatrix G(3,    
		  GC(0),
		  GC(1), GC(2), 
		  GC(3), GC(4), GC(5));     PrPP2(G);
    
#endif
#else /* ! __USE_COV__  */
#ifdef __2DCASE__
    TRSymMatrix G(2,    //sigma = 0.1 cm
		  1e+2, 
		  0.  , 1e+2);     PrPP2(G);
#else
    TRSymMatrix G(3,    //sigma = 0.1 cm
		  1e+2, 
		  0.  , 1e+2,
		  0.  ,   0., 1e+2);     PrPP2(G);
#endif
#endif /* !  __USE_COV__ */
    TRVector mGX(G,TRArray::kSxA,mX);    PrPP2(mGX);
    Double_t Chi2t = G.Product(mX);
    TRVector AmX(A,TRArray::kATxB,mGX);  PrPP2(AmX);
    TRSymMatrix SX(A,TRArray::kATxSxA,G);   PrPP2(SX);
    Int_t sec = sector-1;
    Int_t q = particle.GetQ();
    Int_t charge = 1;
    if (q < 0) charge = 2;
    for (Int_t k = 0; k < 2; k++) {
      Int_t i = 0;
      if (! k) i = charge;
      for (Int_t j = 0; j < NPlots; j++) {
	plots3D[i][j]->Fill(sector, V(j,1), V(j,0));
      }
      Chi2T[i]->Fill(Chi2t);
      //      if (Chi2t > 500) continue;
      Double_t *array = LSF[i][sec]->GetArray();
      Double_t *amX = AmX.GetArray();
      Double_t *sX  = SX.GetArray();
      array[0]++;
      Int_t im = 1;
      Int_t is = im + 6;
      TCL::vadd(amX,array+im,array+im,6); if (Debug()) {TRVector XX(6,array+im);  PrPP2(XX);}
      TCL::vadd(sX,array+is,array+is,21); if (Debug()) {TRSymMatrix S(6,array+is);  PrPP2(S);}
      array[28] += Chi2t;
    }
  }
}
//________________________________________________________________________________
void MuTpcG(Long64_t nevent = 9999999,
	    const char* file="*.MuDst.root",
	    const  char* outFile="Mu.root") {
  TString OutFile(outFile);
  if (OutFile == "") {
    OutFile = "Mu"; OutFile += file;
    OutFile.ReplaceAll("*","");
    OutFile.ReplaceAll(".MuDst.root",".root");
  }
  TFile *fOut = new TFile(OutFile,"recreate");
  Process1Event();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
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
				    ,"KFTracks"
				    ,"KFVertices"
#endif
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
    Process1Event(mu,ev);
  }
  if (fOut) fOut->Write();
}
//________________________________________________________________________________
Double_t g2g(Double_t *xx, Double_t *par) {
  Double_t x     = xx[0];
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t dev1  = (x - mu)/sigma;
  Double_t value = 0;
  if (par[0] > -20) {
    Double_t A     = TMath::Exp(par[0]);
    value = A*TMath::Exp(-0.5*dev1*dev1);
  }
#if 0
  if (A < 1 && TMath::Abs(x -mu) < 3*sigma) {TF1::RejectPoint(); return 0;}
  Double_t B    = TMath::Exp(par[3]);
  Double_t mu2  = par[4];
  Double_t sig2 = par[5];
  Double_t gra  = par[6];
  Double_t dev1 = (x - mu)/sigma;
  Double_t dev2 = (x - mu2)/sig2;
  Double_t value += B*TMath::Exp(-0.5*dev2*dev2) + gra;
#else
  if (par[3] > -20) {
    Double_t B     = TMath::Exp(par[3]);
    Double_t width = par[4];
    Double_t power = par[5];
    Double_t dev2 = TMath::Abs(x - mu)/width;
    Double_t Dom  = B;
    if (dev2 > 1e-7) {
      Dom /= (1. + TMath::Exp(power*TMath::Log(dev2)));
    }
    value += Dom;
  }
#endif
  return value;
}
//________________________________________________________________________________
void SetG2G(TF1 *gp) {
  struct Par_t {
    const Char_t *Name;
    Double_t p, pmin, pmax;
  };
  const Par_t par[6] = {
    {"logN",     9.,    0.,   25.},
    {"mu",       0.,   -1.,    1.},
    {"sigma ", 0.05, 0.001,  0.10},
    {"logN2",    1.,  -25.,   25.},
    {"width",  0.1,  1e-2,     1},
    {"power",    1.0,  0.10,  10.}
  };
  for (Int_t i = 0; i < 6; i++) {
    gp->ReleaseParameter(i);
    gp->SetParName(i,par[i].Name);
    gp->SetParameter(i,par[i].p);
    gp->SetParLimits(i,par[i].pmin, par[i].pmax);
  }
  gp->FixParameter(0, -25.);
  gp->FixParameter(2,  1.);
}
//________________________________________________________________________________
TF1 *G2g() {
  TF1 *gp = new TF1("gp",g2g,-2,2,6);
  SetG2G(gp);
  return gp;
}
//________________________________________________________________________________
Double_t student(Double_t *x, Double_t *par) {
  Double_t norm = par[0];
  Double_t mu   = par[1];
  Double_t sigma = par[2];
  Double_t ndf   = par[3];
  if (ndf <= 0) ndf = 1;
  return TMath::Exp(norm)*TMath::Student((x[0]-mu)/sigma,ndf) + par[4];
}
//________________________________________________________________________________
TString &FitLSF(TH1D *LSF, Val_t ValA[7], Int_t charge) {
  static TString line;
  line = "";
  if (! LSF) return *&line;
  Double_t *array = LSF->GetArray();
  Int_t NP = array[0];
  if (NP <= 0) return *&line;
  Double_t yTy = array[28];
  Int_t im = 1;
  Int_t is = im + 6;
  TRVector AmX(6,array+im);  PrPP(AmX);
  TRSymMatrix S(6,array+is); PrPP(S);
  TRSymMatrix SInv(S,TRArray::kInverted);  PrPP(SInv);
  TRVector  X(SInv,TRArray::kSxA,AmX);     PrPP(X);
  Double_t chi2 = yTy;
  chi2 -= AmX*X;                           PrPP(chi2);
  line = "";
  for (Int_t m = 0; m < 6; m++) {
    if (SInv(m,m) > 0) {
      Double_t scale = 1e4;   // => mkm
      if (m > 2) scale = 1e3; // => mrad
      ValA[m].val =  scale*X(m);
      ValA[m].valError = scale*TMath::Sqrt(SInv(m,m));
      //      if (m == 3 || m == 5) 
      ValA[m].iFlag = 1;
    } else {
      ValA[m].val = ValA[m].valError = 0; ValA[m].iFlag = 0;
    }
    line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
  }
  return *&line;
}
//________________________________________________________________________________
void Draw() {
  TMinuitMinimizer::UseStaticMinuit();
  if (! gMinuit) new TMinuit(10);
  gMinuit->SetPrintLevel(-2);
#define __G2G__
#ifdef __G2G__
  TF1 *gp = G2g();
#else
  TF1 *gp = new TF1(*((TF1 *) gROOT->GetFunction("gaus")));
  gp->SetName("gp");
#if 0
  TF1 *gp = new TF1("gp","gaus(0)",-100,100);
  gp->SetParameters(100.,0.,1.);
#endif
#endif
  Int_t nx = 24;
  Int_t NCHARGE = 1;
  Int_t ny = NCHARGE*NPlots;
  Int_t scaleX = 800/nx;
  Int_t scaleY = 600/ny;
  //  Int_t scale  = TMath::Min(scaleX,scaleY);
  TCanvas *c1 = new TCanvas("Sector2Tpc","Super Sector to Tpc  Alignment" ,10,10,10+scaleX*nx,10+scaleY*ny);
  cout << "nx/ny = " << nx << "/" << ny << endl;
  c1->Divide(ny,nx);
  TCanvas *c2 = new TCanvas("c2","Pol1 Fit");
  TCanvas *c3 = new TCanvas("c3","Gaus Fit");
  TString line("");
  TString lTitle("");
  TString lineC("");
  ofstream out;
  ofstream outC;
  TString Out("Results.IO_");
  Out += gSystem->BaseName(gDirectory->GetName());
  Out.ReplaceAll(".root","");
  Out.ReplaceAll("*","");
  //  Out += t.AsString();
  Out.ReplaceAll(" ","");
  if (gSystem->AccessPathName(Out)) out.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              out.open(Out, ios::app);
  Out += ".h";
  if (gSystem->AccessPathName(Out)) outC.open(Out, ios::out); //"Results.list",ios::out | ios::app);
  else                              outC.open(Out, ios::app);
  Int_t head = 0;
  for (Int_t i = 1; i <= nx; i++) {
    if (! head) {
      out  <<  "_______________________________________________________________________________________________________"  << endl;
      out  <<  "| x mkm         | y mkm         | z mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      cout <<  "_______________________________________________________________________________________________________"  << endl;
      cout <<  "| x mkm         | y mkm         | z mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment" << endl;
      outC << "struct data_t {" << endl;
      outC << "\tInt_t sector;" << endl;
      outC << "\tDouble_t x, Dx, y, Dy, z, Dz, alpha, Dalpha, beta, Dbeta, gamma, Dgamma;" << endl;
      outC << "\tconst Char_t *Comment;" << endl;
      outC << "};" << endl;
      outC << "data_t Data[] = {" << endl;
    }
    head++;
    Int_t sector = i;
    out  << "__________________________________________________________________________________________________ " << sector << endl;
    cout << "__________________________________________________________________________________________________ " << sector << endl;
    Val_t ValA[7]; memset (ValA, 0, sizeof(ValA));
    //#define __LSF_ONLY__
#define __LSF__
#define __FITSLICES__
#ifdef __LSF__
    for (Int_t charge = 2; charge >=  0; charge--) {
      TH1D *LSF = (TH1D *) gDirectory->Get(Form("LSF_%02i%s",sector,NCharge[charge]));
      if (LSF) {
	line = FitLSF(LSF, ValA, charge);
	if (line != "") {
	  cout << line << "|" << TCharge[charge] << endl;
	  out  << line << "|" << TCharge[charge] << endl;
	}
      }
    }
#endif /* __LSF__ */
    for (Int_t charge = 0; charge < NCHARGE; charge++) {
      for (Int_t j = 0; j < NPlots; j++) {
	TString PlotNameD(Form("%s%s",plotNameD[j].Name,NCharge[charge]));
	TH3 *h3 = (TH3 *) gDirectory->Get(PlotNameD);
	if (! h3) continue;
	Int_t ij = j + 1 + NCHARGE*ny*(i-1);
	TH1 *fit = 0;
	h3->GetXaxis()->SetRange(sector,sector);
	TH1 *sp = h3->Project3D("z");
	if (sp->GetEntries() < 100) continue;
#ifdef __G2G__     
	SetG2G(gp);
#endif
	if (c3) {
	  c3->cd();
	  sp->Fit(gp,"qem");
	  c3->Update();
	} else {
	  sp->Fit(gp,"qem");
	}
	Double_t Mu = 0;
	Double_t dMu = 0;
	Mu = sp->GetFunction("gp")->GetParameter(1);
	dMu = sp->GetFunction("gp")->GetParError(1);
	if (dMu > 99.99e-4) dMu=  99.99e-4;
	static const Char_t *dxv[3] = {"dX", "dY","dZ"};
	Val_t Vals[6]; memset (Vals, 0, sizeof(Vals));
	if (dMu > 99.99e-4) dMu =  99.99e-4;
	TString Name(sp->GetName());
	TString Title(sp->GetTitle());
	for (Int_t m = 0; m < 3; m++) {
	  if (Name.BeginsWith(dxv[m]) && dMu > 0 && dMu < 99.99e-4) {
	    Vals[m].val =     -1e4*Mu;
	    Vals[m].valError = 1e4*dMu;
	    Vals[m].iFlag = 1;
	    lTitle += Form(" %s = %7.2f+-%5.2f (#mum)", dxv[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	  }
	}	  
#ifdef __FITSLICES__
	TH2 *h = (TH2 *) h3->Project3D("zy");
	if (c2) {
	  c2->cd()->SetLogz(1); 
	  h->Draw("colz"); 
	  c2->Update();
	}
	Name = h->GetName();
	Title = h->GetTitle();
	h->SetName(Form("%s_%i",h->GetName(),sector));
	//	h->FitSlicesY(gp,0,-1,100,"qeg3s");
	h->FitSlicesY(0,0,-1,100,"qeg5s");
	fit = (TH1 *) gDirectory->Get(Form("%s_1",h->GetName()));
	//       TH1 *sig = (TH1 *) gDirectory->Get(Form("%s_2",h->GetName()));
	//       TH1 *gra = (TH1 *) gDirectory->Get(Form("%s_3",h->GetName()));
	Double_t slope = 0;
	Double_t dslope = 0;
	TLegend *leg = new TLegend(0.1,0.2,0.6,0.3,"");
	lTitle = "";
	leg->SetTextSize(0.025);
	if (fit) {
	  fit->SetTitle(h->GetTitle());
	  fit->SetMarkerStyle(20);
	  fit->SetMarkerColor(1);
	  fit->SetMaximum(0.2);
	  fit->SetMinimum(-.2);
	  fit->SetStats(1);
	  if (c2) {c2->cd()->SetLogz(1); h->Draw("colz"); fit->Fit("pol1","qe","same"); c2->Update();}
	  else                                            fit->Fit("pol1","qe");
	  TF1 *pol1 = fit->GetFunction("pol1");
	  if ( pol1 ) {
	    Double_t prob = pol1->GetProb();
	    slope  = pol1->GetParameter(1);
	    dslope = pol1->GetParError(1);
	    if (dslope > 99.99e-3) dslope = 99.99e-3;
	    Int_t index = Title.Index("=>");
	    TString tag("");
	    if (index >= 0) {
	      index = index+2;
	      static TString separator("[^ ;,]+");
	      TString t(Title.Data()+index);
	      TObjArray *array = t.Tokenize(separator);
	      tag = ((TObjString *) array->At(0))->GetString();
	      delete array;
	    }
	    static const Char_t *lvar[6] = {"dx","dy","dz","#alpha","#beta","#gamma"};
	    for (Int_t m = 0; m < 6; m++) {
	      if (Vals[m].iFlag) continue;
	      if (m < 3 && 
		  (m == 0 && tag.Contains("x0") || 
		   m == 1 && tag.Contains("y0") || 
		   m == 2 && tag.Contains("z0")) && 
		  dMu > 0 && dMu < 99.99e-4) {
		Vals[m].val =      1e4*slope;
		Vals[m].valError = 1e4*dslope;
		Vals[m].iFlag = 1;
		lTitle += Form(" %s = %7.2f+-%5.2f (#mum)", lvar[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
	      } else if (dslope > 0 && dslope < 99.99e-3) {
		if ((m == 3 && tag.Contains("alpha")) ||
		    (m == 4 && tag.Contains("beta"))  ||
		    (m == 5 && tag.Contains("gamma"))) {
		  Vals[m].val =      1e3*slope;
		  Vals[m].valError = 1e3*dslope;
		  Vals[m].iFlag = 1;
		  lTitle += Form(" %s = %7.2f+-%5.2f (mrad)", lvar[m],Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
		}
	      }
	    }
	    lTitle += Form(" prob = %5.3f",prob);
	    leg->AddEntry(pol1,lTitle);
	    if (c2) {
	      c2->cd();
	      fit->Draw("sames");
	      leg->Draw();
	      c2->Update();
	    }
#endif /* __FITSLICES__ */	  
	    line = "";
	    lineC = Form("\t{%2i",sector);
	    TString Reject("R");
	    if (plotNameD[j].flag) Reject = "A";
	    for (Int_t m = 0; m < 6; m++) {
	      if (! Vals[m].iFlag) {
		line  += "|               ";
		lineC += ",      0,-9.99";
	      } else {
		line  += Form("|%7.2f+-%5.2f ", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
		lineC += Form(",%7.2f,%5.2f", Vals[m].val,TMath::Min(99.99,Vals[m].valError)); 
		if (Reject != "R") {
#ifndef  __LSF_ONLY__
		  if (charge == 0) {
		    if (ValA[m].iFlag) {
		      Double_t w0 = 1./(Vals[m].valError*Vals[m].valError);
		      Double_t w1 = 1./(ValA[m].valError*ValA[m].valError);
		      ValA[m].val = (w0*Vals[m].val + w1*ValA[m].val)/(w0 + w1);
		      ValA[m].valError = 1./TMath::Sqrt(w0 + w1);
		      ValA[m].iFlag++;
		    } else {
		      ValA[m] = Vals[m];
		    }
		  }
#endif
		}
	      }
	    }
	    line += "|"; line += Reject; line += " "; line += fit->GetName(); line += "/"; line += h->GetTitle(); 
	    lineC += ",\""; lineC += fit->GetName(); lineC += "\"},";
#ifdef __FITSLICES__
	    line += "|"; line += Reject; line += " "; line += h->GetName(); line += "/"; line += sp->GetTitle(); 
	    lineC += ",\""; lineC += h->GetName(); lineC += "\"},";
#else /*! __FITSLICES__ */
	    line += "|"; line += Reject; line += " "; line += sp->GetName(); line += "/"; line += sp->GetTitle(); 
	    lineC += ",\""; lineC += sp->GetName(); lineC += "\"},";
#endif /* __FITSLICES__ */
	    cout << line << endl;
	    out << line << endl;
	  }
	}
	if (h) {
	  c1->cd(ij + charge)->SetLogz(1);
	  h->Draw("colz");
	  if (fit) {
	    fit->Draw("same"); 
	    TF1 *pol1 = fit->GetFunction("pol1"); 
	    if (pol1) {pol1->SetLineColor(2); pol1->Draw("same");}
	  }
	  leg->Draw();
	  c1->cd(ij)->Update();
	}
      }
      out  << "__________________________________________________________________________________________________ " << sector << endl;
      cout << "__________________________________________________________________________________________________ " << sector << endl;
      line = ""; 
      lineC = Form("\t{%2i",sector);
      for (Int_t m = 0; m < 6; m++) {
	if (! ValA[m].iFlag 
#ifdef   FREEZE_BETA  
	    || (m == 4) 
#endif
#ifdef   FREEZE_ALPHA_BETA  
	    || (m == 3 || m == 4) 
#endif
	    ) {
	  line  += "|               ";
	  lineC += ",      0,-9.99";
	} else {
	  line  += Form("|%7.2f+-%5.2f ", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	  lineC += Form(",%7.2f,%5.2f", ValA[m].val,TMath::Min(99.99,ValA[m].valError)); 
	}
      }
#ifndef __LSF_ONLY__
      lineC += ",\"Average ";
#else
      lineC += ",\"LSF ";
#endif
#ifdef   FREEZE_ALPHA_BETA  
      lineC += " Fixed alpha beta";
#endif
#ifdef   FREEZE_BETA  
      lineC += ",\" Fixed beta";
#endif
      lineC += "\"},";
      cout << line << endl;
      out << line << endl;
      outC << lineC << endl;
    }
  }
  out.close();
  outC.close();
}
  
