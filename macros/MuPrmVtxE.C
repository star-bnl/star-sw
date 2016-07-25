/* 
   Reconstruction of primary vertices from MuDst.
   In directory where you have *MuDst.root files run
   root.exe lMuDst.C MuPrmVtxE.C+
 */
#define DEBUG
#define __IDTRUTH__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "StDcaGeometry.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFPTrack.h"
#include "KFParticle/KFPVertex.h"
#include "TH1K.h"
#include "TSpectrum.h"
#include "TVirtualFitter.h"
#include "TPolyMarker.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "Math/Functor.h"
#include "Math/GSLMinimizer1D.h"
#include "TROOT.h"
#include "TVector3.h"
#include "TClonesArray.h"
#endif
TCanvas *c1 = 0;
TH1 *Vtx = 0;
static Int_t _debug = 0;
static Int_t npeaks = 100;
#if 0
static Int_t beamLine = 0;
static KFParticle beam;
static Double_t pZ = 1000;
#endif
class StKFVerticesCollection;
static StKFVerticesCollection *Vertices  = 0;
class StKFTrack;
ostream&  operator<<(ostream& os,  const StKFTrack& p);
class StKFVertex;
ostream&  operator<<(ostream& os,  const StKFVertex& v);
//________________________________________________________________________________
class StAnneling {
public:
  static void      SetTemperature(Double_t  Temperature=1)   {fTemperature = Temperature;}
  static Double_t  Temperature() {return fTemperature;}
  static void      SetChi2Cut    (Double_t chi2Cut=12.25)  {fChi2Cut = chi2Cut;}
  static Double_t  Chi2Cut()     {return fChi2Cut;}
  static Double_t  Weight()   {
    return TMath::Exp(Chi2Cut()/(2*Temperature())) + 
      Temperature()*TMath::Log(1 + TMath::Exp(Chi2Cut()/(2*Temperature())));
  }
private:
  static Double_t  fTemperature;
  static Double_t  fChi2Cut;
};
Double_t StAnneling::fChi2Cut     = 12.25; // 13.81551055; // Prob = 1e-3
Double_t StAnneling::fTemperature = 1; 
const Char_t *GeNames[52] = {
  //   1       2       3      4         5           6       7        8         9          10
  "",
  "gamma"   ,"e+"   ,"e-"  ,"nu"   ,"mu+"   ,"mu-"   ,"pi0"  ,"pi+"   ,"pi-"      ,"K0L",
  "K+"      ,"K-"   ,"N"   ,"P"    ,"Pbar"  ,"K0S"   ,"eta"  ,"Lambda","Sigma+"   ,"Sigma0",
  "S-"      ,"Xi0"  ,"Xi-" ,"Omega","Nbar"  ,"LamBar","SBar-","SBar0" ,"SBar+"    ,"XiBar0",
  "XiBar+"  ,"OmBar","tau+","tau-" ,"D+"    ,"D-"    ,"D0"   ,"Dbar0" ,"Ds+"      ,"Ds-"   ,
  "LambC+"  ,"W+"   ,"W-"  ,"Z0"   ,"H2"    ,"H3"    ,"alpha","geanti","He3"      ,"Cerenk",
  "??????"};
//________________________________________________________________________________
class StKFTrack : public TObject {
public:
  StKFTrack(Int_t k = -1, KFParticle *particle = 0, Double_t chi2=-1, Int_t iWE = 0) : 
    fK(k), fWeight(-1), fW(-1), fOrigKFParticle(particle), fWestOrEast(iWE) {
    if (particle) {
      fParticle = KFParticle(*particle);
      SetChi2(chi2);
    }
  }
  virtual ~StKFTrack() {}
  void         SetChi2(Double_t chi2=-1) {
    fChi2 = chi2;
    if (fChi2 >= 0) {
      fWeight = TMath::Exp(-fChi2/(2.*StAnneling::Temperature()));
    } else {
      fWeight = -1;
    }
  }
  void Reset() {fParticle = KFParticle(*fOrigKFParticle);}
  Int_t        K()       const {return fK;}       // index in particle array 
  Double_t     Weight()  const {return fWeight;}  // adaptive weight 
  Double_t     W()       const {return fW;}       // adaptive weigth for multi vertices
  Double_t     Chi2()    const {return fChi2;}
  KFParticle   Particle()const {return fParticle;}// particle with modified covariance matrix accourdingly to weight
  KFParticle  &Particle(){return *&fParticle;}// particle with modified covariance matrix accourdingly to weight
  
  Double_t    &Weight()  {return *&fWeight;}  // adaptive weight 
  Double_t    &W()       {return *&fW;}       // adaptive weigth for multi vertices
  Double_t    &Chi2()    {return *&fChi2;}
  KFParticle *OrigParticle() const {return fOrigKFParticle;} // 
  Bool_t      IsWest()   {return fWestOrEast > 0;}
  Bool_t      IsEast()   {return fWestOrEast < 0;}
  void Print(Option_t *option="") const {if (option) {}; cout << *this << endl;}
  static Int_t CorrectGePid(Int_t gePid) {
    // By pass embedding particle redefinition
    if (gePid ==           99) gePid =         11151;
    if (gePid ==          207) gePid =            41;
    if (gePid ==        40001) gePid =            24;
    if (gePid ==           98) gePid =            18;
    if (gePid ==        40002) gePid =            32;
    if (gePid ==           97) gePid =            26;
    if (gePid ==        40003) gePid =            23;
    if (gePid ==        40004) gePid =            31;
    if (gePid ==        40005) gePid =            22;
    if (gePid ==        40006) gePid =            30;
    if (gePid ==        10150) gePid =           150;
    if (gePid ==        10151) gePid =           151;
    if (gePid ==        11151) gePid =         10151;
    if (gePid ==        10018) gePid =            98;
    if (gePid ==        10026) gePid =            97;
    if (gePid ==        10017) gePid =            17;
    if (gePid ==        10039) gePid =            39;
    if (gePid ==        10040) gePid =            40;
    if (gePid ==           98) gePid =            18;
    if (gePid ==           97) gePid =            26;
    if (gePid < 0 || gePid > 50) {
      cout << "Illegal gePid " << gePid << endl;
    }
    if (gePid < 0 || gePid > 50) gePid = 51;
    return gePid;
  }
private:
  const Int_t fK;       // index in particle array 
  Double_t    fWeight;  // adaptive weight 
  Double_t    fW;       // adaptive weigth for multi vertices
  Double_t    fChi2;
  KFParticle  fParticle;// particle with modified covariance matrix accourdingly to weight
  KFParticle *fOrigKFParticle; // 
  Int_t       fWestOrEast;  
  ClassDef(StKFTrack,0)
};
ostream&  operator<<(ostream& os,  const StKFTrack& p) {
  os << Form("%5i %9.3f %9.3f %9.3f %9.3f",
	     p.K(),p.Weight(),p.W(),p.OrigParticle()->GetZ(),p.Chi2());
  return os;
}
//________________________________________________________________________________
class StKFVertex : public TObject  {
public:
  StKFVertex(Int_t id = -1) : fID(id) , fIdTruth(0), fQuality(0), fIdParentTk(0),
			      fTimeMc(0), fNoDaughtersMc(0), fgePidMc(0)
  {fKFTracks.SetOwner(kTRUE); Clear();}
  virtual     ~StKFVertex() {Clear();}
  void         Add(const StKFTrack *track) {fKFTracks.AddLast((TObject *)track);}
  virtual void Clear(Option_t *opt="")     {fKFTracks.Clear(opt);}
  StKFTrack*   Remove(Int_t k=0)           {return (StKFTrack *) fKFTracks.RemoveAt(k);}
  StKFTrack*   Remove(StKFTrack *track)    {return (StKFTrack *) fKFTracks.Remove(track);}
  StKFTrack*   Remove(KFParticle *particle)    {
    Int_t N = NoTracks();
    for (Int_t k = 0; k < N; k++) if (particle == Track(k)->OrigParticle()) return Remove(k);
    return 0;
  }
  Int_t        ID()                  const {return   fID;}
  KFVertex    &Vertex()                    {return *&fVertex;}
  KFVertex     Vertex()              const {return   fVertex;}
  TObjArray   &Tracks()                    {return *&fKFTracks;}
  Int_t        NoTracks()            const {return   fKFTracks.GetEntriesFast();}
  Int_t        Charge()              const {return   fVertex.GetQ();}
  StKFTrack*   Track(Int_t k = 0)          {return (StKFTrack* ) fKFTracks[k];}
  const StKFTrack*   Track(Int_t k = 0) const {return (const StKFTrack* ) fKFTracks[k];}
  Double_t     UpdateVertex2TrackChi2();
  void         Compress()                  {fKFTracks.Compress();}
  void         Fit(); 
  Int_t           IdTruth() const { return fIdTruth;}
  Int_t           QaTruth() const { return fQuality; }
  Int_t           IdParentTk() const {return fIdParentTk;}
  void         SetIdTruth(Int_t idtru,Int_t qatru=0) {fIdTruth = (UShort_t) idtru; fQuality = (UShort_t) qatru;}
  void         SetIdParentTk(Int_t id) {fIdParentTk = id;}
  Int_t        MultW() const {
    Int_t N = NoTracks();
    Int_t iWest = 0;
    for (Int_t i = 0; i < N; i++) {
      const StKFTrack*  t = Track(i);
      if (t) {
	Int_t id = (t->OrigParticle()->GetID()/10000)%10;
	if (id == 1) iWest++;
      }
    }
    return iWest;
  }
  Int_t        MultE() const {
    Int_t N = NoTracks();
    Int_t iEast = 0;
    for (Int_t i = 0; i < N; i++) {
      const StKFTrack*  t = Track(i);
      if (t) {
	Int_t id = (t->OrigParticle()->GetID()/10000)%10;
	if (id == 2) iEast++;
      }
    }
    return iEast;
  }
  Int_t Q() const {
    Int_t iQ = 0;
    Int_t N = NoTracks();
    for (Int_t i = 0; i < N; i++) {
      const StKFTrack*  t = Track(i);
      if (t) {iQ += t->OrigParticle()->GetQ();}
    }
    return iQ;
  }
  void operator +=(StKFVertex &vtx) {
    Int_t N2 = vtx.NoTracks();
    for (Int_t i = N2-1; i >= 0; i--) {
      fKFTracks.AddLast(vtx.Remove(i));
    }
    vtx.Compress();
  }
  void Print(Option_t *option="") const {cout << option << *this << endl; }
  void PrintW(Option_t *option="") const {
    Int_t N = NoTracks();
    cout << Form("Vertex %5i with %5i tracks\t",fID,N);
    Print(option);
    cout   << Form("    i     k    Weight          W          Z       chi2") << endl;
    for (Int_t i = 0; i < N; i++) {
      const StKFTrack*  t = Track(i);
      cout << Form("%3i",i) << *t << endl;
    }
  }
  void SetMc(Float_t time, Float_t x, Float_t y, Float_t z, Int_t NoDaughters, Int_t gePid) {
    fTimeMc = 1e9*time;
    fXyzMc = TVector3(x,y,z);
    fNoDaughtersMc = NoDaughters;
    fgePidMc = StKFTrack::CorrectGePid(gePid);
  }
  Float_t   TimeMc()   const {return fTimeMc;}
  const TVector3 &XyzMc()    const {return *&fXyzMc;}
  Int_t     NoDaughtersMc() const {return fNoDaughtersMc;}
  Int_t     gePidMc()  const {return fgePidMc;}
private:
  Int_t     fID;
  KFVertex  fVertex;
  TObjArray fKFTracks;
  UShort_t      fIdTruth; // MC vertex id 
  UShort_t      fQuality; // quality of this information (percentage of hits coming from the above MC track)
  Int_t         fIdParentTk;
  Float_t  fTimeMc;
  TVector3 fXyzMc;
  Int_t    fNoDaughtersMc;
  Int_t    fgePidMc;

  ClassDef(StKFVertex,0)
};
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StKFVertex& v) {
  Int_t iWest = v.MultW();
  Int_t iEast = v.MultE();
  os << Form(" with %4i tracks Q:%3i W/E = %3i/%3i",v.NoTracks(),v.Charge(),iWest,iEast);
  for (Int_t i = 0; i < 3; i++) {
    os << Form("%9.3f +/- %5.3f",v.Vertex().GetParameter(i),TMath::Sqrt(v.Vertex().GetCovariance(i,i)));
  }
  Double_t prob = TMath::Prob(v.Vertex().GetChi2(),v.Vertex().GetNDF());
  os << Form("\tchi2/NDF = %8.2f/%4i",v.Vertex().GetChi2(),v.Vertex().GetNDF())
     << Form(" prob = %6.4f",prob);
  Float_t M, dM;
  if (! v.Vertex().GetMass(M,dM)) {
    os << " M = " << Form("%7.3f +/- %5.3f",M,dM);
  }
  Int_t kv = v.IdTruth();
  if (kv > 0) {
    os << Form(" Mc/QA/t:%4i/%3i/%6.0f xyz: %8.3f%8.3f%8.3f m:%4i %6s",kv, v.QaTruth(),
	       v.TimeMc(), v.XyzMc().X(), v.XyzMc().Y(), v.XyzMc().Z(), 
	       v.NoDaughtersMc(),GeNames[v.gePidMc()]);
  }
  return os;
}
//________________________________________________________________________________
class StKFVerticesCollection;
ostream&  operator<<(ostream& os,  const StKFVerticesCollection& vc);
class StKFVerticesCollection : public TObject  {
public:
  StKFVerticesCollection(Int_t NoPeaks = 0, Double_t *zOfPeaks = 0) : 
    fsigmaXY(1.5), fVertices(NoPeaks,0) {
    fVertices.SetOwner(kTRUE);
    for (Int_t peak = 0; peak < NoPeaks; peak++) {
      AddVertex(0.,0., zOfPeaks[peak], fsigmaXY, fsigmaZ);
    }
  }
  void AddVertex(Double_t x, Double_t y, Double_t z, Double_t sigmaXY, Double_t sigmaZ) {
    StKFVertex *vtx = new StKFVertex(fVertices.GetEntriesFast() + 1);
    vtx->Vertex().SetBeamConstraint(x, y, z, sigmaXY, sigmaXY, sigmaZ);
    fVertices.AddLast(vtx);
  }
  void AddVertex(StKFVertex*vtx) {fVertices.AddLast(vtx);}
  virtual  ~StKFVerticesCollection() {}
  void      SetSigmaXY    (Double_t  sigmaXY=1.5)     {fsigmaXY = sigmaXY;}
  static void      SetSigmaZ     (Double_t  sigmaZ =2.0)     {fsigmaZ  = sigmaZ; }
  Int_t     NoVertices() const  {return fVertices.GetEntriesFast();}
  StKFVertex*   Remove(Int_t k=0)           {return (StKFVertex *) fVertices.RemoveAt(k);}
  static Double_t  SigmaZ()      {return fsigmaZ;}
  Double_t  SigmaXY()     {return fsigmaXY;}
  Double_t  DoTrack2VertexAssociation(const TObjArray &particles); //   associate tracks to vertex
  Double_t  UpdateVertexTrackAssociation();                 // reassociate tracks to vertex
  void      CleanDuplicatedVertices();
  void      MergeDuplicatedVertices();
  void      UpdateWeights();
  void      UniqueTracks2VertexAssociation();
  void      Compress() {fVertices.Compress();}
  Double_t  Fit(Int_t marker = 0);
  StKFVertex *&Vertex(Int_t l) {return (StKFVertex *&) fVertices[l];}
  const StKFVertex *Vertex(Int_t l) const {return (const StKFVertex *) fVertices[l];}
  static Double_t AnnelingFcn(Double_t TInv=1);
  void operator +=(StKFVerticesCollection &col) {
    Int_t N2 = col.NoVertices();
    for (Int_t i = N2-1; i >= 0; i--) {
      fVertices.AddLast(col.Remove(i));
    }
    col.Compress();
  }
  void     SetMc(Int_t NoMuMcVertex = 0, Int_t NoMuMcTrack = 0, const Float_t *time = 0,
		 const Float_t *x = 0,const Float_t *y = 0,const Float_t *z = 0,
		 const Int_t *NoDaughters = 0,const Int_t *IdParTrk = 0,const Int_t *gePid = 0) {
    Int_t Nvtx = NoVertices();
    for (Int_t l = 0; l < Nvtx; l++) {
      StKFVertex *V = Vertex(l);
      if (! V) continue;
      Int_t kv = V->IdTruth();
      if (kv > 0 && kv <= NoMuMcVertex) {
	if (time && x && y && z && NoDaughters && IdParTrk) {
	  Int_t kvp = IdParTrk[kv-1];
	  Int_t ge = 0;
	  if (kvp > 0 && kvp <= NoMuMcTrack) ge = gePid[kvp-1];
	  V->SetMc(time[kv-1],x[kv-1],y[kv-1],z[kv-1],NoDaughters[kv-1],ge);
	}
      }
    }
  }
  virtual void Print(const Option_t*  opt = "") const  {if (opt) {};  cout << *this;}
private:
  Double_t  fsigmaXY;
  static Double_t  fsigmaZ;
  TObjArray fVertices;
  ClassDef(StKFVerticesCollection,0)
};
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StKFVerticesCollection& vc) {
  Int_t Nvtx = vc.NoVertices();
  for (Int_t l = 0; l < Nvtx; l++) {
    const StKFVertex *V = vc.Vertex(l);
    if (! V) continue;
    os << Form("Vtx: %3i",l) << *V << endl;
  }
  return os;
}
Double_t StKFVerticesCollection::fsigmaZ = 2;
//________________________________________________________________________________
class MuDstVtxT;
ostream&  operator<<(ostream& os,  const MuDstVtxT& v);
class MuDstVtxT : public TObject {
public:
  MuDstVtxT(Double_t x = 0, Double_t y = 0, Double_t z = 0, 
	    Double_t sigma_x = 0, Double_t sigma_y = 0, Double_t sigma_z = 0,
	    Int_t multU = 0, Int_t mult = 0, Int_t multC = 0, Int_t multW = 0, 
	    Int_t multE = 0, Int_t q = 0, Int_t r = 0,
	    Short_t idTruth = 0, Short_t Qual = 0, Int_t idParentTk = 0) :
    fMultU(multU), fMult(mult), fMultC(multC), fMultW(multW), fMultE(multE), fQ(q), fRank(r),
    fXyz(x,y,z), fSigmaXyz(sigma_x,sigma_y,sigma_z),
    fIdTruth(idTruth), fQuality(Qual), fIdParentTk(idParentTk), fTimeMc(0), fNoDaughtersMc(0), fgePidMc(0)  {}
  virtual ~MuDstVtxT() {}
  TVector3 &Xyz()            {return *&fXyz;}
  TVector3 &SigmaXyz()       {return *&fSigmaXyz;}
  TVector3  Xyz() const      {return fXyz;}
  TVector3  SigmaXyz() const {return fSigmaXyz;}
  Int_t     Mult()     const {return fMult;}
  Int_t     MultU()    const {return fMultU;}
  Int_t     MultC()    const {return fMultC;}
  Int_t     MultW()    const {return fMultW;}
  Int_t     MultE()    const {return fMultE;}
  Int_t     Q()        const {return fQ;}
  Int_t     Rank()     const {return fRank;}
  Short_t   IdTruth()  const {return fIdTruth;}
  Short_t   QaTruth()  const {return fQuality;}
  Int_t     IdParentTk() const {return fIdParentTk;}
  Float_t   TimeMc()   const {return fTimeMc;}
  const TVector3 &XyzMc()    const {return *&fXyzMc;}
  Int_t     NoDaughtersMc() const {return fNoDaughtersMc;}
  Int_t     gePidMc()  const {return fgePidMc;}
  void Print(Option_t *option="") const {if (option) {}; cout << *this << endl;}
  void SetMc(Int_t NoMuMcVertex = 0, Int_t NoMuMcTrack = 0, const Float_t *time = 0,
	     const Float_t *x = 0,const Float_t *y = 0,const Float_t *z = 0,
	     const Int_t *NoDaughters = 0,const Int_t *IdParTrk = 0,const Int_t *gePid = 0) {
      Int_t kv = IdTruth();
      if (kv > 0 && kv <= NoMuMcVertex) {
	if (time && x && y && z && NoDaughters && IdParTrk) {
	  fTimeMc = 1e9*time[kv-1];
	  fXyzMc = TVector3(x[kv-1],y[kv-1],z[kv-1]);
	  fNoDaughtersMc = NoDaughters[kv-1];
	  Int_t kvp = IdParTrk[kv-1];
	  if (kvp > 0 && kvp <= NoMuMcTrack) {
	    fgePidMc = StKFTrack::CorrectGePid(gePid[kvp-1]);
	  }
	}
      }
  }
private:
  Int_t fMultU, fMult, fMultC, fMultW, fMultE;
  Int_t fQ; // charge
  Int_t fRank; // MC=>0
  TVector3 fXyz;
  TVector3 fSigmaXyz;
  Short_t  fIdTruth;
  Short_t  fQuality;
  Int_t    fIdParentTk;
  Float_t  fTimeMc;
  TVector3 fXyzMc;
  Int_t    fNoDaughtersMc;
  Int_t    fgePidMc;
  ClassDef(MuDstVtxT,1)
};
ostream&  operator<<(ostream& os,  const MuDstVtxT& v) {
  os << Form("Q:%3i M:%3i/%3i/%3i W/E %3i/%3i tracks rank %5i xyz = %9.3f +/- %5.3f, %9.3f +/- %5.3f, %9.3f +/- %5.3f",
	     v.Q(),v.MultU(),v.Mult(),v.MultC(),v.MultW(),v.MultE(),v.Rank(),
	     v.Xyz().x(),v.SigmaXyz().x(),
	     v.Xyz().y(),v.SigmaXyz().x(),
	     v.Xyz().z(),v.SigmaXyz().x())
     << Form(" Mc/QA/t:%4i/%3i/%6.0f xyz: %8.3f%8.3f%8.3f m:%4i %6s",v.IdTruth(), v.QaTruth(),
	     v.TimeMc(), v.fXyzMc.X(), v.fXyzMc.Y(), v.fXyzMc.Z(), 
	     v.NoDaughtersMc(),GeNames[v.gePidMc()]);
  return os;
}

//________________________________________________________________________________
class VertexT : public MuDstVtxT {
public:
  VertexT(Double_t x = 0, Double_t y = 0, Double_t z = 0, 
	  Double_t sigma_x = 0, Double_t sigma_y = 0, Double_t sigma_z = 0,
	  Int_t multU = 0, Int_t mult = 0, Int_t multW = 0, Int_t multE = 0, Int_t q = 0, Int_t r = 0,
	  Int_t ndf = 0, Double_t prob = 0, Double_t chi2 = 0,
	  Short_t idTruth = 0, Short_t Qual = 0, Int_t idParentTk = 0) : 
    MuDstVtxT(x,y,z,sigma_x,sigma_y,sigma_z,multU,mult,multW,multE,q,r,idTruth,Qual,idParentTk),
    fNDF(ndf), fProb(prob), fChi2(chi2),
    fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {}
  VertexT(MuDstVtxT &dst, Int_t ndf = 0, Double_t prob = 0, Double_t chi2 = 0) : 
    MuDstVtxT(dst), fNDF(ndf), fProb(prob), fChi2(chi2),
    fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {} 
  VertexT(StKFVertex &v) : MuDstVtxT(v.Vertex().GetX(),
				     v.Vertex().GetY(),
				     v.Vertex().GetZ(),
				     TMath::Sqrt(v.Vertex().GetCovariance(0,0)),
				     TMath::Sqrt(v.Vertex().GetCovariance(1,1)),
				     TMath::Sqrt(v.Vertex().GetCovariance(2,2)),
				     -1, v.NoTracks(), -1, v.MultW(), v.MultE(), v.Q(), 0,
				     v.IdTruth(), v.QaTruth(), v.IdParentTk()), 
			   fNDF(v.Vertex().GetNDF()),fProb(TMath::Prob(v.Vertex().GetChi2(),v.Vertex().GetNDF())),
			   fChi2(v.Vertex().GetChi2()),
			   fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {
    fTimeMc = v.TimeMc(); 
    fXyzMc = v.XyzMc(); 
    fNoDaughtersMc = v.NoDaughtersMc(); 
    fgePidMc = v.gePidMc();
  }
  virtual ~VertexT() {}
  void SetMuDst(Int_t i, Double_t p) {fiMuDst = i; fChi2MuDst = p;}
  void SetKF(Int_t i, Double_t p) {fiKF = i; fChi2KF = p;}
private:
  Int_t    fNDF;
  Double_t fProb;
  Double_t fChi2;
  Int_t    fiMuDst; // KFVertex best match with MuDst
  Double_t fChi2MuDst; // chi2 for the above match
  Int_t    fiKF;    // KFVertex best match with KFVertex in (x,y)
  Double_t fChi2KF; // chi2 for the above match
  ClassDef(VertexT,1)
};
//________________________________________________________________________________
class VertexP : public TObject { // Vertex pair
public:
  VertexP() : fI(0), fJ(0), fChi2(0) {}
  VertexP(Int_t i, Int_t j, VertexT &VI, VertexT &VJ, Double_t chi2) : fI(i), fJ(j), fChi2(chi2), fVI(VI), fVJ(VJ) {}
  virtual ~VertexP() {}
private:
  Int_t   fI;
  Int_t   fJ;
  Double_t fChi2;
  VertexT fVI;
  VertexT fVJ;
  ClassDef(VertexP,1)
}; // for pair vertices
//________________________________________________________________________________
class KFEvent : public TObject {
public:
  KFEvent() {
    if (!fgMuDstVtx) fgMuDstVtx = new TClonesArray("VertexT", 100);
    fMuDstVtx = fgMuDstVtx; fNMuDstVtx = 0;
    if (!fgKFVtx) fgKFVtx = new TClonesArray("VertexT", 100);
    fKFVtx = fgKFVtx; fNKFVtx = 0;
    if (!fgDKFPair) fgDKFPair = new TClonesArray("VertexP", 100);
    fDKFPair = fgDKFPair; fNDKFPair = 0;
    if (!fgKFKFPair) fgKFKFPair = new TClonesArray("VertexP", 100);
    fKFKFPair = fgKFKFPair; fNKFKFPair = 0;
  }
  virtual ~KFEvent() {Clear();}
  void SetTemperature(Double_t T) {fTemperature = T;}
  void AddMuVtx(MuDstVtxT &muDstVtx)      {TClonesArray &MuDstVtxs = *fMuDstVtx; new(MuDstVtxs[fNMuDstVtx++]) VertexT(muDstVtx);}
  void AddKFVtx(StKFVertex  &kfVtx)       {TClonesArray &KFVtxs    = *fKFVtx;    new(KFVtxs[fNKFVtx++])       VertexT(kfVtx);   }
  void AddDKFPair(Int_t i, Int_t j, VertexT &muDstVtx, VertexT  &kfVtx,  Double_t chi2 = 0) {
    TClonesArray &DKFPairs    = *fDKFPair;    new(DKFPairs[fNDKFPair++])   VertexP(i,j,muDstVtx,kfVtx,chi2);   
  }
  void AddKFKFPair(Int_t i, Int_t j, VertexT  &kfVtxI, VertexT  &kfVtxJ, Double_t chi2 = 0) {
    TClonesArray &KFKFPairs    = *fKFKFPair;    new(KFKFPairs[fNKFKFPair++])       VertexP(i,j,kfVtxI,kfVtxJ,chi2);   
  }
  void Clear(Option_t *option = "")       {
    fTemperature = 0;
    fNMuDstVtx = 0; fMuDstVtx->Clear(option); 
    fNKFVtx = 0; fKFVtx->Clear(option);
    fNDKFPair = 0; fDKFPair->Clear(option);
    fNKFKFPair = 0; fKFKFPair->Clear(option);
  }
  void Reset(Option_t */* option = "" */) {SafeDelete(fgMuDstVtx); SafeDelete(fgKFVtx); SafeDelete(fgDKFPair); SafeDelete(fgKFKFPair);}
  Int_t NoMuDstVtx()       {return fNMuDstVtx;}
  TClonesArray *MuDstVtx() {return fMuDstVtx;}
  Int_t NoKFVtx()          {return fNKFVtx;}
  TClonesArray *KFVtx()    {return fKFVtx;}
  Int_t NoDKFPair()        {return fNDKFPair;}
  TClonesArray *DKFPair()  {return fDKFPair;}
  Int_t NoKFKFPair()       {return fNKFKFPair;}
  TClonesArray *KFKFPair() {return fKFKFPair;}
private:
  Double_t fTemperature;
  Int_t fNMuDstVtx;
  Int_t fNKFVtx;
  Int_t fNDKFPair;
  Int_t fNKFKFPair;
  TClonesArray *fMuDstVtx; //->
  TClonesArray *fKFVtx;    //->
  TClonesArray *fDKFPair;    //->
  TClonesArray *fKFKFPair;    //->
  static TClonesArray *fgMuDstVtx;
  static TClonesArray *fgKFVtx;
  static TClonesArray *fgDKFPair;
  static TClonesArray *fgKFKFPair;

  ClassDef(KFEvent,1)
};
ClassImp(KFEvent);
TClonesArray *KFEvent::fgMuDstVtx = 0;
TClonesArray *KFEvent::fgKFVtx = 0;
TClonesArray *KFEvent::fgDKFPair = 0;
TClonesArray *KFEvent::fgKFKFPair = 0;
//________________________________________________________________________________
void StKFVertex::Fit() {
  Compress();
  Int_t N = NoTracks();
  if (! N) return;
  KFParticle **particles = new KFParticle*[N];
  for (Int_t i = 0; i < N; i++) {
    // Track(i)->Reset(); 
    particles[i] = &(Track(i)->Particle());
  }
  TArrayC Flag(N);
  Vertex().ConstructPrimaryVertex((const KFParticle **) particles, N, 
				  (Bool_t*) Flag.GetArray(),TMath::Sqrt(StAnneling::Chi2Cut()/2));
  //Check Covariance Matrix
  //  Double_t prob = TMath::Prob(Vertex().GetChi2(),Vertex().GetNDF());
  TRSymMatrix CL(3,Vertex().CovarianceMatrix());
  if (CL[0] <= 0 || CL[2] <= 0 || CL[5] <= 0) {
    for (Int_t i = N-1; i >= 0; i--)  delete Remove(i);
  } else {
    for (Int_t i = N-1; i >= 0; i--) if (! Flag[i]) delete Remove(i);
  }
  delete [] particles;
  Compress();
  N = NoTracks();
  // Assign MC and RC
  struct vertexPing {
    Int_t  Id;
    Int_t nPings;
  };
  static vertexPing candidates[20];
  memset(candidates,0,sizeof(candidates));
  Int_t NC = 0;
  for (Int_t i = 0; i < N; i++) {
    const StKFTrack *pTrack = Track(i);
    if (! pTrack) continue;
    Int_t IdVx = pTrack->Particle().IdParentVx();
    if (IdVx <= 0) continue;
    Int_t J = -1;
    for (Int_t j = 0; j < NC; j++) if (candidates[j].Id == IdVx) {J = j; break;}
    if (J < 0) {J = NC; if (NC < 18) NC++;}
    candidates[J].Id = IdVx;
    candidates[J].nPings++;
  }
  Int_t dominant = -1;
  Int_t J = -1;
  for (Int_t j = 0; j < NC; j++) if (candidates[j].nPings > dominant) {dominant = candidates[j].nPings; J = j;}
  if (J > -1) {
    Int_t Id = candidates[J].Id;
    Int_t QA      = (100*dominant)/N;
    SetIdTruth(Id,QA);
  }
  for (Int_t i = 0; i < N; i++) {
    Track(i)->Particle().SetProductionVertex(Vertex());
  }
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::DoTrack2VertexAssociation(const TObjArray &particles) {
  Double_t chi2Total = 0;
  Int_t NVtx    = NoVertices();
  Int_t Ntracks = particles.GetEntriesFast();
  TArrayI Idx(Ntracks);
  TArrayD Chi2s(Ntracks);
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
    //    vtx->PrintW();
    KFParticle *particle = 0;
    for (Int_t k = 0; k < Ntracks; k++) {
      Chi2s[k] = 1e10;
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (particle->GetID() > 100000) continue;
      Double_t chi2il = particle->GetDeviationFromVertex(vtx->Vertex());
      chi2il *= 2*chi2il;
      Chi2s[k] = chi2il;
    }
    vtx->Clear();    
    TMath::Sort(Ntracks,Chi2s.GetArray(),Idx.GetArray(),0);
    Double_t chi2Vx = 0;
    for (Int_t j = 0; j < Ntracks; j++) {
      Int_t k = Idx[j];
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (Chi2s[k] > StAnneling::Chi2Cut()) break;
      StKFTrack *track = new StKFTrack(k,particle,Chi2s[k]);
      chi2Vx += track->Chi2()/2 + TMath::Log(track->Weight() + StAnneling::Weight());
      vtx->Add(track);
    }
    if (vtx->NoTracks() < 2) {
      delete vtx; Vertex(l) = 0;
      continue;
    }
    if (_debug) vtx->PrintW("DoTrack2VertexAssociation ");
    chi2Total += chi2Vx;
    //    vtx->PrintW();
  }
  fVertices.Compress();
  if (NoVertices()) UpdateWeights();
  return chi2Total;
}
//________________________________________________________________________________
Double_t StKFVertex::UpdateVertex2TrackChi2() {
  Int_t Ntracks = NoTracks();
  Double_t chi2Vx = 0;
  if (_debug) PrintW("old Weghts ");
  for (Int_t k = Ntracks - 1; k >= 0; k--) {
    KFVertex vTmp = fVertex;
    StKFTrack &track = *Track(k);
    vTmp -= track.Particle();
    KFParticle *particle = track.OrigParticle();
    if (! particle) continue;
    Double_t chi2il = particle->GetDeviationFromVertex(vTmp);
    chi2il *= 2*chi2il;
    if (chi2il > StAnneling::Chi2Cut()) {
      Remove(k);
      continue;
    }
    track.SetChi2(chi2il);
    chi2Vx += track.Chi2()/2 + TMath::Log(track.Weight() + StAnneling::Weight());
  }
  Compress();
  if (_debug) PrintW("new Weights ");
  return chi2Vx;
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::UpdateVertexTrackAssociation() {
  Double_t chi2Total = 0;
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
    chi2Total += vtx->UpdateVertex2TrackChi2();
  }
  return chi2Total;
}
//________________________________________________________________________________
void StKFVerticesCollection::CleanDuplicatedVertices() {
  Int_t NVtx = NoVertices();
  // Check that vertices are the same
  for (Int_t l = 1; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    Int_t NL = vtxl->NoTracks();
    TRVector vL(3,vtxl->Vertex().Parameters());
    TRSymMatrix CL(3,vtxl->Vertex().CovarianceMatrix());
    for (Int_t m = 0; m < l; m++) {
      StKFVertex *vtxm = Vertex(m);
      if (! vtxm) continue;
      // compliete overlap by an other vertex
      Int_t NM = vtxm->NoTracks();
      Int_t Nmatched = 0;
      for (Int_t i = 0; i < NL; i++) 
	for (Int_t j = 0; j < NM; j++) 
	  if (vtxl->Track(i)->OrigParticle() == vtxm->Track(j)->OrigParticle()) Nmatched++;
      if (Nmatched == TMath::Min(NL,NM)) {
	TRVector vM(3,vtxm->Vertex().Parameters());
	TRSymMatrix CM(3,vtxm->Vertex().CovarianceMatrix());
	vM -= vL;
	CM += CL;
	TRSymMatrix G(CM,TRArray::kInverted);
	Double_t chi2 = G.Product(vM,TRArray::kATxSxA);
	Double_t prob = TMath::Prob(chi2,3);
	if (prob > 0.10) {
	  if ((NL > NM) || ((NL == NM) && (vtxl->Vertex().GetChi2() < vtxm->Vertex().GetChi2()))) {
	    if (_debug) {
	      vtxm->Print(Form("Cleaned Vertex prob %7.2f M %3i keep L %3i L(%3i/%7.2f) M (%3i/%7.2f)\t",
			       prob,m,l,NL,vtxl->Vertex().GetChi2(),NM,vtxm->Vertex().GetChi2()));
	    }
	    delete vtxm; Vertex(m) = 0;
	    continue;
	  } else {
	    if (_debug) {
	      vtxl->Print(Form("Cleaned Vertex prob %7.2f L %3i keep M %3i M(%3i/%7.2f) L (%3i/%7.2f)",
			       prob,l,m,NM,vtxm->Vertex().GetChi2(),NL,vtxl->Vertex().GetChi2()));
	    }
	    delete vtxl; Vertex(l) = 0;
	    break;
	  }
	}
      }
    }
  }
  fVertices.Compress();
}
//________________________________________________________________________________
void StKFVerticesCollection::MergeDuplicatedVertices() {
  // Check that vertices are the same
  for (Int_t l = 1; l < NoVertices(); l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    if (! vtxl->NoTracks()) {delete vtxl; vtxl = Vertex(l) = 0; continue;}
    TRVector vL(3,vtxl->Vertex().Parameters());
    TRSymMatrix CL(3,vtxl->Vertex().CovarianceMatrix());
    for (Int_t m = 0; m < l; m++) {
      StKFVertex *vtxm = Vertex(m);
      if (! vtxm) continue;
      if (! vtxm->NoTracks()) {delete vtxm; vtxm = Vertex(m) = 0; continue;}
      TRVector vM(3,vtxm->Vertex().Parameters());
      vM -= vL;
      if (vM.Mag() > 5.) continue;
      TRSymMatrix CM(3,vtxm->Vertex().CovarianceMatrix());
      CM += CL;
      CM[0] += 0.0001; // 100 mkm tolerance
      CM[2] += 0.0001;
      CM[5] += 0.0001;
      TRSymMatrix G(CM,TRArray::kInverted);
      Double_t chi2 = G.Product(vM,TRArray::kATxSxA);
      Double_t prob = TMath::Prob(chi2,3);
      if (prob > 1e-4) {
	*vtxm += *vtxl;
	//	if (vtxl->NoTracks()) vtxl->Clear("keep");
	delete vtxl; vtxl = Vertex(l) = 0;
	vtxm->Fit();
	break;
      }
    }
  }
  fVertices.Compress();
}
//________________________________________________________________________________
void StKFVerticesCollection::UpdateWeights() {
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    // recalculate weights
    if (_debug) vtxl->PrintW("Weights to Update ");
    for (Int_t i = 0; i < vtxl->NoTracks(); i++) {
      Double_t Dominator = TMath::Exp(-StAnneling::Chi2Cut()/(2*StAnneling::Temperature())) + vtxl->Track(i)->Weight();
      for (Int_t m = 0; m < NVtx; m++) {
	if (l == m) continue;
	StKFVertex *vtxm = Vertex(m);
	if (! vtxm) continue;
	for (Int_t j = 0; j < vtxm->NoTracks(); j++) {
	  if (vtxl->Track(i)->OrigParticle() == vtxm->Track(j)->OrigParticle()) {
	    Dominator += vtxm->Track(j)->Weight();
	    break;
	  }
	}
      }
      vtxl->Track(i)->W() = vtxl->Track(i)->Weight()/Dominator;
      vtxl->Track(i)->Particle() = *(vtxl->Track(i)->OrigParticle());
      Float_t *CovXyz  =                 vtxl->Track(i)->Particle().CovarianceMatrix();
      for (Int_t j = 0; j < 36; j++) CovXyz[j] = CovXyz[j]/vtxl->Track(i)->W();
    }
    if (_debug) vtxl->PrintW("Updated Weights ");
  }
}
//________________________________________________________________________________
void StKFVerticesCollection::UniqueTracks2VertexAssociation(){
  // Make track associated with only vertex (by maximum weight to the vertex) 
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    // recalculate weights
    for (Int_t i = vtxl->NoTracks()-1; i >= 0; i--) {
      if (! vtxl->Track(i)) continue;
      Double_t WMax = vtxl->Track(i)->Weight();
      Int_t    iMax = i;
      Int_t    lMax = l;
      Int_t    nPart = 1;
      KFParticle *particleMax = vtxl->Track(i)->OrigParticle();
      for (Int_t m = 0; m < NVtx; m++) {
	if (l == m) continue;
	StKFVertex *vtxm = Vertex(m);
	if (! vtxm) continue;
	for (Int_t j = 0; j < vtxm->NoTracks(); j++) {
	  if (particleMax == vtxm->Track(j)->OrigParticle()) {
	    nPart++;
	    if (vtxm->Track(j)->Weight() > WMax) {
	      WMax = vtxm->Track(j)->Weight();
	      iMax = j;
	      lMax = m;
	      particleMax = vtxm->Track(j)->OrigParticle();
	      break;
	    }
	  }
	}
      }
      if (WMax < 0.01) {
	for (Int_t m = 0; m < NVtx; m++) {
	  StKFVertex *vtxm = Vertex(m);
	  if (! vtxm) continue;
	  delete vtxm->Remove(particleMax);
	  vtxm->Compress();
	}
	if (! vtxl->NoTracks()) break;
	continue;
      }
      if (nPart > 1) {
	for (Int_t m = 0; m < NVtx; m++) {
	  StKFVertex *vtxm = Vertex(m);
	  if (! vtxm) continue;
	  if (m != lMax) {
	    delete vtxm->Remove(particleMax);
	    vtxm->Compress();
	    if (vtxm->NoTracks() == 0) {delete vtxm; Vertex(m) = 0;}
	  }
	  else vtxm->Track(iMax)->W() = vtxm->Track(iMax)->Weight();
	}
      }
    }
  }
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    if (vtxl->NoTracks() == 0) {delete vtxl; Vertex(l) = 0;}
  }
  fVertices.Compress();
  NVtx = NoVertices();
  // Set particle ID
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    Int_t N = vtxl->NoTracks();
    for (Int_t i = 0; i < N; i++) {
      KFParticle *particle = vtxl->Track(i)->OrigParticle();;
      Int_t ID = particle->GetID()%100000 + 100000*vtxl->ID();;
      particle->SetID(ID);
    }
  }
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::Fit(Int_t marker) {
  // Primary Vertex fit
  Double_t chi2Total = 1e10;
  fVertices.Compress();
  Int_t NVtx = NoVertices();
  if (! NVtx) return chi2Total; 
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
#if 0
    if (marker) 
      vtx->Print(Form("Vtx: %3i",l));
#endif
    vtx->Vertex().SetBeamConstraintOff();
    vtx->Fit();
    if (vtx->Vertex().GetNDF() < 1 || vtx->NoTracks() < 2) {
#if 0
      if (marker) 	cout << "\t failed" << endl;
#endif
      delete vtx;
      Vertex(l) = 0;
      continue;
    } else {
      chi2Total += 1000;
    }
  }
  fVertices.Compress();
  CleanDuplicatedVertices();
  fVertices.Compress();
  chi2Total = UpdateVertexTrackAssociation();
  UpdateWeights();
  //
  cout << "chi2Total = " << chi2Total 
       << " at Temperature " << StAnneling::Temperature() 
       << " and Log(Temperature) " << TMath::Log(StAnneling::Temperature()) 
       << " no. vertices " << NoVertices()
       << endl;
  Double_t ymax = Vtx->GetMaximum();
  for (Int_t i = 0; i < NVtx; i++) {
    StKFVertex *vtx = Vertex(i);
    if (! vtx) continue;
    Double_t X = vtx->Vertex().GetParameter(2);
    Double_t Y = vtx->NoTracks();
    if (Y > ymax) ymax = Y;
    TPolyMarker * pm = new TPolyMarker(1, &X, &Y);
    Vtx->GetListOfFunctions()->Add(pm);
    pm->SetMarkerColor(TMath::Log(StAnneling::Temperature())+2);
    Int_t m = 22;
    if (marker) {
      m = marker;
      pm->SetMarkerColor(4);
      if (_debug) vtx->PrintW();
    }
    pm->SetMarkerStyle(m);
    pm->SetMarkerSize(2);
    //	chi2NDF->Fill(TMath::Log10(vtx->NoTracks()),vtx->Vertex().GetChi2()/vtx->Vertex().GetNDF());
  }
  if (c1) {
    Vtx->SetMaximum(ymax);
    Vtx->Draw("same");
    c1->Update();
  }
  return chi2Total;
}
//________________________________________________________________________________
 Double_t StKFVerticesCollection::AnnelingFcn(Double_t TInv) {
  if (! Vertices) return 0;
  Double_t Temperature = 1./TInv;
  StAnneling::SetTemperature(Temperature);
  return Vertices->Fit();
}
//________________________________________________________________________________
void MuPrmVtxE(const Char_t *files = "./*MuDst.root", const Char_t *Out="") { 
  TDirIter Dir(files);
  TString Files(files);
#if 0
  //  if (Files.Contains("pp500") && ! Files.Contains("sim")) beamLine = 1;
  /* beam line
     SELECT * FROM Calibrations_rhic.vertexSeed v where beginTime > "2009-03-01" and beginTime < "2009-04-01";
     |  beginTime           | x0         | dxdz       | y0         | dydz        | err_x0     | err_dxdz   | err_y0     | err_dydz   
     +----------------------+------------+------------+------------+-------------+------------+------------+------------+------------
     |  2009-03-26 04:10:21 | 0.42600000 | 0.00118000 | 0.00800000 |  0.00024000 | 0.00900000 | 0.00003000 | 0.00900000 | 0.00003000  
     |  2009-03-26 21:40:24 | 0.42600000 | 0.00136000 | 0.01600000 | -0.00005000 | 0.00900000 | 0.00003000 | 0.00900000 | 0.00003000  
     |  2010-03-20 01:00:00 | 0.27000001 | 0.00000000 |-0.03000000 |  0.00000000 | 0.15000001 | 0.00000000 | 0.15000001 | 0.00000000 
     |  2010-04-09 07:15:15 | 0.27664959 | 0.00127356 |-0.07965557 | -0.00001392 | 0.00289759 | 0.00007723 | 0.00288955 | 0.00008473 
  */
  struct vertexSeed_t {Double_t x0,       dxdz,         y0,        dydz,     err_x0,   err_dxdz,     err_y0,   err_dydz;};
  vertexSeed_t b    = { 0.42600000, 0.00118000, 0.00800000,  0.00024000, 0.00900000, 0.00003000, 0.00900000, 0.00003000};
#endif
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  TString output(Out);
  if (output == "") {
    const Char_t *file1 = iter.Chain()->GetListOfFiles()->At(0)->GetTitle();
    TString dir = gSystem->BaseName(file1); 
    dir.ReplaceAll(".MuDst","");
    output += dir;
  }
  TFile *fOut = new TFile(output,"recreate");
  TTree *ftree = new TTree("VertexT","Vertex tree");
  ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  Int_t bufsize = 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  KFEvent *fKFEvent = new KFEvent();
  TTree::SetBranchStyle(1); //new style by default
  TBranch *branch = ftree->Branch("KFEvent", "KFEvent", &fKFEvent, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  
  const Int_t*&      MuEvent_mEventInfo_mRunId                = iter("MuEvent.mEventInfo.mRunId");
  const Int_t*&      MuEvent_mEventInfo_mId                   = iter("MuEvent.mEventInfo.mId");
  const Int_t&       NoPrimaryVertices                        = iter("PrimaryVertices");
  //  const Float_t*&    MuEvent_mRunInfo_mTpcDriftVelocity       = iter("MuEvent.mRunInfo.mTpcDriftVelocity[2]");
  const Float_t*&    PrimaryVertices_mPosition_mX1            = iter("PrimaryVertices.mPosition.mX1");
  const Float_t*&    PrimaryVertices_mPosition_mX2            = iter("PrimaryVertices.mPosition.mX2");
  const Float_t*&    PrimaryVertices_mPosition_mX3            = iter("PrimaryVertices.mPosition.mX3");
  const Float_t*&    PrimaryVertices_mPosError_mX1            = iter("PrimaryVertices.mPosError.mX1");
  const Float_t*&    PrimaryVertices_mPosError_mX2            = iter("PrimaryVertices.mPosError.mX2");
  const Float_t*&    PrimaryVertices_mPosError_mX3            = iter("PrimaryVertices.mPosError.mX3");
  const Float_t*&    PrimaryVertices_mRanking                 = iter("PrimaryVertices.mRanking");
  const UShort_t*&   PrimaryVertices_mNTracksUsed             = iter("PrimaryVertices.mNTracksUsed");
#ifdef  __IDTRUTH__
  const UShort_t*&   PrimaryVertices_mIdTruth                 = iter("PrimaryVertices.mIdTruth");
  const UShort_t*&   PrimaryVertices_mQuality                 = iter("PrimaryVertices.mQuality");
  const Int_t*&      PrimaryVertices_mIdParent                  = iter("PrimaryVertices.mIdParent"); // >>
#endif  
  const Int_t*&      PrimaryTracks_mVertexIndex               = iter("PrimaryTracks.mVertexIndex");
  const Int_t& NoPrimaryTracks = iter("PrimaryTracks");
  const Int_t*& PrimaryTracks_mIndex2Global = iter("PrimaryTracks.mIndex2Global");
  const Float_t*&    PrimaryTracks_mChiSqZ                    = iter("PrimaryTracks.mChiSqZ");
#ifdef  __IDTRUTH__0
  const UShort_t*&   PrimaryTracks_mIdTruth                   = iter("PrimaryTracks.mIdTruth");
  const UShort_t*&   PrimaryTracks_mQuality                   = iter("PrimaryTracks.mQuality");
  const Int_t*&      PrimaryTracks_mIdParentVx                = iter("PrimaryTracks.mIdParentVx");
#endif
  const Int_t& NoGlobalTracks = iter("GlobalTracks");
  const Short_t*& GlobalTracks_mFlag = iter("GlobalTracks.mFlag");
  const Float_t*&    GlobalTracks_mEta                        = iter("GlobalTracks.mEta");
  const Float_t*&    GlobalTracks_mFirstPoint_mX3             = iter("GlobalTracks.mFirstPoint.mX3");
#ifdef  __IDTRUTH__
  const UShort_t*&   GlobalTracks_mIdTruth                    = iter("GlobalTracks.mIdTruth");
  const UShort_t*&   GlobalTracks_mQuality                    = iter("GlobalTracks.mQuality");
  const Int_t*&      GlobalTracks_mIdParentVx                 = iter("GlobalTracks.mIdParentVx");
#endif
  //  const UChar_t*&    GlobalTracks_mNHitsFit                   = iter("GlobalTracks.mNHitsFit");
  //  const Float_t*&    GlobalTracks_mChiSqXY                    = iter("GlobalTracks.mChiSqXY");
  const Int_t*& GlobalTracks_mIndex2Cov = iter("GlobalTracks.mIndex2Cov");
  const Int_t& NoCovGlobTrack = iter("CovGlobTrack");
  const Float_t*& CovGlobTrack_mImp = iter("CovGlobTrack.mImp");
  const Float_t*& CovGlobTrack_mZ = iter("CovGlobTrack.mZ");
  const Float_t*& CovGlobTrack_mPsi = iter("CovGlobTrack.mPsi");
  const Float_t*& CovGlobTrack_mPti = iter("CovGlobTrack.mPti");
  const Float_t*& CovGlobTrack_mTan = iter("CovGlobTrack.mTan");
  const Float_t*& CovGlobTrack_mCurv = iter("CovGlobTrack.mCurv");
  const Float_t*& CovGlobTrack_mImpImp = iter("CovGlobTrack.mImpImp");
  const Float_t*& CovGlobTrack_mZImp = iter("CovGlobTrack.mZImp");
  const Float_t*& CovGlobTrack_mZZ = iter("CovGlobTrack.mZZ");
  const Float_t*& CovGlobTrack_mPsiImp = iter("CovGlobTrack.mPsiImp");
  const Float_t*& CovGlobTrack_mPsiZ = iter("CovGlobTrack.mPsiZ");
  const Float_t*& CovGlobTrack_mPsiPsi = iter("CovGlobTrack.mPsiPsi");
  const Float_t*& CovGlobTrack_mPtiImp = iter("CovGlobTrack.mPtiImp");
  const Float_t*& CovGlobTrack_mPtiZ = iter("CovGlobTrack.mPtiZ");
  const Float_t*& CovGlobTrack_mPtiPsi = iter("CovGlobTrack.mPtiPsi");
  const Float_t*& CovGlobTrack_mPtiPti = iter("CovGlobTrack.mPtiPti");
  const Float_t*& CovGlobTrack_mTanImp = iter("CovGlobTrack.mTanImp");
  const Float_t*& CovGlobTrack_mTanZ = iter("CovGlobTrack.mTanZ");
  const Float_t*& CovGlobTrack_mTanPsi = iter("CovGlobTrack.mTanPsi");
  const Float_t*& CovGlobTrack_mTanPti = iter("CovGlobTrack.mTanPti");
  const Float_t*& CovGlobTrack_mTanTan = iter("CovGlobTrack.mTanTan");
  //  const Float_t*& Event_mMagneticField = iter("Event.mMagneticField");
  const Double_t*&   Event_mMagneticField         = iter("MuEvent.mRunInfo.mMagneticFieldZ");
#ifdef  __IDTRUTH__
  const Int_t&       NoMuMcVertex                             = iter("StMuMcVertex");
#if 0
  const Int_t*&      StMuMcVertex_Id                          = iter("StMuMcVertex.mId");
#endif
  const Int_t*&      StMuMcVertex_NoDaughters                 = iter("StMuMcVertex.mNoDaughters");
  const Int_t*&      StMuMcVertex_IdParTrk                    = iter("StMuMcVertex.mIdParTrk");
  const Float_t*&    StMuMcVertex_time                        = iter("StMuMcVertex.mTime");
  const Float_t*&    StMuMcVertex_xyzV_mX1                    = iter("StMuMcVertex.mXyzV.mX1");
  const Float_t*&    StMuMcVertex_xyzV_mX2                    = iter("StMuMcVertex.mXyzV.mX2");
  const Float_t*&    StMuMcVertex_xyzV_mX3                    = iter("StMuMcVertex.mXyzV.mX3");
  const Int_t&       NoMuMcTrack                              = iter("StMuMcTrack");
#if 0
  const Int_t*&      StMuMcTrack_Id                           = iter("StMuMcTrack.mId");
#endif
  const Int_t*&      StMuMcTrack_gePid                        = iter("StMuMcTrack.mGePid");
#if 0
  const Int_t*&      StMuMcTrack_IdVx                         = iter("StMuMcTrack.mIdVx");
  const Int_t*&      StMuMcTrack_IdVxEnd                      = iter("StMuMcTrack.mIdVxEnd");
  const Float_t*&    StMuMcTrack_pxyz_mX1                     = iter("StMuMcTrack.mPxyz.mX1");
  const Float_t*&    StMuMcTrack_pxyz_mX2                     = iter("StMuMcTrack.mPxyz.mX2");
  const Float_t*&    StMuMcTrack_pxyz_mX3                     = iter("StMuMcTrack.mPxyz.mX3");
#endif
#endif
  const Int_t NzBins =  2500;
  const Double_t zmax =  250;
  const Double_t zmin = - zmax;
  const Double_t dZ = (zmax - zmin)/NzBins;
  const Double_t zWindow = StKFVerticesCollection::SigmaZ();
  TH1F *Vtxs[2];
  TH1K *VtxKs[2];
  Int_t NPasses = 2;
  for (Int_t pass = 0; pass < NPasses; pass++) {
    Vtxs[pass] = (TH1F *) gDirectory->Get(Form("Vtx%1i",pass));
    if (Vtxs[pass]) delete Vtxs[pass];
    Vtxs[pass] = new TH1F(Form("Vtx%1i",pass),Form("z-dca distribution for pass = %1i",pass),NzBins,zmin,zmax);
    if (pass)  Vtxs[pass]->SetLineColor(5);
    Vtxs[pass]->SetDefaultSumw2();
    Vtxs[pass]->SetStats(0);
    VtxKs[pass] = (TH1K *) gDirectory->Get(Form("VtxK%1i",pass));
    if (VtxKs[pass]) delete VtxKs[pass];
    VtxKs[pass] = new TH1K(Form("VtxK%1i",pass),Form("z-dca distribution for pass = %1i",pass),NzBins,zmin,zmax);
    VtxKs[pass]->SetStats(0);
    VtxKs[pass]->SetLineColor(2);
  }
  TH1F *VtxM = new TH1F("VtxM","MuDst reconstructed multiplicities versus Z",NzBins,zmin,zmax);
  npeaks = 100;
  TSpectrum *spectrum = new TSpectrum(2*npeaks);
  if (! gROOT->IsBatch()) c1 = new TCanvas("c1","c1",1400,600);
  ROOT::Math::Functor1D func(&StKFVerticesCollection::AnnelingFcn);
  ROOT::Math::GSLMinimizer1D minBrent;
  Double_t TempLog = 2; // default Temperature Log
  //         Now iterations
  while (iter.Next()) {
    fKFEvent->Clear();
    KFParticle::SetField(Event_mMagneticField[0]);
#ifdef DEBUG
    cout << "Run " << MuEvent_mEventInfo_mRunId[0] << "\tEvent " << MuEvent_mEventInfo_mId[0] << endl;
    cout << "NoTracks\t" << (int) NoGlobalTracks << " global\t" <<  (int) NoPrimaryTracks << " primary" << endl;
#endif
    for (Int_t pass = 0; pass < NPasses; pass++) {
      VtxKs[pass]->Reset();
      VtxKs[pass]->SetMaximum();
      Vtxs[pass]->Reset();
      Vtxs[pass]->SetMaximum();
    }
    Vtx = Vtxs[0]; // << switch between types    Vtx = VtxKs[0];
    VtxM->Reset();
    TObjArray tracks;
    tracks.SetOwner(kTRUE);
    TObjArray particles;
    particles.SetOwner(kTRUE);
    StKFVerticesCollection *vertices[3] = {0, 0, 0};
    // Add measured multiplicities
    Double_t ymax = Vtx->GetMaximum();
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      Int_t mult = 0;
      Int_t multP = 0;
      Int_t mWest = 0;
      Int_t mEast = 0;
      Int_t Q     = 0;
      for (Int_t k = 0; k <NoPrimaryTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	Int_t kg = PrimaryTracks_mIndex2Global[k];
	if (kg < 0 ||   kg >= NoGlobalTracks) continue;
	if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
	if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
	if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
	if (kgc < 0 || kgc > NoCovGlobTrack) continue;
	mult++;
	if (CovGlobTrack_mPti[kgc] < 0) Q -= 1;
	else                            Q += 1;
	if (PrimaryTracks_mChiSqZ[k] <  StAnneling::Chi2Cut()) multP++;
	if (GlobalTracks_mEta[kg] > 0 &&  GlobalTracks_mFirstPoint_mX3[kg] > 0) mWest++;
	if (GlobalTracks_mEta[kg] < 0 &&  GlobalTracks_mFirstPoint_mX3[kg] < 0) mEast++;
      }
      MuDstVtxT V(PrimaryVertices_mPosition_mX1[l],PrimaryVertices_mPosition_mX2[l],PrimaryVertices_mPosition_mX3[l],
		  PrimaryVertices_mPosError_mX1[l],PrimaryVertices_mPosError_mX2[l],PrimaryVertices_mPosError_mX3[l],
		  PrimaryVertices_mNTracksUsed[l],mult,multP,mWest,mEast,Q,PrimaryVertices_mRanking[l],
		  PrimaryVertices_mIdTruth[l],PrimaryVertices_mQuality[l],PrimaryVertices_mIdParent[l]);
#ifdef  __IDTRUTH__
      if (V.QaTruth() > 0) {
	V.SetMc(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
		StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
		StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
#if 0
	if (V.QaTruth() < 100) {
	  for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	    if (PrimaryTracks_mVertexIndex[k] != l) continue;
	    Int_t kg = PrimaryTracks_mIndex2Global[k];
	    if (kg < 0 ||   kg >= NoGlobalTracks) continue;
	    if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
	    if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
	    if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
	    Int_t kgc = GlobalTracks_mIndex2Cov[kg];
	    if (kgc < 0 || kgc > NoCovGlobTrack) continue;
	    Int_t ktk = GlobalTracks_mIdTruth[kg];
	    Int_t ivx = StMuMcTrack_IdVx[ktk-1];
	    if (ivx != kv) {
	      Int_t ktp = StMuMcVertex_IdParTrk[ivx-1];
	      if (ktp <= 0 || ktp >  NoMuMcTrack) ktp = ktk;
	      cout << endl
		   << Form("I:%4i %6s pxyz: %8.3f %8.3f %8.3f",ivx,GeNames[StMuMcTrack_gePid[ktp-1]-1],
			   StMuMcTrack_pxyz_mX1[ktp-1],StMuMcTrack_pxyz_mX2[ktp-1],StMuMcTrack_pxyz_mX3[ktp-1]) 
		   << Form(" Vxyz:  %8.3f %8.3f %8.3f",StMuMcVertex_xyzV_mX1[ivx-1],StMuMcVertex_xyzV_mX2[ivx-1],
			   StMuMcVertex_xyzV_mX3[ivx-1]);
	    }
	  }
	}
#endif
      }
#endif
      cout << Form("MuDst       Primary Vertex: %3i with ",l) << V << endl;
      fKFEvent->AddMuVtx(V);
      Double_t X = PrimaryVertices_mPosition_mX3[l];
      Double_t Y = mult;
      if (1.1*Y > ymax) ymax = 1.1*Y;
      TPolyMarker * pm = new TPolyMarker(1, &X, &Y);
      VtxM->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(20);
      pm->SetMarkerColor(l+2);
      pm->SetMarkerSize(2);
      Y = multP;
      pm = new TPolyMarker(1, &X, &Y);
      VtxM->GetListOfFunctions()->Add(pm);
      pm->SetMarkerStyle(21);
      pm->SetMarkerColor(l+2);
      pm->SetMarkerSize(2);
    };
    Vtx->SetMaximum(ymax);
    Int_t NGoodGlobals = 0;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      tracks.AddAtAndExpand (0,kg);
      if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
      if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
      if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
      //      if (TMath::Abs(GlobalTracks_mEta[kg]) > 5) continue;
      Int_t kgc = GlobalTracks_mIndex2Cov[kg];
      if (kgc < 0 || kgc > NoCovGlobTrack) continue;
      //      if (TMath::Abs(CovGlobTrack_mImp[kgc]) >   10) continue;
      if (TMath::Abs(CovGlobTrack_mZ[kgc])   > zmax) continue;
      Double_t parsT[6] = {
	CovGlobTrack_mImp[kgc],CovGlobTrack_mZ[kgc],CovGlobTrack_mPsi[kgc],
	CovGlobTrack_mPti[kgc],CovGlobTrack_mTan[kgc],CovGlobTrack_mCurv[kgc]};
      Double_t errsT[15] = {
	CovGlobTrack_mImpImp[kgc],
	CovGlobTrack_mZImp[kgc],  CovGlobTrack_mZZ[kgc],
	CovGlobTrack_mPsiImp[kgc],CovGlobTrack_mPsiZ[kgc],CovGlobTrack_mPsiPsi[kgc],
	CovGlobTrack_mPtiImp[kgc],CovGlobTrack_mPtiZ[kgc],CovGlobTrack_mPtiPsi[kgc],CovGlobTrack_mPtiPti[kgc],
	CovGlobTrack_mTanImp[kgc],CovGlobTrack_mTanZ[kgc],CovGlobTrack_mTanPsi[kgc],CovGlobTrack_mTanPti[kgc],
	CovGlobTrack_mTanTan[kgc]};
      StDcaGeometry *dca = new StDcaGeometry();
      dca->set(parsT, errsT);
      tracks.AddAt(dca,kg);
      particles.AddAtAndExpand (0,kg);
      Double_t xyzp[6], CovXyzp[21];
      dca->GetXYZ(xyzp,CovXyzp);
      static KFPTrack track;
      track.SetParameters(xyzp);
      track.SetCovarianceMatrix(CovXyzp);
      track.SetNDF(1);
      //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
      track.SetID(kg+1);
      Int_t q   = 1;
      Int_t pdg = 211;
      if (dca->charge() < 0) {
	q = -1;
	pdg = -211;
      } 
      track.SetCharge(q);
      KFParticle *particle = new KFParticle(track, pdg);
      Int_t iWE  = 0;
      if (GlobalTracks_mEta[kg] > 0 &&  GlobalTracks_mFirstPoint_mX3[kg] > 0) iWE = 1;
      if (GlobalTracks_mEta[kg] < 0 &&  GlobalTracks_mFirstPoint_mX3[kg] < 0) iWE = 2;
      particle->SetID(10000*iWE + kg+1);
      particle->SetIdTruth(GlobalTracks_mIdTruth[kg],GlobalTracks_mQuality[kg]);
      particle->SetIdParentMcVx(GlobalTracks_mIdParentVx[kg]);
#ifdef DEBUG2
      cout << "particle: " << *particle << endl;
#endif
      particles.AddAt(particle,kg);
      NGoodGlobals++;
    }
    if (NGoodGlobals < 2) continue;
    Int_t NgoodTracks = tracks.GetEntriesFast();
    for (Int_t pass = 0; pass < NPasses; pass++) {
      Int_t nAccepted = 0;
      for (Int_t k = 0; k < NGoodGlobals; k++) {
	KFParticle *particle = (KFParticle *) particles[k];
	if (! particle) continue;
	if (particle->GetID() > 100000) continue;
	StDcaGeometry *dca = (StDcaGeometry *) tracks[k];
	if (! dca) {
	  cout << "dca track has not been found for particle " << k << "!!!!!!!!!!" << endl;
	  continue;
	}
	Double_t offset = 0.5*dca->tanDip();
	Double_t SigmaZ = TMath::Sqrt(dca->errMatrix()[2] + offset*offset);
	SigmaZ += dZ;
	Double_t Z = dca->z();
	VtxKs[pass]->Fill(Z);
	Int_t bin1 = Vtxs[pass]->FindBin(Z - 5*SigmaZ);
	if (bin1 < 1) bin1 = 1;
	Int_t bin2 = Vtxs[pass]->FindBin(Z + 5*SigmaZ);
	if (bin2 > NzBins) bin2 = NzBins;
	Double_t z = Vtxs[pass]->GetBinCenter(bin1);
	for (Int_t bin = bin1; bin <= bin2; bin++, z += dZ) {
	  Vtxs[pass]->Fill(z,(TMath::Erfc((z - Z - zWindow)/SigmaZ) - TMath::Erfc((z - Z + zWindow)/SigmaZ))/2.);
	}
	nAccepted++;
      }
      Double_t F = VtxKs[pass]->GetEntries();
      if (F < 2) continue;
      VtxKs[pass]->SetNormFactor(F/dZ);
      Vtx = Vtxs[0]; // << switch between types    Vtx = VtxKs[0];
      Int_t nfound = spectrum->Search(Vtx,3,"new",TMath::Min(0.1,5./NgoodTracks));
      if (c1) {
	Vtxs[0]->Draw(); VtxKs[0]->Draw("same");
	VtxM->Draw("same");
	if (pass)    Vtx->Draw("same");
	c1->Update();
      }
      cout << "Found " << nfound 
	   << " candidate peaks to fit with " << NGoodGlobals 
	   << " good globals from " << NgoodTracks 
	   << " with " <<  nAccepted  << " accepted" << endl;
      if (! nfound) continue;
      Double_t *zOfPeaks = new Double_t[nfound];
      npeaks = 0;
      Float_t *xpeaks = spectrum->GetPositionX();
      for (Int_t p=0;p<nfound;p++) {
	Float_t xp = xpeaks[p];
	Int_t bin = Vtx->GetXaxis()->FindBin(xp);
	Double_t yp = Vtx->GetBinContent(bin);
	Double_t ep = Vtx->GetBinError(bin);
	if (yp-1.25*ep < 0) continue;
	zOfPeaks[npeaks] = xp;
#if 0
	cout << Form("peak: %3i \tNorm: %9.3f +/- %5.3f\tPos: %7.1f",npeaks,yp,ep, xp);
	Int_t m = -1;
	Double_t diff = 10;
	for (Int_t l = 0; l < NoPrimaryVertices; l++) {
	  Double_t dif = TMath::Abs(xp -  PrimaryVertices_mPosition_mX3[l]);
	  if (dif < diff) {
	    diff = dif;
	    m = l;
	  }
	}
	if (m > -1) cout << Form("\tMatched with Vertex %3i\tdZ = %9.3f\t%9.3f +/- %5.3f",
				 m,xp -  PrimaryVertices_mPosition_mX3[m],
				 PrimaryVertices_mPosition_mX3[m],PrimaryVertices_mPosError_mX3[m]);
	cout << endl;
#endif
	npeaks++;
      }
      cout << "Found " << npeaks << " useful peaks to fit" << endl;
      if (! npeaks) continue;
      if (vertices[pass]) {delete vertices[pass]; vertices[pass] = 0;}
      vertices[pass] = new StKFVerticesCollection(npeaks, zOfPeaks);
      Vertices = vertices[pass];
      delete [] zOfPeaks;
      Vertices->DoTrack2VertexAssociation(particles);
      if (! Vertices->NoVertices())                         continue;
      if (Vertices->AnnelingFcn(TMath::Exp(-TempLog)) <= 0) continue;
      if (! Vertices->NoVertices())                         continue;
      Vertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
//       Vertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
// 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
// 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
    }
    if (! vertices[0]) continue;
    if (NPasses > 1 && c1) {Vtxs[1]->Draw("same"); c1->Update();}
    Int_t N1 = vertices[0]->NoVertices();
    if (! N1) continue;
    if (vertices[1]) {
      *vertices[0] += *vertices[1];
    }
    Vertices = vertices[0];
    Vertices->MergeDuplicatedVertices();
    if (! Vertices->NoVertices()) continue;
    // secondary vertices
    Int_t pass = NPasses;
    if (vertices[pass]) {delete vertices[pass]; vertices[pass] = 0;}
    vertices[pass] = new StKFVerticesCollection();
    Vertices = vertices[pass];
    TempLog = 2;
    Double_t Temperature = TMath::Exp(TempLog);
    StAnneling::SetTemperature(Temperature);
    for (Int_t k = 0; k < NGoodGlobals; k++) {
      KFParticle *particleK = (KFParticle *) particles[k];
      if (! particleK) continue;
      if (particleK->GetID() > 100000) continue;
      StKFVertex *vtx = 0;
      for (Int_t l = k+1; l < NGoodGlobals; l++) {
	KFParticle *particleL = (KFParticle *) particles[l];
	if (! particleL) continue;
	if (particleL->GetID() > 100000) continue;
	Double_t dist = particleK->GetDistanceFromParticle(*particleL);
	if (dist > 5.0) continue;
	if (! vtx) {
	  vtx = new StKFVertex(Vertices->NoVertices() + 1);
	  vtx->Add(new StKFTrack(k,particleK));
	}
	vtx->Add(new StKFTrack(k,particleL));
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
#if 0
      cout << Form("sec:%4i",vtx->ID()); vtx->Print();
#endif
      Vertices->AddVertex(vtx);
    }
    cout << "Candidate for secondary vertices: " << Vertices->NoVertices() << endl;
    if ( Vertices->NoVertices() ) {
//       Vertices->PrintV(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
// 		       StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
// 		       StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
      *vertices[0] += *vertices[NPasses];
    }
    // end of loop for secondary vertices
    Vertices = vertices[0];
    Vertices->Compress();
    if (! Vertices->NoVertices()) continue;
    Vertices->MergeDuplicatedVertices();
#if 0
    Vertices->DoTrack2VertexAssociation(particles);
#endif
    minBrent.SetFunction(func,TMath::Exp(-0.5*(TempLog)),TMath::Exp(-TempLog),1);
    if (! minBrent.Minimize(10,0.1,0.1)) {
      cout << "Temperature fit has failed" << endl;
      Temperature = 1;
    } else {
      Temperature = 1./minBrent.XMinimum();
    }
    StAnneling::SetTemperature(Temperature);
    Vertices->UniqueTracks2VertexAssociation(); // Make track associated with only vertex
    Vertices->Fit(29);
    
    Vertices->SetMc(NoMuMcVertex,NoMuMcTrack,StMuMcVertex_time,
		    StMuMcVertex_xyzV_mX1,StMuMcVertex_xyzV_mX2,StMuMcVertex_xyzV_mX3,
		    StMuMcVertex_NoDaughters,StMuMcVertex_IdParTrk,StMuMcTrack_gePid);
    Vertices->Print();
      //========
    Int_t Nvtx = Vertices->NoVertices();
    for (Int_t l = 0; l < Nvtx; l++) {
      StKFVertex *V = Vertices->Vertex(l);
      if (V) fKFEvent->AddKFVtx(*V);
    }
    // Matching Dst => KFVertex 3D chi2
    Int_t NoMuDstVtx = fKFEvent->NoMuDstVtx();
    Int_t NoKFVtx    = fKFEvent->NoKFVtx();
    for (Int_t i = 0; i < NoMuDstVtx; i++) {
      VertexT *VI = (VertexT *) (*(fKFEvent->MuDstVtx()))[i];
      for (Int_t j = 0; j < NoKFVtx; j++) {
	VertexT *VJ = (VertexT *) (*(fKFEvent->KFVtx()))[j];
	TVector3 diff = VI->Xyz() - VJ->Xyz();
	Double_t chi2 =
	  diff.x()*diff.x()/(VI->SigmaXyz().x()*VI->SigmaXyz().x() + VJ->SigmaXyz().x()*VJ->SigmaXyz().x()) +
	  diff.y()*diff.y()/(VI->SigmaXyz().y()*VI->SigmaXyz().y() + VJ->SigmaXyz().y()*VJ->SigmaXyz().y()) +
	  diff.z()*diff.z()/(VI->SigmaXyz().z()*VI->SigmaXyz().z() + VJ->SigmaXyz().z()*VJ->SigmaXyz().z());
	if (chi2 < 1e3) {
	  fKFEvent->AddDKFPair(i,j,*VI,*VJ,chi2);
	}
      }
    }
    for (Int_t i = 1; i < NoKFVtx; i++) {
      VertexT *VI = (VertexT *) (*(fKFEvent->KFVtx()))[i];
      for (Int_t j = 0; j < i; j++) {
	VertexT *VJ = (VertexT *) (*(fKFEvent->KFVtx()))[j];
	TVector3 diff = VI->Xyz() - VJ->Xyz();
	Double_t chi2 =
	  diff.x()*diff.x()/(VI->SigmaXyz().x()*VI->SigmaXyz().x() + VJ->SigmaXyz().x()*VJ->SigmaXyz().x()) +
	  diff.y()*diff.y()/(VI->SigmaXyz().y()*VI->SigmaXyz().y() + VJ->SigmaXyz().y()*VJ->SigmaXyz().y());
	if (chi2 < 1e3) {
	  fKFEvent->AddKFKFPair(i,j,*VI,*VJ,chi2);
	}
      }
    }
    ftree->Fill();
    if (! gROOT->IsBatch()) {
      Int_t iok = 0;
      cout << "type <(-1 end of event loop)" << endl;
      cin >> iok;
      if (iok < 0) break;
    }
  } // loop ove events
  fOut->Write();
}
//________________________________________________________________________________
void Analysis(const Char_t *files="./*.root") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Char_t *file1 = 0;
  Int_t NFiles = 0;
  TTreeIter iter("VertexT");
  while ((file = (Char_t *) Dir.NextFile())) {
    TString File(file);
    if (File.Contains("event") || File.Contains("geant") ||
	File.Contains("hist")  || File.Contains("tags") ||  File.Contains("runco") ||
	File.Contains("minimc") || File.Contains("event") ||
	File.Contains("MuDst")) continue;
    TFile *f = new TFile (File);
    if (! f) continue;
    TTree *tree = (TTree *) f->Get("VertexT");
    if (tree ) {
      iter.AddFile(file); 
      NFiles++; 
      file1 = file;
    }
    delete f;
  }
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! file1 ) return;
  TChain *chain =  iter.Chain();
  if (! chain) return;
  KFEvent *fKFEvent;
  chain->SetBranchAddress("KFEvent",&fKFEvent);
  Int_t nbytes = 0, nb = 0;// ierr = 0, nevt = 0;
  Long64_t nentries = chain->GetEntries();
  
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    chain->LoadTree(jentry); 
    nb = chain->GetEntry(jentry);   nbytes += nb;
  }
}
