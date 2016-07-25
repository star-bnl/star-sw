class StSPtrVecObject;
class StSPtrVecPrimaryTrack;
class StSPtrVecTrackPidTraits;
class StPtrVecHit;
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <stdlib.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TFile.h"
#include "TNetFile.h"
#include "TRandom.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "TStopwatch.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "TRefArray.h"
#include "TRef.h"
#include "TH1.h"
#include "TMath.h"
#include "TProcessID.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StTrack.h"
#include "StTrackDetectorInfo.h"
#include "StTrackGeometry.h"
#include "StSvtHit.h"
#include "StSsdHit.h"
#include "StChain.h"
// #include "StDbUtilities/StSvtCoordinateTransform.hh"
// #include "StDbUtilities/StGlobalCoordinate.hh"
// #include "StDbUtilities/StSvtLocalCoordinate.hh"
// #include "StDbUtilities/StSvtWaferCoordinate.hh"
#include "TGeoMatrix.h"
#include "THashList.h"
#include "St_db_Maker/St_db_Maker.h"
#include "tables/St_svtWafersPosition_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "StarRoot/THelixTrack.h"
#else
class TROOT;
class TFile;
class TNetFile;
class TRandom;
class TTree;
class TBranch;
class TClonesArray;
class TStopwatch;
class StThreeVectorF;
class TRefArray;
class TRef;
class TH1;
class TProcessID;
class StContainers;
class StEvent;
class StPrimaryVertex;
class StEventInfo;
class StEventSummary;
class StTrack;
class StTrackDetectorInfo;
class StTrackGeometry;
class StSvtHit;
class StSsdHit;
class StChain;
class StDetectorId;
class TGeoMatrix;
class TGeoRotation;
class TGeoTranslation;
class TGeoCombiTrans;
class TGeoHMatrix;
class THashList;
class St_db_Maker;
#endif
THashList *fRotList = 0;

//________________________________________________________________________________
class Hit : public TObject {
 private:
  Char_t start;
  Int_t Id;
  Int_t barrel, layer, ladder, wafer, hybrid; // SSD: barrel = layer = hybrid = 0
  Double32_t xG, yG, zG; // Global
  Double32_t u, v, w; // Local (Wafer) xL == u_m, yL == v_m
  Double32_t tuP, tvP;   // tangs 
  Double32_t uP, vP;
  Double32_t pT, pMom;
  Bool_t     fValidDerivatives;
  Double32_t duPdxV,duPdyV,duPdzV,duPddip,duPdphi,duPdRho; // derivatives uP wrt xV,yV,zV,1/pT,dip,phi
  Double32_t dvPdxV,dvPdyV,dvPdzV,dvPddip,dvPdphi,dvPdRho; // derivatives vP wrt xV,yV,zV,1/pT,dip,phi
  Char_t end;
 public:
  Hit(Int_t B = 0, Int_t L = 0, Int_t l = 0, Int_t W = 0, Int_t H = 0,
      Double32_t X = 0, Double32_t Y = 0, Double32_t Z = 0,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    memset(&start, 0, &end - &start);
    SetId(B,L,l,W,H); Set(X,Y,Z,XL,YL,ZL);
  }
  virtual ~Hit() {}
  void Set(Double32_t X, Double32_t Y, Double32_t Z,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    xG = X; yG = Y; zG = Z; 
    u = XL; v = YL; w = ZL;
  }
  void SetId(Int_t B = 0, Int_t L = 0, Int_t l = 0, Int_t W = 0, Int_t H = 0) {
    barrel = B; layer = L; ladder = l; wafer = W; hybrid = H;
    if (barrel == 0) Id = 7000       + 100*wafer + ladder;
    else             Id = 1000*layer + 100*wafer + ladder;
  }
  void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0],xyzG[1],xyzG[2],xyzL[0],xyzL[1],xyzL[2]);}
  void SetpT(Double32_t p) {pT = p;}
  void SetMom(Double32_t p) {pMom = p;}
  void SetUVPred(Double32_t u, Double32_t v) {uP = u; vP = v;}
  void SettUVPred(Double32_t tu, Double32_t tv) {tuP = tu; tvP = tv;}
  void SetValidDerivatives(Bool_t p=kTRUE) {fValidDerivatives = p;}
  void SetDerivatives(Double32_t *der) {Double32_t *d = &duPdxV; for (Int_t i = 0; i < 12; i++) d[i] = der[i];}
  Double32_t  GetU()           const {return u;}
  Double32_t  GetV()           const {return v;}
  Double32_t  GetPredtU()      const {return tuP;}
  Double32_t  GetPredtV()      const {return tvP;}
  Double32_t  GetPredU()       const {return uP;}
  Double32_t  GetPredV()       const {return vP;}
  const Double32_t *GetDerivatives() const {return &duPdxV;}
  Int_t       GetId()          const {return Id;}
  Bool_t      ValidDerivatives() const {return fValidDerivatives;}
  ClassDef(Hit,1)
};
//________________________________________________________________________________
#define NSP 10
class Track : public TObject {
  
 private:
  Char_t          beg;
  Double32_t      fInvpT;        //signed
  Double32_t      fDip;
  Double32_t      fPhi;
  Double32_t      fRho;
  UInt_t          fNpoint;       //Number of points for this track
  Short_t         fValid;        //Validity criterion
  UInt_t          fNsp;          //Number of points for this track with a special value
  UInt_t          fIdHit[NSP];   //Index of Hit in fHit array
  Char_t          end;
 public:
  Track() { Clear(); }
  virtual ~Track() {Clear();}
  void          Clear(Option_t *option="") {if (option); memset(&beg, 0, &end - &beg);}
  Double32_t    GetpX()     const { return GetpT()*TMath::Cos(fPhi);}
  Double32_t    GetpY()     const { return GetpT()*TMath::Sin(fPhi);}
  Double32_t    GetpZ()     const { return GetpT()*TMath::Tan(fDip);}
  Double32_t    GetInvpT()  const { return fInvpT;}
  Double32_t    GetDip()    const { return fDip;}
  Double32_t    GetPhi()    const { return fPhi;}
  Double32_t    GetRho()    const { return fRho;}
  Double32_t    GetpT()     const { return TMath::Abs(fInvpT) > 1.e-7 ? 1./TMath::Abs(fInvpT): 1e7; }
  UInt_t        GetNpoint() const { return fNpoint; }
  Short_t       GetCharge() const { return (Short_t) TMath::Sign(1., fInvpT); }
  Short_t       GetValid()  const { return fValid; }
  UInt_t        GetN()      const { return fNsp; }
  const UInt_t *GetIndx()   const { return fIdHit;}
  Int_t         GetHitId(UInt_t i=0) const {return i < fNsp ? ((Int_t) fIdHit[i])-1 : -1;}

  virtual void SetInvpT(Double32_t p)  {fInvpT = p; }
  virtual void SetDip(Double32_t p)  {fDip = p; }
  virtual void SetPhi(Double32_t p)  {fPhi = p; }
  virtual void SetRho(Double32_t p)  {fRho = p; }
  virtual void SetNpoint(UInt_t p)     {fNpoint = p; }
  virtual void SetValid(Short_t p=1)   {fValid = p; }
  virtual void SetN(UInt_t n) {if (n <= NSP) fNsp = n; else fNsp = NSP;}
  virtual void SetHitId(UInt_t i) {fIdHit[fNsp] = i+1; if ( fNsp < NSP) fNsp++;}
  ClassDef(Track,1)
};

class EventHeader {
  
 private:
  Int_t      fEvtNum;
  Int_t      fRun;
  Int_t      fDate;
  Double32_t fField;
 public:
  EventHeader() : fEvtNum(0), fRun(0), fDate(0), fField(0) { }
  virtual ~EventHeader() { }
  void   Set(Int_t i, Int_t r, Int_t d, Double32_t Field = 0) 
  { fEvtNum = i; fRun = r; fDate = d; fField = Field;}
  Int_t  GetEvtNum() const { return fEvtNum; }
  Int_t  GetRun() const { return fRun; }
  Int_t  GetDate() const { return fDate; }
  Double32_t GetField() const {return fField;}
  
  ClassDef(EventHeader,1)  //Event Header
};


class Event : public TObject {
  
 private:
  UInt_t         fNPTracks;
  UInt_t         fNtrack;            //Number of tracks
  UInt_t         fNhit;              //Number of hits
  UInt_t         fFlag;
  EventHeader    fEvtHdr;
  Double32_t     fVertex[3];         //
  Double32_t     fCovariantMatrix[6];//
  TClonesArray  *fTracks;            //->array with all tracks
  TClonesArray  *fHits;              //->array with all hits
  TRef           fLastTrack;         //reference pointer to last track
  TRef           fLastHit;         //reference pointer to last track
  Bool_t         fIsValid;           //
  
  static TClonesArray *fgTracks;
  static TClonesArray *fgHits;
  
 public:
  Event();
  virtual ~Event();
  Int_t         Build(StEvent *pEvent);
  void          Clear(Option_t *option ="");
  Bool_t        IsValid() const { return fIsValid; }
  static void   Reset(Option_t *option ="");
  void          SetNtrack(UInt_t n) { fNtrack = n; }
  void          SetNhit(UInt_t n) { fNhit = n; }
  void          SetFlag(UInt_t f) { fFlag = f; }
  void          SetHeader(Int_t i, Int_t run, Int_t date, Double32_t field);
  Track        *AddTrack();
  Hit          *AddHit();
  Double32_t    GetVertex(UInt_t i=0) {return (i<3)?fVertex[i]:0;}
  
  UInt_t        GetNtrack() const { return fNtrack; }
  UInt_t        GetNhit() const { return fNhit; }
  UInt_t        GetFlag() const { return fFlag; }
  EventHeader  *GetHeader() { return &fEvtHdr; }
  TClonesArray *GetTracks() const {return fTracks;}
  TClonesArray *GetHits() const {return fHits;}
  Track        *GetLastTrack() const {return (Track*)fLastTrack.GetObject();}
  Hit          *GetLastHit() const {return (Hit*)fLastHit.GetObject();}
  Track        *GetTrack(UInt_t i=0) const {return fTracks && i < fNtrack ? (Track*) fTracks->At(i): 0;}
  Hit          *GetHit(UInt_t i=0) const {return fHits && i < fNhit ? (Hit*) fHits->At(i): 0;}
  Int_t         GetIndexOfTrack(const Track *obj) const {return fgTracks->IndexOf(obj);}
  Int_t         GetIndexOfHit(const Hit *obj) const {return fgHits->IndexOf(obj);}
  ClassDef(Event,1)  //Event structure
    };
#ifndef __CINT__
  ClassImp(EventHeader)
  ClassImp(Event)
  ClassImp(Track)
  ClassImp(Hit)
#endif
  TClonesArray *Event::fgTracks = 0;
  TClonesArray *Event::fgHits = 0;

//______________________________________________________________________________
Event::Event() : fIsValid(kFALSE)
{
  // Create an Event object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("Track", 1000);
  fTracks = fgTracks;
  fNtrack = 0;
  if (!fgHits) fgHits = new TClonesArray("Hit", 1000);
  fHits = fgHits;
  fNhit = 0;
}

//______________________________________________________________________________
Event::~Event()
{
  Clear();
}

//______________________________________________________________________________
Int_t  Event::Build(StEvent *pEvent) {
  Int_t iok = 1;
  fIsValid = kFALSE;
  if (! pEvent) return iok;
  StPrimaryVertex *pVertex = pEvent->primaryVertex();
  if (! pVertex) return iok;
  const StThreeVectorF& xyzP = pVertex->position();
  fVertex[0] = xyzP.x();
  fVertex[1] = xyzP.y();
  fVertex[2] = xyzP.z();
  StMatrixF vCM = pVertex->covariantMatrix();
  fCovariantMatrix[0] = vCM(1,1); // left triangular
  fCovariantMatrix[1] = vCM(1,2);
  fCovariantMatrix[2] = vCM(2,2);
  fCovariantMatrix[3] = vCM(1,3);
  fCovariantMatrix[4] = vCM(2,3);
  fCovariantMatrix[5] = vCM(3,3);
  fNPTracks = pVertex->numberOfDaughters();
  //Save current Object count
  Int_t ObjectNumber = TProcessID::GetObjectCount();
  Clear();
  
  StEventInfo*      info = pEvent->info();
  Int_t ev = 0, run = 0, time = 0;
  if (info) {
    ev   = info->id();
    run  = info->runId();
    time = info->time();
  }
  StEventSummary* summary = pEvent->summary();
  Double32_t field = 0;
  if (summary) field = summary->magneticField();
  SetHeader(ev,run,time,field);
  SetFlag(1);
  //  Create and Fill the Track objects
  for (UInt_t t = 0; t < fNPTracks; t++) {
    StTrack *pTrack = pVertex->daughter(t,primary);
    if (! pTrack) continue;
    static StDetectorId ids[2] = {kSvtId, kSsdId};
    StTrackDetectorInfo*    dInfo = pTrack->detectorInfo();
    if (! dInfo) continue;
    UInt_t npoints = dInfo->numberOfPoints();
    UInt_t Nsp = dInfo->numberOfPoints(ids[0]) + dInfo->numberOfPoints(ids[1]);
    if ( ! Nsp) continue;
    Track *track = AddTrack();
    StThreeVectorD g3 = pTrack->geometry()->momentum();
    Double_t pT = g3.perp();
    Double_t pMom = g3.mag();
    Double_t InvpT = 0;
    Double_t Dip = 0;
    if (TMath::Abs(pT) > 1.e-7) {
      InvpT = pTrack->geometry()->charge()/pT;
      Dip = TMath::ATan2(g3.z(),pT);
    }
    track->SetInvpT(InvpT);
    track->SetPhi(TMath::ATan2(g3.y(),g3.x()));
    track->SetDip(Dip);
    static const Double_t EC = 2.9979251E-4;
    Double_t Rho = - EC*InvpT*field;
    track->SetRho(Rho);
    track->SetN(0);
    track->SetNpoint(npoints);
    const Double_t XyzDirRho[7] = {
      fVertex[0],
      fVertex[1],
      fVertex[2],
      track->GetDip(),
      track->GetPhi(),
      track->GetRho()
    };
    static Double_t dXyzDirRho[6] = {0.01, 0.01, 0.01, 0.001, 0.001, -0.01}; // step for nummerical derivatives
    for (Int_t i = 0; i < 2; i++) {
      StPtrVecHit hits =  dInfo->hits(ids[i]);
      if (! hits.size()) continue;
      for (UInt_t k = 0; k < hits.size(); k++) {
	Hit* h = AddHit();
	Int_t IdH = GetIndexOfHit(h);
	track->SetHitId(IdH);
	h->SetpT(pT);
	h->SetMom(pMom);
	UInt_t B = 0, L = 0, l = 0, W = 0, H = 0;
	StHit *Hit = (StHit *) hits[k];
	StThreeVectorF position = Hit->position();
	if (ids[i] == kSvtId) {
	  StSvtHit *hit = (StSvtHit *) hits[k];
	  B = hit->barrel();
	  L = hit->layer();
	  l = hit->ladder();
	  W = hit->wafer();
	  H = hit->hybrid();
	}
	if (ids[i] == kSsdId) {
	  StSsdHit *hit = (StSsdHit *) hits[k];
	  l = hit->ladder();
	  W = hit->wafer();
	}
	h->SetId(B,L,l,W,H);
	Int_t id = h->GetId();
	Double_t xyzG[3] = {position.x(),position.y(),position.z()};
	Double_t xyzL[3] = {0,0,0};
	TGeoCombiTrans *comb = 0;
	if (fRotList) comb = (TGeoCombiTrans *) fRotList->FindObject(Form("R%4i",id));
	if (comb) comb->MasterToLocal(xyzG,xyzL);
	h->Set(xyzG,xyzL);
	if (comb) {
	  h->SetValidDerivatives();
	  Double_t Derivatives[12];
	  Double_t uP, vP, tuP, tvP;
	  Double_t xyz[3], dir[3], rho;
	  for (Int_t k = -1; k < 6; k++) {
	    Double_t XyzDirRhoI[6];
	    for (Int_t j = 0; j < 6; j++) {
	      XyzDirRhoI[j] = XyzDirRho[j];
	      if (k == j) 
		if (dXyzDirRho[k] > 0) XyzDirRhoI[j] += dXyzDirRho[k];
		else if (dXyzDirRho[k] < 0) XyzDirRhoI[j] = (1. - dXyzDirRho[k])*XyzDirRho[j];
	    }
	    for (Int_t l = 0; l < 3; l++) xyz[l] = XyzDirRhoI[l];
	    dir[0] = TMath::Cos(XyzDirRhoI[3])*TMath::Cos(XyzDirRhoI[4]);
	    dir[1] = TMath::Cos(XyzDirRhoI[3])*TMath::Sin(XyzDirRhoI[4]);
	    dir[2] = TMath::Sin(XyzDirRhoI[3]);
	    rho = XyzDirRhoI[5];
	    THelixTrack vHelixI(xyz,dir,rho);
	    static const Double_t stepMX = 1.e2;
	    const Double_t *tr = comb->GetTranslation();
	    const Double_t *rot = comb->GetRotationMatrix();
	    Double_t RefSurface[4] = {-(tr[0]*rot[2]+tr[1]*rot[5]+tr[2]*rot[8]),
				      rot[2], rot[5], rot[8]};
	    Double_t xyzI[3], dirI[3];
	    //	    Double_t step = 
	    vHelixI.Step(stepMX, RefSurface, 4, xyzI, dirI);
	    Double_t mt0  = xyzI[0]-tr[0];
	    Double_t mt1  = xyzI[1]-tr[1];
	    Double_t mt2  = xyzI[2]-tr[2];
	    Double_t local[3];
	    if (k < 0) {
	      uP = mt0*rot[0] + mt1*rot[3] + mt2*rot[6];
	      vP = mt0*rot[1] + mt1*rot[4] + mt2*rot[7];
	      local[2] = mt0*rot[2] + mt1*rot[5] + mt2*rot[8];
	      h->SetUVPred(uP,vP);
	      tuP = dirI[0]*rot[0] + dirI[1]*rot[3] + dirI[2]*rot[6];
	      tvP = dirI[0]*rot[1] + dirI[1]*rot[4] + dirI[2]*rot[7];
	      local[2] = dirI[0]*rot[2] + dirI[1]*rot[5] + dirI[2]*rot[8];
	      if (TMath::Abs(local[2]) > 1e-7) {
		tuP /= local[2];
		tvP /= local[2];
	      } else {
		tuP = tvP = 0;
		h->SetValidDerivatives(kFALSE);
	      }
	      h->SettUVPred(tuP,tvP);
	    } else {
	      Double_t u = mt0*rot[0] + mt1*rot[3] + mt2*rot[6];
	      Double_t v = mt0*rot[1] + mt1*rot[4] + mt2*rot[7];
	      Double_t dx = XyzDirRhoI[k] - XyzDirRho[k];
	      Derivatives[k]   = (u - uP)/dx;
	      Derivatives[6+k] = (v - vP)/dx;
	    }
	  }
	  h->SetDerivatives(Derivatives);
	}
      }
    }
  }
  TProcessID::SetObjectCount(ObjectNumber);
  if (! GetNtrack()) return iok;
  //Restore Object count 
  //To save space in the table keeping track of all referenced objects
  //we assume that our events do not address each other. We reset the 
  //object count to what it was at the beginning of the event.
  iok = 0;
  fIsValid = kTRUE;
  return iok;
}  

//______________________________________________________________________________
Track *Event::AddTrack()
{
  // Add a new track to the list of tracks for this event.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new Track object will be created
  // otherwise the previous Track[i] will be overwritten.
  
  TClonesArray &tracks = *fTracks;
  Track *track = new(tracks[fNtrack++]) Track();
  //Save reference to last Track in the collection of Tracks
  fLastTrack = track;
  return track;
}
//______________________________________________________________________________
Hit *Event::AddHit()
{
  // Add a new hit to the list of hits for this event.
  // To avoid calling the very time consuming operator new for each hit,
  // the standard but not well know C++ operator "new with placement"
  // is called. If hits[i] is 0, a new Hit object will be created
  // otherwise the previous Hit[i] will be overwritten.
  
  TClonesArray &hits = *fHits;
  Hit *hit = new(hits[fNhit++]) Hit();
  //Save reference to last Hit in the collection of Hits
  fLastHit = hit;
  return hit;
}

//______________________________________________________________________________
void Event::Clear(Option_t * /*option*/)
{
  fTracks->Clear("C"); //will also call Track::Clear
  fHits->Clear("C"); //will also call Hit::Clear
}

//______________________________________________________________________________
void Event::Reset(Option_t * /*option*/)
{
  // Static function to reset all static objects for this event
  //   fgTracks->Delete(option);
  
  delete fgTracks; fgTracks = 0;
  delete fgHits; fgHits = 0;
}

//______________________________________________________________________________
void Event::SetHeader(Int_t i, Int_t run, Int_t date, Double32_t field)
{
  fNtrack = 0;
  fNhit = 0;
  fEvtHdr.Set(i, run, date, field);
}
//________________________________________________________________________________
void SvtTree(const Char_t *Out="Event.root", Int_t nevent = 999) {
  // root4star  'bfc.C(0,"in,StEvent,db,svtDb,ssdDb,simu,nodefault","/star/data06/ITTF/cucu62_ssd/evgen_1.event.root")' SvtTree.C+
  StChain *chain = (StChain *) StMaker::GetChain();
  if (! chain) return;
  // root.exe 
  Int_t split  = 99;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  
  TFile *hfile;
  Event *event = 0;
  Long64_t nb = 0;
  Int_t ev;
  Int_t bufsize;
  //Authorize Trees up to 2 Terabytes (if the system can do it)
  TTree::SetMaxTreeSize(1000*Long64_t(2000000000));
  hfile = new TFile(Out,"RECREATE","TTree with SVT + SSD hits and tracks");
  hfile->SetCompressionLevel(comp);
  // Create a ROOT Tree and one superbranch
  TTree *tree = new TTree("T","TTree with SVT + SSD hits and tracks");
  tree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  bufsize = 64000;
  if (split)  bufsize /= 4;
  event = new Event();
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = tree->Branch("event", &event, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  for (ev = 0; ev < nevent; ev++) {
    chain->Clear();
    if (chain->Make()) break;
    if (! fRotList) {
      fRotList = new THashList(100,0);
      St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
      Char_t *Names[2] = {"svt","ssd"};
      for (Int_t i = 0; i<2; i++) {
	TDataSet *set = dbMk->GetDataBase(Form("Geometry/%s",Names[i]));
	if (! set) continue;
	TDataSetIter next(set,10);
	TDataSet *dat = 0;
	while ((dat = next())) {
	  TString Name(dat->GetName());
	  if (! Name.EndsWith("WafersPosition")) continue;
	  TGeoCombiTrans *comb = 0;
	  if (Name == "svtWafersPosition") {
	    St_svtWafersPosition *table = (St_svtWafersPosition *) dat;
	    svtWafersPosition_st *wafer = table->GetTable();
	    Double_t rot[9] = {
	      wafer->driftDirection[0], wafer->transverseDirection[0], wafer->normalDirection[0],
	      wafer->driftDirection[1], wafer->transverseDirection[1], wafer->normalDirection[1],
	      wafer->driftDirection[2], wafer->transverseDirection[2], wafer->normalDirection[2]};
	    TGeoRotation *r = new TGeoRotation(Form("R%4i",wafer->ID));
	    r->SetMatrix(rot);
	    comb = new TGeoCombiTrans(Form("R%4i",wafer->ID), 
				      wafer->centerPosition[0],
				      wafer->centerPosition[1],
				      wafer->centerPosition[2],
				      r);
	    fRotList->Add(comb);
	  }
	  if (Name == "ssdWafersPosition") {
	    St_ssdWafersPosition *table = (St_ssdWafersPosition *) dat;
	    ssdWafersPosition_st *wafer = table->GetTable();
	    Int_t N = table->GetNRows();
	    for (Int_t k = 0; k < N; k++, wafer++) {
	      Double_t rot[9] = {
		wafer->driftDirection[0], wafer->transverseDirection[0], wafer->normalDirection[0],
		wafer->driftDirection[1], wafer->transverseDirection[1], wafer->normalDirection[1],
		wafer->driftDirection[2], wafer->transverseDirection[2], wafer->normalDirection[2]};
	      TGeoRotation *r = new TGeoRotation(Form("R%4i",wafer->id));
	      r->SetMatrix(rot);
	      comb = new TGeoCombiTrans(Form("R%4i",wafer->id), 
					wafer->centerPosition[0],
					wafer->centerPosition[1],
					wafer->centerPosition[2],
					r);
	      fRotList->Add(comb);
	    }
	  }
	}
      }
    }
    StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
    if (! pEvent) continue;
    if (! pEvent->primaryVertex()) continue;
    if (event->Build(pEvent)) continue;
    nb += tree->Fill();  //fill the tree
  }
  hfile = tree->GetCurrentFile(); //just in case we switched to a new file
  hfile->Write();
  tree->Print();
  
}
