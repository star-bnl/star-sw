//$Id: LaserEvent.h,v 1.3 2007/05/09 13:36:44 fisyak Exp $

#ifndef Laser_Event
#define Laser_Event

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// LaserEvent                                                           //
//                                                                      //
// Description of a TTree file for Laser Event analysis                 //
// Works also for Cosmic Ray data                                       //
// Each event has a header and TClonesArrays of tracks
// hits that are on tracks carry the track Id and some track properties //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TMath.h"
#include "LaserBeams.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"
#include "StHelixModel.h"
#include "TString.h"
#ifdef __CINT__
class StPrimaryVertex;
class StTrack;
#else
#include "StPrimaryVertex.h"
#include "StTrack.h"
#endif
class LaserRaft : public TObject {
 public: 
  LaserRaft() {}
  Int_t Sector, Raft, Bundle, Mirror;
  StThreeVectorD XyzL;
  StThreeVectorD XyzU;
  StThreeVectorD dirL;
  StThreeVectorD dirU;
  Double_t Theta, Phi; // [rad]
  //  Double_t psi, dpsi, tanl, dtanl, xl, dxL, yl, dyL, zl, dzl;
  Char_t *Name;
  virtual void Print(const Option_t* opt="") const {
	  cout << Form("Raft:%2i,%2i,%1i,%1i,%9.4f,%9.4f",
		       Sector, Raft, Bundle, Mirror, Theta, Phi) 
	       << " XyzL: " << XyzL << " U: " << XyzU
	       << endl;
  }
  ClassDef(LaserRaft,1)
};
class  FitDV : public TObject {
 public:
  FitDV() {Clear();}
  virtual ~FitDV() {}
  void Clear(Option_t *opt="") {N = Sector = 0; xM = yM = x2M = y2M = xyM = offset = slope = doffset = dslope = 0; chisq = -1;}
  Int_t      N;
  Int_t      Sector;
  Double32_t xM;
  Double32_t yM;
  Double32_t x2M;
  Double32_t y2M;
  Double32_t xyM;
  Double32_t offset;
  Double32_t slope;
  Double32_t doffset;
  Double32_t dslope;
  Double32_t chisq;
  virtual void Print(const Option_t* opt="") const {
    cout << Form("FitDV:%5i,%2i,%9.4f,%9.4f,%9.4f,%9.4f,%9.4,%9.4f,%9.4f,%9.4f,%9.4f,%9.4",
		 N, Sector, xM, yM, x2M, y2M, xyM, offset, slope, doffset, dslope, chisq) 
	 << endl;
    
  }
  ClassDef(FitDV,1)    
};
class  LaserB {
 public:
  LaserB(const LaserRaft &laser);
  LaserB() : IsValid(1), Sector(0), Raft(0), Bundle(0), Mirror(0),   
    Theta(0), Phi(0), ThetaG(0), PhiG(0) {}
  virtual ~LaserB() {}
  Int_t IsValid;
  Int_t Sector, Raft, Bundle, Mirror;
  StThreeVectorD XyzG; // [cm]  in GCS
  StThreeVectorD XyzL; // [cm]  in TPC
  StThreeVectorD XyzU; // [cm]  in raft
  StThreeVectorD dirG;
  StThreeVectorD dirL;
  StThreeVectorD dirU;
  Double_t Theta,  Phi;
  Double_t ThetaG, PhiG; // in GCS
  virtual void Print(const Option_t* opt="") const {
    cout << "LaserB:" << IsValid << " S/R/B/M = " << Sector << "/" << Raft << "/" << Bundle << "/" << Mirror
	 << " xyz U: " << XyzU << " L: " << XyzL << " G: " << XyzG << endl;
    cout << "\tdir U: " << dirU << " L: " << dirL << " G: " << dirG << endl;
    cout << "\tTheta L: " << Theta << " G: " << ThetaG << " Phi L: " << Phi << " G: " << PhiG 
	 << endl;
  }
  ClassDef(LaserB,2)
};

class EventHeader {

private:
   Int_t   fEvtNum;
   Int_t   fRun;
   Int_t   fDate;
   Int_t   fTime;
   Float_t ftZero;
   Float_t fDriVel;
   Float_t fClock;
   Float_t fTrigger;
   Float_t fDriftDistance;  
   Float_t fInnerSectorzOffset;
   Float_t fOuterSectorzOffset;
   Float_t ftriggerTimeOffset; 
   Float_t fOnlClock;
public:
   EventHeader() : fEvtNum(0), fRun(0), fDate(0), fTime(0), fOnlClock(0) { }
   virtual ~EventHeader() { }
   void   Set(Int_t i, Int_t r, Int_t d, Int_t t) { fEvtNum = i; fRun = r; fDate = d; fTime = t; }
   void   SetE(Float_t tz, Float_t dv, Float_t ck) {
          ftZero = tz; fDriVel = dv; fClock = ck; }
   void   SetE(Float_t tz, Float_t dv, Float_t ck, Float_t tg) {
          ftZero = tz; fDriVel = dv; fClock = ck; fTrigger = tg; }
   void   SetDriftDistance(Double_t p) {fDriftDistance = p;}
   void   SetInnerSectorzOffset(Double_t p) {fInnerSectorzOffset= p;}
   void   SetOuterSectorzOffset(Double_t p) {fOuterSectorzOffset= p;}
   void   SettriggerTimeOffset(Double_t p)  {ftriggerTimeOffset= p;}
   void   SetOnlClock(Double_t p)           {fOnlClock = p;} 
   Int_t  GetEvtNum() const { return fEvtNum; }
   Int_t  GetRun() const { return fRun; }
   Int_t  GetDate() const { return fDate; }
   Int_t  GetTime() const { return fTime; }
   Float_t tZeor() {return ftZero;}
   Float_t DriftVel() {return fDriVel;}
   Float_t Clock()    {return fClock;}
   Float_t OnlClock()    {return fOnlClock;}
   Float_t Trigger()  {return fTrigger;}
   Float_t DriftDistance()      {return   fDriftDistance;}  
   Float_t InnerSectorzOffset() {return fInnerSectorzOffset;}
   Float_t OuterSectorzOffset() {return fOuterSectorzOffset;}
   Float_t triggerTimeOffset()  {return  ftriggerTimeOffset;} 
   
   ClassDef(EventHeader,1)  //Event Header
};
class Zees : public TObject {
 public:
  Zees() {zP = zL = dP = tD = tL = 0;}
  virtual ~Zees() {}
  Double32_t zP; // track at mirror
  Double32_t zL; // mirror Z position
  Double32_t dP; // drift distance for prediction; dP = |zE - zP|;
  Double32_t tD; // drift time for track at mirror tD = dP/v
  Double32_t tL; // light delay time tL = (zE - zL)/c;
  virtual void Print(const Option_t* opt="") const {
    cout << Form("Zees:%9.4f,%9.4f,%9.4f,%9.4f,%9.4f",zP, zL, dP, tD, tL)
	 << endl;
  }
  ClassDef(Zees,1)
};

class Vertex : public TObject {
 public:
  Vertex(StPrimaryVertex *vertex=0);
  virtual ~Vertex() {}
  StVertexId     mType;
  Int_t          WestEast; // = 1 for West and 2 for East, 0 - undefined
  StThreeVectorD Xyz;
  StThreeVectorD XyzL;
  UInt_t         numberOfDaughter;
  virtual void Print(const Option_t* opt="") const {
    cout << "Vertex: WE = " << WestEast << " XYZ L: " << XyzL << " G: " << Xyz << " Nd " << numberOfDaughter << endl;
  }    
  ClassDef(Vertex,1) 
};

class Track : public TObject {
 public:
  Int_t          Flag;// 0 there no is match between track and laser; 1 if it is  matched, 2 primary track 
  // StVertex
  StVertexId     mType;
  StThreeVectorD Vertex;
  // StTrack
  Int_t          mSector;
  UShort_t       mKey;
  Short_t        mFlag;
  UShort_t       mNumberOfPossiblePointsTpc;
  Float_t        mImpactParameter;
  Float_t        mLength;
  // StTrackFitTraits
  UShort_t       mNumberOfFitPointsTpc;
  UShort_t       mPrimaryVertexUsedInFit;
#if 1    
  Int_t          fNdEdx;      // Number of points used in dE/dx calc 
  Float_t        fdEdx;      
#endif
  StHelixModel   fgeoIn;
  StHelixModel   fgeoOut;
  Double32_t     fTheta;
  Double32_t     fPhi;
  StThreeVectorD XyzP; // laser prediction on the mirror in GCS
  StThreeVectorD XyzPL;// laser prediction on the mirror in TPC CS
  StThreeVectorD XyzPU;// laser in raft coordinate system 
  StThreeVectorD dirP; // laser prediction on the mirror in GCS
  StThreeVectorD dirPL;// laser prediction on the mirror in TPC CS
  StThreeVectorD dirPU;// laser in raft coordinate system 
  StThreeVectorD dU;
  Double32_t     thePath;
  LaserB         Laser;
  Double32_t     dPhi;
  Double32_t     dTheta;
 public:
  Track() {};
  Track(Int_t sector, StTrack *track, LaserB *laser = 0);
  virtual ~Track() { }
  Int_t Matched();
  virtual void Print(const Option_t* opt="") const {
    cout << "Track F: " << Flag << " Vtx: " << Vertex << " Sec/Key = " << mSector << "/" << mKey << endl;
    cout << "\tgeoIn pxyz " << fgeoIn.momentum() << " geoIn xyz " << fgeoIn.origin() << endl;
    cout << "\tgeoOut pxyz " << fgeoOut.momentum() << " geoOut xyz " << fgeoOut.origin() << endl;
    cout << "\tTheta " << fTheta << " Phi " << fPhi << endl;
    cout << "\tXyzP U: " << XyzPU  << " L: " << XyzPL << " G: " << XyzP << endl; 
    cout << "\tdU : " << dU << endl;
    Laser.Print();
    cout << "\tdTheta " << dTheta << " dPhi " << dPhi << endl;
  }
  ClassDef(Track,1)  //An expanded tpt_track rep of a laser track. 
};
class LaserEvent : public TObject {

private:
   Int_t          fNtrack;
   Int_t          fNvertex;
   Int_t          fNzEast;
   Int_t          fNzWest;
   Int_t          fNFit;
   EventHeader    fEvtHdr;
   TClonesArray  *fVertices; //->
   TClonesArray  *fTracks; //->
   TClonesArray  *fzEast; //->
   TClonesArray  *fzWest; //->
   TClonesArray  *fFit;   //->
 public:
   TClonesArray  *Vertices(){return  fVertices;} 
   TClonesArray  *Tracks(){return    fTracks;}   
   TClonesArray  *zEast(){return     fzEast;}    
   TClonesArray  *zWest(){return     fzWest;}    
   TClonesArray  *Fit(){return       fFit;}      
   
 private:
   static TClonesArray *fgTracks;
   static TClonesArray *fgVertices;
   static TClonesArray *fgzEast;
   static TClonesArray *fgzWest;
   static TClonesArray *fgFit;
 public:
   LaserEvent();
   virtual ~LaserEvent();
   void          Clear(Option_t *option ="");
   static void   Reset();
   void          SetNtrack(Int_t n) { fNtrack = n; }
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time);
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
                 Float_t tzero, Float_t drivel, Float_t clock); 
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
			   Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger);
   Vertex       *AddVertex(StPrimaryVertex *vertex = 0);
   Track        *AddTrack(Int_t sector = 0, StTrack *track = 0, LaserB *laser = 0);
   Int_t         GetNtrack() const { return fNtrack; }
   EventHeader  *GetHeader() { return &fEvtHdr; }
   TClonesArray *GetTracks() const { return fTracks; }
   ClassDef(LaserEvent,1)  //LaserEvent structure
};

#endif
