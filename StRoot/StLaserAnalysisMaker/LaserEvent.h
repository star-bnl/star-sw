//$Id: LaserEvent.h,v 1.10 2014/03/13 21:59:44 fisyak Exp $
//$Log: LaserEvent.h,v $
//Revision 1.10  2014/03/13 21:59:44  fisyak
//add cluster position in Local Sector Coordinate System
//
//Revision 1.9  2014/02/13 18:21:28  fisyak
//Add protection against cicling in fitting
//
//Revision 1.8  2011/05/27 18:25:32  genevb
//Propagate StTrack::key => Int_t to other codes
//
//Revision 1.7  2008/06/02 13:48:03  fisyak
//Add  t0 handlers for Tpx/Tpc time offsets
//
//Revision 1.6  2007/12/10 19:54:02  fisyak
//Add Id and Log, correct spelling error in README
//
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
#include "StDcaGeometry.h"
#include "TString.h"
#include "TGeoMatrix.h"
#ifdef __CINT__
class StPrimaryVertex;
class StTrack;
class StTpcHit;
#else
#include "StPrimaryVertex.h"
#include "StTrack.h"
#include "StTpcHit.h"
#endif
class LaserRaft : public TObject {
 public: 
  LaserRaft() {}
  Int_t Sector, Raft, Bundle, Mirror;
  StThreeVectorD XyzL, dirL; // in TPC
  StThreeVectorD XyzU, dirU; // in Raft
  StThreeVectorD XyzB, dirB; // in Bundle
  
  Double_t Theta, Phi; // [rad]
  //  Double_t psi, dpsi, tanl, dtanl, xl, dxL, yl, dyL, zl, dzl;
  Char_t *Name;
  virtual void Print(const Option_t* opt="") const {
    if (opt) {}
	  cout << Form("Raft:%2i,%2i,%1i,%1i,%9.4f,%9.4f",
		       Sector, Raft, Bundle, Mirror, Theta, Phi) << endl;
	  cout << " XyzL: " << XyzL << " U: " << XyzU << " B: " << XyzB << endl;
	  cout << " dirL: " << dirL << " U: " << dirU << " B: " << dirB	<< endl;
  }
  ClassDef(LaserRaft,1)
};
class  FitDV : public TObject {
 public:
  FitDV() {Clear();}
  virtual ~FitDV() {}
  void Clear(Option_t *opt="") {if (opt) {}; memset(first, 0, last - first); chisq = -1;}
  Char_t     first[1];
  Int_t      N;
  Int_t      Sector;
  Int_t      Bundle[42];
  Int_t      Mirror[42];
  Double32_t offset;
  Double32_t slope;
  Double32_t doffset;
  Double32_t dslope;
  Double32_t chisq;
  Double32_t X[42];
  Double32_t Y[42];
  Double32_t Prob;
  Int_t      ndf;
  Int_t      Flag[42];
  Char_t     last[1];
  virtual void Print(const Option_t* opt="") const {
    if (opt) {}
    cout << Form("FitDV:%5i,%2i,%9.4f,%9.4f,%9.4f,%9.4f,%9.4f",
		 N, Sector, offset, slope, doffset, dslope, chisq) 
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
  StThreeVectorD XyzS; // [cm]  in TPC super Sector
  StThreeVectorD XyzU; // [cm]  in raft
  StThreeVectorD XyzB; // [cm]  in bundle
  StThreeVectorD dirG;
  StThreeVectorD dirL;
  StThreeVectorD dirS;
  StThreeVectorD dirU;
  StThreeVectorD dirB;
  Double_t Theta,  Phi;
  Double_t ThetaG, PhiG; // in GCS
  virtual void Print(const Option_t* opt="") const {
    if (opt) {}
    cout << "LaserB:" << IsValid << " S/R/B/M = " << Sector << "/" << Raft << "/" << Bundle << "/" << Mirror
	 << " xyz B:" << XyzB << " U: " << XyzU << " L: " << XyzL << " G: " << XyzG << endl;
    cout << "\tdir B:" << dirB << " U: " << dirU << " L: " << dirL << " G: " << dirG << endl;
    cout << "\tTheta L: " << Theta << " G: " << ThetaG << " Phi L: " << Phi << " G: " << PhiG 
	 << endl;
  }
  ClassDef(LaserB,3)
};

class EventHeader {

 private:
   Int_t   fEvtNum;
   Int_t   fRun;
   Int_t   fDate;
   Int_t   fTime;
   Float_t ftZero;
   Float_t fDriVel;
 public:
   Float_t fDriVelWest;
   Float_t fDriVelEast;
 private:
   Float_t fClock;
   Float_t fTrigger;
   Float_t fDriftDistance;  
   Float_t fInnerSectorzOffset;
   Float_t fOuterSectorzOffset;
   Float_t ftriggerTimeOffset; 
   Float_t fOnlClock;
   Float_t fBField;
 public:
   Float_t fScaleY;
public:
   EventHeader() : fEvtNum(0), fRun(0), fDate(0), fTime(0), fOnlClock(0), fBField(0), fScaleY(0) { }
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
   void   SetBField(Float_t p) {fBField = p;}
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
   Float_t GetBField() {return fBField;}
   
   ClassDef(EventHeader,3)  //Event Header
};
class Vertex : public TObject {
 public:
  Vertex(StPrimaryVertex *vertex=0);
  virtual ~Vertex() {}
  StVertexId     mType;
  Int_t          WestEast; // = 1 for West and 2 for East, 0 - undefined
  StThreeVectorD Xyz;
  StThreeVectorD XyzL;     // Tpc Local
  UInt_t         numberOfDaughter;
  virtual void Print(const Option_t* opt="") const {
    if (opt) {}
    cout << "Vertex: WE = " << WestEast << " XYZ L: " << XyzL << " G: " << Xyz << " Nd " << numberOfDaughter << endl;
  }    
  ClassDef(Vertex,1) 
};
class Hit : public TObject {
 public:
  Hit(StTpcHit *tpcHit=0, Int_t trackKey=0);
  virtual ~Hit() {}
  UShort_t sector;
  UShort_t row;
  Float_t  charge;
  UInt_t   flag;
  Int_t    usedInFit;
  StThreeVectorF xyz;
  StThreeVectorF xyzL;     // Tpc Local Sector 
  StThreeVectorF xyzS;     // Tpc Super Sector 
  StThreeVectorF xyzTpcL;  // Tpc Local
  Float_t  pad;
  Float_t  tbk;
  Int_t    trackKey;
  StTpcHit hit;
  ClassDef(Hit,6) 
};
class Track : public TObject {
 public:
  Int_t          Flag;// 0 there no is match between track and laser; 1 if it is  matched, 2 primary track 
  // StVertex
  StVertexId     mType;
  StThreeVectorD Vertex;
  // StTrack
  Int_t          mSector;
  Int_t          mKey;
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
  StDcaGeometry  fDca;
  Double32_t     fpTInv;
  Double32_t     fTheta;
  Double32_t     fPhi;
  StThreeVectorD XyzP; // laser prediction on the mirror in GCS
  StThreeVectorD XyzPL;// laser prediction on the mirror in TPC CS
  StThreeVectorD XyzPU;// laser in raft coordinate system 
  StThreeVectorD XyzPB;// laser in raft coordinate system 
  StThreeVectorD XyzPM;// laser in raft coordinate system 
  StThreeVectorD dirP; // laser prediction on the mirror in GCS
  StThreeVectorD dirPL;// laser prediction on the mirror in TPC CS
  StThreeVectorD dirPU;// laser in raft coordinate system 
  StThreeVectorD dirPB;
  StThreeVectorD dirPM;
  StThreeVectorD dU;
  Double32_t     thePath;
  LaserB         Laser;
  Double32_t     dPhi;
  Double32_t     dTheta;
#if 0
  FitDV          fit;
#endif
  Double32_t     zLastHit;
 public:
  Track() {};
  Track(Int_t sector, StTrack *track, LaserB *laser = 0, Double_t z=0);
  virtual ~Track() { }
  Int_t Matched();
  void SetPredictions(TGeoHMatrix *Raft2Tpc = 0, TGeoHMatrix *Bundle2Tpc = 0, TGeoHMatrix *Mirror2Tpc = 0);
  virtual void Print(const Option_t* opt="") const {
    if (opt) {}
    cout << "Track F: " << Flag << " Vtx: " << Vertex << " Sec/Key = " << mSector << "/" << mKey << endl;
    cout << "\tgeoIn pxyz " << fgeoIn.momentum() << " geoIn xyz " << fgeoIn.origin() << endl;
    cout << "\tgeoOut pxyz " << fgeoOut.momentum() << " geoOut xyz " << fgeoOut.origin() << endl;
    cout << "\tTheta " << fTheta << " Phi " << fPhi << endl;
    cout << "\tXyzP M " << XyzPM << " B: " << XyzPB << " U: " << XyzPU  << " L: " << XyzPL << " G: " << XyzP << endl; 
    cout << "\tdirP M " << dirPM << " B: " << dirPB << " U: " << dirPU  << " L: " << dirPL << " G: " << dirP << endl; 
    cout << "\tdU : " << dU << endl;
    Laser.Print();
    cout << "\tdTheta " << dTheta << " dPhi " << dPhi << endl;
  }
  ClassDef(Track,3)  
};
class LaserEvent : public TObject {

private:
   Int_t          fNtrack;
   Int_t          fNhit;
   Int_t          fNvertex;
   EventHeader    fEvtHdr;
   TClonesArray  *fVertices; //->
   TClonesArray  *fTracks; //->
   TClonesArray  *fHits; //->
   TClonesArray  *fFit;   //->
 public:
   TClonesArray  *Vertices(){return  fVertices;} 
   TClonesArray  *Tracks()  {return  fTracks;}   
   TClonesArray  *Hits()    {return  fHits;}   
   TClonesArray  *Fit()     {return  fFit;}      
   
 private:
   static TClonesArray *fgTracks;
   static TClonesArray *fgHits;
   static TClonesArray *fgVertices;
   static TClonesArray *fgFit;
 public:
   LaserEvent();
   virtual ~LaserEvent();
   void          Clear(Option_t *option ="");
   static void   Reset();
   void          SetNtrack(Int_t n) { fNtrack = n; }
   void          SetNhit(Int_t n) { fNhit = n; }
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time);
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
                 Float_t tzero, Float_t drivel, Float_t clock); 
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
			   Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger);
   void          SetDVWest(Float_t dv) {fEvtHdr.fDriVelWest = dv;}
   void          SetDVEast(Float_t dv) {fEvtHdr.fDriVelEast = dv;}
   void          SetScaleY(Float_t sY) {fEvtHdr.fScaleY = sY;}
   Vertex       *AddVertex(StPrimaryVertex *vertex = 0);
   Track        *AddTrack(Int_t sector = 0, StTrack *track = 0, LaserB *laser = 0, Double_t zLastHit=0);
   Hit          *AddHit(StTpcHit *tpcHit = 0, Int_t trackKey = 0);
   void          AddTrackFit(Track *t = 0);
   Int_t         GetNtrack() const { return fNtrack; }
   Int_t         GetNhit() const { return fNhit; }
   EventHeader  *GetHeader() { return &fEvtHdr; }
   TClonesArray *GetTracks() const { return fTracks; }
   TClonesArray *GetHits() const { return fHits; }
   ClassDef(LaserEvent,2)  //LaserEvent structure
};

#endif
