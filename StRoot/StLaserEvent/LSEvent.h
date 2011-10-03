#ifndef Laser_Event
#define Laser_Event

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// LSEvent                                                                //
//                                                                      //
// Description of the event and track parameters                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TMath.h"

#include <iostream.h>


class TDirectory;


class EventHeader {

private:
   Int_t   fEvtNum;
   Int_t   fRun;
   Int_t   fDate;

public:
   EventHeader() : fEvtNum(0), fRun(0), fDate(0) { }
   virtual ~EventHeader() { }
   void   Set(Int_t i, Int_t r, Int_t d) { fEvtNum = i; fRun = r; fDate = d; }
   Int_t  GetEvtNum() const { return fEvtNum; }
   Int_t  GetRun() const { return fRun; }
   Int_t  GetDate() const { return fDate; }

   ClassDef(EventHeader,1)  //Event Header
};


class LSEvent : public TObject {

private:
   Int_t          fNtrack;
   Int_t          fNhit;
   Int_t          fNpixel;
   Int_t          fNseg;
   Int_t          fNvertex;
   UInt_t         fFlag;
   EventHeader    fEvtHdr;
   TClonesArray  *fTracks;
   TClonesArray  *fHits;
   TClonesArray  *fPixels;

   static TClonesArray *fgTracks;
   static TClonesArray *fgHits;
   static TClonesArray *fgPixels;

public:
   LSEvent();
   virtual ~LSEvent();
   void          Clear(Option_t *option ="");
   static void   Reset();
   void          SetNseg(Int_t n) { fNseg = n; }
   void          SetNtrack(Int_t n) { fNtrack = n; }
   void          SetNhit(Int_t n) { fNhit = n; }
   void          SetNpixel(Int_t n) { fNpixel = n; }
   void          SetNvertex(Int_t n) { fNvertex = n; }
   void          SetFlag(UInt_t f) { fFlag = f; }
   void          SetHeader(Int_t i, Int_t run, Int_t date);
   void          AddTrack(Int_t tid,Int_t nfit, Float_t ax, Float_t az,
                     Float_t x0, Float_t y0, Float_t z0,
                     Float_t Chixy, Float_t Chizy);
   void          SetLastTrack(Int_t nlast);
   void          AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag);
   void          SetLastHit(Int_t nlast);
   void          AddPixel(Int_t row,Int_t pad,Int_t time,Int_t adc,
                         Float_t x,Float_t y,Float_t z);
   void          SetLastPixel(Int_t nlast);

   Int_t         GetNtrack() const { return fNtrack; }
   Int_t         GetNhit() const { return fNhit; }
   Int_t         GetNPixel() const { return fNpixel; }
   Int_t         GetNseg() const { return fNseg; }
   Int_t         GetNvertex() const { return fNvertex; }
   UInt_t        GetFlag() const { return fFlag; }
   EventHeader  *GetHeader() { return &fEvtHdr; }
   TClonesArray *GetTracks() const { return fTracks; }
   TClonesArray *GetHits() const { return fHits; }
   TClonesArray *GetPixels() const { return fPixels; }

   ClassDef(LSEvent,1)  //LSEvent structure
};


class Hit : public TObject {

 private:
      Float_t fx;
      Float_t fy;
      Float_t fz;
      Float_t fq;
      Int_t   ftrack;
      Int_t   frow;
      Int_t   fflag;

public:
   Hit() { }
   Hit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag);
   virtual ~Hit() { }      
   ClassDef(Hit,1)  //A TPC TPhit object
};

class Pixel : public TObject {
private:
   Int_t frow;
   Int_t fpad;
   Int_t ftime;
   Int_t fadc;
   Float_t fx;
   Float_t fy;
   Float_t fz;
public:
   Pixel() { }
   Pixel(Int_t row, Int_t pad, Int_t time, Int_t adc,
       Float_t x,Float_t y,Float_t z);
   virtual ~Pixel() { }
   ClassDef(Pixel,1)  //An ADC in a time bin on a pad.
};

class Track : public TObject {

private:
   Float_t      fPx;           //X component of the momentum
   Float_t      fPy;           //Y component of the momentum
   Float_t      fPz;           //Z component of the momentum

   Float_t      fBx;           //X position at the vertex
   Float_t      fBy;           //Y position at the vertex
   Float_t      fBz;           //Z position at the vertex
   Float_t      fDedx;   //Mean q deposition of all hits of this track
   Float_t      fChisqxy;      //Chi square of the xy fit
   Float_t      fChisqzy;      //Chi square of the zy fit
   Int_t        fNpoint;       //Number of points for this track
   Int_t        fTrackId;      //Track number
   Short_t      fValid;        //Validity criterion

public:
   Track() { }
   Track(Int_t tid, Int_t nfit, Float_t ax, Float_t az,
         Float_t x0, Float_t y0, Float_t z0,
         Float_t Chixy, Float_t Chizy);
   virtual ~Track() { }
   Float_t       GetPx() const { return fPx; }
   Float_t       GetPy() const { return fPy; }
   Float_t       GetPz() const { return fPz; }
   Float_t       GetPt() const { return TMath::Sqrt(fPx*fPx + fPy*fPy); }
   Float_t       GetBx() const { return fBx; }
   Float_t       GetBy() const { return fBy; }
   Float_t       GetBz() const { return fBz; }
   Float_t       GetX(Float_t y) const {return fBx + (fPx/fPy)*(y-fBy);}
   Float_t       GetZ(Float_t y) const {return fBz + (fPz/fPy)*(y-fBy);}
   Float_t       GetDedx() const { return fDedx; }
   void          SetDedx(Float_t Dedx=0.0)  { fDedx = Dedx; }
   Float_t       GetChisqxy() const { return fChisqxy; }
   Float_t       GetChisqzy() const { return fChisqzy; }
   Int_t         GetTrackId() const { return fTrackId; }
   Int_t         GetNpoint() const { return fNpoint; }
   Short_t       GetValid()  const { return fValid; }
   virtual void  SetValid(Int_t valid=1) { fValid = valid; }

   ClassDef(Track,1)  //A straight laser track 
};




#endif
