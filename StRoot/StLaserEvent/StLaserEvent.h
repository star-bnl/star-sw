//$Id: StLaserEvent.h,v 1.12 2007/10/16 15:27:39 fisyak Exp $
// Header file for TPC Laser event - Bill Love
//$Log: StLaserEvent.h,v $
//Revision 1.12  2007/10/16 15:27:39  fisyak
//Add default AddTrack
//
//Revision 1.11  2003/09/02 17:58:40  perev
//gcc 3.2 updates + WarnOff
//
//Revision 1.10  2001/12/23 20:07:09  pfachini
//*** empty log message ***
//
//Revision 1.7  2001/12/12 20:49:35  love
//Added //-> to clones array declarations
//
//Revision 1.6  2001/07/17 17:16:41  love
//Add phi to laser track def
//
//Revision 1.5  2001/03/23 15:27:59  love
//Updated README text
//
//Revision 1.4  2000/04/24 14:28:03  love
//Added clock, drivel and tZero to event Header
//
//Revision 1.3  2000/02/01 16:06:30  love
//Added track invp and nfit to Hit object
//
//Revision 1.2  1999/12/01 15:22:38  love
//Bringing up to date with new StLaserEventMaker.  Sorry 'bout that.
//
//Revision 1.1  1999/09/27 21:44:27  love
//LSEvent -> StLaserEvent
//

#ifndef Laser_Event
#define Laser_Event

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLaserEvent                                                         //
//                                                                      //
// Description of a TTree file for Laser Event analysis                 //
// Works also for Cosmic Ray data                                       //
// Each event has a header and TClonesArrays of tracks, hits and pixels //
// hits that are on tracks carry the track Id and some track properties //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TMath.h"

#include <Stiostream.h>


class TDirectory;
class St_tpt_track;

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


public:
   EventHeader() : fEvtNum(0), fRun(0), fDate(0), fTime(0) { }
   virtual ~EventHeader() { }
   void   Set(Int_t i, Int_t r, Int_t d, Int_t t) { fEvtNum = i; fRun = r; fDate = d; fTime = t; }
   void   SetE(Float_t tz, Float_t dv, Float_t ck) {
          ftZero = tz; fDriVel = dv; fClock = ck; }
   void   SetE(Float_t tz, Float_t dv, Float_t ck, Float_t tg) {
          ftZero = tz; fDriVel = dv; fClock = ck; fTrigger = tg; }
   Int_t  GetEvtNum() const { return fEvtNum; }
   Int_t  GetRun() const { return fRun; }
   Int_t  GetDate() const { return fDate; }
   Int_t  GetTime() const { return fTime; }

   ClassDef(EventHeader,1)  //Event Header
};


class StLaserEvent : public TObject {

private:
   Int_t          fNtrack;
   Int_t          fNhit;
   Int_t          fNpixel;
   Int_t          fNseg;
   Int_t          fNvertex;
   UInt_t         fFlag;
   EventHeader    fEvtHdr;
   TClonesArray  *fTracks; //->
   TClonesArray  *fHits;   //->
   TClonesArray  *fPixels;  //->

   static TClonesArray *fgTracks;
   static TClonesArray *fgHits;
   static TClonesArray *fgPixels;

public:
   StLaserEvent();
   virtual ~StLaserEvent();
   void          Clear(Option_t *option ="");
   static void   Reset();
   void          SetNseg(Int_t n) { fNseg = n; }
   void          SetNtrack(Int_t n) { fNtrack = n; }
   void          SetNhit(Int_t n) { fNhit = n; }
   void          SetNpixel(Int_t n) { fNpixel = n; }
   void          SetNvertex(Int_t n) { fNvertex = n; }
   void          SetFlag(UInt_t f) { fFlag = f; }
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time);
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
                 Float_t tzero, Float_t drivel, Float_t clock); 
   void          SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
              Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger);
   void          AddTrack(Int_t flag = 0,Int_t hitid = 0,Int_t tid = 0,Int_t id_globtrk = 0,
         Int_t ndedx = 0, Int_t nfit = 0, Int_t nrec = 0, Int_t npos = 0,
         Int_t q = 0, Float_t Chixy = 0, Float_t Chiyz = 0, Float_t dedx = 0,
         Float_t invp = 0, Float_t curvature = 0, Float_t psi = 0, Float_t tanl = 0,
         Float_t phi0 = 0, Float_t r0 = 0, Float_t z0 = 0, Int_t sector = 0, 
         Float_t xl = 0, Float_t yl = 0, Float_t zl = 0, Float_t phi = 0);

   void          AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag);
   void          AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag, Int_t sector,
                        Float_t zl, Float_t psi,
                        Float_t dx, Float_t dz,Float_t alpha,
                        Float_t lambda,Float_t prf,Float_t zrf);
   void          AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag, Int_t sector,
                        Float_t zl, Float_t psi, Float_t invp, Int_t nfit,
                        Float_t dx, Float_t dz,Float_t alpha,
                        Float_t lambda, Float_t prf, Float_t zrf);
   void          AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag, Int_t sector,
                        Float_t zl, Float_t psi, Float_t invp, Int_t nfit,
                        Float_t dx, Float_t dz,Float_t alpha,
                        Float_t lambda, Float_t prf, Float_t zrf, 
                        Float_t exbdx, Float_t exbdy);
   void          AddPixel(Int_t row,Int_t pad,Int_t time,Int_t adc,
                         Float_t x,Float_t y,Float_t z);

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

   ClassDef(StLaserEvent,1)  //StLaserEvent structure
};


class Hit : public TObject {

 private:
      Float_t fx;      Float_t fy;      Float_t fz;
      Float_t fdx;     Float_t fdz;
      Float_t fq;      Float_t falpha;  Float_t flambda;
      Float_t fprf;    Float_t fzrf;
      Float_t ftkzl;    Float_t ftkpsi; Float_t ftkinvp;
      Float_t fexbdx; Float_t fexbdy;

      Int_t   ftrack;  Int_t   frow;    Int_t   fflag;
      Int_t ftksector; Int_t ftknfit;

public:
   Hit() { }
   Hit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag);
   Hit(Float_t q,Float_t x,Float_t y,Float_t z, Int_t row, Int_t track,
  Int_t flag, Int_t tksector, Float_t tkzl, Float_t tkpsi, Float_t dx,
  Float_t dz,Float_t alpha, Float_t lambda, Float_t prf,Float_t zrf);
  
    Hit(Float_t q,Float_t x,Float_t y,Float_t z, Int_t row, Int_t track,
   Int_t flag, Int_t tksector, Float_t tkzl, Float_t tkpsi, Float_t ftkinvp,
   Int_t tknfit, Float_t dx, Float_t dz,Float_t alpha, Float_t lambda,
   Float_t prf,Float_t zrf);

  Hit(Float_t q,Float_t x,Float_t y,Float_t z, Int_t row, Int_t track,
  Int_t flag, Int_t tksector, Float_t tkzl, Float_t tkpsi, Float_t ftkinvp,
  Int_t tknfit, Float_t dx, Float_t dz,Float_t alpha, Float_t lambda,
  Float_t prf,Float_t zrf, Float_t exbdx, Float_t exbdy);

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
  //  struct tpt_track {          /* Tptrack_row_st */ 
       Int_t       fStatus;       // status flag */
       Int_t       fhitid;      // ID of the hit where parameters are given */
       Int_t       ftid;         // track id 
       Int_t       fid_globtrk; // Pointer to the globtrk table row. 
       Int_t       fndedx;      // Number of points used in dE/dx calc 
       Int_t       fnfit;       // Number of points included in the fit 
       Int_t       fnrec;       // Number of points assigned to that track
       Int_t       fnpos;       // Number of geometrically possible points
       Int_t       fq;          // charge */
       Float_t     fChixy;     // chi squared of the momentum fit */
       Float_t     fChiyz;     // chi squared of the momentum fit */
    // Float_t     cov[15];    // covariance matrix(psi,invp,tanl,phi,z) */
    //  Float_t     dedx[2];    // dE/dx information */
       Float_t     fdedx;
       Float_t     finvp;       // 1/pt (transverse momentum) at (r,phi,z) */
       Float_t     fcurvature;  // 1/radius */
       Float_t     fpsi;        // azimuthal angle of the momentum at (r,.. */
       Float_t     ftanl;       // tg of the dip angle at (r,phi,z) */
       Float_t     fphi0;       // azimuthal angle of the first point */
       Float_t     fr0;         // r (in cyl. coord.) for the first point */
       Float_t     fz0;         // z coordinate of the first point */
       Float_t     fxl;         // x of point of close app. to laser source.
       Float_t     fyl;         // y of point of closest approach
       Float_t     fzl;         // z of point of closest approach
       Float_t     fphi;        // phi of track at the xl, yl, zl point.
       Int_t       fsector;     // sector of laser source point.
 
public:
   Track() { }
   Track(Int_t flag,Int_t hitid,Int_t tid,Int_t id_globtrk,
         Int_t ndedx, Int_t nfit, Int_t nrec, Int_t npos,
         Int_t q, Float_t Chixy, Float_t Chiyz, Float_t dedx,
         Float_t invp, Float_t curvature, Float_t psi, Float_t tanl,
         Float_t phi0, Float_t r0, Float_t z0, Int_t sector, Float_t xl,
         Float_t yl, Float_t zl, Float_t phi);

   virtual ~Track() { }
   // Float_t       GetPx() const { return fPx; }
  
   Float_t       GetDedx() const { return fdedx; }
   Float_t       GetChixy() const { return fChixy; }
   Float_t       GetChiyz() const { return fChiyz; }
   Int_t         GetTrackId() const { return ftid; }
   Int_t         GetNrec() const { return fnrec; }
   Int_t         GetNfit() const { return fnfit; }
   Int_t         GetNposs() const { return fnpos; }
   Float_t       GetInvp() const { return finvp; }
   Float_t       GetTanl() const { return ftanl; }
   Float_t       GetPsi() const { return fpsi; }
   Int_t         GetQ() const { return fq; }

   ClassDef(Track,1)  //An expanded tpt_track rep of a laser track. 
};




#endif
