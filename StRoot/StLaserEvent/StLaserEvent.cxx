// 
// ROOT Tree for Laser Events tracking by tpt. -- Bill Love
//$Log: StLaserEvent.cxx,v $
//Revision 1.5  2000/04/24 14:28:03  love
//Added clock, drivel and tZero to event Header
//
//Revision 1.4  2000/02/01 16:06:30  love
//Added track invp and nfit to Hit object
//
//Revision 1.3  2000/01/12 21:54:08  love
//Changed first line from //&Id$ to //
//
//Revision 1.2  1999/12/01 15:22:38  love
//Bringing up to date with new StLaserEventMaker.  Sorry 'bout that.
//
//Revision 1.1  1999/09/27 21:44:27  love
//LSEvent -> StLaserEvent
//

//* adapted from Laser straight track code 27/9/99 by Bill Love

////////////////////////////////////////////////////////////////////////
//
//                       St_LaserEvent and Track classes
//                       =======================
//
//  The St_LaserEvent class is a simple event structure.
//     public:
//        Int_t          fNtrack;
//        Int_t          fNhit;
//        Int_t          fNpixel;
//        Int_t          fNseg;
//        Int_t          fNvertex;
//        UInt_t         fFlag;
//        EventHeader    fEvtHdr;
//        TClonesArray  *fTracks;
//        TClonesArray  *fHits;
//        TClonesArray  *fPixels;
//
//   The EventHeader class has 3 data members (integers):
//     public:
//        Int_t          fEvtNum;
//        Int_t          fRun;
//        Int_t          fDate;
//
//
//   The St_LaserEvent data members fTracks, fHits, fPixels are pointers to 
//   TClonesArrays, each an array of a variable number of tracks, hits,
//   pixels per event.
//   Each element of each array is an object of class Track or Hit or Pixel


#include "TRandom.h"
#include "TDirectory.h"

#include "StLaserEvent.h"


ClassImp(EventHeader)
ClassImp(StLaserEvent)
ClassImp(Track)
ClassImp(Hit)
ClassImp(Pixel)

TClonesArray *StLaserEvent::fgTracks = 0;
TClonesArray *StLaserEvent::fgHits = 0;
TClonesArray *StLaserEvent::fgPixels = 0;

//______________________________________________________________________________
StLaserEvent::StLaserEvent()
{
   // Create an StLaserEvent object.
   // When the constructor is invoked for the first time, the class static
   // variable fgTracks is 0 and the TClonesArray fgTracks is created.

   if (!fgTracks) fgTracks = new TClonesArray("Track", 1000);
   fTracks = fgTracks;
   fNtrack = 0;
   if (!fgHits) fgHits = new TClonesArray("Hit", 1000);
   fHits = fgHits;
   fNhit = 0;
   if (!fgPixels) fgPixels = new TClonesArray("Pixel", 1000);
   fPixels = fgPixels;
   fNpixel = 0;
}

//______________________________________________________________________________
StLaserEvent::~StLaserEvent()
{
   Clear();
}

//______________________________________________________________________________
void StLaserEvent::AddTrack(Int_t flag,Int_t hitid,Int_t tid,Int_t id_globtrk,
         Int_t ndedx, Int_t nfit, Int_t nrec, Int_t npos,
         Int_t q, Float_t Chixy, Float_t Chiyz, Float_t dedx,
         Float_t invp, Float_t curvature, Float_t psi, Float_t tanl,
         Float_t phi0, Float_t r0, Float_t z0, Int_t sector, Float_t xl,
         Float_t yl, Float_t zl )
{
   // Add a new track to the list of tracks for this event.
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

   TClonesArray &tracks = *fTracks;
   new(tracks[fNtrack++]) Track(flag, hitid, tid, id_globtrk,
         ndedx, nfit, nrec, npos, q, Chixy, Chiyz, dedx,
         invp, curvature, psi, tanl, phi0, r0, z0, sector, xl, yl, zl);
}

//______________________________________________________________________________
void StLaserEvent::AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
                        Int_t row, Int_t track, Int_t flag)
{
   // Add a new hit to the list of hits for this event.
   // To avoid calling the very time consuming operator new for each hit,
   // the standard but not well know C++ operator "new with placement"
   // is called. If hits[i] is 0, a new Hit object will be created
   // otherwise the previous Hit[i] will be overwritten.

   TClonesArray &hits = *fHits;
   new(hits[fNhit++]) Hit(q,x,y,z,row,track,flag);
}
//______________________________________________________________________________
void StLaserEvent::AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
   Int_t row, Int_t track, Int_t flag, Int_t tksector, Float_t tkzl,
 Float_t tkpsi, Float_t dx,Float_t dz,
 Float_t alpha,Float_t lambda, Float_t prf,Float_t zrf)
{
   // Add a new hit to the list of hits for this event.
   // To avoid calling the very time consuming operator new for each hit,
   // the standard but not well know C++ operator "new with placement"
   // is called. If hits[i] is 0, a new Hit object will be created
   // otherwise the previous Hit[i] will be overwritten.

   TClonesArray &hits = *fHits;
   new(hits[fNhit++]) Hit(q,x,y,z,row,track,flag,tksector,tkzl,tkpsi,
   dx,dz,alpha,lambda,prf,zrf);
}
//______________________________________________________________________________
void StLaserEvent::AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
   Int_t row, Int_t track, Int_t flag, Int_t tksector, Float_t tkzl,
 Float_t tkpsi, Float_t tkinvp, Int_t tknfit, Float_t dx,Float_t dz,
 Float_t alpha,Float_t lambda, Float_t prf,Float_t zrf)
{
   // Add a new hit to the list of hits for this event.
   // To avoid calling the very time consuming operator new for each hit,
   // the standard but not well know C++ operator "new with placement"
   // is called. If hits[i] is 0, a new Hit object will be created
   // otherwise the previous Hit[i] will be overwritten.

   TClonesArray &hits = *fHits;
   new(hits[fNhit++]) Hit(q,x,y,z,row,track,flag,tksector,tkzl,tkpsi,
   tkinvp,tknfit,dx,dz,alpha,lambda,prf,zrf);
}

//______________________________________________________________________________
void StLaserEvent::AddPixel(Int_t row, Int_t pad,Int_t time,Int_t adc,
                    Float_t x, Float_t y, Float_t z)
{
   // Add a new Pixel to the list of Pixels for this event.
   // To avoid calling the very time consuming operator new for each Pixel,
   // the standard but not well know C++ operator "new with placement"
   // is called. If Pixels[i] is 0, a new Pixel object will be created
   // otherwise the previous Pixel[i] will be overwritten.

   TClonesArray &Pixels = *fPixels;
   new(Pixels[fNpixel++]) Pixel(row,pad,time,adc,x,y,z);
}

//______________________________________________________________________________
void StLaserEvent::Clear(Option_t *option)
{
   fTracks->Clear(option);
   fHits->Clear(option);
   fPixels->Clear(option);
}

//______________________________________________________________________________
void StLaserEvent::Reset()
{
// Static function to reset all static objects for this event
//   fgTracks->Delete(option);
   delete fgTracks; fgTracks = 0;
   delete fgHits; fgHits = 0;
   delete fgPixels; fgPixels = 0;
}

//______________________________________________________________________________
void StLaserEvent::SetHeader(Int_t i, Int_t run, Int_t date)
{
   fNtrack = 0;
   fNhit = 0;
   fNpixel = 0;
   fEvtHdr.Set(i, run, date);
}
//______________________________________________________________________________
void StLaserEvent::SetHeader(Int_t i, Int_t run, Int_t date,
     Float_t tzero, Float_t drivel, Float_t clock)
{
   fNtrack = 0;
   fNhit = 0;
   fNpixel = 0;
   fEvtHdr.Set(i, run, date);
   fEvtHdr.SetE(tzero, drivel, clock);
}

//______________________________________________________________________________
Track::Track(Int_t flag,Int_t hitid,Int_t tid,Int_t id_globtrk,
         Int_t ndedx, Int_t nfit, Int_t nrec, Int_t npos,
         Int_t q, Float_t Chixy, Float_t Chiyz, Float_t dedx,
         Float_t invp, Float_t curvature, Float_t psi, Float_t tanl,
	 Float_t phi0, Float_t r0, Float_t z0, Int_t sector,
         Float_t xl, Float_t yl,Float_t zl ) : TObject()
{
   // Create a track object.

   fStatus = flag;
   fhitid = hitid;
   ftid = tid;
   fid_globtrk = id_globtrk;
   fndedx = ndedx;
   fnfit = nfit;
   fnrec = nrec;
   fnpos = npos;
   fq = q;
   
   fChixy = Chixy;
   fChiyz = Chiyz;
   fdedx = dedx;
   finvp = invp;
   fcurvature = curvature;
   fpsi = psi;
   ftanl = tanl;
   fphi0 = phi0;
   fr0 = r0;
   fz0 = z0;
   fxl =  xl;
   fyl =  yl;
   fzl =  zl;
   fsector =  sector;
}

//______________________________________________________________________________
Hit::Hit(Float_t q,Float_t x, Float_t y, Float_t z, 
                 Int_t row, Int_t track, Int_t flag) : TObject()
{
   // Create a hit object.
   fx = x;
   fy = y;
   fz = z;
   fq = q;
   frow = row;
   ftrack = track;
   fflag = flag;
}
//______________________________________________________________________________
Hit::Hit(Float_t q,Float_t x, Float_t y, Float_t z,Int_t row, Int_t track,
  Int_t flag, Int_t tksector, Float_t tkzl, Float_t tkpsi, Float_t dx,
  Float_t dz, Float_t alpha, Float_t lambda, Float_t prf,Float_t zrf) : TObject()
{
   // Create a hit object.
   fx = x;
   fy = y;
   fz = z;
   fq = q;
   frow = row;
   ftrack = track;
   fflag = flag;
   ftksector = tksector;
   ftkpsi = tkpsi;
   ftkzl = tkzl;
   fdx = dx;
   fdz = dz;
   falpha = alpha;
   flambda = lambda;
   fprf = prf;
   fzrf = zrf;
}
//______________________________________________________________________________
Hit::Hit(Float_t q,Float_t x, Float_t y, Float_t z,Int_t row, Int_t track,
  Int_t flag, Int_t tksector, Float_t tkzl, Float_t tkpsi, Float_t tkinvp,
  Int_t tknfit, Float_t dx, Float_t dz, Float_t alpha, Float_t lambda,
  Float_t prf,Float_t zrf) : TObject()
{
   // Create a hit object.
   fx = x;
   fy = y;
   fz = z;
   fq = q;
   frow = row;
   ftrack = track;
   fflag = flag;
   ftksector = tksector;
   ftkpsi = tkpsi;
   ftkzl = tkzl;
   ftkinvp = tkinvp;
   ftknfit = tknfit;
   fdx = dx;
   fdz = dz;
   falpha = alpha;
   flambda = lambda;
   fprf = prf;
   fzrf = zrf;
}

//_____________________________________________________________________________

Pixel::Pixel(Int_t row, Int_t pad, Int_t time,Int_t adc,
           Float_t x, Float_t y, Float_t z) : TObject()
{
   // Create a pixel object.
   frow = row;
   fpad = pad;
   ftime = time;
   fadc = adc;
   fx = x;   fy = y;   fz = z;
}
//_____________________________________________________________________________
  void Track::DOCA(Float_t r0,Float_t phi0,Float_t z0, Float_t psi,
                      Float_t tanl, Float_t curvature , Int_t q,
                      Int_t *sector, Float_t *xl, Float_t *yl, Float_t *zl) {
  // calculate distance of closest approach to the 6 laser sources
  // for the track and return the sector number for the smallest.
  static const Float_t xpt[6]={-171.88,-169.36,2.505,171.88,169.36,-2.505};
  static const Float_t ypt[6]={96.33,-100.67,-197.02,-96.31,100.68,197.01};
    Float_t x, y;
  //
    Float_t ang = 0.017453292 * phi0;
    Float_t x0 = r0 * cos(ang);
    Float_t y0 = r0 * sin(ang);
    // calculate circle center position
    ang = 0.017453292 * psi;
    Float_t px = cos(ang); Float_t py = sin(ang);
    Float_t xc = x0 + q*py/curvature;
    Float_t yc = y0 - q*px/curvature;
    Float_t test = 200.0; // cutoff the source match at 10 X 10 cm
    *sector = 99;  *xl = 0.0; *yl = 0.0; *zl = 0.0;
       for (int i=0;i<6;i++){
    Float_t xp = xpt[i]; Float_t yp= ypt[i];
    Float_t d = xc - xp; Float_t a = yc - yp;
    Float_t c = d/a;  
    Float_t dy = 1./sqrt(1. + c*c)/curvature;
    Float_t dx = c*dy;
    if(a<0) { x = xc + dx;  y = yc + dy;}
    else    { x = xc - dx;  y = yc - dy;}
    Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
      if (disq<test) {test=disq; 
                    *xl=x; *yl=y;  *sector = 2*i+14;
		    Float_t sign =1.0; // account for direction to origin
         if((*xl*px+*yl*py)<0) sign=-1.0;
                  *zl = z0 + sign*tanl*sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));}
       }
  }
//______________________________________________________________________________

