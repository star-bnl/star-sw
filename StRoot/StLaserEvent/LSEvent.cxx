//* adapted to Laser straight track events 4/16/99 by Bill Love
//*CMZ :  2.21/06 17/02/99  19.02.12  by  Rene Brun
//*CMZ :  2.20/00 06/11/98  15.21.16  by  Rene Brun
//*CMZ :  2.00/13 28/10/98  12.53.06  by  Fons Rademakers
//*CMZ :  1.03/09 11/12/97  11.00.17  by  Rene Brun
//*-- Author :    Rene Brun   19/08/96

////////////////////////////////////////////////////////////////////////
//
//                       LSEvent and Track classes
//                       =======================
//
//  The LSEvent class is a simple event structure.
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
//   The LSEvent data members fTracks, fHits, fPixels are pointers to 
//   TClonesArrays, each an array of a variable number of tracks, hits,
//   pixels per event.
//   Each element of each array is an object of class Track or Hit or Pixel


#include "TRandom.h"
#include "TDirectory.h"

#include "LSEvent.h"


ClassImp(EventHeader)
ClassImp(LSEvent)
ClassImp(Track)
ClassImp(Hit)
ClassImp(Pixel)

TClonesArray *LSEvent::fgTracks = 0;
TClonesArray *LSEvent::fgHits = 0;
TClonesArray *LSEvent::fgPixels = 0;

//______________________________________________________________________________
LSEvent::LSEvent()
{
   // Create an LSEvent object.
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
LSEvent::~LSEvent()
{
   Clear();
}

//______________________________________________________________________________
void LSEvent::AddTrack(Int_t tid, Int_t nfit, Float_t ax, Float_t az,
     Float_t x0, Float_t y0, Float_t z0, Float_t Chixy, Float_t Chizy)
{
   // Add a new track to the list of tracks for this event.
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

   TClonesArray &tracks = *fTracks;
   new(tracks[fNtrack++]) Track(tid,nfit,ax,az,x0,y0,z0,Chixy,Chizy);
}

//______________________________________________________________________________
void LSEvent::AddHit(Float_t q,Float_t x,Float_t y,Float_t z, 
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
void LSEvent::AddPixel(Int_t row, Int_t pad,Int_t time,Int_t adc,
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
void LSEvent::Clear(Option_t *option)
{
   fTracks->Clear(option);
   fHits->Clear(option);
   fPixels->Clear(option);
}

//______________________________________________________________________________
void LSEvent::Reset()
{
// Static function to reset all static objects for this event
//   fgTracks->Delete(option);
   delete fgTracks; fgTracks = 0;
   delete fgHits; fgHits = 0;
   delete fgPixels; fgPixels = 0;
}

//______________________________________________________________________________
void LSEvent::SetHeader(Int_t i, Int_t run, Int_t date)
{
   fNtrack = 0;
   fNhit = 0;
   fNpixel = 0;
   fEvtHdr.Set(i, run, date);
}
//______________________________________________________________________________
void LSEvent::SetLastTrack(Int_t nlast)
{
   fTracks->SetLast(nlast);
}
//______________________________________________________________________________
void LSEvent::SetLastHit(Int_t nlast)
{
   fHits->SetLast(nlast);
}
//______________________________________________________________________________
void LSEvent::SetLastPixel(Int_t nlast)
{
   fPixels->SetLast(nlast);
}

//______________________________________________________________________________
Track::Track(Int_t tid, Int_t nfit, Float_t ax, Float_t az,
     Float_t x0, Float_t y0, Float_t z0,
     Float_t Chixy, Float_t Chizy) : TObject()
{
   // Create a track object.

   fPy = TMath::Sqrt(1.0/(1.0+ax*ax+az*az));
   fPx = ax*fPy;
   fPz = az*fPy;

   fBx = x0;
   fBy = y0;
   fBz = z0;
   fDedx = 0.0;
   fChisqxy = Chixy;
   fChisqzy = Chizy;

   fTrackId = tid;
   fNpoint = nfit;
   fValid  = 1;
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

//______________________________________________________________________________

