#include "dEdxTrack.h"

ClassImp(dEdxTrack)
ClassImp(Point)

TClonesArray *dEdxTrack::fgPoints = 0;

Point::Point(dEdx_t point) {
 sector = point.sector;
 row = point.row;
 pad = point.pad;
 Fee = point.Fee;
 dE = point.dE;
 dEU = point.dEU; // before correction
 dx = point.dx;
 dEdx  = point.dEdx; 
 dEdxL = point.dEdxL; // log of dEdx
 dEdxN = point.dEdxN; // normolized to BB
 dEUdx = point.dEUdx; 
 dEUdxL = point.dEUdxL; // log of dEdx
 dEUdxN = point.dEUdxN; // log of dEdx
 dETot = point.dETot; 
 xyz[0] = point.xyz[0];
 xyz[1] = point.xyz[1];
 xyz[2] = point.xyz[2];
 Prob = point.Prob; 
 SigmaFee = point.SigmaFee;
 xscale = point.xscale;
 dEIpad = point.dEIpad;  // total charge integrated so far in the pad
 dEI3pad = point.dEI3pad; // total charge integrated so far in the pad +/-
 dEIrow = point.dEIrow;  // total charge integrated so far in the row
 dETrow = point.dETrow;  // total charge not integrated (time bucket only) in the row
 dET3row = point.dET3row; // total charge not integrated (+0 + 2 time buckets only) in the row
 dET5row = point.dET5row; // total charge not integrated (+0 + 4 time buckets only) in the row
 zdev = point.zdev; 
 dY = point.dY;      // Projection on the wire
 RMS = point.RMS;     // rms from volume charge
}

dEdxTrack::dEdxTrack() {
   if (!fgPoints) fgPoints = new TClonesArray("Point", 100);
   fPoints = fgPoints;
   fNPoint = 0;
}

//______________________________________________________________________________
dEdxTrack::~dEdxTrack()
{
   Clear();
}

//______________________________________________________________________________
void dEdxTrack::AddPoint(dEdx_t &point)
{
   TClonesArray &points = *fPoints;
   new(points[fNPoint++]) Point(point);
}

//______________________________________________________________________________
void dEdxTrack::Clear(Option_t *option)
{
   fNPoint = 0;
   fPoints->Clear(option);
}

//______________________________________________________________________________
void dEdxTrack::Reset(Option_t *option)
{
   delete fgPoints; fgPoints = 0;
}
//______________________________________________________________________________
  
