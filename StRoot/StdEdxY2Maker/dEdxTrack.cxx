#include "dEdxTrack.h"

ClassImp(dEdxTrack)
  //ClassImp(dEdx_t)

TClonesArray *dEdxTrack::fgPoints = 0;


dEdxTrack::dEdxTrack() {
   if (!fgPoints) fgPoints = new TClonesArray("dEdx_t", 100);
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
   new(points[fNPoint++]) dEdx_t(point);
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
  
