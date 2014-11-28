#include "dEdxTrackY2.h"

ClassImp(dEdxTrackY2);

TClonesArray *dEdxTrackY2::fgPoints = 0;


dEdxTrackY2::dEdxTrackY2() {
   if (!fgPoints) fgPoints = new TClonesArray("dEdxY2_t", 100);
   fPoints = fgPoints;
   fNPoint = 0;
}

//______________________________________________________________________________
dEdxTrackY2::~dEdxTrackY2()
{
   Clear();
}

//______________________________________________________________________________
void dEdxTrackY2::AddPoint(dEdxY2_t &point)
{
   TClonesArray &points = *fPoints;
   new(points[fNPoint++]) dEdxY2_t(point);
}

//______________________________________________________________________________
void dEdxTrackY2::Clear(Option_t *option)
{
   fNPoint = 0;
   fPoints->Clear(option);
}

//______________________________________________________________________________
void dEdxTrackY2::Reset(Option_t *option)
{
   delete fgPoints; fgPoints = 0;
}
//______________________________________________________________________________
  
