#include "Stl3MiniEvent.h"
#include "Rtypes.h"

ClassImp(Stl3MiniEvent)
ClassImp(Stl3Track)
ClassImp(Stl3Hit)

//______________________
Stl3MiniEvent::Stl3MiniEvent()
{
  // these dummys must be created to fill Stl3MiniEvent into a Tree
  mTracks = new TClonesArray("Stl3Track",10000);
  mHits   = new TClonesArray("Stl3Hit",100000);
};
//______________________
Stl3MiniEvent::~Stl3MiniEvent()
 {
   // clean up
   delete mTracks ;
   delete mHits ;
};
//______________________
void Stl3MiniEvent::SetVertex(Float_t vx, Float_t vy, Float_t vz)
{
    mVertexX = vx ;
    mVertexY = vy ;
    mVertexZ = vz ;
};
//______________________
void Stl3MiniEvent::SetNHits(Int_t nHits)
{
    mNHits = nHits ;
};
//______________________
void Stl3MiniEvent::SetNTracks(Int_t nTracks)
{
    mNTracks = nTracks ;
};

//______________________
Stl3Track::Stl3Track()
{
  ;
}
//______________________
Stl3Track::Stl3Track(Int_t NHits, Int_t Q, Int_t Flag, Int_t InnerMostRow, Int_t  OuterMostRow, Float_t Pt, Float_t Psi, Float_t Tanl, Float_t Z0, Float_t Phi0, Float_t R0 , Float_t Length)
{
  mNHits        = NHits ;
  mQ            = Q ;
  mFlag         = Flag ;
  mInnerMostRow = InnerMostRow ;
  mOuterMostRow = OuterMostRow ; 
  mPt           = Pt ;
  mPsi          = Psi ; 
  mTanl         = Tanl ; 
  mZ0           = Z0 ;
  mPhi0         = Phi0 ;
  mR0           = R0 ;
  mLength       = Length ;  
}
//______________________
Stl3Track::~Stl3Track()
{
}
//______________________
Stl3Hit::Stl3Hit()
{
}
//______________________
Stl3Hit::Stl3Hit(Float_t x, Float_t y, Float_t z, Float_t charge)
{
  mX = x;
  mY = y;
  mZ = z;
  mCharge = charge;
}
//______________________
Stl3Hit::~Stl3Hit()
{
}
