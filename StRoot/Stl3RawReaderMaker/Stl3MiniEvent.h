#ifndef Stl3MiniEvent_hh
#define Stl3MiniEvent_hh

#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"

class Stl3Track ;


class Stl3MiniEvent : public TObject
{
  
  public :
    Stl3MiniEvent() ;
  virtual ~Stl3MiniEvent() ;  
  //vertex
  virtual void SetVertex(Float_t vx, Float_t vy, Float_t vz) ;
  // tracks
  virtual void SetNTracks(Int_t nTracks) ;
  virtual void SetTrackArray(TClonesArray* tracks ) { mTracks = tracks; };
  TClonesArray* GetTrackArray() { return mTracks;} ;
  // hits
  virtual void SetNHits(Int_t nHits) ;
  virtual void SetHitArray(TClonesArray* hits ) { mHits = hits; } ;
  TClonesArray* GetHitArray() { return mHits;}  ;

  
  private :
    Int_t       mNHits ;
  Int_t           mNTracks ;
  Float_t         mVertexX ;
  Float_t         mVertexY ;
  Float_t         mVertexZ ;
  TClonesArray*   mTracks ;
  TClonesArray*   mHits ;
  
  //ROOT
  ClassDef(Stl3MiniEvent, 1)
        
} ;

class Stl3Track : public TObject
{
  public :
    Stl3Track() ;
  Stl3Track(Int_t NHits, Int_t Q, Int_t Flag, Int_t InnerMostRow, Int_t OuterMostRow, Float_t Pt, Float_t Psi, Float_t Tanl, Float_t Z0, Float_t Phi0, Float_t R0 , Float_t Length);
  virtual ~Stl3Track() ;
  virtual void SetNHits(Int_t NHits) { mNHits = NHits; } ;
  virtual void SetPt(Float_t Pt) { mPt = Pt; } ;
  
  private :
    Int_t mNHits ;
  Int_t mFlag ;
  Char_t mQ ;
  Int_t mInnerMostRow ;
  Int_t mOuterMostRow ;
  Float_t mPt ; 
  Float_t mPsi ;
  Float_t mTanl ;
  Float_t mZ0 ;
  Float_t mPhi0 ;
  Float_t mR0 ;
  Float_t mLength ;
    
  //ROOT
  ClassDef(Stl3Track, 1)
} ;

class Stl3Hit : public TObject
{
  public :
    Stl3Hit() ;
  Stl3Hit(Float_t x, Float_t y, Float_t z, Float_t charge);
  ~Stl3Hit() ;
  
  private :
    Float_t mX ;
  Float_t mY ;
  Float_t mZ ;
  Float_t mCharge ;

  //ROOT
  ClassDef(Stl3Hit, 1)
} ;

#endif // Stl3MiniEvent
