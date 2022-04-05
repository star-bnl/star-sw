#ifndef STAR_StTrackChair
#define STAR_StTrackChair
#include "TChair.h"
#include "StHelixD.hh"

class StTrackChair : public TChair {
 private: 
    UInt_t  mR0;       //        radius at start (cm)                   
    UInt_t  mPhi0;     //        azimuthal angle at start (deg)         
    UInt_t  mZ0;       //        z-coord. at start (cm)                 
    UInt_t  mPsi;      //        azimuthal angle of pT vector (deg)     
    UInt_t  mTanl;     //        tan(dip) =pz/pt at start               
    UInt_t  mInvpt;    //        1/pt at start (GeV/c)^(-1)             
    UInt_t  mCurvature;//        Track curvature (1/cm)                 
    UInt_t  mLength;   //        from first to last point (cm)          
    UInt_t  mCharge;   //        Particle charge in units of |e| 
 protected:
    StTrackChair() {;}
 public:
  static const Char_t *trackTableList[];
  StTrackChair(TTable *track);
 ~StTrackChair(){;}
  StHelixD *MakeHelix(Int_t i, float bField) const ;
  TDataSet* Instance() const {return TDataSet::Instance();}//WarnOff
  static StTrackChair *Instance(TTable *table);
  static Int_t IsTrack(TTable *table);
  Float_t R0(Int_t i)        const;
  Float_t Z0(Int_t i)        const;
  Float_t Phi0(Int_t i)      const;
  Float_t Invpt(Int_t i)     const;
  Float_t Curvature(Int_t i) const;
  Int_t   Charge(Int_t i)    const;         
  Float_t Length(Int_t i)    const;
  ClassDef(StTrackChair,0)
};

inline  Int_t   StTrackChair::Charge(Int_t i) const{return *(Int_t  *)GetOffset(At(i),mCharge); }
inline  Float_t StTrackChair::R0(Int_t i)   const {return *(Float_t *)GetOffset(At(i),mR0); }
inline  Float_t StTrackChair::Z0(Int_t i)   const {return *(Float_t *)GetOffset(At(i),mZ0); }
inline  Float_t StTrackChair::Phi0(Int_t i) const {return *(Float_t *)GetOffset(At(i),mPhi0); }
inline  Float_t StTrackChair::Invpt(Int_t i)const {return *(Float_t *)GetOffset(At(i),mInvpt); }
inline  Float_t StTrackChair::Curvature(Int_t i) const {return *(Float_t *)GetOffset(At(i),mCurvature); }
inline  Float_t StTrackChair::Length(Int_t i) const {return *(Float_t *)GetOffset(At(i),mLength); }

#endif

