#ifndef STAR_StTrackChair
#define STAR_StTrackChair

#include "TChair.h"

class StHelixD;

class StTrackChair : public TChair {
 private: 
    ULong_t mR0;       //        radius at start (cm)                   
    ULong_t mPhi0;     //        azimuthal angle at start (deg)         
    ULong_t mZ0;       //        z-coord. at start (cm)                 
    ULong_t mPsi;      //        azimuthal angle of pT vector (deg)     
    ULong_t mTanl;     //        tan(dip) =pz/pt at start               
    ULong_t mInvpt;    //        1/pt at start (GeV/c)^(-1)             
    ULong_t mCurvature;//        Track curvature (1/cm)                 
    ULong_t mLength;   //        from first to last point (cm)          
    ULong_t mCharge;   //        Particle charge in units of |e| 
 protected:
    StTrackChair() {;}
 public:
  static const Char_t *trackTableList[];
  StTrackChair(St_Table *track);
 ~StTrackChair(){;}
  StHelixD *MakeHelix(Int_t i) const ;
  static StTrackChair *Instance(St_Table *table);
  static Int_t IsTrack(St_Table *table);
  Float_t R0(Int_t i)        const;
  Float_t Z0(Int_t i)        const;
  Float_t Phi0(Int_t i)      const;
  Float_t Invpt(Int_t i)     const;
  Float_t Curvature(Int_t i) const;
  Long_t  Charge(Int_t i)    const;         
  Float_t Length(Int_t i)    const;
  ClassDef(StTrackChair,0)
};

inline  Long_t  StTrackChair::Charge(Int_t i) const{return *(Long_t *)GetOffset(At(i),mCharge); }
inline  Float_t StTrackChair::R0(Int_t i)   const {return *(Float_t *)GetOffset(At(i),mR0); }
inline  Float_t StTrackChair::Z0(Int_t i)   const {return *(Float_t *)GetOffset(At(i),mZ0); }
inline  Float_t StTrackChair::Phi0(Int_t i) const {return *(Float_t *)GetOffset(At(i),mPhi0); }
inline  Float_t StTrackChair::Invpt(Int_t i)const {return *(Float_t *)GetOffset(At(i),mInvpt); }
inline  Float_t StTrackChair::Curvature(Int_t i) const {return *(Float_t *)GetOffset(At(i),mCurvature); }
inline  Float_t StTrackChair::Length(Int_t i) const {return *(Float_t *)GetOffset(At(i),mLength); }

#endif

