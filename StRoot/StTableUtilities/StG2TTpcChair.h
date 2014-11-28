#ifndef STAR_StG2TTpcChair
#define STAR_StG2TTpcChair
///////////////////////////////////////////////////////////////////////////////////
// This chair privides the custom methods to the packed
// tcl_hit columns:
// 	long track; /* track_id*1000+position on on a track (starts from 1) */
//	short row;  /* 100*isector TPC row number */
// and provide extra row check:
///////////////////////////////////////////////////////////////////////////////////

#include "StHitChair.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StDetectorDefinitions.h"

class StG2TTpcChair : public StHitChair {
  protected:
    StG2TTpcChair() {;}
  public:
  StG2TTpcChair(St_g2t_tpc_hit *hits): StHitChair(hits){;}
  virtual  ~StG2TTpcChair(){;}

    virtual Int_t   Sector(Int_t i)          const {return (fTab_g2t[i].volume_id/100)%100;}
    virtual Int_t   PadRow(Int_t i)          const {return  fTab_g2t[i].volume_id%100;}
    virtual Int_t   TrackId(Int_t i)         const {return  fTab_g2t[i].track_p;}
//  virtual Int_t   HitPosition(Int_t i)     const;
    virtual Int_t   DetectorId(Int_t i)      const{return kTpcIdentifier;} 
    virtual Float_t GetX(Int_t i)            const{return fTab_g2t[i].x[0];}
    virtual Float_t GetY(Int_t i)            const{return fTab_g2t[i].x[1];}
    virtual Float_t GetZ(Int_t i)            const{return fTab_g2t[i].x[2];}
    virtual Float_t GetXError(Int_t i)       const{return 0.;}
    virtual Float_t GetYError(Int_t i)       const{return 0.;}
    virtual Float_t GetZError(Int_t i)       const{return 0.;}
//===============================================

 ClassDef(StG2TTpcChair,0)
};


#endif

