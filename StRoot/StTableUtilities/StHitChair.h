#ifndef STAR_StHitChair
#define STAR_StHitChair
#include <assert.h>
#include "TChair.h"
///////////////////////////////////////////////////////////////////////////////////
// This basic chair provides the custom methods for all hits
///////////////////////////////////////////////////////////////////////////////////

class St_g2t_tpc_hit;
class g2t_tpc_hit_st;

class St_tcl_tphit;
class tcl_tphit_st;

class St_dst_point;
class dst_point_st;

class StHitChair : public TChair {
  protected:
    StHitChair() {;}
  public:
    StHitChair(TTable *hits): TChair(hits){fTab = GetArray();}
    virtual ~StHitChair(){;}


    virtual Int_t   DetectorId (Int_t i)        const  {return -1;};
    virtual Int_t   Sector     (Int_t i)	const  {return -1;};
    virtual Int_t   PadRow     (Int_t i)        const  {return -1;};
    virtual Int_t   TrackId    (Int_t i)        const  {return -1;};
    virtual Int_t   HitPosition(Int_t i)     	const  {return -1;};
    virtual Float_t GetX       (Int_t i)        const  {return -999999;};
    virtual Float_t GetY       (Int_t i)        const  {return -999999;};
    virtual Float_t GetZ       (Int_t i)        const  {return -999999;};
    virtual void    SetX       (Float_t x,Int_t i)     {assert(0);};
    virtual void    SetY       (Float_t y,Int_t i)     {assert(0);};
    virtual void    SetZ       (Float_t z,Int_t i)     {assert(0);};
    virtual Float_t GetXError  (Int_t i)       	const  {return -999999;};
    virtual Float_t GetYError  (Int_t i)       	const  {return -999999;};
    virtual Float_t GetZError  (Int_t i)       	const  {return -999999;};

  protected:

union 
{
  char           *fTab;
  g2t_tpc_hit_st *fTab_g2t;
  tcl_tphit_st   *fTab_tcl;
  dst_point_st   *fTab_dst;
};
//    ClassDef(StHitChair,0)
};


#endif

