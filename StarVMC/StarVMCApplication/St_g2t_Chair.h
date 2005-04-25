#ifndef STAR_St_g2t_Chair
#define STAR_St_g2t_Chair
#include <assert.h>
#include "TChair.h"
///////////////////////////////////////////////////////////////////////////////////
// This basic chair provides the custom methods for all hits
///////////////////////////////////////////////////////////////////////////////////
//#include "tables/St_g2t_hits_Table.h"
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_g2t_fst_hit_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_g2t_mwc_hit_Table.h"
#include "tables/St_g2t_pix_hit_Table.h"
#include "tables/St_g2t_pmd_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_ssd_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_vpd_hit_Table.h"
#if 0
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#endif
#include "StarHitVector.h"
class St_g2t_Chair : public TChair {
 public:
  St_g2t_Chair() {}
  St_g2t_Chair(TTable *table): TChair(table){fTab = GetArray();}
  virtual ~St_g2t_Chair(){;}
  
  virtual void    SetTable   (TTable *table) {fTable = table; fTab = GetArray();}
  virtual Int_t   DetectorId (Int_t i)                const  {return -1;};
  virtual Int_t   Sector     (Int_t i)		      const  {return -1;};
  virtual Int_t   PadRow     (Int_t i)                const  {return -1;};
  virtual Int_t   TrackId    (Int_t i)                const  {return -1;};
  virtual Int_t   HitPosition(Int_t i)     	      const  {return -1;};
  virtual Float_t GetX       (Int_t i)                const  {return -999999;};
  virtual Float_t GetY       (Int_t i)                const  {return -999999;};
  virtual Float_t GetZ       (Int_t i)                const  {return -999999;};
  virtual void    SetX       (Float_t x,Int_t i)     {assert(0);};
  virtual void    SetY       (Float_t y,Int_t i)     {assert(0);};
  virtual void    SetZ       (Float_t z,Int_t i)     {assert(0);};
  virtual Float_t GetXError  (Int_t i)                const  {return -999999;};
  virtual Float_t GetYError  (Int_t i)       	      const  {return -999999;};
  virtual Float_t GetZError  (Int_t i)       	      const  {return -999999;};
#ifndef __CINT__
  virtual void    Fill(GHit_t &vect) {}
#else
  virtual void    Fill(GHit_t *vect) {}
#endif
 protected:
#ifndef __CINT__
  union 
  {
    char           *fTab;
    //    g2t_hits_st    *fTab_hits;
    g2t_ctf_hit_st *fTab_ctf; 
    g2t_emc_hit_st *fTab_emc; 
    g2t_fst_hit_st *fTab_fst; 
    g2t_ftp_hit_st *fTab_ftp; 
    g2t_ist_hit_st *fTab_ist; 
    g2t_mwc_hit_st *fTab_mwc; 
    g2t_pix_hit_st *fTab_pix; 
    g2t_pmd_hit_st *fTab_pmd; 
    g2t_rch_hit_st *fTab_rch; 
    g2t_ssd_hit_st *fTab_ssd; 
    g2t_svt_hit_st *fTab_svt; 
    g2t_tpc_hit_st *fTab_tpc; 
    g2t_vpd_hit_st *fTab_vpd; 
#if 0
    particle_st    *fTab_particle;
    g2t_pythia_st  *fTab_pythia;  
    g2t_event_st   *fTab_event;   
    g2t_track_st   *fTab_track;   
    g2t_vertex_st  *fTab_vertex;  
#endif
  };
#else
  char           *fTab;
#endif 
  //    ClassDef(St_g2t_Chair,0)
};
#if 0
#define G2Table(QWERTY) \
  class St_ ## QWERTY ## C : public St_g2t_Chair	{\
    virtual void    Fill(GHit_t &vect);\
  };
//G2Table(g2t_hits);
G2Table(g2t_ctf_hit);
G2Table(g2t_emc_hit);
G2Table(g2t_fst_hit);
G2Table(g2t_ftp_hit);
G2Table(g2t_ist_hit);
G2Table(g2t_mwc_hit);
G2Table(g2t_pix_hit);
G2Table(g2t_pmd_hit);
G2Table(g2t_rch_hit);
G2Table(g2t_ssd_hit);
G2Table(g2t_svt_hit);
G2Table(g2t_tpc_hit);
G2Table(g2t_vpd_hit);
#endif
class St_g2t_ctf_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_ctf_hitC,0)
			    };
class St_g2t_emc_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_emc_hitC,0)
			    };
class St_g2t_fst_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_fst_hitC,0)
			    };
class St_g2t_ftp_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_ftp_hitC,0)
			    };
class St_g2t_ist_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_ist_hitC,0)
			    };
class St_g2t_mwc_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_mwc_hitC,0)
			    };
class St_g2t_pix_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_pix_hitC,0)
			    };
class St_g2t_pmd_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_pmd_hitC,0)
			    };
class St_g2t_rch_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_rch_hitC,0)
			    };
class St_g2t_ssd_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_ssd_hitC,0)
			    };
class St_g2t_svt_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_svt_hitC,0)
			    };
class St_g2t_tpc_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_tpc_hitC,0)
			    };
class St_g2t_vpd_hitC : public St_g2t_Chair	{    virtual void    Fill(GHit_t &vect); 
			  ClassDef(St_g2t_vpd_hitC,0)
			    };
#endif

