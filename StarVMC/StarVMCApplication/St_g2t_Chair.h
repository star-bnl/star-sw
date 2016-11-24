#ifndef STAR_St_g2t_Chair
#define STAR_St_g2t_Chair
#include <assert.h>
#include "TChair.h"
///////////////////////////////////////////////////////////////////////////////////
// This basic chair provides the custom methods for all hits
///////////////////////////////////////////////////////////////////////////////////

#include "tables/St_g2t_hits_Table.h"
#include "tables/St_g2t_ctf_hit_Table.h"  /* Done */
#include "tables/St_g2t_emc_hit_Table.h"  /* Done */
#include "tables/St_g2t_epd_hit_Table.h"
#include "tables/St_g2t_etr_hit_Table.h"
#include "tables/St_g2t_fgt_hit_Table.h"  /* Done */
#include "tables/St_g2t_fst_hit_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"  /* Done */
#include "tables/St_g2t_fts_hit_Table.h"
#include "tables/St_g2t_gem_hit_Table.h"  /* Done */
#include "tables/St_g2t_hpd_hit_Table.h"
#include "tables/St_g2t_igt_hit_Table.h"
#include "tables/St_g2t_ist_hit_Table.h"  /* Done */
#include "tables/St_g2t_mtd_hit_Table.h"
#include "tables/St_g2t_mwc_hit_Table.h"  /* Done */
#include "tables/St_g2t_pix_hit_Table.h"  /* Done */
#include "tables/St_g2t_pmd_hit_Table.h"  /* Done */
#include "tables/St_g2t_rch_hit_Table.h"  /* Done */
#include "tables/St_g2t_ssd_hit_Table.h"  /* Done */
#include "tables/St_g2t_svt_hit_Table.h"  /* Done */
#include "tables/St_g2t_tpc_hit_Table.h"  /* Done */
#include "tables/St_g2t_vpd_hit_Table.h"  /* Done */

#include "StarHitVector.h"
class St_g2t_Chair : public TChair {
 public:
  St_g2t_Chair(TTable *table): TChair(table) {}
  virtual ~St_g2t_Chair(){}
  virtual void    SetTable   (TTable *table) {fTable = table;}
  virtual Int_t   DetectorId (Int_t /* i */)                const  {return -1;};
  virtual Int_t   Sector     (Int_t /* i */)		      const  {return -1;};
  virtual Int_t   PadRow     (Int_t /* i */)                const  {return -1;};
  virtual Int_t   TrackId    (Int_t /* i */)                const  {return -1;};
  virtual Int_t   HitPosition(Int_t /* i */)     	      const  {return -1;};
  virtual Float_t GetX       (Int_t /* i */)                const  {return -999999;};
  virtual Float_t GetY       (Int_t /* i */)                const  {return -999999;};
  virtual Float_t GetZ       (Int_t /* i */)                const  {return -999999;};
  virtual void    SetX       (Float_t /* x */,Int_t /* i */)     {assert(0);};
  virtual void    SetY       (Float_t /* y */,Int_t /* i */)     {assert(0);};
  virtual void    SetZ       (Float_t /* z */,Int_t /* i */)     {assert(0);};
  virtual Float_t GetXError  (Int_t /* i */)                const  {return -999999;};
  virtual Float_t GetYError  (Int_t /* i */)       	      const  {return -999999;};
  virtual Float_t GetZError  (Int_t /* i */)       	      const  {return -999999;};
  virtual void    Fill(GHit_t &/* vect */) {}
  static  void    SetDebug(Int_t m=0) {fDebug = m;}
  static  Int_t   Debug() {return fDebug;}
 protected:
  static  Int_t           fDebug;
  ClassDef(St_g2t_Chair,1)
};
class St_g2t_hitsC : public St_g2t_Chair	{    
 public: 
  St_g2t_hitsC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_hitsC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_hits,g2t_hits_st)
  ClassDef(St_g2t_hitsC,1)
};
class St_g2t_ctf_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_ctf_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_ctf_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_ctf_hit,g2t_ctf_hit_st)
  ClassDef(St_g2t_ctf_hitC,1)
};
class St_g2t_emc_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_emc_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_emc_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_emc_hit,g2t_emc_hit_st)
  ClassDef(St_g2t_emc_hitC,1)
};
class St_g2t_fgt_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_fgt_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_fgt_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_fgt_hit,g2t_fgt_hit_st)
  ClassDef(St_g2t_fgt_hitC,1)
};
class St_g2t_ftp_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_ftp_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_ftp_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_ftp_hit,g2t_ftp_hit_st)
  ClassDef(St_g2t_ftp_hitC,1)
};
class St_g2t_gem_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_gem_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_gem_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_gem_hit,g2t_gem_hit_st)
  ClassDef(St_g2t_gem_hitC,1)
};
class St_g2t_ist_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_ist_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_ist_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_ist_hit,g2t_ist_hit_st)
  ClassDef(St_g2t_ist_hitC,1)
};
class St_g2t_mwc_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_mwc_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_mwc_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_mwc_hit,g2t_mwc_hit_st)
  ClassDef(St_g2t_mwc_hitC,1)
};
class St_g2t_pix_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_pix_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_pix_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_pix_hit,g2t_pix_hit_st)
  ClassDef(St_g2t_pix_hitC,1)
};
class St_g2t_pmd_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_pmd_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_pmd_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_pmd_hit,g2t_pmd_hit_st)
  ClassDef(St_g2t_pmd_hitC,1)
};
class St_g2t_rch_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_rch_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_rch_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_rch_hit,g2t_rch_hit_st)
  ClassDef(St_g2t_rch_hitC,1)
};
class St_g2t_ssd_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_ssd_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_ssd_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_ssd_hit,g2t_ssd_hit_st)
  ClassDef(St_g2t_ssd_hitC,1)
};
class St_g2t_svt_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_svt_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_svt_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_svt_hit,g2t_svt_hit_st)
  ClassDef(St_g2t_svt_hitC,1)
};
class St_g2t_tpc_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_tpc_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_tpc_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_tpc_hit,g2t_tpc_hit_st)
  ClassDef(St_g2t_tpc_hitC,1)
};
class St_g2t_vpd_hitC : public St_g2t_Chair	{    
 public: 
  St_g2t_vpd_hitC(TTable *table=0): St_g2t_Chair(table) {}
  virtual ~St_g2t_vpd_hitC() {}
  virtual void    Fill(GHit_t &vect); 
  ClassDefChair(St_g2t_vpd_hit,g2t_vpd_hit_st)
  ClassDef(St_g2t_vpd_hitC,1)
};
#endif

