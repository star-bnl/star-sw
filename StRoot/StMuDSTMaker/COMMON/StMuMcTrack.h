// $Id: StMuMcTrack.h,v 1.4 2014/08/06 19:19:07 perev Exp $
#ifndef __StMuMcTrack_h__
#define __StMuMcTrack_h__
#include "tables/St_g2t_track_Table.h" 
#include "StThreeVectorF.hh"
#ifdef __TFG__VERSION__
#include "KFParticlePerformance/KFMCTrack.h"
#endif /* __TFG__VERSION__ */
class StMuMcTrack : public TObject {
 public:
  enum EHIT {ktpc, ksvt, kssd,
	     kctb, keem, kemc, kesm, kftp, kgem, khpd, kist, kigt, kfst, 
#ifndef __TFG__VERSION__
	     kfgt, kfpd, kmwc, kpgc, kpmd, ksmd, kpix, ktof, kvpd, ktot};
#else /* __TFG__VERSION__ */
	     kfgt, kfpd, kmwc, kpgc, kpmd, ksmd, kpix, ktof, kvpd, ketr, khca, kfts, keto, kepd, kstg, kwca, ktpcR, ktot};
#endif /* __TFG__VERSION__ */
  StMuMcTrack(const g2t_track_st &t);
#ifdef __TFG__VERSION__
  StMuMcTrack();
#else /* ! __TFG__VERSION__ */
  StMuMcTrack() {}
#endif /* __TFG__VERSION__ */
  virtual ~StMuMcTrack() {}
  Int_t                 GePid()        const {return mGePid;} /* GEANT particle id */	        
#ifdef __TFG__VERSION__
  Int_t                 Pdg()          const {return mPDG;}
#endif /* __TFG__VERSION__ */
  Int_t           	Id()           const {return mId;}    /* primary key */		       
  Bool_t          	IsShower()     const {return mIsShower;} /* 1 if shower track, 0 if not */
#ifndef __TFG__VERSION__
  Int_t                 NoHits()       const {Int_t n = 0; for (Int_t i = ktpc; i < ktot; i++) n+= NoHits(i); return n;}
  UChar_t               NoHits(Int_t k)const {return mHits[k];}					   
  UChar_t     		No_ctb_hit()   const {return NoHits(kctb);}   /* Nhits in ctb */			   
  UChar_t     		No_eem_hit()   const {return NoHits(keem);}   /* Nhits in eem (endcap em cal) */	   
  UChar_t     		No_emc_hit()   const {return NoHits(kemc);}   /* Nhits in emc */			   
  UChar_t     		No_esm_hit()   const {return NoHits(kesm);}   /* Nhits in esm (endcap shower max) */ 
  UChar_t     		No_ftp_hit()   const {return NoHits(kftp);}   /* Nhits in forward tpc */		   
  UChar_t     		No_gem_hit()   const {return NoHits(kgem);}   /* Nhits in gem barrel */		   
  UChar_t     		No_hpd_hit()   const {return NoHits(khpd);}   /* Nhits in hpd */			   
  UChar_t     		No_ist_hit()   const {return NoHits(kist);}   /* Nhits in ist */			   
  UChar_t     		No_igt_hit()   const {return NoHits(kigt);}   /* Nhits in igt */			   
  UChar_t     		No_fst_hit()   const {return NoHits(kfst);}   /* Nhits in fst */			   
  UChar_t     		No_fgt_hit()   const {return NoHits(kfgt);}   /* Nhits in fgt */			   
  UChar_t     		No_fpd_hit()   const {return NoHits(kfpd);}   /* Nhits in fpd */			   
  UChar_t     		No_mwc_hit()   const {return NoHits(kmwc);}   /* Nhits in mwc */			   
  UChar_t     		No_pgc_hit()   const {return NoHits(kpgc);}   /* Nhits in pgc  ???  */		   
  UChar_t     		No_pmd_hit()   const {return NoHits(kpmd);}   /* Nhits in pmd (PMD) */		   
  UChar_t     		No_smd_hit()   const {return NoHits(ksmd);}   /* number of hits in shower max */	   
  UChar_t     		No_ssd_hit()   const {return NoHits(kssd);}   /* Nhits in ssd */			   
  UChar_t     		No_svt_hit()   const {return NoHits(ksvt);}   /* Nhits in svt */			   
  UChar_t     		No_pix_hit()   const {return NoHits(kpix);}   /* Nhits in pix */			   
  UChar_t     		No_tof_hit()   const {return NoHits(ktof);}   /* Nhits in tof */			   
  UChar_t     		No_tpc_hit()   const {return NoHits(ktpc);}   /* Nhits in tpc */			   
  UChar_t     		No_vpd_hit()   const {return NoHits(kvpd);}   /* Nhits in vpd */                     
#else /* __TFG__VERSION__ */
  Int_t                 NoHits()       const {Int_t n = No_tpc_hit(); for (Int_t i = ktpc + 1; i < ktpcR; i++) n+= NoHits(i); return n;}
  Int_t                 NoHits(Int_t k)const {return mHits[k];}					   
  Int_t       		No_ctb_hit()   const {return NoHits(kctb);}   /* Nhits in ctb */			   
  Int_t       		No_eem_hit()   const {return NoHits(keem);}   /* Nhits in eem (endcap em cal) */	   
  Int_t       		No_emc_hit()   const {return NoHits(kemc);}   /* Nhits in emc */			   
  Int_t       		No_esm_hit()   const {return NoHits(kesm);}   /* Nhits in esm (endcap shower max) */ 
  Int_t       		No_ftp_hit()   const {return NoHits(kftp);}   /* Nhits in forward tpc */		   
  Int_t       		No_gem_hit()   const {return NoHits(kgem);}   /* Nhits in gem barrel */		   
  Int_t       		No_hpd_hit()   const {return NoHits(khpd);}   /* Nhits in hpd */			   
  Int_t       		No_ist_hit()   const {return NoHits(kist);}   /* Nhits in ist */			   
  Int_t       		No_igt_hit()   const {return NoHits(kigt);}   /* Nhits in igt */			   
  Int_t       		No_fst_hit()   const {return NoHits(kfst);}   /* Nhits in fst */			   
  Int_t       		No_fgt_hit()   const {return NoHits(kfgt);}   /* Nhits in fgt */			   
  Int_t       		No_fpd_hit()   const {return NoHits(kfpd);}   /* Nhits in fpd */			   
  Int_t       		No_mwc_hit()   const {return NoHits(kmwc);}   /* Nhits in mwc */			   
  Int_t       		No_pgc_hit()   const {return NoHits(kpgc);}   /* Nhits in pgc  ???  */		   
  Int_t       		No_pmd_hit()   const {return NoHits(kpmd);}   /* Nhits in pmd (PMD) */		   
  Int_t       		No_smd_hit()   const {return NoHits(ksmd);}   /* number of hits in shower max */	   
  Int_t       		No_ssd_hit()   const {return NoHits(kssd);}   /* Nhits in ssd */			   
  Int_t       		No_svt_hit()   const {return NoHits(ksvt);}   /* Nhits in svt */			   
  Int_t       		No_pix_hit()   const {return NoHits(kpix);}   /* Nhits in pix */			   
  Int_t       		No_tof_hit()   const {return NoHits(ktof);}   /* Nhits in tof */			   
  Int_t       		No_tpc_hitA()  const {return NoHits(ktpc);}   /* Nhits in tpc */			   
  Int_t       		No_tpc_hit()   const {return NoHits(ktpcR);}  /* Nhits in tpc excluding pseudo pad rows*/			   
  Int_t       		No_vpd_hit()   const {return NoHits(kvpd);}   /* Nhits in vpd */                     
  Int_t       		No_etr_hit()   const {return NoHits(ketr);}   /* Nhits in etr */                     
  Int_t       		No_hca_hit()   const {return NoHits(khca);}   /* Nhits in hca */                     
  Int_t       		No_fts_hit()   const {return NoHits(kfts);}   /* Nhits in fts */                     
  Int_t       		No_eto_hit()   const {return NoHits(keto);}   /* Nhits in eto */                     
  Int_t       		No_epd_hit()   const {return NoHits(kepd);}   /* Nhits in epd */                     
  Int_t       		No_stg_hit()   const {return NoHits(kstg);}   /* Nhits in stg */                     
  Int_t       		No_wca_hit()   const {return NoHits(kwca);}   /* Nhits in wca */                             
#endif /* __TFG__VERSION__ */
  Int_t                 ItrmdVertex()  const {return mItrmdVertex;} /* First intermediate vertex */	   
  Int_t          	IdVx       ()  const {return mIdVx;       } /* Id of start vertex of track */	   
  Int_t          	IdVxEnd    ()  const {return mIdVxEnd;    } /* Id of stop vertex of this track */ 
  Char_t         	Charge     ()  const {return mCharge;     } /* Charge */			   
  Float_t        	E          ()  const {return mE;          } /* Energy */			   
  Float_t        	Eta        ()  const {return mEta;        } /* Pseudorapidity */                  
  const StThreeVectorF& Pxyz       ()  const {return *&mPxyz;     } /* Momentum */                        
  Float_t               pT         ()  const {return mpT;         } /* Transverse momentum */	 	   
  Float_t        	Ptot       ()  const {return mPtot;       } /* Total momentum */	 	   
  Float_t        	Rapidity   ()  const {return mRapidity;   } /* Rapidity */                        
#ifdef __TFG__VERSION__
  void                  SetPdg(Int_t m)      {mPDG = m;}
#endif /* __TFG__VERSION__ */
  virtual void          Print(Option_t* option = "") const;  ///< Print track info
#ifdef __TFG__VERSION__
  virtual void          PrintHits(Option_t* option = "") const;  ///< Print MC hit info
#endif /* __TFG__VERSION__ */
  static Int_t          CorrectGePid(Int_t gePid);
  const Char_t         *GeName();
#ifdef __TFG__VERSION__
  void                  FillKFMCTrack(KFMCTrack &mcTrackKF);
#endif /* __TFG__VERSION__ */
 private:
#ifdef __TFG__VERSION__
  Char_t         mBeg[1];      //!
  Int_t          mEgLabel;     /* generator track label (0 if GEANT track) */
  Int_t          mPDG;         /* PDG,  */
#endif /* __TFG__VERSION__ */
  Int_t          mGePid;       /* GEANT particle id */
  Int_t          mId;          /* primary key */
  Bool_t         mIsShower;    /* 1 if shower track, 0 if not */
  UChar_t        mHits[ktot];  /* Nhits in a detector */
  Int_t          mItrmdVertex; /* First intermediate vertex */
  Int_t          mIdVx;        /* Id of start vertex of track */
  Int_t          mIdVxEnd;     /* Id of stop vertex of this track */
  Char_t         mCharge;      /* Charge */
  Float_t        mE;           /* Energy */
  Float_t        mEta;         /* Pseudorapidity */
#ifndef __TFG__VERSION__
  StThreeVectorF mPxyz;        /* Momentum */
#endif /* ! __TFG__VERSION__ */
  Float_t        mpT;          /* Transverse momentum */
  Float_t        mPtot;        /* Total momentum */
  Float_t        mRapidity;    /* Rapidity */
#ifndef __TFG__VERSION__
  ClassDef(StMuMcTrack,1)
#else /* __TFG__VERSION__ */
  Char_t         mEnd[1];      //!
  StThreeVectorF mPxyz;        /* Momentum */
  ClassDef(StMuMcTrack,5)
#endif /* __TFG__VERSION__ */
};
ostream&              operator<<(ostream& os, StMuMcTrack const & v);
#endif
// $Log: StMuMcTrack.h,v $
// Revision 1.4  2014/08/06 19:19:07  perev
// Move constructor .h ==> .cxx
//
// Revision 1.3  2012/05/07 14:47:06  fisyak
// Add handles for track to fast detector matching
//
// Revision 1.2  2011/10/17 00:19:14  fisyak
// Active handing of IdTruth
//
