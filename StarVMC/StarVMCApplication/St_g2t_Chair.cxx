#include "Stiostream.h"
#include "StarMCHits.h"
#include "St_g2t_Chair.h"
ClassImp(St_g2t_Chair);
ClassImp(St_g2t_ctf_hitC)
ClassImp(St_g2t_emc_hitC)
ClassImp(St_g2t_fgt_hitC)
ClassImp(St_g2t_ftp_hitC)
ClassImp(St_g2t_gem_hitC)
ClassImp(St_g2t_ist_hitC)
ClassImp(St_g2t_mwc_hitC)
ClassImp(St_g2t_pix_hitC)
ClassImp(St_g2t_pmd_hitC)
ClassImp(St_g2t_rch_hitC)
ClassImp(St_g2t_ssd_hitC)
ClassImp(St_g2t_svt_hitC)
ClassImp(St_g2t_tpc_hitC)
ClassImp(St_g2t_vpd_hitC)
Int_t St_g2t_Chair::fDebug = 0;
#define G2TBookTrackHit(A) \
  static g2t_ ## A ## _hit_st g2t_ ## A ## _hit;\
  memset(&g2t_ ## A ## _hit, 0, sizeof(g2t_ ## A ## _hit_st));		\
  St_g2t_ ## A ## _hit *table = (St_g2t_ ## A ## _hit*) GetThisTable(); \
  Int_t  nok = table->GetNRows()+1; 
#define G2TFillNextTrack(A) \
  g2t_ ## A ## _hit_st *row = table->GetTable(); \
  for (Int_t i = nok - 2; i >= 0; i--) if ((row+i)->track_p == vect.iTrack) {g2t_ ## A ## _hit.next_tr_hit_p = i+1; break;} 
#define G2TFillTrackHitB(A) \
  g2t_ ## A ## _hit.id            = nok;				\
  g2t_ ## A ## _hit.x[0]          = vect.Middle.Global.xyzT.X();	\
  g2t_ ## A ## _hit.x[1]          = vect.Middle.Global.xyzT.Y();	\
  g2t_ ## A ## _hit.x[2]          = vect.Middle.Global.xyzT.Z();	\
  g2t_ ## A ## _hit.p[0]          = vect.Middle.Global.pxyzE.X();	\
  g2t_ ## A ## _hit.p[1]          = vect.Middle.Global.pxyzE.Y();	\
  g2t_ ## A ## _hit.p[2]          = vect.Middle.Global.pxyzE.Z();	\
  g2t_ ## A ## _hit.tof           = vect.Middle.Global.xyzT.T();	\
  g2t_ ## A ## _hit.de            = vect.AdEstep;			\
  g2t_ ## A ## _hit.ds            = vect.AStep;				\
  g2t_ ## A ## _hit.track_p       = vect.iTrack;			\
  g2t_ ## A ## _hit.volume_id     = vect.VolumeId;
#define G2TFillTrackHit(A)			\
  G2TFillTrackHitB(A)				\
  G2TFillNextTrack(A)				\
  table->AddAt(&g2t_ ## A ## _hit);
#define G2TFillTrackRCHit(A)			\
  G2TFillTrackHitB(A)				\
  table->AddAt(&g2t_ ## A ## _hit);
#define G2TFillTrackHitNoTOF(A)						\
  g2t_ ## A ## _hit.id            = nok;				\
  g2t_ ## A ## _hit.x[0]          = vect.Middle.Global.xyzT.X();	\
  g2t_ ## A ## _hit.x[1]          = vect.Middle.Global.xyzT.Y();	\
  g2t_ ## A ## _hit.x[2]          = vect.Middle.Global.xyzT.Z();	\
  g2t_ ## A ## _hit.p[0]          = vect.Middle.Global.pxyzE.X();	\
  g2t_ ## A ## _hit.p[1]          = vect.Middle.Global.pxyzE.Y();	\
  g2t_ ## A ## _hit.p[2]          = vect.Middle.Global.pxyzE.Z();	\
  g2t_ ## A ## _hit.de            = vect.AdEstep;			\
  g2t_ ## A ## _hit.track_p       = vect.iTrack;			\
  g2t_ ## A ## _hit.volume_id     = vect.VolumeId;			\
  table->AddAt(&g2t_ ## A ## _hit);
#define G2TTrackHit(A)				\
void St_g2t_## A ##_hitC::Fill(GHit_t &vect) {	\
  G2TBookTrackHit(A);				\
  G2TFillTrackHit(A);				\
  if (Debug()) table->Print(nok-1,2);		\
}
//________________________________________________________________________________
void St_g2t_hitsC::Fill(GHit_t &vect) {}
//________________________________________________________________________________
void St_g2t_ctf_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(ctf);
  g2t_ctf_hit.s_track       = vect.Sleng;	\
  G2TFillTrackHit(ctf);
 }
//________________________________________________________________________________
void St_g2t_emc_hitC::Fill(GHit_t &vect) {
  static g2t_emc_hit_st g2t_emc_hit;
  memset(&g2t_emc_hit, 0, sizeof(g2t_emc_hit_st));
  St_g2t_emc_hit *table = (St_g2t_emc_hit*) GetThisTable(); if (Debug()) table->Print(0,5);
  Int_t nok = table->GetNRows()+1;
  g2t_emc_hit.id            = nok;
  g2t_emc_hit.de            = vect.AdEstep;
  g2t_emc_hit.track_p       = vect.iTrack;
  g2t_emc_hit.volume_id     = vect.VolumeId;
  g2t_emc_hit_st *emc = table->GetTable();
  for (Int_t i = 0; i < nok - 1; i++, emc++) {
    if (emc->volume_id == g2t_emc_hit.volume_id &&
	emc->track_p   == g2t_emc_hit.track_p) {
      emc->de += g2t_emc_hit.de;
      return;
    }
  }
  table->AddAt(&g2t_emc_hit);    
}
//________________________________________________________________________________
G2TTrackHit(fgt);
G2TTrackHit(ftp);
G2TTrackHit(gem);
G2TTrackHit(ist);
//________________________________________________________________________________
void St_g2t_mwc_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(mwc);
  g2t_mwc_hit.s_track       = vect.Sleng;				\
  G2TFillTrackHit(mwc);
}
//________________________________________________________________________________
G2TTrackHit(pix);
//________________________________________________________________________________
void St_g2t_pmd_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(pmd);
  G2TFillTrackHitNoTOF(pmd);
}
//________________________________________________________________________________
void St_g2t_rch_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(rch);
  G2TFillTrackRCHit(rch);
}
//________________________________________________________________________________
G2TTrackHit(ssd);
G2TTrackHit(svt);
//________________________________________________________________________________
void St_g2t_tpc_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(tpc);
  
  Double_t GeKin            = vect.Middle.Global.pxyzE.E() - vect.Mass;
  Double_t lgam             = 0;
  if (vect.Mass > 0 && GeKin > 0 && vect.Charge != 0) 
    lgam = TMath::Log10(GeKin/vect.Mass);
  g2t_tpc_hit.lgam          = lgam;
  G2TFillTrackHit(tpc);
}
//________________________________________________________________________________
void St_g2t_vpd_hitC::Fill(GHit_t &vect) {
  G2TBookTrackHit(vpd);
  g2t_vpd_hit.s_track       = vect.Sleng;				\
  G2TFillTrackHit(vpd);
}
//________________________________________________________________________________
