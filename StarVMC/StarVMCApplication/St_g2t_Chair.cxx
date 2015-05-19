#include "Stiostream.h"
#include "St_g2t_Chair.h"
#include "StarMCHits.h"
ClassImp(St_g2t_Chair);
ClassImp(St_g2t_ctf_hitC)
ClassImp(St_g2t_emc_hitC)
ClassImp(St_g2t_fst_hitC)
ClassImp(St_g2t_fgt_hitC)
ClassImp(St_g2t_ftp_hitC)
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
  memset(&g2t_ ## A ## _hit, 0, sizeof(g2t_ ## A ## _hit_st));			\
  St_g2t_ ## A ## _hit *table = (St_g2t_ ## A ## _hit*) GetThisTable(); if (Debug()) table->Print(0,5); \
  Int_t  nok = table->GetNRows()+1; 
#define G2TFillNextTrack(A) \
  g2t_ ## A ## _hit_st *row = table->GetTable(); \
  for (Int_t i = nok - 2; i >= 0; i--) if ((row+i)->track_p == vect.iTrack) {g2t_ ## A ## _hit.next_tr_hit_p = i; break;} 
#define G2TFillTrackHitB(A) \
  g2t_ ## A ## _hit.id            = nok;					\
  g2t_ ## A ## _hit.x[0]          = vect.Middle.Global.xyzT.X();		\
  g2t_ ## A ## _hit.x[1]          = vect.Middle.Global.xyzT.Y();		\
  g2t_ ## A ## _hit.x[2]          = vect.Middle.Global.xyzT.Z();		\
  g2t_ ## A ## _hit.p[0]          = vect.Middle.Global.pxyzE.X();		\
  g2t_ ## A ## _hit.p[1]          = vect.Middle.Global.pxyzE.Y();		\
  g2t_ ## A ## _hit.p[2]          = vect.Middle.Global.pxyzE.Z();		\
  g2t_ ## A ## _hit.tof           = vect.Middle.Global.xyzT.T();		\
  g2t_ ## A ## _hit.de            = vect.AdEstep;				\
  g2t_ ## A ## _hit.ds            = vect.AStep;				        \
  g2t_ ## A ## _hit.track_p       = vect.iTrack;				\
  g2t_ ## A ## _hit.volume_id     = volume_id; 
#define G2TFillTrackHit(A) \
  G2TFillTrackHitB(A) \
  G2TFillNextTrack(A) \
  table->AddAt(&g2t_ ## A ## _hit);
#define G2TFillTrackRCHit(A) \
  G2TFillTrackHitB(A) \
  table->AddAt(&g2t_ ## A ## _hit);
#define G2TFillTrackHitNoTOF(A) \
  g2t_ ## A ## _hit.id            = nok;					\
  g2t_ ## A ## _hit.x[0]          = vect.Middle.Global.xyzT.X();		\
  g2t_ ## A ## _hit.x[1]          = vect.Middle.Global.xyzT.Y();		\
  g2t_ ## A ## _hit.x[2]          = vect.Middle.Global.xyzT.Z();		\
  g2t_ ## A ## _hit.p[0]          = vect.Middle.Global.pxyzE.X();		\
  g2t_ ## A ## _hit.p[1]          = vect.Middle.Global.pxyzE.Y();		\
  g2t_ ## A ## _hit.p[2]          = vect.Middle.Global.pxyzE.Z();		\
  g2t_ ## A ## _hit.de            = vect.AdEstep;				\
  g2t_ ## A ## _hit.track_p       = vect.iTrack;				\
  g2t_ ## A ## _hit.volume_id     = volume_id;				\
  table->AddAt(&g2t_ ## A ## _hit);
//________________________________________________________________________________
//void St_g2t_hitsC::Fill(GHit_t &vect) {}
//________________________________________________________________________________
void St_g2t_ctf_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = 0;
  if (VolName == "BPOL") // BBC
    volume_id = vect.NUMBV[0]*1000 + vect.NUMBV[1]*100 + vect.NUMBV[2]*10 + vect.NUMBV[3];
  if (VolName == "BXSA") // CTB
    volume_id = vect.NUMBV[0]*1000 + vect.NUMBV[1]*100 + vect.NUMBV[2];
  if (VolName == "BXSB") {// TOF
    Int_t rileft     = 2;              //     east (pre-set)
    Int_t sector     = vect.NUMBV[0];       //     tray
    Int_t innout     = vect.NUMBV[1];       //     4wide/5wide section
    Int_t sub_sector = vect.NUMBV[2];       //     theta-tray
    Int_t section    = vect.NUMBV[3];       //     phi-tray
    if (innout==1) 
      section = section+1;  //  phi-tray (4wide-sections)
    volume_id = 100000*rileft+1000*sector+100*sub_sector+section;
  }
  if (VolName == "BRSG") {// TFR
    Int_t rileft     = vect.NUMBV[0];        //  west(1)/east(2)
    Int_t sector     = vect.NUMBV[1];        //  tray(1-60)
    Int_t module     = vect.NUMBV[2];        //  module (eta)
    Int_t layer      = vect.NUMBV[3];        //  layer (phi, get from hit position)
    volume_id  = layer +10*(module +100*(sector+100*rileft) );
  }
  G2TBookTrackHit(ctf);
  g2t_ctf_hit.s_track       = vect.Sleng;				\
  G2TFillTrackHit(ctf);
 }
//________________________________________________________________________________
void St_g2t_emc_hitC::Fill(GHit_t &vect) {
#if 0
  static g2t_emc_hit_st g2t_emc_hit;
  memset(&g2t_emc_hit, 0, sizeof(g2t_emc_hit_st));
  St_g2t_emc_hit *table = (St_g2t_emc_hit*) GetThisTable(); if (Debug()) table->Print(0,5);
  Int_t nok = table->GetNRows()+1;
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  g2t_emc_hit.id            = nok;
  Int_t volume_id = 0;
/* PN, MAX:
   OnOff    = (0..3)  -  East-West config:  0-none,            1 - west,     2-east,   3-both
   FillMode = (1..3)  -  Sectors fill mode: 1- one 3rd filled, 2 - one half, 3 - full  */
  Int_t rileft, shift, phi, superl, eta, iWheel, section, phi_30d, phi_sub;
  Int_t zsubsect,zsublayer,eemc_depth,strip, forw_back, depth;
  static Int_t   sector_hash[2][6] = {{  4, 5, 6, 7, 8, 9}, // switch indexes 
				      { 10,11,12, 1, 2, 3}};
  if        (VolName == "CSUP") { // emc 
    rileft = vect.NUMBV[0]; //          both left and right barrels:
    phi    = vect.NUMBV[1];
    superl = vect.NUMBV[2];
    eta = 1; //    eta=idigi(1)+1;
    phi_sub = 0; //    phi_sub=idigi(2)
    if (rileft==1) {
      phi=60-phi+1;
      if (phi_sub==0) phi_sub=2;
      else {
	phi=60+phi;
	phi_sub=phi_sub+1;
      }
    }
    volume_id=10000000*rileft+100000*eta+100*phi+10*phi_sub+superl;
  } else if (VolName == "CSDA") { // smd 
    rileft   =vect.NUMBV[0];
    phi      =vect.NUMBV[1];
    forw_back=vect.NUMBV[2];
    eta  = 1; //idigi(2)+1
    strip= 1; //idigi(3)+1
    Int_t calg_Netfirst = 0; // ???
    Int_t calg_Netsecon = 0;
    Int_t calg_NPhistr = 0; 
    if (forw_back==4) forw_back=3;
    if (rileft==1)   phi=60-phi+1;
    else             phi=60+phi;
	if(rileft<1 || rileft>2)                          
	  cout << "**ERROR at g2t_volume_id: smd rl " << rileft << endl;
/*	else if(eta<1 || eta>calg_NetaSMDp)                   
 *	  cout << "**ERROR at g2t_volume_id: smd eta " << eta << endl;
 *	else if(phi<1 || phi>CALG_MaxModule)             
 *	  cout << "**ERROR at g2t_volume_id: smd phi " << phi  << endl;*/
	else if(forw_back<1 || forw_back>3)             
	  cout << "**ERROR at g2t_volume_id: smd forw_back " << forw_back << endl;
	else if(strip<1)             
	  cout << "**ERROR at g2t_volume_id: smd strip " << strip << endl;
	else if(forw_back==1 && strip>calg_Netfirst)             
	  cout << "**ERROR at g2t_volume_id: smd strip " << strip<< "\t" <<  forw_back << endl;
	else if(forw_back==2 && strip>calg_Netsecon)              
	  cout << "**ERROR at g2t_volume_id: smd strip " << strip<< "\t" <<  forw_back << endl;
	else if(forw_back==3 && strip>calg_NPhistr)             
	  cout << "**ERROR at g2t_volume_id: smd strip " << strip<< "\t" <<  forw_back << endl;
	else 
          volume_id=100000000*rileft+1000000*eta+1000*phi+100*forw_back+strip;
     
  } else if (VolName == "ESCI") { // eem 
    rileft    = vect.NUMBV[0];    // emcg_onoff >= 3 only
    shift     = 1;
    iWheel = vect.NUMBV[shift];   // emcg_version >= 5 && emcg_FillMode > 2
    shift  += 1;
    section   = vect.NUMBV[shift];                           // ECVO
    phi_30d   = sector_hash[iWheel-1][vect.NUMBV[shift+1]-1];// EMOD
    zsubsect  = vect.NUMBV[shift+2];                         // ESEC (no readout)
    zsublayer = vect.NUMBV[shift+3];                         // EMGT (no readout)
    phi       = vect.NUMBV[shift+4];                         // EPER (5 fingers)
    eta       = vect.NUMBV[shift+5];                         // ETAR (radial division)
    eemc_depth = zsubsect + 3*(section-1);
    volume_id = 100000*rileft + 1000*(5*(phi_30d-1)+phi) + 10*eta + eemc_depth;
  } else if (VolName == "EHMS") { // esm 
    rileft    = vect.NUMBV[0];
    shift     = 1;
    Int_t emcg_version = 5;
    Int_t emcg_FillMode = 2;
    // see comment above about >=
    if (emcg_version >= 5) {
      if (emcg_FillMode <= 2 ) 
	iWheel = 1;
      else {
	iWheel = vect.NUMBV[shift];
	shift  += 1;
    }
      depth     = vect.NUMBV[shift];
      //         phi       = vect.NUMBV[shift+1] 
      phi_30d   = sector_hash[iWheel][vect.NUMBV[shift+1]];
      strip     = vect.NUMBV[shift+2];
    } else  {// version before 5
      rileft    = vect.NUMBV[0];
      depth     = vect.NUMBV[1];
      phi       = vect.NUMBV[2];
      strip     = vect.NUMBV[3]; 
    }
    volume_id = 1000000*rileft+10000*phi_30d+1000*depth+strip;
  } else if (VolName == "QSCI") { // zdc 
    volume_id = vect.NUMBV[0]*1000+vect.NUMBV[1];
//   } else if (VolName == "ELGR") { // eem 
//   } else if (VolName == "EPCT") { // eem 
//   } else if (VolName == "EXSE") { // esm 
  } else {
    volume_id=1000*(1000*vect.NUMBV[0] + vect.NUMBV[1]) + vect.NUMBV[2];
  }
  g2t_emc_hit.de            = vect.AdEstep;
  g2t_emc_hit.track_p       = vect.iTrack;
  g2t_emc_hit.volume_id     = volume_id;
  g2t_emc_hit_st *emc = table->GetTable();
  for (Int_t i = 0; i < nok - 1; i++, emc++) {
    if (emc->volume_id == g2t_emc_hit.volume_id &&
	emc->track_p   == g2t_emc_hit.track_p) {
      emc->de += g2t_emc_hit.de;
      return;
    }
  }
  table->AddAt(&g2t_emc_hit);    
#endif
}
//________________________________________________________________________________
void St_g2t_fst_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = vect.NUMBV[0]*1000000 + vect.NUMBV[1]*10000 + vect.NUMBV[2]*100  + vect.NUMBV[3];
  G2TBookTrackHit(fst);
  G2TFillTrackHit(fst);
}
//________________________________________________________________________________
void St_g2t_fgt_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = vect.NUMBV[0]*1000000 + vect.NUMBV[1]*10000 + vect.NUMBV[2]*100  + vect.NUMBV[3];
  G2TBookTrackHit(fgt);
  G2TFillTrackHit(fgt);
}
//________________________________________________________________________________
void St_g2t_ftp_hitC::Fill(GHit_t &vect) {
  static Int_t  ftpc_hash[2][6] = {{ 1, 6, 5, 4, 3, 2}, // switch indexes
				   { 6, 1, 2, 3, 4, 5}};
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
/* ftpv=1 for west, 2 for east part of the FTPC
 * ftpc_sector is the phi division of the gas layer
 * the numbering scheme below designed by Janet Seyboth,
 * but I'm adding the correct mapping between GEANT id's
 * and the ones found on:
 * http://wwwstar.mppmu.mpg.de/ftpc_calibration_page/calibration/calibration.html
 * I use a hash table for a unique and clean way to number sectors
 * --max-- */
  Int_t ftpv       = vect.NUMBV[0];
  Int_t padrow     = vect.NUMBV[1];
  Int_t ftpc_sector= ftpc_hash[ftpv-1][vect.NUMBV[2]-1];
  Int_t volume_id  = (100*ftpv+padrow)*10 + ftpc_sector;
  G2TBookTrackHit(ftp);
  G2TFillTrackHit(ftp);
}
//________________________________________________________________________________
void St_g2t_ist_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = vect.NUMBV[0]*1000000 + vect.NUMBV[1]*10000 + vect.NUMBV[2]*100  + vect.NUMBV[3];
  G2TBookTrackHit(ist);
  G2TFillTrackHit(ist);
}
//________________________________________________________________________________
void St_g2t_mwc_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t rileft    = vect.NUMBV[0];
  Int_t sector    = vect.NUMBV[1]; 
  Int_t innout    = vect.NUMBV[2];
  Int_t innour    = vect.NUMBV[3];
  Int_t volume_id = 1000*rileft+100*innout+10*innour+sector;
  G2TBookTrackHit(mwc);
  g2t_mwc_hit.s_track       = vect.Sleng;				\
  G2TFillTrackHit(mwc);
}
//________________________________________________________________________________
void St_g2t_pix_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = vect.NUMBV[0]*1000000 + vect.NUMBV[1]*10000 + vect.NUMBV[2]*100  + vect.NUMBV[3];
  G2TBookTrackHit(pix);
  G2TFillTrackHit(pix);
}
//________________________________________________________________________________
void St_g2t_pmd_hitC::Fill(GHit_t &vect) {
  Int_t volume_id =  vect.NUMBV[0]*1000000 + vect.NUMBV[1]*100000 + vect.NUMBV[2]*10000 + vect.NUMBV[3]*100 + vect.NUMBV[4];
  G2TBookTrackHit(pmd);
  G2TFillTrackHitNoTOF(pmd);
}
//________________________________________________________________________________
void St_g2t_rch_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t is=0;
  if      (VolName == "RGAP") is=1;
  else if (VolName == "RCSI") is=2;
  else if (VolName == "QUAR") is=3;
  else if (VolName == "FREO") is=4;
  else if (VolName == "OQUA") is=5;
  Int_t volume_id = vect.NUMBV[0] + is*1000;
  G2TBookTrackHit(rch);
  G2TFillTrackRCHit(rch);
}
//________________________________________________________________________________
void St_g2t_ssd_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  Int_t volume_id = 7000+100*vect.NUMBV[1]+vect.NUMBV[0];// ssd
  G2TBookTrackHit(ssd);
  G2TFillTrackHit(ssd);
}
//________________________________________________________________________________
void St_g2t_svt_hitC::Fill(GHit_t &vect) {
  TString VolName(StarMCHits::instance()->GetCurrentDetector()->GetName());
  // Helen altered SVT volume IDs so agrees with hardware defs.
  Int_t      lnumber  = vect.NUMBV[0];
  Int_t      ladder   = vect.NUMBV[1];
  Int_t      wafer    = vect.NUMBV[2];
  Int_t      nladder  = 0;
  Int_t      volume_id = -1;
  if (VolName == "SFSD") {//ssd
    volume_id =  7000+100*vect.NUMBV[1]+vect.NUMBV[0];// ssd
  } else {//      SVTD      svt
    if ( ladder == 0) {// This is the year 1 ladder
      nladder = 1;
      wafer   = lnumber;
      ladder  = 12;
      lnumber = 4;
    } else if (lnumber <= 2) {// Set First barrel ids
      nladder = 8;//               wafer   = 5-wafer 
    } else if (lnumber <= 4) {// Set 2nd barrel ids
    nladder  = 12;//               wafer   = 7-wafer   
    } else if (lnumber <= 6) {// Set 3rd barrel ids
      nladder  = 16;//               wafer   = 8-wafer
    } else {
      cout << " G2T warning: layer number " << lnumber 
	   << "     in svt hits not found" << endl;
    }
    if (nladder>1) {// PN: change geant numbering (CCW) to STAR numbering(CW):
      Int_t lsub    = (lnumber-1)%2;//             inner sub-layer - 0, outer - 1:
#if 0
      if (svtg_version==1) { 
	//             OLD: 3 o'clock is geant's first and STAR N/4 element:
	ladder=nladder/4-(ladder-1)*2-lsub;
	ladder=(nladder+ladder-1)%nladder+1;
      } else
#else
	//             NEW: 12 o'clock is geant's first and STAR last element:
	ladder=nladder-(ladder-1)*2-lsub;
#endif
    }
    volume_id  = 1000*lnumber+100*wafer+ladder;
  }
  G2TBookTrackHit(svt);
#if 0
  //                  add to track linked list 
  g2t_svt_hit.next_tr_hit_p = g2t_track(trac).hit_svt_p;
  g2t_track(trac).hit_svt_p    = nok;
  g2t_track(trac).n_svt_hit    = g2t_track(trac).n_svt_hit + 1;
#endif  
  G2TFillTrackHit(svt);
}
//________________________________________________________________________________
void St_g2t_tpc_hitC::Fill(GHit_t &vect) {
  Int_t      tpgv  = vect.NUMBV[0];
  Int_t      tpss  = vect.NUMBV[1];
  Int_t      sector= tpss+12*(tpgv-1);
  Int_t      tpad  = vect.NUMBV[2];
  Int_t      isdet = 0;
  if (tpad<41) {
    isdet = (41-tpad)%3;
    tpad  = (tpad+2)/3;
  }  else {
    if (tpad<73) tpad=tpad-41+14;
    else {
      isdet = 2;
      tpad  = 45;
    }
  }
  Int_t volume_id=100000*isdet+100*sector+tpad;
  G2TBookTrackHit(tpc);
  
  Double_t GeKin            = vect.Middle.Global.pxyzE.E() - vect.Mass;
  Double_t lgam             = 0;
  if (vect.Mass > 0 && GeKin > 0 && vect.Charge != 0) 
    lgam = TMath::Log10(GeKin/vect.Mass);
  g2t_tpc_hit.lgam          = lgam;
#if 0
  //                  add to track linked list 
  g2t_tpc_hit.next_tr_hit_p = g2t_track(trac).hit_tpc_p;
  g2t_track(trac).hit_tpc_p    = nok;
  g2t_track(trac).n_tpc_hit    = g2t_track(trac).n_tpc_hit + 1;
#endif  
  G2TFillTrackHit(tpc);
}
//________________________________________________________________________________
void St_g2t_vpd_hitC::Fill(GHit_t &vect) {
  Int_t volume_id = 0;
  Int_t rileft    = vect.NUMBV[0];
  Int_t innout    = vect.NUMBV[1];
  Int_t sector    = vect.NUMBV[2];
  if (vect.NVL == 2) {
    rileft    = vect.NUMBV[0];
    innout    = 0;
    sector    = vect.NUMBV[1];
  }
  volume_id  =  1000*rileft + 100*innout + sector;
  G2TBookTrackHit(vpd);
  g2t_vpd_hit.s_track       = vect.Sleng;				\
  G2TFillTrackHit(vpd);
}
//________________________________________________________________________________
