// $Id: StMuMcTrack.cxx,v 1.4 2014/08/06 19:19:02 perev Exp $
#include "StMuMcTrack.h"
#include "StMuMcVertex.h"
#include "StMuDst.h"
#include <string.h>
#include "Stiostream.h"
#include "TString.h"
#include "TMath.h"
#include "TDatabasePDG.h"
ClassImp(StMuMcTrack);
//________________________________________________________________________________
StMuMcTrack::StMuMcTrack() {
  memset(mBeg,0,mEnd-mBeg+1);
}
//________________________________________________________________________________
StMuMcTrack::StMuMcTrack(const g2t_track_st &t) : TObject(),mEgLabel(t.eg_label), mPDG(t.eg_pid), mGePid(t.ge_pid),  mId(t.id),
						  mIsShower(t.is_shower), mItrmdVertex(t.itrmd_vertex_p),
						  mIdVx(t.start_vertex_p), mIdVxEnd(t.stop_vertex_p), mCharge(t.charge), mE(t.e), mEta(t.eta), 
						  mpT(t.pt), mPtot(t.ptot), 
						  mRapidity(t.rapidity), mPxyz(t.p) {
  mHits[kctb] = 0xff & t.n_ctb_hit;  /* Nhits in ctb */
  mHits[keem] = 0xff & t.n_eem_hit;  /* Nhits in eem (endcap em cal) */
  mHits[kemc] = 0xff & t.n_emc_hit;  /* Nhits in emc */
  mHits[kesm] = 0xff & t.n_esm_hit;  /* Nhits in esm (endcap shower max) */
  mHits[kftp] = 0xff & t.n_ftp_hit;  /* Nhits in forward tpc */
  mHits[kgem] = 0xff & t.n_gem_hit;  /* Nhits in gem barrel */
  mHits[khpd] = 0xff & t.n_hpd_hit;  /* Nhits in hpd */
  mHits[kist] = 0xff & t.n_ist_hit;  /* Nhits in ist */
  mHits[kigt] = 0xff & t.n_igt_hit;  /* Nhits in igt */
  mHits[kfst] = 0xff & t.n_fst_hit;  /* Nhits in fst */
  mHits[kfgt] = 0xff & t.n_fgt_hit;  /* Nhits in fgt */
  mHits[kfpd] = 0xff & t.n_fpd_hit;  /* Nhits in fpd */
  mHits[kmwc] = 0xff & t.n_mwc_hit;  /* Nhits in mwc */
  mHits[kpgc] = 0xff & t.n_pgc_hit;  /* Nhits in pgc  ???  */
  mHits[kpmd] = 0xff & t.n_pmd_hit;  /* Nhits in pmd (PMD) */
  mHits[ksmd] = 0xff & t.n_smd_hit;  /* number of hits in shower max */
  mHits[kssd] = 0xff & t.n_ssd_hit;  /* Nhits in ssd */
  mHits[ksvt] = 0xff & t.n_svt_hit;  /* Nhits in svt */
  mHits[kpix] = 0xff & t.n_pix_hit;  /* Nhits in pix */
  mHits[ktof] = 0xff & t.n_tof_hit;  /* Nhits in tof */
  mHits[ktpc] = 0xff & t.n_tpc_hit;  /* Nhits in tpc */
  mHits[ktpc] = 0xff & t.n_tpc_hit;  /* Nhits in tpc */
  mHits[ktpcR]= 0xff &(t.n_tpc_hit >> 8);  /* Nhits in tpc real pad rows */
  mHits[kvpd] = 0xff & t.n_vpd_hit;  /* Nhits in vpd */
  mHits[ketr] = 0xff & t.n_etr_hit;  /* Nhits in etr */
  mHits[khca] = 0xff & t.n_hca_hit;  /* Nhits in hca */
  mHits[kfts] = 0xff & t.n_fts_hit;  /* Nhits in fts */
  mHits[keto] = 0xff & t.n_eto_hit;  /* Nhits in eto */
  
  assert(t.pt<0 || mPxyz.perp()>1e-6);
}
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuMcTrack& v) {
  os << Form("McTk:%4i Vx:%4i Ge:%4i Pdg:%i7 NoHits:%3i",v.Id(),v.IdVx(),v.GePid(),v.Pdg(),v.NoHits());
  os << Form(" q:%2i pT:%7.3f eta:%6.3f phi:%6.3f p:%8.3f px:%8.3f py:%8.3f pz:%8.3f",v.Charge(),v.pT(), v.Eta(), 
	     TMath::ATan2(v.Pxyz().y(),v.Pxyz().x()), v.Ptot(), v.Pxyz().x(),v.Pxyz().y(),v.Pxyz().z());
  return os;
}
//________________________________________________________________________________
Int_t StMuMcTrack::CorrectGePid(Int_t gePid) {
  // By pass embedding particle redefinition
  if (gePid ==           99) gePid =         11151;
  if (gePid ==          207) gePid =            41;
  if (gePid ==        40001) gePid =            24;
  if (gePid ==           98) gePid =            18;
  if (gePid ==        40002) gePid =            32;
  if (gePid ==           97) gePid =            26;
  if (gePid ==        40003) gePid =            23;
  if (gePid ==        40004) gePid =            31;
  if (gePid ==        40005) gePid =            22;
  if (gePid ==        40006) gePid =            30;
  if (gePid ==        10150) gePid =           150;
  if (gePid ==        10151) gePid =           151;
  if (gePid ==        11151) gePid =         10151;
  if (gePid ==        10018) gePid =            98;
  if (gePid ==        10026) gePid =            97;
  if (gePid ==        10017) gePid =            17;
  if (gePid ==        10039) gePid =            39;
  if (gePid ==        10040) gePid =            40;
  if (gePid ==           98) gePid =            18;
  if (gePid ==           97) gePid =            26;
  if (gePid < 0 || gePid > 50) {
    //    cout << "Illegal gePid " << gePid << endl;
    gePid = 51;
  }
  return gePid;
}
//________________________________________________________________________________
const Char_t *StMuMcTrack::GeName() {
  static const Char_t *GeNames[52] = {
    //   1       2       3      4         5           6       7        8         9          10
    "",
    "gamma"   ,"e+"   ,"e-"  ,"nu"   ,"mu+"   ,"mu-"   ,"pi0"  ,"pi+"   ,"pi-"      ,"K0L",
    "K+"      ,"K-"   ,"N"   ,"P"    ,"Pbar"  ,"K0S"   ,"eta"  ,"Lambda","Sigma+"   ,"Sigma0",
    "S-"      ,"Xi0"  ,"Xi-" ,"Omega","Nbar"  ,"LamBar","SBar-","SBar0" ,"SBar+"    ,"XiBar0",
    "XiBar+"  ,"OmBar","tau+","tau-" ,"D+"    ,"D-"    ,"D0"   ,"Dbar0" ,"Ds+"      ,"Ds-"   ,
    "LambC+"  ,"W+"   ,"W-"  ,"Z0"   ,"H2"    ,"H3"    ,"alpha","geanti","He3"      ,"Cerenk",
    "??????"};
  static TString Name;
  Int_t iGe = CorrectGePid(GePid());
  Name = GeNames[iGe];
  return Name.Data();
}
//________________________________________________________________________________
void StMuMcTrack::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
void StMuMcTrack::PrintHits(Option_t *option) const {
  cout << *this << endl << "No hits:\t";
  if (No_ctb_hit() ) cout << No_ctb_hit() << " ctb\t";			     
  if (No_eem_hit() ) cout << No_eem_hit() << " eem (endcap em cal)\t";	     
  if (No_emc_hit() ) cout << No_emc_hit() << " emc\t";			     
  if (No_esm_hit() ) cout << No_esm_hit() << " esm (endcap shower max)\t";	     
  if (No_ftp_hit() ) cout << No_ftp_hit() << " forward tpc\t";		     
  if (No_gem_hit() ) cout << No_gem_hit() << " gem barrel\t";		   	     
  if (No_hpd_hit() ) cout << No_hpd_hit() << " hpd\t";			     
  if (No_ist_hit() ) cout << No_ist_hit() << " ist\t";			     
  if (No_igt_hit() ) cout << No_igt_hit() << " igt\t";			     
  if (No_fst_hit() ) cout << No_fst_hit() << " fst\t";			     
  if (No_fgt_hit() ) cout << No_fgt_hit() << " fgt\t";			     
  if (No_fpd_hit() ) cout << No_fpd_hit() << " fpd\t";			     
  if (No_mwc_hit() ) cout << No_mwc_hit() << " mwc\t";			     
  if (No_pgc_hit() ) cout << No_pgc_hit() << " pgc  ??? \t";		   	     
  if (No_pmd_hit() ) cout << No_pmd_hit() << " pmd (PMD)\t";		   	     
  if (No_smd_hit() ) cout << No_smd_hit() << " shower max\t";	     
  if (No_ssd_hit() ) cout << No_ssd_hit() << " ssd\t";			     
  if (No_svt_hit() ) cout << No_svt_hit() << " svt\t";			     
  if (No_pix_hit() ) cout << No_pix_hit() << " pix\t";			     
  if (No_tof_hit() ) cout << No_tof_hit() << " tof\t";			     
  if (No_tpc_hitA()) cout << No_tpc_hitA()<< " tpc\t";			     
  if (No_tpc_hit() ) cout << No_tpc_hit() << " tpc excluding pseudo pad rows\t";   
  if (No_vpd_hit() ) cout << No_vpd_hit() << " vpd\t";                    	     
  if (No_etr_hit() ) cout << No_etr_hit() << " etr\t";                    	     
  if (No_hca_hit() ) cout << No_hca_hit() << " hca\t";                    	     
  if (No_fts_hit() ) cout << No_fts_hit() << " fts\t";                    	     
  if (No_eto_hit() ) cout << No_eto_hit() << " etof\t";                    	     
  if (No_epd_hit() ) cout << No_epd_hit() << " epd\t";                    	     
  if (No_stg_hit() ) cout << No_stg_hit() << " stg\t";                    	     
  if (No_wca_hit() ) cout << No_wca_hit() << " wca";
  cout << endl;                             
}
//________________________________________________________________________________
void StMuMcTrack::FillKFMCTrack(KFMCTrack &mcTrackKF) {
  Float_t q = Charge();
  Int_t pdgRoot = Pdg();
  if (! pdgRoot) {
    pdgRoot = TDatabasePDG::Instance()->ConvertGeant3ToPdg(GePid());
    if(pdgRoot == 0 && q>0) pdgRoot = 211;
    if(pdgRoot == 0 && q<0) pdgRoot = -211;
    mcTrackKF.SetPDG(pdgRoot);
  }
  mcTrackKF.SetPDG( pdgRoot );
  Float_t pXYZ[3] = {Pxyz().x(), Pxyz().y(), Pxyz().z()};
  for (Int_t i = 0; i < 3; i++) {
    mcTrackKF.SetPar( 3+i, pXYZ[i] );
  }
  mcTrackKF.SetPar(6, q/mcTrackKF.P()/3 ); // q=3q            
  mcTrackKF.SetNMCPoints(No_tpc_hit());
  mcTrackKF.SetMotherId(-1);
  Int_t IdV = IdVx() - 1;
  StMuMcVertex *mcVertex = StMuDst::instance()->MCvertex(IdV);
  Int_t IdP = mcVertex->IdParTrk() - 1;
  if( IdP < 0 ) IdP = -(IdV+1);
  mcTrackKF.SetMotherId(IdP);
  Float_t vXYZ[3] = {mcVertex->XyzV().x(), mcVertex->XyzV().y(), mcVertex->XyzV().z()};
  for (Int_t i = 0; i < 3; i++) { // TODO
    mcTrackKF.SetPar( i,  vXYZ[i] );
  }
}
//________________________________________________________________________________
// $Log: StMuMcTrack.cxx,v $
// Revision 1.4  2014/08/06 19:19:02  perev
// Move constructor .h ==> .cxx
//
// Revision 1.3  2012/11/26 23:08:23  fisyak
// Fix print out, thanks to Jonathan (bug #2442)
//
// Revision 1.2  2012/05/07 14:47:06  fisyak
// Add handles for track to fast detector matching
//
// Revision 1.1  2011/10/17 00:19:13  fisyak
// Active handing of IdTruth
//
