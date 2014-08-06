// $Id: StMuMcTrack.cxx,v 1.4 2014/08/06 19:19:02 perev Exp $
#include "StMuMcTrack.h"
#include "Stiostream.h"
#include "TString.h"
#include "TMath.h"
ClassImp(StMuMcTrack);
//________________________________________________________________________________
StMuMcTrack::StMuMcTrack(const g2t_track_st &t) : TObject(), mGePid(t.ge_pid), mId(t.id), mIsShower(t.is_shower), mItrmdVertex(t.itrmd_vertex_p),
    mIdVx(t.start_vertex_p), mIdVxEnd(t.stop_vertex_p), mCharge(t.charge), mE(t.e), mEta(t. eta), mPxyz(t.p), mpT(t.pt), mPtot(t.ptot), 
    mRapidity(t.rapidity) {
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
    mHits[kvpd] = 0xff & t.n_vpd_hit;  /* Nhits in vpd */

assert(t.pt<0 || mPxyz.perp()>1e-6);

}




ostream&              operator<<(ostream& os,  const StMuMcTrack& v) {
//________________________________________________________________________________
  os << Form("Tk:%4i Vx:%4i Ge:%4i NoHits:%3i",v.Id(),v.IdVx(),v.GePid(),v.NoHits());
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
