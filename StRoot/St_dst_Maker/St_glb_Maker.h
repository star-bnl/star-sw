// $Id: St_glb_Maker.h,v 1.19 1999/06/25 22:48:39 caines Exp $
// $Log: St_glb_Maker.h,v $
// Revision 1.19  1999/06/25 22:48:39  caines
// Added ability to perform ev0 evaluation using eval flag
//
// Revision 1.18  1999/03/11 03:12:19  perev
// new schema
//
// Revision 1.17  1999/03/04 01:19:22  fisyak
// Put release tag to run_summary table
//
// Revision 1.16  1999/02/22 21:27:20  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.15  1999/02/18 16:43:12  caines
// Added in est the 4th layer tracking
//
// Revision 1.14  1999/02/16 03:03:47  fisyak
// Split Make and Histograms
//
// Revision 1.13  1999/02/13 20:22:32  caines
// Added exi and temp dir for when svt not there
//
// Revision 1.12  1999/02/12 22:27:39  ogilvie
// added in spectra/pid QA histograms
//
// Revision 1.11  1999/02/12 19:23:46  didenko
// updated v0 finding code from Helen
//
// Revision 1.10  1999/01/02 19:08:17  fisyak
// Add ctf
//
// Revision 1.9  1998/12/21 19:41:51  fisyak
// Move dst 2 glb
//
// Revision 1.8  1998/12/21 19:26:09  fisyak
// Make ROOT include non system
//
// Revision 1.7  1998/12/16 22:22:41  fisyak
// New global from Spiros
//
// Revision 1.6  1998/12/12 02:37:53  fisyak
// fix evr
//
// Revision 1.5  1998/11/01 16:42:27  fisyak
// dst analysis
//
// Revision 1.4  1998/10/31 00:26:13  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.2  1998/09/08 22:43:11  fisyak
// Modify St_glb_Maker to account new calling sequence
//
// Revision 1.1  1998/08/18 14:06:07  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_glb_Maker
#define STAR_St_glb_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_glb_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_svm_ctrl;
class St_est_ctrl;
class St_evr_privert;
class St_evr_evrpar;
class St_egr_propagate;
class St_egr_egrpar;
class St_ev0_ev0par;
class St_ev0_ev0par2;
class St_exi_exipar;
class St_exi_aux;
class St_mft_control; 
class St_particle_dst_param;
class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;
class St_dst_summary_param;

class St_glb_Maker : public StMaker {

 private:
  Bool_t drawinit;
  Bool_t m_ev0EvalOn; 	//switch for the evaluation
  // static Char_t m_VersionCVS = "$Id: St_glb_Maker.h,v 1.19 1999/06/25 22:48:39 caines Exp $";
  // egr
  Int_t         m_scenario;   
  //#1: Real TPC Stand-Alone Tracking: Use this when running the TPC only.  
  //    EGR will refit the TPC tracks, reproducing the parameters in tptrack.
  //#2: Perfect TPC Stand-Alone Tracking: Use this when running the TPC only.  
  //    EGR will perform a direct copy of tptrack into globtrk.
  //#3: Real SVT Stand-Alone Tracking: Use this when running the SVT only.  
  //    EGR will refit the SVT tracks, reproducing the parameters in svt_track, sort of.
  //#4: Perfect SVT Stand-Alone Tracking: Use this when running the SVT only.  
  //    EGR will copy the svt_track table directly into globtrk.
  //#5: Fit TPC tracks to the event vertex: Will refit the TPC tracks including 
  //    the event vertex point.
  //#6: Real SVT+TPC Matched Tracking: Refits the SVT and TPC tracks matched 
  //    by the svm matcher. Copies only tracks with a TPC track in it to globtrk.
  //#7: Perfect SVT+TPC Tracking: Copies SVT and TPC tracks matched using the 
  //    Monte Carlo ID. Copies only tracks with a TPC track in it to globtrk. Mtm from
  //    tpc track, angles from svt track
  //#8: Improved Real SVT+TPC Matched Tracking: Refits the SVT and TPC tracks 
  //    matched by the svm matcher. Fills globtrk with all matched SVT+TPC tracks, 
  //    followed by all eftover TPC tracks, followed by all leftover SVT tracks with a
  //    chi-square(1) fit better than epar.svtchicut
  //#9: Improved Perfect SVT+TPC Tracking: Refits the SVT and TPC tracks matched using 
  //    the Monte Carlo ID. Fills globtrk with all matched SVT+TPC tracks, followed by 
  //    all leftover TPC tracks, followed by all leftover SVT tracks with a chi-square(1) 
  //    fit better than epar.svtchicut
  //#10:Refits the SVT and TPC matched tracks followed by unmatched tracks
  //    Fills globtrk with all matched SVT+TPC tracks, followed by all
  //    leftover TPC tracks copied from tpctrack.
  //    The matched tracks are straight copies of svt_track but with
  //    the 1/pt value taken from tptrack.
  //#11:Description Perfect SVT/TPC matching (M.C.). Only tracks w/ TPC into 
  Int_t         m_svtchicut;  // = 0 all unmatched svt tracks copied
  Int_t         m_useglobal;  
  // = 1 - Perfect matching between tpc and svt (checks mc_ids)
  // = 2 - Refit tracks
  // = 3 - Copies info from svt tracks + 1/invpt from tptrack
  Int_t         m_usesvt;     // = 0 Dont' copy unmatched svt tracks
  // = 1 Copy unmatched tracks with chisq>svtchicut, no refit 
  // = 2 Copy unmatched tracks with chisq>svtchicut Refit
  Int_t         m_usetpc;     // = 0 Don't copy unmatched tracks
  // = 1 Copy unmatched tracks, no refit
  // = 2 Copy unmatched tracks, refit
  
  Int_t            m_usevert;    // 
  Int_t            m_flag;       //
  St_svm_ctrl    *m_svm_ctrl;    // ! 
  St_evr_privert *m_evr_privert; //!  
  St_evr_evrpar  *m_evr_evrpar;  //!
  St_egr_propagate *m_tp_param;  //!
  St_ev0_ev0par  *m_ev0par;      //!
  St_ev0_ev0par2 *m_ev0par2;     //!
  St_exi_exipar  *m_exipar;      //!
  St_exi_aux     *m_exiaux;      //!
  St_mft_control *m_magf;        //!
  St_egr_egrpar  *m_egr_egrpar;  //!
  St_egr_egrpar  *m_egr2_egrpar; //!
  St_particle_dst_param *m_particle_dst_param; //!
  St_est_ctrl    *m_est_ctrl; //!

  St_svg_shape   *m_svt_shape  ; //!
  St_svg_config  *m_svt_config ; //!
  St_svg_geom    *m_svt_geom   ; //!
  St_srs_activea *m_srs_activea; //!
  St_srs_srspar  *m_srspar     ; //!
  

  St_dst_summary_param *m_dst_summary_param; //!

 protected:

  
 public: 
  St_glb_Maker(const char *name="global");
  virtual       ~St_glb_Maker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   Set_scenario (Int_t m = 8){m_scenario  = m;} // *MENU*
  virtual void   Set_svtchicut(Int_t m = 0){m_svtchicut = m;} // *MENU*
  virtual void   Set_useglobal(Int_t m = 2){m_useglobal = m;} // *MENU*
  virtual void   Set_usesvt   (Int_t m = 1){m_usesvt    = m;} // *MENU*
  virtual void   Set_usetpc   (Int_t m = 1){m_usetpc    = m;} // *MENU*
  virtual void   Set_usevert  (Int_t m = 0){m_usevert   = m;} // *MENU*
  virtual void   Set_flag     (Int_t m = 0){m_flag = m;}      // *MENU*
  virtual void   ev0Eval(Bool_t flag=kFALSE){m_ev0EvalOn=flag;}
  virtual void   ev0EvalOn() {ev0Eval(kTRUE);}                       // *MENU*
  virtual void   ev0EvalOff(){ev0Eval();}                            // *MENU
  ClassDef(St_glb_Maker, 1)   //StAF chain virtual base class for Makers
    };

#endif
