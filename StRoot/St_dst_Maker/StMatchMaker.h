#ifndef STAR_StMatchMaker
#define STAR_StMatchMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMatchMaker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_svm_ctrl;
class St_est_ctrl;
class St_egr_egrpar;

class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;

class StMatchMaker : public StMaker {

 private:
  Bool_t drawinit;
  // static Char_t m_VersionCVS = "$Id: StMatchMaker.h,v 1.2 1999/07/01 17:30:45 fisyak Exp $";
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
  St_egr_egrpar  *m_egr_egrpar;  //!
  St_est_ctrl    *m_est_ctrl; //!
  St_svg_shape   *m_svt_shape  ; //!
  St_svg_config  *m_svt_config ; //!
  St_svg_geom    *m_svt_geom   ; //!
  St_srs_activea *m_srs_activea; //!
  St_srs_srspar  *m_srspar     ; //!
  
 protected:

  
 public: 
  StMatchMaker(const char *name="match");
  virtual       ~StMatchMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual void   Set_scenario (Int_t m = 8){m_scenario  = m;} // *MENU*
  virtual void   Set_svtchicut(Int_t m = 0){m_svtchicut = m;} // *MENU*
  virtual void   Set_useglobal(Int_t m = 2){m_useglobal = m;} // *MENU*
  virtual void   Set_usesvt   (Int_t m = 1){m_usesvt    = m;} // *MENU*
  virtual void   Set_usetpc   (Int_t m = 1){m_usetpc    = m;} // *MENU*
  virtual void   Set_usevert  (Int_t m = 0){m_usevert   = m;} // *MENU*
  virtual void   Set_flag     (Int_t m = 0){m_flag = m;}      // *MENU*
  ClassDef(StMatchMaker, 1)   //StAF chain virtual base class for Makers
    };

#endif
