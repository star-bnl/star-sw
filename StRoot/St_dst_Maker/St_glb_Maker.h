// $Id: St_glb_Maker.h,v 1.3 1998/10/06 18:00:34 perev Exp $
// $Log: St_glb_Maker.h,v $
// Revision 1.3  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.2  1998/09/08 22:43:11  fisyak
// Modify St_dst_Maker to account new calling sequence
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
#ifndef STAR_St_dst_Maker
#define STAR_St_dst_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_svm_ctrl;
class St_evr_privert;
class St_evr_evrpar;
class St_egr_egrpar;
class St_ev0_ev0par;
class St_mft_control;
class St_ev0_ev0par;
class St_mft_control; 
class St_dst_run_header;
class St_particle_dst_param;
class St_dst_Maker : public StMaker {
 private:
   Bool_t drawinit;
// static Char_t m_VersionCVS = "$Id: St_glb_Maker.h,v 1.3 1998/10/06 18:00:34 perev Exp $";
   // egr
   Int_t         m_scenario;   //#1: Real TPC Stand-Alone Tracking: Use this when running the TPC only.  
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
   Int_t         m_useglobal;  // = 1 - Perfect matching between tpc and svt (checks mc_ids)
                               // = 2 - Refit tracks
                               // = 3 - Copies info from svt tracks + 1/invpt from tptrack
   Int_t         m_usesvt;     // = 0 Dont' copy unmatched svt tracks
                               // = 1 Copy unmatched tracks with chisq>svtchicut, no refit 
                               // = 2 Copy unmatched tracks with chisq>svtchicut Refit
   Int_t         m_usetpc;     // = 0 Don't copy unmatched tracks
                               // = 1 Copy unmatched tracks, no refit
                               // = 2 Copy unmatched tracks, refit

   Int_t         m_usevert;    // 
   St_svm_ctrl    *m_svm_ctrl;    // ! 
   St_evr_privert *m_evr_privert; //!
   St_evr_evrpar  *m_evr_evrpar;  //!
   
   St_ev0_ev0par  *m_ev0par;      //!
   St_mft_control *m_magf;        //!
   St_egr_egrpar  *m_egr_egrpar;  //!
   St_dst_run_header *m_run_header;//!
   St_particle_dst_param *m_particle_dst_param; //!
 protected:
 public: 
                  St_dst_Maker();
                  St_dst_Maker(const char *name, const char *title);
   virtual       ~St_dst_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   virtual void   Set_scenario (Int_t m = 8){m_scenario  = m;} // *MENU*
   virtual void   Set_svtchicut(Int_t m = 0){m_svtchicut = m;} // *MENU*
   virtual void   Set_useglobal(Int_t m = 2){m_useglobal = m;} // *MENU*
   virtual void   Set_usesvt   (Int_t m = 1){m_usesvt    = m;} // *MENU*
   virtual void   Set_usetpc   (Int_t m = 1){m_usetpc    = m;} // *MENU*
   virtual void   Set_usevert  (Int_t m = 0){m_usevert   = m;} // *MENU*
   ClassDef(St_dst_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
