#ifndef STAR_StMatchMaker
#define STAR_StMatchMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMatchMaker virtual base class for Maker                            //
//                                                                      //
// $Id: StMatchMaker.h,v 1.12 2003/09/10 19:47:45 perev Exp $
// $Log: StMatchMaker.h,v $
// Revision 1.12  2003/09/10 19:47:45  perev
// ansi corrs
//
// Revision 1.11  2001/02/02 16:09:23  caines
//  Remove usage of svm
//
// Revision 1.10  2001/01/29 21:01:14  caines
// Remove est calls from StMatchMaker no longer used
//
// Revision 1.9  2000/11/01 00:53:01  lbarnby
// Move field dependent set up from Init into InitRun
//
// Revision 1.8  2000/03/01 14:48:09  caines
// Removed references to scs_cluster
//
// Revision 1.7  2000/02/25 02:38:27  caines
// Stuff to fill bit map, cov correctly
//
// Revision 1.6  1999/10/29 23:23:26  caines
// Removed scenario methods
//
// Revision 1.5  1999/07/15 13:57:53  perev
// cleanup
//
// Revision 1.4  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif


class St_egr_egrpar;

class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;

class StMatchMaker : public StMaker {
  
 private:
  Bool_t drawinit;
  // static Char_t m_VersionCVS = "$Id: StMatchMaker.h,v 1.12 2003/09/10 19:47:45 perev Exp $";
  // egr

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

  St_egr_egrpar  *m_egr_egrpar;  //!
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
  virtual Int_t  InitRun(int);
  virtual void   Set_svtchicut(Int_t m = 0){m_svtchicut = m;} // *MENU*
  virtual void   Set_useglobal(Int_t m = 2){m_useglobal = m;} // *MENU*
  virtual void   Set_usesvt   (Int_t m = 1){m_usesvt    = m;} // *MENU*
  virtual void   Set_usetpc   (Int_t m = 1){m_usetpc    = m;} // *MENU*
  virtual void   Set_usevert  (Int_t m = 0){m_usevert   = m;} // *MENU*
  virtual void   Set_flag     (Int_t m = 0){m_flag = m;}      // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StMatchMaker.h,v 1.12 2003/09/10 19:47:45 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StMatchMaker,0)   //StAF chain virtual base class for Makers
};
    
#endif
    
