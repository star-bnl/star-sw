#ifndef STAR_St_scm_Maker
#define STAR_St_scm_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_scm_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class TFile;
class TH1S;
class TH2S;

class St_sdm_geom_par;
class St_sdm_condition_db;
class St_svg_geom;
class St_sls_ctrl;
class St_scm_ctrl;

class St_scm_Maker : public StMaker {
 private:
  St_sdm_geom_par      *m_geom_par;//!
  St_sdm_condition_db  *m_condition_db;//!
  St_svg_geom          *m_geom;//!
  St_sls_ctrl          *m_sls_ctrl;//!
  St_scm_ctrl          *m_scm_ctrl;//!
  void makeScmCtrlHistograms(); //!
  void writeScmCtrlHistograms(); //!

 protected:

  TFile *ScmCtrlFile; //!
  TH2S *matchisto;  //! (1p-1n) packages control matching.
  TH1S *orthoproj;  //! orthonormal projection and perfect matching deviation.

 public: 
                  St_scm_Maker(const char *name="scm_spt");
   virtual       ~St_scm_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();
   ClassDef(St_scm_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif







