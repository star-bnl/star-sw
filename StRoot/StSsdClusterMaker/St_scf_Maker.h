#ifndef STAR_St_scf_Maker
#define STAR_St_scf_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_scf_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
class TFile;
class TH1F;

class St_sdm_geom_par;
class St_sdm_calib_db;
class St_scf_ctrl;
class St_sls_ctrl;

class St_scf_Maker : public StMaker {
 private:
  St_sdm_geom_par      *m_geom_par;//!
  St_sdm_calib_db      *m_noise;//!
  St_scf_ctrl          *m_scf_ctrl;//!
  St_sls_ctrl          *m_sls_ctrl;//!
  void makeScfCtrlHistograms(); //!
  void writeScfCtrlHistograms(); //!

 protected:

  TFile *ScfCtrlFile; //!

  TH1F  *noisDisP;  //! p-side distribution of noise.
  TH1F  *snRatioP;  //! p-side distribution of signal to noise ratio.
  TH1F  *stpClusP;  //! p-side distribution of strips per cluster.
  TH1F  *totChrgP;  //! p-side distribution of cluster total charge.
  TH1F  *noisDisN;  //! n-side distribution of noise.
  TH1F  *snRatioN;  //! n-side distribution of signal to noise ratio.
  TH1F  *stpClusN;  //! n-side distribution of strips per cluster.
  TH1F  *totChrgN;  //! n-side distribution of cluster total charge.

 public: 
                  St_scf_Maker(const char *name="scf_cluster");
   virtual       ~St_scf_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();
   ClassDef(St_scf_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif
