/*!
 * \class St_ssd_Maker
 * \author B.Hippolyte, W.Pinganaud, C.Suire.   
 * \date 2000-2002
 *
 *  Cluster finder and matching for the Silicon Strip Detectors
 * 
 *  This maker controls now both the cluster reconstruction
 *  and the space-point reconstruction. The St_scf_Maker and
 *  the St_scm_Maker have been merged as well as the classes
 *  inside. However the different steps are still the same and
 *  described below.
 *
 *  This maker controls the cluster reconstruction in the SSD :
 *  fired strips are read from a table and associated with
 *  neighbouring ones to form clusters. Cluster splitting is also
 *  done at this stage if a local minimum is found inside the 
 *  cluster.
 *  The cluster finding is performs independantly on each side
 *  of a single detector. 
 *
 * See documentation at http://star.in2p3.fr/STAR_informatique/hit_reconstruction.html#scf
 *
 *  This maker controls  the space-point reconstruction in the SSD :
 *  clusters from each side of a single silicon strip detector
 *  are associated in different packages types. Solving the
 *  cluster package give one or several solutions for the hits   
 *  positions in the silicon strip detector.
 * 
 * See documentation at http://star.in2p3.fr/STAR_informatique/hit_reconstruction.html#scm
 */
#ifndef STAR_St_ssd_Maker
#define STAR_St_ssd_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TFile;
class TH1F;
class TH1S;
class TH2S;

class St_sdm_geom_par;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_svg_geom;
class StSsdClusterControl;
class St_scf_ctrl;
class St_sls_ctrl;
class St_scm_ctrl;

class St_ssd_Maker : public StMaker {
 private:
  St_sdm_geom_par      *m_geom_par;//!
  St_sdm_calib_db      *m_noise;//!
  St_sdm_condition_db  *m_condition_db;//!
  St_svg_geom          *m_geom;//!
  St_scf_ctrl          *m_scf_ctrl;//!
  St_sls_ctrl          *m_sls_ctrl;//!
  St_scm_ctrl          *m_scm_ctrl;//!
  void makeScfCtrlHistograms(); //!
  void writeScfCtrlHistograms(); //!
  void makeScmCtrlHistograms(); //!
  void writeScmCtrlHistograms(); //!

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

  TFile *ScmCtrlFile; //!
  TH2S *matchisto;    //! (1p-1n) packages control matching.
  TH1S *orthoproj;    //! orthonormal projection and perfect matching deviation.

 public: 
                  St_ssd_Maker(const char *name="ssd_maker");
   virtual       ~St_ssd_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: St_ssd_Maker.h,v 1.2 2004/01/26 23:04:18 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_ssd_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * 
 *  $Log: St_ssd_Maker.h,v $
 *  Revision 1.2  2004/01/26 23:04:18  perev
 *  WarnOff
 *
 *  Revision 1.1  2003/10/08 03:18:09  suire
 *  *** empty log message ***
 *
 *  Revision 1.3  2002/03/25 20:13:05  suire
 *  Small memory leak fixes, doxygen documentation
 *
 *
 **************************************************************************/
