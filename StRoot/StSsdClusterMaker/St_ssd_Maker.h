// $Id: St_ssd_Maker.h,v 1.5 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: St_ssd_Maker.h,v $
// Revision 1.5  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.4  2005/05/17 14:16:42  lmartin
// CVS tags added
//
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

class St_ssdDimensions;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_ssdWafersPosition;
class StSsdClusterControl;
class St_scf_ctrl;
class St_slsCtrl;
class St_scm_ctrl;

class St_ssd_Maker : public StMaker {
 private:
  St_ssdDimensions      *m_geom_par;//!
  St_sdm_calib_db      *m_noise;//!
  St_sdm_condition_db  *m_condition_db;//!
  St_ssdWafersPosition          *m_geom;//!
  St_scf_ctrl          *m_scf_ctrl;//!
  St_slsCtrl          *m_slsCtrl;//!
  St_scm_ctrl          *m_scm_ctrl;//!
  void makeScfCtrlHistograms(); //!
  void makeScmCtrlHistograms(); //!
   
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
     {static const char cvs[]="Tag $Name:  $ $Id: St_ssd_Maker.h,v 1.5 2006/09/15 21:04:50 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_ssd_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * 
 *  $Log: St_ssd_Maker.h,v $
 *  Revision 1.5  2006/09/15 21:04:50  bouchet
 *  noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
 *
 *  Revision 1.4  2005/05/17 14:16:42  lmartin
 *  CVS tags added
 *
 *  Revision 1.3  2005/05/13 15:16:54  bouchet
 *  reading ssd/geom and no more writeScfCtrlHistograms and writeScmCtrlHistograms methods
 *
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
