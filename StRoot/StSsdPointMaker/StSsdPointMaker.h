/*!
 * \class StSsdPointMaker
 * \author B.Hippolyte, W.Pinganaud, C.Suire.   
 * \date 2000-2004
 *
 *  Cluster finder and matching for the Silicon Strip Detectors
 * 
 *  This maker controls now both the cluster reconstruction
 *  and the space-point reconstruction. The St_scf_Maker and
 *  the St_scm_Maker have been merged as well as the classes
 *  inside. However the different steps are still the same and
 *  described below.
 *
 *  1) the cluster reconstruction in the SSD :
 *  - fired strips are read from a table and associated with
 *  neighbouring ones to form clusters.
 *  - cluster splitting is also done at this stage if a local
 *  minimum is found inside the cluster.
 *  - cluster finding is performs independantly on each side
 *  of a single detector. 
 *  - see documentation at http://star.in2p3.fr/STAR_informatique/hit_reconstruction.html#scf
 *
 *  2) the space-point reconstruction in the SSD :
 *  - clusters from each side of a single silicon strip detector
 *  are associated in different packages types.
 *  - solving the cluster package give one or several solutions
 *  for the hits positions in the silicon strip detector.
 *  - see documentation at http://star.in2p3.fr/STAR_informatique/hit_reconstruction.html#scm
 */
#ifndef STAR_StSsdPointMaker
#define STAR_StSsdPointMaker

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
class StEvent;
class StSsdHitCollection;
class StSsdDynamicControl;
class StSsdClusterControl;

class StSsdPointMaker : public StMaker {
 private:
  St_sdm_geom_par      *m_geom_par;    //!
  St_sdm_calib_db      *m_noise;       //!
  St_sdm_condition_db  *m_condition_db;//!
  St_svg_geom          *m_geom;        //!
  void makeScfCtrlHistograms();        //!
  void writeScfCtrlHistograms();       //!
  void makeScmCtrlHistograms();        //!
  void writeScmCtrlHistograms();       //!

 protected:

  StEvent                *mCurrentEvent;   //!
  StSsdHitCollection     *mSsdHitColl;     //!
  StSsdDynamicControl    *mDynamicControl; //!
  StSsdClusterControl    *mClusterControl; //!
  
  TFile *ScfCtrlFile;  //!

  TH1F  *noisDisP;     //! p-side distribution of noise.
  TH1F  *snRatioP;     //! p-side distribution of signal to noise ratio.
  TH1F  *stpClusP;     //! p-side distribution of strips per cluster.
  TH1F  *totChrgP;     //! p-side distribution of cluster total charge.
  TH1F  *noisDisN;     //! n-side distribution of noise.
  TH1F  *snRatioN;     //! n-side distribution of signal to noise ratio.
  TH1F  *stpClusN;     //! n-side distribution of strips per cluster.
  TH1F  *totChrgN;     //! n-side distribution of cluster total charge.

  TFile *ScmCtrlFile;  //!
  TH2S  *matchisto;    //! (1p-1n) packages control matching.
  TH1S  *orthoproj;    //! orthonormal projection and perfect matching deviation.

 public: 
                  StSsdPointMaker(const char *name="scm_spt");
   virtual       ~StSsdPointMaker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const 
     {static const char cvs[]="Tag $Name:  $ $Id: StSsdPointMaker.h,v 1.1 2004/03/12 06:12:37 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StSsdPointMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
