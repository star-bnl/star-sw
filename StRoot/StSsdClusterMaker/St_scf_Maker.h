/*!
 * \class St_scf_Maker
 * \author B.Hippolyte, W.Pinganaud   
 * \date 2000
 *
 *  Cluster finder  for the Silicon Strip Detectors
 * 
 *  This maker controls the cluster reconstruction in the SSD :
 *  fired strips are read from a table and associated with
 *  neighbouring ones to form clusters. Cluster splitting is also
 *  done at this stage if a local minimum is found inside the 
 *  cluster.
 *  The cluster finding is performed independantly on each side
 *  of a Ssd detector. 
 *
 * See documentation at http://star.in2p3.fr/STAR_informatique/hit_reconstruction.html#scf
 */
#ifndef STAR_St_scf_Maker
#define STAR_St_scf_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TFile;
class TH1F;

class St_sdm_geom_par;
class St_sdm_calib_db;
class StSsdClusterControl;
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

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: St_scf_Maker.h,v 1.7 2004/01/26 23:03:14 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_scf_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * 
 *  $Log: St_scf_Maker.h,v $
 *  Revision 1.7  2004/01/26 23:03:14  perev
 *  WarnOff
 *
 *  Revision 1.6  2003/10/08 03:18:09  suire
 *  *** empty log message ***
 *
 *  Revision 1.3  2002/03/25 20:13:05  suire
 *  Small memory leak fixes, doxygen documentation
 *
 *
 **************************************************************************/
