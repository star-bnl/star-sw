// $Id: St_scf_Maker.h,v 1.13 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: St_scf_Maker.h,v $
// Revision 1.13  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.12  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.11  2005/06/14 12:20:25  bouchet
// cleaner version
//
// Revision 1.10  2005/06/13 16:01:01  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.9  2005/05/17 14:16:41  lmartin
// CVS tags added
//
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
class TNtuple;

class St_ssdDimensions;
class St_sdm_calib_db;
class StSsdClusterControl;
class St_scf_ctrl;
class St_slsCtrl;

class StScfBarrel;
class StScfCluster;
class StScfWafer;
class StScfListCluster;

class St_scf_cluster;
class St_ssdStripCalib;

class St_scf_Maker : public StMaker {
 private:
  St_ssdDimensions   *m_geom_par;//!
  St_ssdStripCalib     *m_noise;//
  St_scf_ctrl        *m_scf_ctrl;//!
  St_slsCtrl         *m_slsCtrl;//!
  //St_sdm_calib_db      *m_noise;//!
  float ClusterNtuple[12];
  TFile *qFile;
  TNtuple* qHitNtuple;
 
  void makeScfCtrlHistograms(); //!
  void makeNewScfCtrlHistograms(StScfBarrel *barrel); //!
  void PrintClusterDetails(StScfBarrel *barrel,int id_wafer);//!
  void PrintIdMctrack(St_scf_cluster *scf_cluster,int mywafer);
  void DeclareNtuple(); //!
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
   virtual Int_t  InitRun(Int_t runNumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: St_scf_Maker.h,v 1.13 2006/09/15 21:04:50 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_scf_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif

 /**************************************************************************
 * 
 *  $Log: St_scf_Maker.h,v $
 *  Revision 1.13  2006/09/15 21:04:50  bouchet
 *  noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
 *
 *  Revision 1.12  2005/11/22 03:57:05  bouchet
 *  id_mctrack is using for setIdTruth
 *
 *  Revision 1.11  2005/06/14 12:20:25  bouchet
 *  cleaner version
 *
 *  Revision 1.10  2005/06/13 16:01:01  reinnart
 *  Jonathan and Joerg changed the update function
 *
 *  Revision 1.9  2005/05/17 14:16:41  lmartin
 *  CVS tags added
 *
 *  Revision 1.8  2005/05/13 15:16:54  bouchet
 *  reading ssd/geom and no more writeScfCtrlHistograms and writeScmCtrlHistograms methods
 *
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
