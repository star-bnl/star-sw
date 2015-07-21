//$Id: StSstPointMaker.h,v 1.2 2015/07/20 19:56:37 bouchet Exp $
//
//$Log: StSstPointMaker.h,v $
//Revision 1.2  2015/07/20 19:56:37  bouchet
//inserted blanks to make C++11 happy
//
//Revision 1.1  2015/06/23 16:29:04  jeromel
//Version of the SSD code for the SST - strated revision 1
//
//Revision 1.6  2015/06/18 22:29:29  bouchet
//CPP-CHECK : C-style coding ; cleanup : removed unused libraries and variables ; init ctor ; replace ROOT types by C++ types
//
//Revision 1.5  2015/04/28 15:59:56  bouchet
//remove old methods, update ctor, update methods to fill gain calib and wafer status, cleanup
//
//Revision 1.4  2015/04/27 14:07:38  bouchet
//remove mode member from sstStripCalib (was first version) ; cleanup
//
//Revision 1.3  2015/04/26 17:56:36  bouchet
//ChipGain calibration methods removed ; cleanup
//
//Revision 1.2  2015/04/21 22:05:48  bouchet
//prototype methods to use sstGainCalibChip, remove unused arrays, typos ssd -> sst, print the # of events processed by the maker
//
//Revision 1.1  2015/04/19 18:52:10  bouchet
//initial commit ; SST classes only
//
//fork from the SSD code, move along - see history therein
/*
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
#ifndef STAR_StSstPointMaker
#define STAR_StSstPointMaker
#include "Riostream.h"
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StSstUtil/StSstDynamicControl.h"
#include "StSstUtil/StSstClusterControl.h"

class St_sstNoise;
class StSstHitCollection;
class StSstBarrel;
class StSstLadder;
class StSstWafer;
class StSstStrip;
class StSstStripList;
class StSstPoint;
class StSstPointList;
class StSstCluster;
class StSstClusterList;
class StSstPackage;
class StSstPackageList;
class sstSlsCtrl_st;
class sstClusterControl_st;
class sstStripCalib_st;
class sstGainCalibWafer_st;
class sstWaferConfiguration_st;
class StSstPointMaker : public StMaker {
 public:
  StSstPointMaker(const char *name="SstPoint"); 
  virtual       ~StSstPointMaker() {}
  virtual Int_t  Init();
  virtual Int_t  InitRun(int runumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   PrintInfo();
 private:
  void debugUnPeu(StSstBarrel *mySst); 
  void PrintStripSummary(StSstBarrel *mySst); //!
  void PrintClusterSummary(StSstBarrel *mySst); //!
  void PrintPointSummary(StSstBarrel *mySst); //!
  void PrintStripDetails(StSstBarrel *mySst, int mywafer); //!
  void PrintClusterDetails(StSstBarrel *mySst, int mywafer); //!
  void PrintPointDetails(StSstBarrel *mySst, int mywafer); //!
  void PrintPackageDetails(StSstBarrel *mySst, int mywafer); //!
  void FillCalibTable();
  void FillWaferTable();
  void FillDefaultCalibTable();
  void FillDefaultWaferTable();
  void FillDefaultChipNoiseTable();
  int  ReadNoiseTable(StSstBarrel *mySst);
 private:
  sstGainCalibWafer_st     *mGain;        //!< Pointer to the ssdGainCalib table (calibration gain)) 
  sstWaferConfiguration_st *mWafConfig;   //!< Pointer to the ssdWaferConfiguration table (wafer status))
  sstStripCalib_st         *mPedRmsData;
  sstSlsCtrl_st            *mCtrl;//!
  sstClusterControl_st     *mClusterCtrl;//!
  StSstDynamicControl      *mDynamicControl; //!
  StSstClusterControl      *mClusterControl; //!
  float                    mCalibArray[320];
  int                      mWaferStatus[20][16];
  int                      mEventCounter; 
  virtual const char *GetCVS() const 
  {static const char cvs[]="Tag $Name:  $ $Id: StSstPointMaker.h,v 1.2 2015/07/20 19:56:37 bouchet Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StSstPointMaker, 1)   //StAF chain virtual base class for Makers
    };
#endif
