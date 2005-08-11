// $Id: StSsdPointMaker.h,v 1.14 2005/08/11 13:51:39 lmartin Exp $
//
// $Log: StSsdPointMaker.h,v $
// Revision 1.14  2005/08/11 13:51:39  lmartin
// PrintStripDetails, PrintPackageDetails and PrintPointDetails methods added
//
// Revision 1.13  2005/06/16 14:29:22  bouchet
// no more makeSsdPedestalHistograms() method
//
// Revision 1.12  2005/06/14 12:09:16  bouchet
// add a histo for the pedestal and new name of the class : SsdPoint
//
// Revision 1.11  2005/06/08 15:50:05  bouchet
// add methods to fill the Tuple
//
// Revision 1.9  2005/06/07 12:04:46  reinnart
// Make Stuff moved to Initrun
//
// Revision 1.8  2005/06/07 11:55:09  reinnart
// Initrun and good database connection
//
// Revision 1.7  2005/04/25 14:13:24  bouchet
// new method makeScfCtrlHistograms and makeScmCtrlHistograms and Clusternoise is coded as a float
//
// Revision 1.6  2005/03/23 16:07:26  lmartin
// PrintClusterSummary and PrintPointSummary methods added
//
// Revision 1.5  2005/03/22 13:48:57  lmartin
// PrintStripSummary method added
//
// Revision 1.4  2005/03/18 13:39:21  lmartin
// Missing cvs header added
//
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
#include "StDbLib/StDbDefs.hh"

class StChain;
class StDbManager;
class StDbConfigNode;
class TFile;
class TH1F;
class TH1S;
class TH2S;
class TNtuple;

class St_sdm_calib_db;
class St_sdm_condition_db;
class St_ssdDimensions;
class St_ssdConfiguration;
class St_ssdWafersPosition;
class St_ssdLaddersPosition;
class St_ssdSectorsPosition;
class St_ssdBarrelPosition;
class St_ssdStripCalib;

class StEvent;
class StSsdHitCollection;
class StSsdDynamicControl;
class StSsdClusterControl;
class StSsdBarrel;

class StSsdLadder;
class StSsdWafer;
class StSsdStrip;
class StSsdStripList;
class StSsdPoint;
class StSsdPointList;
class StSsdCluster;
class StSsdClusterList;
class StSsdPackage;
class StSsdPackageList;
class ssdWafersPosition_st;
class ssdDimensions_st;
class ssdConfiguration_st;
class StRunInfo;
class StEventInfo;

class StSsdPointMaker : public StMaker {
 private:
  StDbManager* mDbMgr;           //!
  StDbConfigNode* maccess;      //!
  St_DataSet* DbConnector;
  ssdWafersPosition_st  *position;
  ssdDimensions_st      *dimensions;
  ssdConfiguration_st   *config;
  int positionSize;
  St_sdm_calib_db       *m_noise;         //!< Pointer to the calib_db table (noise values) 
  St_ssdStripCalib      *m_noise2;        //!< Pointer to the ssdStripCalib table (noise values) 
  St_sdm_condition_db   *m_condition_db;  //!< Pointer to the condition_db table (active/inactive strip)
  St_ssdDimensions      *m_dimensions;    //!< Pointer to the ssdDimensions table (wafer size)
  St_ssdConfiguration   *m_configuration; //!< Pointer to the ssdConfiguration table (ladder on/off)
  St_ssdWafersPosition  *m_wafpos;        //!< Pointer to the ssdWaferPosition table (wafer positions)
  //  St_ssdWafersPosition  *position;        //!< Pointer to the ssdWaferPosition table (wafer positions)
  St_ssdLaddersPosition *m_ladpos;        //!< Pointer to the ssdLadderPosition table (ladder positions)
  St_ssdSectorsPosition *m_secpos;        //!< Pointer to the ssdSectorPosition table (sector positions)
  St_ssdBarrelPosition  *m_barpos;        //!< Pointer to the ssdBarrelPosition table (barrel positions)
  float ClusterNtuple[10];
  float hitNtuple[9];    
  TFile *nFile;
  TNtuple* nHitNtuple;
  TFile *mFile;
  TNtuple* mHitNtuple;

  void makeScfCtrlHistograms(StSsdBarrel *mySsd);        //!
  void makeScmCtrlHistograms(StSsdBarrel *mySsd);        //!
  void DeclareNtuple(int *flag);
  void debugUnPeu(StSsdBarrel *mySsd); 
  void PrintStripSummary(StSsdBarrel *mySsd); //!
  void PrintClusterSummary(StSsdBarrel *mySsd); //!
  void PrintPointSummary(StSsdBarrel *mySsd); //!
  void WriteScfTuple(StSsdBarrel *mySsd);
  void WriteScmTuple(StSsdBarrel *mySsd);
  void PrintStripDetails(StSsdBarrel *mySsd, int mywafer); //!
  void PrintClusterDetails(StSsdBarrel *mySsd, int mywafer); //!
  void PrintPointDetails(StSsdBarrel *mySsd, int mywafer); //!
  void PrintPackageDetails(StSsdBarrel *mySsd, int mywafer); //!
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
  TH1S  *kind;         //! kind of hits -->see StSsdWafer for definition
  TH2S  *matchisto_1;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_2;    //! (1p-1n) packages control matching.
  TH2S  *matchisto_3;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_4;    //! (1p-1n) packages control matching
  TH2S  *matchisto_5;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_6;    //! (1p-1n) packages control matching.
  TH2S  *matchisto_7;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_8;    //! (1p-1n) packages control matching
  TH2S  *matchisto_9;    //! (1p-1n) packages control matching
  TH2S  *matchisto_10;    //! (1p-1n) packages control matching
  TH2S  *matchisto_11;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_12;    //! (1p-1n) packages control matching.
  TH2S  *matchisto_13;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_14;    //! (1p-1n) packages control matching
  TH2S  *matchisto_15;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_16;    //! (1p-1n) packages control matching.
  TH2S  *matchisto_17;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_18;    //! (1p-1n) packages control matching
  TH2S  *matchisto_19;    //! (1p-1n) packages control matching  
  TH2S  *matchisto_20;    //! (1p-1n) packages control matching.
  int flag ;
  
 public:
                  StSsdPointMaker(const char *name="SsdPoint");
   virtual       ~StSsdPointMaker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(int runumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();

   virtual const char *GetCVS() const 
     {static const char cvs[]="Tag $Name:  $ $Id: StSsdPointMaker.h,v 1.14 2005/08/11 13:51:39 lmartin Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StSsdPointMaker, 1)   //StAF chain virtual base class for Makers
};
#endif
