//$Id: StSstBarrel.hh,v 1.3 2016/06/08 20:53:01 bouchet Exp $
//
//$Log: StSstBarrel.hh,v $
//Revision 1.3  2016/06/08 20:53:01  bouchet
//coverity : PASS_BY_VALUE
//
//Revision 1.2  2015/06/27 19:48:51  bouchet
//removed obsolete libraries : ssdConfiguration, ssdDimensions, ssdWafersPosition ; fixed static StSstBarrel name
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.6  2015/06/19 14:16:24  bouchet
//readNoiseFromTable() takes sstStripCalib data as input, not the structure
//
//Revision 1.5  2015/05/23 20:52:08  bouchet
//ADC hits set properly in writePointToContainer()
//
//Revision 1.4  2015/04/21 18:26:08  bouchet
//typos between SSD and SST name fixed
//
//Revision 1.3  2015/04/20 20:09:20  bouchet
//added readNoiseFromTable() method using sstStripCalib table ; added includes for sstStripCalib and sstWafersPosition
//
//Revision 1.2  2015/04/20 19:01:25  bouchet
//removed some hard coded constants ; removed unused writePointToContainer() method
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTBARREL_HH
#define STSSTBARREL_HH

#include "StSstUtil/StSstWafer.hh"
#include "StSstUtil/StSstLadder.hh"

class TFile;
//20150406
class sstDimensions_st;
class sstConfiguration_st;
class sstWafersPosition_st;
class sstSlsCtrl_st;
class sstStripCalib_st;
class slsCtrl_st;
class St_sls_strip;
class St_spa_strip;
class St_ssdWafersPosition;
class St_sstWafersPosition;
class St_sstStripCalib;
class St_ssdStripCalib;
class St_ssdPedStrip;
class St_spa_strip;
class St_sdm_calib_db;
class St_sdm_condition_db;
class St_scf_cluster;
class St_scm_spt;
class St_ssdStripCalib;
class StSstClusterControl;
class StSstDynamicControl;
class StSstHitCollection;
class St_ssdGainCalibWafer;
class St_ssdNoise;
class St_ssdWaferConfiguration; 
//new
class St_sstGainCalibWafer;
class St_sstNoise;
class St_sstWaferConfiguration; 
class StMcEvent;
class StMcSsdHitCollection;
class StMcSsdHit;
#include <vector>

class StSstBarrel
{
 public:
  StSstBarrel(sstDimensions_st  *dimensions, sstConfiguration_st *config=0);
  ~StSstBarrel();

  StSstBarrel(const StSstBarrel & originalBarrel);
  StSstBarrel& operator=(const StSstBarrel & originalBarrel);

  void  initLadders(St_sstWafersPosition *wafpos);

  //   Int_t   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  void  addNoiseToStrip(slsCtrl_st* ctrl); //
  Int_t readStripFromTable(St_spa_strip *spa_strip);
  Int_t readStripFromTable(St_sls_strip *sls_strip); //
  Int_t readNoiseFromTable(St_sdm_calib_db  *spa_noise, StSstDynamicControl *dynamicControl);
  Int_t readNoiseFromTable(St_ssdStripCalib *strip_noise, StSstDynamicControl *dynamicControl);
  Int_t readNoiseFromTable(St_ssdStripCalib *noise); //
  Int_t readNoiseFromTable(St_ssdNoise *strip_noise, StSstDynamicControl *dynamicControl); 
  Int_t readNoiseFromTable(sstStripCalib_st *noise, StSstDynamicControl *dynamicControl); 
  Int_t readNoiseDefault(StSstDynamicControl *dynamicControl);
  Int_t readNoiseDefaultForSimu();
  Int_t readConditionDbFromTable(St_sdm_condition_db *condition);//
  Int_t writeNoiseToFile(St_spa_strip *spa_strip);
  Int_t writeNoiseToFile(St_ssdPedStrip *pedStrip, char myLabel[]);
  Int_t readClusterFromTable(St_scf_cluster *scf_cluster);
  Int_t writeClusterToTable(St_scf_cluster *cluster);
  Int_t writeClusterToTable(St_scf_cluster *scf_cluster,St_spa_strip *spa_strip);
  Int_t writePointToContainer(St_scm_spt *scm_spt, StSstHitCollection* sstHitColl,St_scf_cluster *scf_cluster,StSstDynamicControl *dynamicControl);
  Int_t writePointToContainer(St_scm_spt *scm_spt, StSstHitCollection* sstHitColl,St_scf_cluster *scf_cluster,StSstDynamicControl *dynamicControl,StMcEvent *mcEvent);    
  Int_t writeStripToTable(St_spa_strip * spa_strip); //
  Int_t writeStripToTable(St_spa_strip * spa_strip,St_sls_strip *sls_strip); //
  Int_t writeNewNoiseToFile3(St_ssdPedStrip *pedStrip, char myLabel[]);
  void  doSideClusterisation(Int_t *numberOfCluster);   
  void  doSideClusterisation(Int_t *numberOfCluster,Int_t WafStatus[20][16]);
  Int_t doClusterMatching(Float_t CalibArray[320]);
  void  doDaqSimulation(slsCtrl_st* ctrl); //
  void  convertDigitToAnalog(StSstDynamicControl *dynamicControl);
  void  convertGlobalFrameToOther();
  void  convertUFrameToOther();
  void  convertToStrip(Double_t pairCreationEnergy,
		       Int_t nstripInACluster,
		       Double_t parDiffP,
		       Double_t parDiffN,
		       Double_t parIndRightP,
		       Double_t parIndRightN,
		       Double_t parIndLeftP,
		       Double_t parIndLeftN
		       );
  void  sortListStrip();
  void  sortListCluster();
  Int_t getNumberOfLadders() { return mNLadder;}
  Int_t getNWaferPerLadder() { return mNWaferPerLadder;}
  Int_t getSstLayer() { return mSstLayer;};
  void  Calculation_Ratio(int idWafer,int idClusterP,int idClusterN,std::vector<const StMcSsdHit*> hitCol, int *ratio, int *idTruth);
  static Int_t FindMcHit(const std::vector<int> &id,const std::vector<const StMcSsdHit*> &hitCol);
  Int_t isSplit(StSstCluster *currentCluster,int iSide,int lad,int waf);
  StSstLadder         *getLadder(Int_t i=0) {return mLadders[i];}
  sstDimensions_st    *getDimensions() {return mDimensions;}
  StSstClusterControl *getClusterControl() {return mClusterControl;}
  Int_t isActiveLadder(Int_t i);
  void  debugUnPeu(Int_t monLadder, Int_t monwafer);
  void  setSstParameters(sstDimensions_st *geom_par);
  void  setLorentzShift(sstDimensions_st *geom_par);
  void  setClusterControl(StSstClusterControl *clusterControl) {mClusterControl = clusterControl;}
  void  initWafers(St_sstWafersPosition *geom_class) {initLadders(geom_class);}
  void  renumHitAfterRemove();
  
  Int_t idWaferToWaferNumb(Int_t idWafer); //  idwafer = layer*1000+waf*100+ladder => waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1
  Int_t idWaferToLadderNumb(Int_t idWafer);//  idwafer => ladder-1
  Int_t waferNumbToIdWafer(Int_t waferNumb);// waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1 => idwafer
  Int_t idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  StSstPointList *getInactiveHitList();
  void  Reset();
  void  SetDebug(Int_t k = 0) {mDebug = k;}
  Int_t Debug() {return mDebug;}
 private:
  Char_t   first[1];
 public:
  StSstLadder** mLadders;
  static StSstBarrel* Instance() {return fSstBarrel;}
 private:
  Int_t    mSstLayer;
  Int_t    mNLadder;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Int_t    mActiveLadders[20];
  Float_t  mDetectorLargeEdge;
  Float_t  mDetectorSmallEdge;
  Float_t  mStripPitch;
  Float_t  mTheta;
  Float_t  mShift_hole;
  Float_t  mShift_elec;
  sstDimensions_st    *mDimensions;
  StSstClusterControl *mClusterControl;
  Int_t    mDebug;
  Char_t   last[1];
  static   StSstBarrel* fSstBarrel;
};
#endif
