#ifndef STSSDBARREL_HH
#define STSSDBARREL_HH

class TFile;
class ssdDimensions_st;
class ssdConfiguration_st;
class St_ssdWafersPosition;
class St_ssdStripCalib;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;
class StSsdClusterControl;
class StSsdDynamicControl;
class StSsdLadder;

class StSsdHitCollection;

class StSsdBarrel
{
 public:
  StSsdBarrel(ssdDimensions_st  *dimensions, ssdConfiguration_st *configuration);
  ~StSsdBarrel();

  StSsdBarrel(const StSsdBarrel & originalBarrel);
  StSsdBarrel& operator=(const StSsdBarrel  originalBarrel);

  void  initLadders(St_ssdWafersPosition *wafpos);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, StSsdDynamicControl *dynamicControl);
  int   readNoiseFromTable(St_ssdStripCalib *strip_noise, StSsdDynamicControl *dynamicControl);
  int   writeNoiseToFile(St_spa_strip *spa_strip);
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  int   writeClusterToTable(St_scf_cluster *cluster);
  int   writePointToContainer(St_scm_spt *scm_spt,StSsdHitCollection *ssdHitColl);   
  void  doSideClusterisation(int *numberOfCluster,StSsdClusterControl *clusterControl);   
  int   doClusterMatching(ssdDimensions_st *dimensions,StSsdClusterControl *clusterControl);
  void  convertDigitToAnalog(StSsdDynamicControl *dynamicControl);
  void  convertUFrameToOther(ssdDimensions_st *dimensions);
  void  sortListStrip();
  void  sortListCluster();
  int   getNumberOfLadders();
  void  debugUnPeu(int monLadder, int monwafer);

  StSsdLadder** mLadders;
//   StSsdWafer** mWafers;
//   int** mDeadStripP;
//   int** mDeadStripN;
  
 private:
  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
};
inline int StSsdBarrel::getNumberOfLadders() { return mNLadder;}
#endif
