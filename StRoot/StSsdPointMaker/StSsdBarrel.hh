#ifndef STSSDBARREL_HH
#define STSSDBARREL_HH

class St_svg_geom;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;
class StSsdClusterControl;
class StSsdDynamicControl;
class sdm_geom_par_st;
class StSsdLadder;

class StSsdHitCollection;

class StSsdBarrel
{
 public:
  StSsdBarrel(sdm_geom_par_st  *geom_par);
  ~StSsdBarrel();

  StSsdBarrel(const StSsdBarrel & originalBarrel);
  StSsdBarrel& operator=(const StSsdBarrel  originalBarrel);

  void  initLadders(St_svg_geom *geom_class);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, StSsdDynamicControl *dynamicControl);
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  int   writeClusterToTable(St_scf_cluster *cluster);
  int   writePointToContainer(St_scm_spt *scm_spt,StSsdHitCollection *ssdHitColl);   
  void  doSideClusterisation(int *numberOfCluster,StSsdClusterControl *clusterControl);   
  int   doClusterMatching(sdm_geom_par_st *geom_par,StSsdClusterControl *clusterControl);
  void  convertDigitToAnalog(StSsdDynamicControl *dynamicControl);
  void  convertUFrameToOther(sdm_geom_par_st *geom_par);
  void  sortListStrip();
  void  sortListCluster();

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
#endif
