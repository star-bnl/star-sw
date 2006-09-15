// $Id: StScmBarrel.hh,v 1.6 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScmBarrel.hh,v $
// Revision 1.6  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.5  2005/12/19 10:52:13  kisiel
// Properly encode Cluster Size and Mean strip into the hardware information for the SSDHit
//
// Revision 1.4  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.3  2005/05/17 14:57:28  lmartin
// saving SSD hits into StEvent
//
// Revision 1.2  2005/05/17 14:16:35  lmartin
// CVS tags added
//
#ifndef STSCMBARREL_HH
#define STSCMBARREL_HH
#include "StScmWafer.hh"
#include "tables/St_slsCtrl_Table.h"


class St_ssdWafersPosition;
class St_scf_cluster;
class St_scm_spt;
class StSsdHitCollection;

class StScmBarrel
{
 public:
  StScmBarrel(ssdDimensions_st  *geom_par);
  ~StScmBarrel();

  void  setSsdParameters(ssdDimensions_st  *geom_par);
  void  initWafers(St_ssdWafersPosition *geom_class);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  void  sortListCluster();
  int   doClusterMatching(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  void  convertDigitToAnalog(slsCtrl_st *slsCtrl);
  void  convertUFrameToOther(ssdDimensions_st *geom_par);
  int   writePointToTable(St_scm_spt *scm_spt);
  int   writePointToContainer(St_scm_spt *scm_spt,StSsdHitCollection *ssdHitColl , St_scf_cluster *scf_cluster );  
  // OBSOLETE ?
  //  int   writePointToContainer(St_scm_spt *scm_spt,StSsdHitCollection *ssdHitColl);   

  StScmWafer** mWafers;
//   int** mDeadStripP;
//   int** mDeadStripN;
  
 private:
//   StScmBarrel(const StScmBarrel & originalBarrel);
//   StScmBarrel& operator=(const StScmBarrel  originalBarrel);

  int   mSsdLayer;
  int   mNLadder;
  int   mNWaferPerLadder;
  int   mNStripPerSide;

  int   idWaferToWaferNumb(int idWafer);
  int   waferNumbToIdWafer(int waferNumb);
};
#endif
