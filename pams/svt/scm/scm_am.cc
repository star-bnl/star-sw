/*:>--------------------------------------------------------------------
**: FILE:       scm_am.cc
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.17 1999/06/19 19:21:00 fisyak Exp  
**:<------------------------------------------------------------------*/
#include "scm_am.h"
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StScmBarrel.hh"

long type_of_call scm_am_(
  TABLE_HEAD_ST   *geom_par_h,      SDM_GEOM_PAR_ST         *geom_par ,
  TABLE_HEAD_ST   *condition_db_h,  SDM_CONDITION_DB_ST     *condition_db ,
  TABLE_HEAD_ST   *geom_h,          SVG_GEOM_ST             *geom ,
  TABLE_HEAD_ST   *cluster_h,       SCF_CLUSTER_ST          *cluster ,
  TABLE_HEAD_ST   *sls_ctrl_h,      SLS_CTRL_ST             *sls_ctrl ,
  TABLE_HEAD_ST   *scm_ctrl_h,      SCM_CTRL_ST             *scm_ctrl ,
  TABLE_HEAD_ST   *spt_h,           SCM_SPT_ST              *spt
                         ) 
{
  cout<<"#################################################"<<endl;
  cout<<"####      START OF SSD CLUSTER MATCHING      ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StScmBarrel *mySsd = new StScmBarrel(geom_par);
  cout<<"####        SSD WAFERS INITIALIZATION        ####"<<endl;
  mySsd->initWafers(geom_h,geom);
//   int deadStripTableSize = mySsd->readDeadStripFromTable(condition_db_h, condition_db);
//   cout<<"####   -> "<<deadStripTableSize<<" DEAD STRIPS IN THE SSD ####"<<endl;
   int nReadCluster = mySsd->readClusterFromTable(cluster_h, cluster);
   cout<<"####   -> "<<nReadCluster<<" CLUSTERS READ FROM TABLE      ####"<<endl;
   mySsd->sortListCluster();
   int nPackage = mySsd->doClusterMatching(geom_par, scm_ctrl);
   cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
   mySsd->convertDigitToAnalog(sls_ctrl);
   mySsd->convertUFrameToOther(geom_par);
   int nSptWritten = mySsd->writePointToTable(spt_h, spt);
   cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
  cout<<"####       END OF SSD CLUSTER MATCHING       ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  return STAFCV_OK;
}
