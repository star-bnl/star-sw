/*:>--------------------------------------------------------------------
**: FILE:       sce_am.cc
**: HISTORY:
**:             21fev00-v000a-hb- Created by hand
**:  Id: idl.y,v 1.16 1998/12/16 18:49:48 ward Exp  
**:<------------------------------------------------------------------*/
# include "sce_am.h"
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StSceBarrel.hh"
 
long type_of_call sce_am_(
    TABLE_HEAD_ST  *svt_hit_h,     G2T_SVT_HIT_ST    *svt_hit ,
    TABLE_HEAD_ST  *sls_spt_h,     SLS_SPT_ST        *sls_spt ,
    TABLE_HEAD_ST  *scf_cluster_h, SCF_CLUSTER_ST    *scf_cluster ,
    TABLE_HEAD_ST  *scm_spt_h,     SCM_SPT_ST        *scm_spt  ,
    TABLE_HEAD_ST  *geom_h,        SVG_GEOM_ST       *geom ,
    TABLE_HEAD_ST  *geom_par_h,    SDM_GEOM_PAR_ST   *geom_par ,
    TABLE_HEAD_ST  *sce_dspt_h,    SCE_DSPT_ST       *sce_dspt ,
    TABLE_HEAD_ST  *ctrl_h,        SCE_CTRL_ST       *ctrl
 )
{
  cout<<"#################################################"<<endl;
  cout<<"####      START OF SSD CHAIN EVALUATOR       ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StSceBarrel *mySsd = new StSceBarrel(geom_par);
  cout<<"####        SSD WAFERS INITIALIZATION        ####"<<endl;
  mySsd->initWafers(geom_h, geom);
  int nSsdHits = mySsd->readPointFromTable(svt_hit_h, svt_hit);
  cout<<"####    ->  "<<nSsdHits<<" HITS READ FROM TABLE        ####"<<endl;
  mySsd->convertGlobalFrameToOther();
  int inactiveHit = mySsd->removeInactiveHitInTable(svt_hit_h, svt_hit);
  cout<<"####    ->   "<<inactiveHit<<" DEAD ZONE HITS REMOVED      ####"<<endl;
  int nReadCluster = mySsd->readClusterFromTable(scf_cluster_h, scf_cluster);
  cout<<"####   -> "<<nReadCluster<<" CLUSTERS READ FROM TABLE      ####"<<endl;
  int nSsdSimSpt = mySsd->readSimPointFromTable(sls_spt_h, sls_spt);
  cout<<"####   -> "<<nSsdSimSpt<<" SIMULATED POINTS READ         ####"<<endl;
  int nEvaluatedCluster = mySsd->doEvalCluster(ctrl);
  cout<<"####   -> "<<nEvaluatedCluster<<" CLUSTERS EVALUATED            ####"<<endl;
  int nSsdRecSpt = mySsd->readRecPointFromTable(scm_spt_h, scm_spt);
  cout<<"####   -> "<<nSsdRecSpt<<" RECONSTRUCTED POINTS READ     ####"<<endl;
  int nEvaluatedSpt = mySsd->doEvalSpt(ctrl);
  cout<<"####   -> "<<nEvaluatedSpt<<" SPACE POINTS EVALUATED        ####"<<endl;
  int nSsdWrittenCompSpt = mySsd->writeComPointToTable(sce_dspt_h, sce_dspt);
  cout<<"####   -> "<<nSsdWrittenCompSpt<<" COMPARED WRITTEN TO TABLE     ####"<<endl;
  cout<<"####       END OF SSD CHAIN EVALUATOR        ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  return STAFCV_OK;
}
