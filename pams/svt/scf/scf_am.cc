#include "scf_am.h"
#include "StScfBarrel.hh"

long scf_am_(
  TABLE_HEAD_ST       *geom_par_h,   SDM_GEOM_PAR_ST         *geom_par ,
  TABLE_HEAD_ST      *spa_strip_h,      SPA_STRIP_ST        *spa_strip ,
  TABLE_HEAD_ST       *sls_ctrl_h,       SLS_CTRL_ST         *sls_ctrl ,
  TABLE_HEAD_ST          *noise_h,   SDM_CALIB_DB_ST            *noise ,
  TABLE_HEAD_ST       *scf_ctrl_h,       SCF_CTRL_ST         *scf_ctrl ,
  TABLE_HEAD_ST        *cluster_h,    SCF_CLUSTER_ST          *cluster )
{
  cout<<"#################################################"<<endl;
  cout<<"####       START OF SSD CLUSTER FINDER       ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StScfBarrel *barrel = new StScfBarrel(geom_par);
  barrel->setScfParameters(geom_par);
  int stripTableSize = barrel->readStripFromTable(spa_strip_h,spa_strip);
  cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
  barrel->sortListStrip();
  int noiseTableSize = barrel->readNoiseFromTable(noise_h,noise,sls_ctrl);
  cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
  int nClusterPerSide[2];
  nClusterPerSide[0] = 0;
  nClusterPerSide[1] = 0;
  barrel->doSideClusterisation(nClusterPerSide, sls_ctrl,scf_ctrl);
  cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
  cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
  barrel->sortListCluster();
  int nClusterWritten = barrel->writeClusterToTable(cluster_h,cluster);
  cout<<"####      NUMBER OF CLUSTER SAVED  "<<nClusterWritten<<"      ####"<<endl;
  delete barrel;
  cout<<"#################################################"<<endl;
  return STAFCV_OK;
}
