#include "spa_am.h"
#include "StSpaBarrel.hh"

long type_of_call spa_am_(
  TABLE_HEAD_ST      *geom_par_h,      SDM_GEOM_PAR_ST    *geom_par ,
  TABLE_HEAD_ST       *cal_par_h,     SDM_CALIB_PAR_ST     *cal_par ,
  TABLE_HEAD_ST     *sls_strip_h,         SLS_STRIP_ST   *sls_strip ,
  TABLE_HEAD_ST          *ctrl_h,          SLS_CTRL_ST        *ctrl ,
  TABLE_HEAD_ST     *out_strip_h,         SPA_STRIP_ST   *out_strip ,
  TABLE_HEAD_ST         *noise_h,      SDM_CALIB_DB_ST       *noise ,
  TABLE_HEAD_ST     *condition_h,  SDM_CONDITION_DB_ST   *condition )
{
  cout<<"#################################################"<<endl;
  cout<<"####    START OF SSD PEDESTAL ANNIHILATOR    ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StSpaBarrel *mySsd = new StSpaBarrel(geom_par, cal_par);
  mySsd->readStripFromTable(sls_strip_h, sls_strip);
  cout<<"####        NUMBER OF SLS STRIPS "<<sls_strip_h->nok<<"       ####"<<endl;
  mySsd->readNoiseFromTable(noise_h, noise);
  cout<<"####       NUMBER OF DB ENTRIES "<<noise_h->nok<<"       ####"<<endl;
  mySsd->readConditionDbFromTable(condition_h, condition);
  cout<<"####             ADD SPA NOISE               ####"<<endl;
  mySsd->addNoiseToStrip(ctrl);
  cout<<"####           DO DAQ SIMULATION             ####"<<endl;
  mySsd->doDaqSimulation(ctrl);
  int nSsdStrips = mySsd->writeStripToTable(out_strip_h, out_strip, sls_strip_h, sls_strip);
  cout<<"####       NUMBER OF SPA STRIP "<<nSsdStrips<<"          ####"<<endl;
  delete mySsd;
  cout<<"#################################################"<<endl;
  return STAFCV_OK;
}
