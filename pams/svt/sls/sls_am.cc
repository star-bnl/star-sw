#include "sls_am.h"
#include "StSlsBarrel.hh"
 
long type_of_call sls_am_(
  TABLE_HEAD_ST        *svt_hit_h,    G2T_SVT_HIT_ST          *svt_hit,
  TABLE_HEAD_ST           *geom_h,       SVG_GEOM_ST             *geom,
  TABLE_HEAD_ST       *geom_par_h,   SDM_GEOM_PAR_ST         *geom_par,
  TABLE_HEAD_ST           *ctrl_h,       SLS_CTRL_ST             *ctrl ,
  TABLE_HEAD_ST            *spt_h,        SLS_SPT_ST              *spt ,
  TABLE_HEAD_ST          *strip_h,      SLS_STRIP_ST            *strip )
{
  cout<<"#################################################"<<endl;
  cout<<"####       START OF SSD LAZY SIMULATOR       ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StSlsBarrel *mySsd = new StSlsBarrel(geom_par);
  cout<<"####        SSD WAFERS INITIALIZATION        ####"<<endl;
  mySsd->initWafers(geom_h,geom);
  int nSsdHits = mySsd->readPointFromTable(svt_hit_h, svt_hit);
  cout<<"####    ->  "<<nSsdHits<<" HITS READ FROM TABLE        ####"<<endl;
  mySsd->convertGlobalFrameToOther();
  int inactiveHit = mySsd->removeInactiveHitInTable(svt_hit_h, svt_hit);
  cout<<"####    ->   "<<inactiveHit<<" DEAD ZONE HITS REMOVED      ####"<<endl;
  mySsd->chargeSharingOverStrip(ctrl);
  int nSsdSpts = mySsd->writePointToTable(spt_h, spt);
  cout<<"####    ->  "<<nSsdSpts<<" HITS WRITTEN INTO TABLE     ####"<<endl;
  int nSsdStrips = mySsd->writeStripToTable(strip_h, strip);
  cout<<"####    -> "<<nSsdStrips<<" FIRED STRIPS INTO TABLE     ####"<<endl;
  cout<<"####        END OF SSD LAZY SIMULATOR        ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  return STAFCV_OK;
}
