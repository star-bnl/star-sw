 /**************************************************************************
 * Class      : St_spa_maker.cxx
 **************************************************************************
 *
 * $Log: St_spa_Maker.cxx,v $
 * Revision 1.5  2003/10/08 03:46:34  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:06:44  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include <stdlib.h>
#include "St_spa_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
//#include "svt/St_spa_am_Module.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StSpaBarrel.hh"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sdm_calib_par_Table.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_sdm_calib_db_Table.h"

ClassImp(St_spa_Maker)
  
//_____________________________________________________________________________
St_spa_Maker::St_spa_Maker(const char *name):
StMaker(name),
m_cond_par(0),
m_geom_par(0),
m_cal_par(0),
m_noise(0),
m_condition(0),
m_ctrl(0)

{
}
//_____________________________________________________________________________
St_spa_Maker::~St_spa_Maker(){
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Init(){
  
  // 		Create tables
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  
  m_cond_par  = (St_sdm_condition_par *)local("ssd/sdm_condition_par");
  m_geom_par  = (St_sdm_geom_par      *)local("ssd/sdm_geom_par");
  m_cal_par   = (St_sdm_calib_par     *)local("ssd/sdm_calib_par");
  m_noise     = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");
  m_condition = (St_sdm_condition_db  *)local("ssd/sdm_condition_db");
  m_ctrl      = (St_sls_ctrl          *)local("ssd/sls_ctrl");

  if (!m_geom_par) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_cond_par) {
    gMessMgr->Error() << "No  access to condition parameters" << endm;
  }   
  if (!m_cal_par) {
    gMessMgr->Error() << "No  access to calibration parameters" << endm;
  }   
  if (!m_ctrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
  } 
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_spa_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 
  
  St_sls_strip *sls_strip = (St_sls_strip *)GetDataSet("sls_strip/.data/sls_strip");
  
  St_spa_strip *spa_strip = new St_spa_strip("spa_strip",40000);
  m_DataSet->Add(spa_strip);
  
  sdm_geom_par_st  *geom_par =  m_geom_par->GetTable();
  sdm_calib_par_st *cal_par  =  m_cal_par->GetTable();
  sls_ctrl_st      *ctrl     =  m_ctrl->GetTable(); 

  cout<<"#################################################"<<endl;
  cout<<"####    START OF SSD PEDESTAL ANNIHILATOR    ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StSpaBarrel *mySsd = new StSpaBarrel(geom_par, cal_par);
  mySsd->readStripFromTable(sls_strip);
  cout<<"####        NUMBER OF SLS STRIPS "<<sls_strip->GetNRows()<<"       ####"<<endl;
  mySsd->readNoiseFromTable(m_noise);
  cout<<"####       NUMBER OF DB ENTRIES "<<m_noise->GetNRows()<<"       ####"<<endl;
  mySsd->readConditionDbFromTable(m_condition);
  cout<<"####             ADD SPA NOISE               ####"<<endl;
  mySsd->addNoiseToStrip(ctrl);
  cout<<"####           DO DAQ SIMULATION             ####"<<endl;
  mySsd->doDaqSimulation(ctrl);
  int nSsdStrips = mySsd->writeStripToTable(spa_strip);
  spa_strip->Purge();
  cout<<"####       NUMBER OF SPA STRIP "<<nSsdStrips<<"          ####"<<endl;
  delete mySsd;
  cout<<"#################################################"<<endl;
  if (nSsdStrips)  res =  kStOK;

  if(res!=kStOK){
    gMessMgr->Warning("St_spa_Maker: no output");
    return kStWarn;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
void St_spa_Maker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_spa_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}

