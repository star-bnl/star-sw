//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_spa_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_spa_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_spa_am_Module.h"
#include "TFile.h"
#include "StMessMgr.h"

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
  
  res =  spa_am(m_geom_par, m_cal_par, sls_strip, m_ctrl,
		spa_strip, m_noise, m_condition);
  
   if(res!=kSTAFCV_OK){
     gMessMgr->Warning("St_spa_Maker: no output");
     return kStWarn;
   }
  
  return kStOK;
}
//_____________________________________________________________________________
void St_spa_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_spa_Maker.cxx,v 1.2 2000/08/15 19:31:19 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_spa_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}

