 /**************************************************************************
 * Class      : St_spa_maker.cxx
 **************************************************************************
 * $Id: St_spa_Maker.cxx,v 1.19 2017/04/26 20:16:30 perev Exp $
 *
 * $Log: St_spa_Maker.cxx,v $
 * Revision 1.19  2017/04/26 20:16:30  perev
 * Hide m_DataSet
 *
 * Revision 1.18  2009/01/26 15:00:34  fisyak
 * Take care about sector number packed in volume Id
 *
 * Revision 1.17  2008/08/12 22:48:39  bouchet
 * retrieve positions and dimensions tables using Get methods
 *
 * Revision 1.16  2008/05/29 03:07:28  bouchet
 * remove inactive variables;fix a potential memory leak
 *
 * Revision 1.15  2008/05/07 22:59:11  bouchet
 * EmbeddingMaker:initial version ; modified reading of GEANT hits
 *
 * Revision 1.14  2008/04/15 21:04:43  bouchet
 * remove latest change
 *
 * Revision 1.13  2008/04/12 14:21:28  bouchet
 * Add a switch to use constant noise and pedestal
 *
 * Revision 1.12  2007/04/28 17:57:00  perev
 * Redundant StChain.h removed
 *
 * Revision 1.11  2007/03/21 17:19:56  fisyak
 * use new StSsdBarrel
 *
 * Revision 1.10  2007/01/17 18:14:37  bouchet
 * replace printf, cout statements with LOG statements
 *
 * Revision 1.9  2006/10/16 16:36:08  bouchet
 * Unify classes : Remove StSlsStrip, StSlsPoint, StSpaStrip, StSpaNoise by the same classes used in StSsdPointMaker (StSsdStrip,StSsdPoint) ; The methods for these classes are in StSsdUtil
 *
 * Revision 1.8  2006/09/15 21:09:52  bouchet
 * read the noise and pedestal from ssdStripCalib
 *
 * Revision 1.7  2005/11/22 03:56:46  bouchet
 * id_mctrack is using for setIdTruth
 *
 * Revision 1.6  2005/05/13 08:39:33  lmartin
 * CVS tags added
 *
 * Revision 1.5  2003/10/08 03:46:34  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:06:44  suire
 * Doxygen documentation, cleaning
 *
 *
 **************************************************************************/
#include <assert.h>
#include <Stiostream.h>
#include <stdlib.h>
#include "St_spa_Maker.h"
#include "TDataSetIter.h"
//#include "svt/St_spa_am_Module.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdPointMaker/StSsdPointMaker.h"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_sdm_calib_par_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_ssdStripCalib_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "StSsdDbMaker/StSsdDbMaker.h"

ClassImp(St_spa_Maker)
  
//_____________________________________________________________________________
  St_spa_Maker::St_spa_Maker(const char *name): StMaker(name),m_noise(0),m_condition(0),m_ctrl(0) {}
//_____________________________________________________________________________
St_spa_Maker::~St_spa_Maker(){}
//_____________________________________________________________________________
Int_t St_spa_Maker::Init(){
  
  // 		Create tables
  TDataSet *ssdparams = GetInputDB("svt/ssd");
  TDataSetIter       local(ssdparams);
  
  m_condition = (St_sdm_condition_db  *)local("sdm_condition_db");
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_spa_Maker::InitRun(Int_t runnumber){
  m_noise = (St_ssdStripCalib*) GetDataBase("Calibrations/ssd/ssdStripCalib");
  if (! m_noise) return kStFATAL;
  m_ctrl = gStSsdDbMaker->GetSlsCtrl();
  if (!m_ctrl) {
    LOG_ERROR << "No  access to control parameters" << endm;
    return kStFatal;
  } 
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Make()
{
  if (Debug()==true)  {LOG_DEBUG << "Make() ..." << endm;}
  // 		Create output tables
  Int_t res = 0; 
  
  St_sls_strip *sls_strip = (St_sls_strip *)GetDataSet("sls_strip/.data/sls_strip");
  
  St_spa_strip *spa_strip = new St_spa_strip("spa_strip",40000);
  AddData(spa_strip);
  
  //slsCtrl_st      *ctrl     =  m_ctrl->GetTable(); 

  LOG_INFO<<"#################################################"<<endm;
  LOG_INFO<<"####    START OF SSD PEDESTAL ANNIHILATOR    ####"<<endm;
  LOG_INFO<<"####        SSD BARREL INITIALIZATION        ####"<<endm;
  StSsdBarrel *mySsd = StSsdBarrel::Instance();
  assert(mySsd);
  mySsd->readStripFromTable(sls_strip);
  LOG_INFO<<"####        NUMBER OF SLS STRIPS "<<sls_strip->GetNRows()<<"       ####"<<endm;
  Int_t numberOfNoise = 0;
  if (m_noise) {
    numberOfNoise = mySsd->readNoiseFromTable(m_noise);
    LOG_INFO<<"####       NUMBER OF DB ENTRIES "<<numberOfNoise<<"       ####"<<endm;
  } else {
    LOG_ERROR<<"m_noise is missing" <<endm;
  }
  if (m_condition) {
    mySsd->readConditionDbFromTable(m_condition);
    LOG_INFO<<"####             ADD SPA NOISE               ####"<<endm;
  } else {
    LOG_ERROR<<"m_condition is missing" <<endm;
  }
  if (m_ctrl) {
    mySsd->addNoiseToStrip(m_ctrl);
    LOG_INFO<<"####           DO DAQ SIMULATION             ####"<<endm;
  } else {
    LOG_ERROR<<"m_ctrl is missing" <<endm;
  }
  mySsd->doDaqSimulation(m_ctrl);
  Int_t nSsdStrips = mySsd->writeStripToTable(spa_strip,sls_strip);
  //Int_t nSsdStrips = mySsd->writeStripToTable(spa_strip);
  spa_strip->Purge();
  LOG_INFO<<"####       NUMBER OF SPA STRIP "<<nSsdStrips<<"          ####"<<endm;
  mySsd->Reset();
  LOG_INFO<<"#################################################"<<endm;
  if (nSsdStrips)  res =  kStOK;

  if(res!=kStOK){
    LOG_WARN <<"no output"<<endm;
    return kStWarn;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
void St_spa_Maker::PrintInfo()
{
  if (Debug()==true){ StMaker::PrintInfo();}
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Finish() {
  if (Debug()==true) {LOG_DEBUG << "Finish() ... " << endm; }
  return kStOK;
}

