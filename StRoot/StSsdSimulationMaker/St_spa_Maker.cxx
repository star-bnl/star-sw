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
// #include "svt/St_sdm_am_Module.h"
#include "TFile.h"


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

  int res = 1;
  if ((!m_cond_par)||(!m_geom_par)||(!m_cal_par)||(!m_ctrl))
    {
      res = 0;
      cout<<"*** sdm parameter tables are missing ***"<<endl;
      if (!m_cond_par) cout<<"condition_par is missing"<<endl;
      if (!m_geom_par) cout<<"geom_par is missing"<<endl;
      if (!m_cal_par) cout<<"calibration_par is missing"<<endl;
      if (!m_ctrl) cout<<"sls_ctrl is missing"<<endl;
      return kStWarn;
      
    }
//   else
//     {
//       if ((!m_noise)||(!m_condition))
// 	{
// 	  cout<<"******************************************"<<endl;
// 	  cout<<"***  database is empty for the moment  ***"<<endl;
// 	  cout<<"******************************************"<<endl;
// 	  m_noise         = new St_sdm_calib_db("sdm_calib_db",500000);
// 	  m_condition     = new St_sdm_condition_db("sdm_condition_db",500000);
// 	  res = sdm_am(m_cond_par, m_geom_par, m_cal_par, m_noise, m_condition);
// 	  TFile *f1 = new TFile("/afs/in2p3.fr/group/star/users/hippolyt/params/svt/ssd/sdm_calib_db.root","RECREATE");
//           m_noise->Write();
// 	  f1->Close();
// 	  cout<<"***   File sdm_calib_db.root created   ***"<<endl;
// 	  TFile *f2 = new TFile("/afs/in2p3.fr/group/star/users/hippolyt/params/svt/ssd/sdm_condition_db.root","RECREATE");
//           m_condition->Write();
// 	  f2->Close();
// 	  cout<<"*** File sdm_condition_db.root created ***"<<endl;
// 	  cout<<"******************************************"<<endl;
// 	  cout<<"***       database is set up now       ***"<<endl;
// 	  cout<<"******************************************\n"<<endl;
          
// 	}
//       if(res!=kSTAFCV_OK) 
// 	{
// 	  cout<<"*** sdm_calib_db or sdm_condition_db is missing ***"<<endl;
// 	  return kStWarn;
// 	}
//     }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_spa_Maker::Make()
{
  // 		Create output tables
  Int_t res = 0; 
  
  St_sls_strip *sls_strip = (St_sls_strip *)GetDataSet("sls_strip/.data/sls_strip");
  
  St_spa_strip *spa_strip = new St_spa_strip("spa_strip",40000);
  m_DataSet->Add(spa_strip);
  
  res =  spa_am(m_geom_par, m_cal_par, sls_strip, m_ctrl,
		spa_strip, m_noise, m_condition);
  
  if(res!=kSTAFCV_OK) return kStWarn;
  if (Debug()) m_DataSet->ls("*");
  
  
  return kStOK;
}
//_____________________________________________________________________________
void St_spa_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_spa_Maker.cxx,v 1.1 2000/07/21 15:08:56 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}




