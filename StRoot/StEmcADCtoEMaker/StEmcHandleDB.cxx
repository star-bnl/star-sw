/***************************************************************************
 *
 * $Id: StEmcHandleDB.cxx,v 1.1 2001/07/17 00:14:37 perev Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Incorporation of cluster finder here
 ***************************************************************************
 *
 * See Log Comments at bottom
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcHandleDB.h"
#include "TDataSet.h"
#include "StEmcUtil/emcDetectorName.h"
#include "Stypes.h" 

ClassImp(StEmcHandleDB) // macro

StEmcGeom*  geo[4];   
//-----------------------------------------------------------------

    StEmcHandleDB::StEmcHandleDB(const TDataSet* calibdb)
      : m_calibdbptr(calibdb)
{}

//-----------------------------------------------------------------

StEmcHandleDB::~StEmcHandleDB() {}

//-----------------------------------------------------------------

Int_t StEmcHandleDB::ProcessDB() {
    cout << "HandleDB::ProcessDB()" << endl;

    for (Int_t i=0; i<4; i++) {
   geo[i]=new StEmcGeom(detname[i].Data());
    }
    //initialize DB arrays

    for(Int_t i=0;i<120;i++){
      for(Int_t j=0;j<20;j++){
	for(Int_t k=0;k<2;k++){
	  m_TowerPeds[i][j][k].clear();
	  m_TowerCalibs[i][j][k].clear();
	  m_TowerEquals[i][j][k].clear();
	}
      }

      for(Int_t ise=0;ise<150;ise++){
	m_SmdEPeds[i][ise].clear();
	m_SmdECalibs[i][ise].clear();
	m_SmdEEquals[i][ise].clear();
      }

      for(Int_t isp1=0;isp1<10;isp1++){
	for(Int_t isp2=0;isp2<15;isp2++){
	m_SmdPPeds[i][isp1][isp2].clear();
	m_SmdPCalibs[i][isp1][isp2].clear();
	m_SmdPEquals[i][isp1][isp2].clear();
	}
      }

    }
    ////////////////////////////////////////////

    // get tower tables and fill arrays
    Int_t towerstat=Process_TowerDB();
    // get tower tables and fill arrays
    Int_t smdestat=Process_SmdEDB();
    // get tower tables and fill arrays
    Int_t smdpstat=Process_SmdPDB();
     return kStOK;
}

Int_t StEmcHandleDB::Process_TowerDB()
{
  cout<<"In Process Tower DB**"<<endl;
  // Get pdestal tables from m_calibdb
  TString TableName=detname[0]+"Calibration"; 
     St_emcCalibration* caltemp;
 
      caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caltemp)
      {
        cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
       }
   else{
      m_Towercalibdb=caltemp->GetTable();
      for(Int_t idh=1;idh<4801;idh++){
     if(m_Towercalibdb[idh-1].Status==1 && m_Towercalibdb[idh-1].CalibStatus==1)
        {
	  Int_t m=0,e=0,s=0;
        geo[0]->getBin(idh,m,e,s);
	if(m!=0||e!=0||s!=0){
	  Float_t ped=(Float_t)m_Towercalibdb[idh-1].AdcPedestal;
        m_TowerPeds[m-1][e-1][s-1].push_back(ped);
        m_TowerEquals[m-1][e-1][s-1].push_back(ped);

	for(Int_t ic=0;ic<5;ic++){
	  Float_t conv=(Float_t)m_Towercalibdb[idh-1].AdcToE[ic];
          m_TowerCalibs[m-1][e-1][s-1].push_back(conv);
	}
	}
        }  
    }
   }

   ////////////////////////////////////////////////////////////
 // Get pdestal tables from m_calibdb
  cout<<"In Process Tower DB, get equal **"<<endl;
  TableName=detname[0]+"Equalization"; 
     St_emcCalibration* caleq;
 
      caleq = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caleq)
      {
        cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
      }

  else{
   m_Towerequaldb=caleq->GetTable();
   for(Int_t idh=1;idh<4801;idh++){
  if(m_Towerequaldb[idh-1].Status==1 && m_Towerequaldb[idh-1].CalibStatus==1)
        {
	  Int_t m=0,e=0,s=0;
        geo[0]->getBin(idh,m,e,s);
	if(m!=0||e!=0||s!=0){
	  Float_t ped=(Float_t)m_Towerequaldb[idh-1].AdcPedestal;
        m_TowerEquals[m-1][e-1][s-1].push_back(ped);
	}
        }  
   }
 }


   return kStOK;
}

Int_t StEmcHandleDB::Process_SmdEDB()
{

  // Get pdestal tables from m_calibdb
  TString TableName=detname[2]+"Calibration"; 
     St_emcCalibration* caltemp;
 
      caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caltemp)
      {
        cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
      }
 
                                                                         
   m_Smdecalibdb=caltemp->GetTable();
   for(Int_t idh=0;idh<4800;idh++){
  if(m_Smdecalibdb[idh-1].Status==1 && m_Smdecalibdb[idh-1].CalibStatus==1)
        {
	  Int_t m=0,e=0,s=0;
        geo[2]->getBin(idh,m,e,s);
	if(m!=0||e!=0||s!=0){
        m_SmdEPeds[m-1][e-1].push_back(m_Smdecalibdb[idh-1].AdcPedestal);
        m_SmdEEquals[m-1][e-1].push_back(m_Smdecalibdb[idh-1].AdcPedestal);
	for(Int_t ic=0;ic<5;ic++){
          m_SmdECalibs[m-1][e-1].push_back(m_Smdecalibdb[idh-1].AdcToE[ic]);
	}
	}
        }  
   }
   return kStOK;
}

Int_t StEmcHandleDB::Process_SmdPDB()
{

  // Get pdestal tables from m_calibdb
  TString TableName=detname[3]+"Calibration"; 
     St_emcCalibration* caltemp;
 
      caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caltemp)
      {
        cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
      }
 
                                                                         
   m_Smdpcalibdb=caltemp->GetTable();
   for(Int_t idh=0;idh<4800;idh++){
  if(m_Smdpcalibdb[idh-1].Status==1 && m_Smdpcalibdb[idh-1].CalibStatus==1)
        {
	  Int_t m=0,e=0,s=0;
        geo[3]->getBin(idh,m,e,s);
	if(m!=0||e!=0||s!=0){
        m_SmdPPeds[m-1][e-1][s-1].push_back(m_Smdpcalibdb[idh-1].AdcPedestal);
        m_SmdPEquals[m-1][e-1][s-1].push_back(m_Smdpcalibdb[idh-1].AdcPedestal);
	for(Int_t ic=0;ic<5;ic++){
          m_SmdPCalibs[m-1][e-1][s-1].push_back(m_Smdpcalibdb[idh-1].AdcToE[ic]);
	}
	}
        }  
   }
   return kStOK;
}

 Int_t StEmcHandleDB::GetTowerPeds(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_TowerPeds[m-1][e-1][s-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetTowerCalibs(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_TowerCalibs[m-1][e-1][s-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetTowerEquals(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_TowerEquals[m-1][e-1][s-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetSmdEPeds(Int_t m,Int_t e,Float_t& ped)
{
  ped=m_SmdEPeds[m-1][e-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetSmdECalibs(Int_t m,Int_t e,Float_t& ped)
{
  ped=m_SmdECalibs[m-1][e-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetSmdEEquals(Int_t m,Int_t e,Float_t& ped)
{
  ped=m_SmdEEquals[m-1][e-1][0];
  return kStOK;
}
///////////////////////////////////////////////////////////////
 Int_t StEmcHandleDB::GetSmdPPeds(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_SmdPPeds[m-1][e-1][s-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetSmdPCalibs(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_SmdPCalibs[m-1][e-1][s-1][0];
  return kStOK;
}

 Int_t StEmcHandleDB::GetSmdPEquals(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  ped=m_SmdPEquals[m-1][e-1][s-1][0];
  return kStOK;
}
