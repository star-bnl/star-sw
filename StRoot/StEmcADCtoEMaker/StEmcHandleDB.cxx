/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: EMC DB Handling
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>
#include "StEmcHandleDB.h"
#include "TDataSet.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcUtil/emcDetectorName.h"
#include "Stypes.h" 

ClassImp(StEmcHandleDB) // macro

static StEmcGeom*  geo[4];   

StEmcHandleDB::StEmcHandleDB(TDataSet* calibdb)
: m_calibdbptr(calibdb)
{}

StEmcHandleDB::~StEmcHandleDB() {}

Int_t 
StEmcHandleDB::processDB() {
  cout << "HandleDB::processDB()" << endl;

  for(Int_t i=0; i<4; i++) {
    geo[i] = StEmcGeom::getEmcGeom(i+1);
  }
  //initialize DB arrays

  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<20;j++){
      for(Int_t k=0;k<2;k++){
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

    // get tower tables and fill arrays
    Int_t towerstat=processTowerPedDB();
    // get tower tables and fill arrays
    Int_t smdestat=processSmdEDB();
    // get tower tables and fill arrays
    Int_t smdpstat=processSmdPDB();
     return kStOK;
}


Int_t 
StEmcHandleDB::processTowerPedDB()
{
  cout<<"EMCHandleDB:: In process Tower Ped DB**"<<endl;
  // get pedestal tables from m_calibdb
  TString TableNamePed=detname[0]+"Pedestal"; 
  St_emcCalibration* calped=0;
  //get Pedestal tables
  cout<<"GETTING PEDS FOR TOWER***"<<endl;
  calped = (St_emcCalibration*)m_calibdbptr->Find(TableNamePed.Data());
  if(!calped) {
    cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to pedestal table for det." 
    << TableNamePed << endl;
  } else{
    cout<<"Table Name "<<TableNamePed<<" NRows " <<calped->GetNRows()
        <<" max rows " << calped->GetTableSize() << endl; 
    m_TowerPeddb=calped->GetTable();
    for(Int_t idh=1;idh<4801;idh++){
      Int_t m=0,e=0,s=0;
      geo[0]->getBin(idh,m,e,s);
      if(m_TowerPeddb[idh-1].Status==1) {
// Does it (m,e,s) in (0,0,0) or (1,1,1)
	if(m!=0||e!=0||s!=0){
	  m_TowerPeds[m-1][e-1][s-1].clear();
	  Float_t ped=(Float_t)m_TowerPeddb[idh-1].AdcPedestal;
// if(m>47 && m<59) cout<<" DB***m,e,s  "<<m<<" "<<e<<" "<<s<<" "<<"ped  "<<ped<<endl;
          m_TowerPeds[m-1][e-1][s-1].push_back(ped);
        }
      }
    }
  }
  return kStOK;
}


Int_t 
StEmcHandleDB::processTowerCalibDB()
{
  cout<<"EMCHandleDB:: In process Tower Calib DB**"<<endl;
  TString TableName=detname[0]+"Calibration"; 
  St_emcCalibration* caltemp=0;
  //Get Calibration tables
  caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
  if(!caltemp) {
    cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." 
    << TableName << endl;
  } else{
    m_Towercalibdb=caltemp->GetTable();
    for(Int_t idh=1;idh<4801;idh++){
      Int_t m=0,e=0,s=0;
      geo[0]->getBin(idh,m,e,s);
      if(m_Towercalibdb[idh-1].Status==1 && m_Towercalibdb[idh-1].CalibStatus==1){
// Does it (m,e,s) in (0,0,0) or (1,1,1)
	if(m!=0||e!=0||s!=0){
	  Float_t ped=(Float_t)m_Towercalibdb[idh-1].AdcPedestal;
          m_TowerEquals[m-1][e-1][s-1].push_back(ped);
	  for(Int_t ic=0;ic<5;ic++){ // ic - what is this
	    Float_t conv=(Float_t)m_Towercalibdb[idh-1].AdcToE[ic];
            m_TowerCalibs[m-1][e-1][s-1].push_back(conv);
	  }
	}
     } else{
       cout<<"error in ped table**"<<m-1<<" "<<e-1<<" "<<s-1<<"stat "
       <<m_Towercalibdb[idh-1].Status<<"calibstat "<<m_Towercalibdb[idh-1].CalibStatus<<endl;
     } 
   }
 }

 ////////////////////////////////////////////////////////////
 // Get pdestal tables from m_calibdb
 cout<<"In process Tower DB, get equal **"<<endl;
 TableName=detname[0]+"Equalization"; 
 St_emcCalibration* caleq=0;
 caleq = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());

 if(!caleq) {
   cout<<"StEmcHandleDB::Make() - Can not get pointer to Calibration table for det." 
   << TableName << endl;
 } else{
   m_Towerequaldb=caleq->GetTable();
/*
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
*/
 }
 return kStOK;
}

Int_t StEmcHandleDB::processSmdEDB()
{

  // Get pdestal tables from m_calibdb
  TString TableName=detname[2]+"Calibration"; 
     St_emcCalibration* caltemp;
 
      caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caltemp)
      {
        cout<<"StHandleDB::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
      }
   else{ 
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
   }
   return kStOK;
}

Int_t StEmcHandleDB::processSmdPDB()
{

  // Get pdestal tables from m_calibdb
  TString TableName=detname[3]+"Calibration"; 
     St_emcCalibration* caltemp;
 
      caltemp = (St_emcCalibration*)m_calibdbptr->Find(TableName.Data());
      if(!caltemp)
      {
        cout<<"StEmcAdcToEMaker::Make() - Can not get pointer to Calibration table for det." << TableName << endl;
      }
   else{ 
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
   }
   return kStOK;
}

 Int_t StEmcHandleDB::getTowerPeds(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
// Call m,e,s starting from 0,0,0 not 1,1,1
//
  if(m_TowerPeds[m][e][s].size()>0){ped=(Float_t)m_TowerPeds[m][e][s][0];}
  else{ped=0;}
  return kStOK;
}

 Int_t StEmcHandleDB::getTowerCalibs(Int_t m,Int_t e,Int_t s,Float_t* ped)
{
  if(m_TowerCalibs[m][e][s].size()>0){
//     for(UInt_t l=0;l<m_TowerCalibs[m][e][s].size();l++)cout<<m_TowerCalibs[m][e][s][l]<<endl;
     ped=&m_TowerCalibs[m][e][s][0];}
  else{for(Int_t i=0;i<5;i++){ped[i]=1.;}}
  return kStOK;
}

 Int_t StEmcHandleDB::getTowerEquals(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  if(m_TowerEquals[m][e][s].size()>0)ped=m_TowerEquals[m][e][s][0];
  else{ped=1;}
  return kStOK;
}

 Int_t StEmcHandleDB::getSmdEPeds(Int_t m,Int_t e,Float_t& ped)
{
  if(m_SmdEPeds[m][e].size()>0)ped=m_SmdEPeds[m][e][0];
  else{ped=0;}
  return kStOK;
}

 Int_t StEmcHandleDB::getSmdECalibs(Int_t m,Int_t e,Float_t& ped)
{
  if(m_SmdECalibs[m][e].size()>0)ped=m_SmdECalibs[m][e][0];
  else{ped=1;}
  return kStOK;
}

 Int_t StEmcHandleDB::getSmdEEquals(Int_t m,Int_t e,Float_t& ped)
{
  if(m_SmdEEquals[m][e].size()>0)ped=m_SmdEEquals[m][e][0];
  else{ped=1;}
  return kStOK;
}
///////////////////////////////////////////////////////////////
 Int_t StEmcHandleDB::getSmdPPeds(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  if(m_SmdPPeds[m][e][s].size()>0)ped=m_SmdPPeds[m][e][s][0];
  else{ped=0;}
  return kStOK;
}

 Int_t StEmcHandleDB::getSmdPCalibs(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  if(m_SmdPPeds[m][e][s].size()>0)ped=m_SmdPCalibs[m][e][s][0];
  else{ped=1;}
  return kStOK;
}

 Int_t StEmcHandleDB::getSmdPEquals(Int_t m,Int_t e,Int_t s,Float_t& ped)
{
  if(m_SmdPPeds[m][e][s].size()>0)ped=m_SmdPEquals[m][e][s][0];
  else{ped=1;}
  return kStOK;
}
