 /**************************************************************************
 * Class      : St_scf_maker.cxx
 **************************************************************************
 * $Id: St_scf_Maker.cxx,v 1.13 2006/09/17 18:55:24 bouchet Exp $
 *
 * $Log: St_scf_Maker.cxx,v $
 * Revision 1.13  2006/09/17 18:55:24  bouchet
 * Correction in makeScfCtrlHistograms() : fill correctly the noise of p-clusters
 *
 * Revision 1.12  2006/09/15 21:04:50  bouchet
 * noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
 *
 * Revision 1.11  2005/12/23 14:47:32  fisyak
 * DeclareNtuple only if m_Mode != 0
 *
 * Revision 1.10  2005/11/22 03:57:05  bouchet
 * id_mctrack is using for setIdTruth
 *
 * Revision 1.9  2005/06/14 12:20:25  bouchet
 * cleaner version
 *
 * Revision 1.8  2005/06/13 16:01:01  reinnart
 * Jonathan and Joerg changed the update function
 *
 * Revision 1.7  2005/05/17 14:16:41  lmartin
 * CVS tags added
 *
 * Revision 1.6  2005/05/13 15:16:54  bouchet
 * reading ssd/geom and no more writeScfCtrlHistograms and writeScmCtrlHistograms methods
 *
 * Revision 1.5  2003/10/08 03:18:09  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:13:05  suire
 * Small memory leak fixes, doxygen documentation
 *
 *
 **************************************************************************/
#include <stdlib.h>
#include "St_scf_Maker.h"
#include "StChain.h"
#include "TDataSetIter.h"
#include "TH1.h"
#include "TFile.h"
#include "TNtuple.h"
#include "StMessMgr.h"

#include "StScfBarrel.hh" // introducing StSsdClusterBarrel
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"

#include "tables/St_ssdDimensions_Table.h" //needed to call StTable->GetTable()
#include "StSsdClusterControl.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_scf_ctrl_Table.h"

#include "StScfCluster.hh"
#include "StScfWafer.hh"
#include "StScfListCluster.hh"
ClassImp(St_scf_Maker)
//_____________________________________________________________________________
St_scf_Maker::St_scf_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_noise(0),
m_scf_ctrl(0),
m_slsCtrl(0)

{
}
//_____________________________________________________________________________
St_scf_Maker::~St_scf_Maker(){
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Init(){
  if (Debug())  gMessMgr->Debug() << "In St_scf_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create tables
  TDataSet *svtparams = GetInputDB("svt");
  TDataSetIter       local(svtparams);

  // Replace tables for control parameters
  StSsdClusterControl *control = new StSsdClusterControl();
  // Set Control Parameters  
  // Former Scf
  control->setHighCut(5.0);
  control->setTestTolerance(0.2);
  // Former Scm
  control->setClusterTreat(13);
  control->setAdcTolerance(0.2);
  control->setMatchMean(0.);
  control->setMatchSigma(8.);
  control->printParameters();
  // 

  //m_noise     = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");
  m_scf_ctrl  = (St_scf_ctrl          *)local("ssd/scf_ctrl");
  //if (!m_noise) {
  //gMessMgr->Error() << "No  access to noise condition" << endm;
  //}
  //09/14 : the noise is read later in the InitRun()

  if ((!m_slsCtrl)||(!m_scf_ctrl)) {
   gMessMgr->Error() << "No  access to control parameters" << endm;
  } 

  // 		Create SCF histograms
  noisDisP = new TH1F("Noise_p","Noise Distribution",25,0,25);
  snRatioP = new TH1F("SN_p","Signal/Noise (p)",200,0,200);
  stpClusP = new TH1F("NumberOfStrips_p","Strips per Cluster",8,0,8);
  totChrgP = new TH1F("ChargeElectron_p","Total Cluster Charge",100,0,300000);
  noisDisN = new TH1F("Noise_n","Noise Distribution",25,0,25);
  snRatioN = new TH1F("SN_n","Signal/Noise",200,0,200);
  stpClusN = new TH1F("NumberOfStrips_n","Strips per Cluster",8,0,8);
  totChrgN = new TH1F("ChargeElectron_n","Total Cluster Charge",100,0,300000);

  if (m_Mode)DeclareNtuple();
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t  St_scf_Maker::InitRun(Int_t runNumber) {
// 		geometry parameters
  St_DataSet *CalibDbConnector = GetDataBase("Calibrations/ssd");
  if (!CalibDbConnector) {
   gMessMgr->Error()<<"InitRun: Can not found the calibration db.."<<endm;
   return kStFATAL;
  }
  else{
   m_noise = (St_ssdStripCalib*) CalibDbConnector->Find("ssdStripCalib");
   if (! m_noise) return kStFATAL;
  }
  
  TDataSet *ssdparams = GetInputDB("Geometry/ssd");
  if (! ssdparams) {
    gMessMgr->Error() << "No  access to Geometry/ssd parameters" << endm;
    return kStErr;
  }
  TDataSetIter    local(ssdparams);
  m_slsCtrl        = (St_slsCtrl           *)local("slsCtrl");
  m_geom_par    = (St_ssdDimensions     *)local("ssdDimensions");
  if (!m_slsCtrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
    return kStErr;
  }   
  if ( ! m_geom_par ) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
    return kStErr;
  }  
  return kStOK;
}
//_____________________________________________________________________________
void St_scf_Maker::DeclareNtuple()
{
  qFile = new TFile("Clusters.root","RECREATE");
  string varlist3 = "side:ladder:wafer:nstrip:snratio:noise:first_strip:adc_count";
  qHitNtuple     = new TNtuple("ClusTuple","Clusters Ntuple",varlist3.c_str());
  //11/22 : jb : ***the NTuple is not written by defaults*** 
 }
//_____________________________________________________________________________
Int_t St_scf_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_scf_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 
  
 // two different tables can exist (physics data or pedestal data)
  TDataSet *SpaStrip = GetDataSet("SpaStrip");
  if (! SpaStrip) {
    gMessMgr->Error() << "Make : no input data set, wrong chain option" << endm;
    return kStErr;
  }
  St_spa_strip *spa_strip = dynamic_cast<St_spa_strip *> (SpaStrip->Find("spa_strip"));
  if (! spa_strip ) {
    gMessMgr->Warning() << "Make : no input data spa_strip" << endm;
    return kStWarn;
    
  }
  
  St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",10000);
  m_DataSet->Add(scf_cluster);
  
  ssdDimensions_st  *geom_par = m_geom_par->GetTable();
  slsCtrl_st *slsCtrl;
  scf_ctrl_st *scf_ctrl;
  slsCtrl = m_slsCtrl->GetTable();
  scf_ctrl = m_scf_ctrl->GetTable();

  cout<<"#################################################"<<endl;
  cout<<"####       START OF SSD CLUSTER FINDER       ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StScfBarrel *barrel = new StScfBarrel(geom_par);
    barrel->setSsdParameters(geom_par);
  int stripTableSize = barrel->readStripFromTable(spa_strip);
  cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
  barrel->sortListStrip();
  int noiseTableSize = barrel->readNoiseFromTable(m_noise,slsCtrl);
  cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
  int nClusterPerSide[2];
  nClusterPerSide[0] = 0;
  nClusterPerSide[1] = 0;
  barrel->doSideClusterisation(nClusterPerSide,m_slsCtrl,m_scf_ctrl);
  cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
  cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
  barrel->sortListCluster();
  int nClusterWritten = barrel->writeClusterToTable(scf_cluster,spa_strip);
  //int nClusterWritten = barrel->writeClusterToTable(scf_cluster);
  cout<<"####      NUMBER OF CLUSTER SAVED  "<<nClusterWritten<<"      ####"<<endl;  
  //PrintClusterDetails(barrel,7802);
  //PrintIdMctrack(scf_cluster,7802);
  //scf_cluster->Purge(); //remove all unused rows
  //makeNewScfCtrlHistograms(barrel);
  delete barrel;
  cout<<"#################################################"<<endl;
  res =  kStOK;
  
  if(res!=kStOK){
    gMessMgr->Warning("St_scf_Maker: no output");
     return kStWarn;
  }
  makeScfCtrlHistograms();
    
  return kStOK;
}
//_____________________________________________________________________________
void St_scf_Maker::makeScfCtrlHistograms()
{ 
  int iWaf;
  int idWaf;
  int iLad;
  int nCluster;
   TDataSetIter scf_iter(m_DataSet);
  St_scf_cluster *scf_cluster = 0;
  scf_cluster = (St_scf_cluster *) scf_iter.Find("scf_cluster"); 

// 		Fill histograms
  if (scf_cluster->GetNRows()){
    Int_t clustSide   = 0;  // pside = 0 et nside = 1 
    scf_cluster_st *dClus = scf_cluster->GetTable();
    slsCtrl_st *slsCtrl_t = m_slsCtrl->GetTable();
    Float_t convAdcToE = (slsCtrl_t[0].adcDynamic*slsCtrl_t[0].nElectronInAMip)/(pow(2.0,slsCtrl_t[0].nbitEncoding));
    for (Int_t iScf = 0; iScf < scf_cluster->GetNRows(); iScf++)
      {

	nCluster=(int)(dClus[iScf].id/100000.);
	idWaf=(dClus[iScf].id_cluster-10000*((int)(dClus[iScf].id_cluster/10000.)));
	//iSide=(dClus[iScf].id_cluster-idWaf-nCluster*100000)/10000;
	iWaf=(int)((idWaf-7000)/100-1);
	iLad=(int)(idWaf - 7000 -(iWaf+1)*100-1); 
	clustSide = ((dClus[iScf].id_cluster/10000)-(dClus[iScf].id_cluster/100000)*10);
	//cout <<"side="<<clustSide<<" wafer== <<iWaf<<" ladder="<<iLad<<" nstrip="<<dClus[iScf].n_strip<<" noise="<<dClus[iScf].noise_count<<" adc ="<<dClus[iScf].adc_count<<endl;
	if(dClus[iScf].noise_count>0)
	  {
	    if(!clustSide)
	      {
		ClusterNtuple[0]=0;
		ClusterNtuple[1]=iLad+1;
		ClusterNtuple[2]=iWaf+1;
		ClusterNtuple[3]=dClus[iScf].n_strip;
		ClusterNtuple[4]=((dClus[iScf].adc_count*dClus[iScf].n_strip)/dClus[iScf].noise_count);
		ClusterNtuple[5]=dClus[iScf].noise_count;
		ClusterNtuple[6]= (int)(dClus[iScf].first_strip/100000.);
		ClusterNtuple[7]=dClus[iScf].adc_count*dClus[iScf].n_strip;
		if (m_Mode)qHitNtuple->Fill(ClusterNtuple);
		noisDisP->Fill(dClus[iScf].noise_count/dClus[iScf].n_strip);
		snRatioP->Fill((dClus[iScf].adc_count*dClus[iScf].n_strip)/dClus[iScf].noise_count);
		stpClusP->Fill(dClus[iScf].n_strip);
		totChrgP->Fill(convAdcToE*dClus[iScf].adc_count);
		
	      }
	    else
	      {
		noisDisN->Fill(dClus[iScf].noise_count/dClus[iScf].n_strip);
		snRatioN->Fill((dClus[iScf].adc_count*dClus[iScf].n_strip)/dClus[iScf].noise_count);
		stpClusN->Fill(dClus[iScf].n_strip);
		totChrgN->Fill(convAdcToE*dClus[iScf].adc_count);
		ClusterNtuple[0]=1;
		ClusterNtuple[1]=iLad+1;
		ClusterNtuple[2]=iWaf+1;
		ClusterNtuple[3]=dClus[iScf].n_strip;
		ClusterNtuple[4]=((dClus[iScf].adc_count*dClus[iScf].n_strip)/dClus[iScf].noise_count);
		ClusterNtuple[5]=dClus[iScf].noise_count;
		ClusterNtuple[6]= (int)(dClus[iScf].first_strip/100000.);
		ClusterNtuple[7]=dClus[iScf].adc_count*dClus[iScf].n_strip;
		if (m_Mode)qHitNtuple->Fill(ClusterNtuple);
	      }
	  }
      }
  }
}
//_____________________________________________________________________________
void St_scf_Maker::makeNewScfCtrlHistograms(StScfBarrel *barrel)
{
  slsCtrl_st *slsCtrl_t = m_slsCtrl->GetTable();
  Float_t convAdcToE = (slsCtrl_t[0].adcDynamic*slsCtrl_t[0].nElectronInAMip)/(pow(2.0,slsCtrl_t[0].nbitEncoding));
  
  for (int j=0; j<320;j++) {
    int iL = 1+(int)((j)/16);
    int iW = j-((iL-1)*16)+1;
    //Looking for the P-side cluster informations
    if (barrel->mWafers[j]->getClusterP()->getSize()!=0) {
      StScfCluster *pClusterP = barrel->mWafers[j]->getClusterP()->first();
      while (pClusterP){
	if(pClusterP->getTotNoise()>0){
	  ClusterNtuple[0]=0;
	  ClusterNtuple[1]=iL;
	  ClusterNtuple[2]=iW;
	  ClusterNtuple[3]=pClusterP->getClusterSize();
	  ClusterNtuple[4]=(pClusterP->getClusterSize()*pClusterP->getTotAdc())/(pClusterP->getTotNoise());
	  ClusterNtuple[5]=pClusterP->getTotNoise()/pClusterP->getClusterSize();
	  ClusterNtuple[6]=pClusterP->getFirstStrip();
	  ClusterNtuple[7]=pClusterP->getClusterSize()*pClusterP->getTotAdc();
	  if (m_Mode)qHitNtuple->Fill(ClusterNtuple);
	  noisDisP->Fill(pClusterP->getTotNoise()/pClusterP->getClusterSize());
	  snRatioP->Fill((pClusterP->getClusterSize()*pClusterP->getTotAdc())/pClusterP->getTotNoise());
	  stpClusP->Fill(pClusterP->getClusterSize());
	  totChrgP->Fill(convAdcToE*pClusterP->getTotAdc());
	}
	pClusterP    = barrel->mWafers[j]->getClusterP()->next(pClusterP);
      }
    }
    if (barrel->mWafers[j]->getClusterP()->getSize()!=0) {
      StScfCluster *pClusterN = barrel->mWafers[j]->getClusterN()->first();
      while (pClusterN){
	if(pClusterN->getTotNoise()>0){
	  ClusterNtuple[0]=1;
	  ClusterNtuple[1]=iL;
	  ClusterNtuple[2]=iW;
	  ClusterNtuple[3]=pClusterN->getClusterSize();
	  ClusterNtuple[4]=(pClusterN->getClusterSize()*pClusterN->getTotAdc())/pClusterN->getTotNoise();
	  ClusterNtuple[5]=pClusterN->getTotNoise()/pClusterN->getClusterSize();
	  ClusterNtuple[6]=pClusterN->getFirstStrip();
	  ClusterNtuple[7]=pClusterN->getClusterSize()*pClusterN->getTotAdc();
	  if (m_Mode)qHitNtuple->Fill(ClusterNtuple);
	  noisDisN->Fill(pClusterN->getTotNoise()/pClusterN->getClusterSize());
	  snRatioN->Fill((pClusterN->getClusterSize()*pClusterN->getTotAdc())/pClusterN->getTotNoise());
	  stpClusN->Fill(pClusterN->getClusterSize());
	  totChrgN->Fill(convAdcToE*pClusterN->getTotAdc());
	}
	pClusterN    = barrel->mWafers[j]->getClusterN()->next(pClusterN);
      }
    } 
  }     
}

//_____________________________________________________________________________
void St_scf_Maker::PrintClusterDetails(StScfBarrel *barrel,int mywafer)
{
  int found;
  gMessMgr->Info() <<"St_scf_Maker::PrintClusterDetails() - Wafer "<<mywafer<< endm;  
  for (int j=0; j<320;j++) {
    int iL = 1+(int)((j)/16);
    int iW = j-((iL-1)*16)+1;
    int IdWafer = 7*1000 + iW*100 + iL;
    if (IdWafer==mywafer) {
      found=1;
      //Looking for the P-side cluster informations
      if (barrel->mWafers[j]->getClusterP()->getSize()==0) {
	gMessMgr->Info() <<"St_scf_Maker::PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
      }
      else {
	gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - "
			<<barrel->mWafers[j]->getClusterP()->getSize()<<" cluster(s) on the P-side of this wafer "<< endm;  
	gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise/idMcHit[0]/idMcHit[1]/idMcHit[2]/idMcHit[3]/idMcHit[4]"<< endm;  
	StScfCluster *pClusterP = barrel->mWafers[j]->getClusterP()->first();
	while (pClusterP){
	  gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - "
			  <<pClusterP->getNCluster()<<" "
			  <<pClusterP->getFlag()<<" "
			  <<pClusterP->getClusterSize()<<" "
			  <<pClusterP->getFirstStrip()<<" "
			  <<pClusterP->getStripMean()<<" "
			  <<pClusterP->getTotAdc()<<" "
			  <<pClusterP->getFirstAdc()<<" "
			  <<pClusterP->getLastAdc()<<" "
			  <<pClusterP->getTotNoise()<<" "
			  <<pClusterP->getIdMcHit(0)<<" "
			  <<pClusterP->getIdMcHit(1)<<" "
			  <<pClusterP->getIdMcHit(2)<<" "
			  <<pClusterP->getIdMcHit(3)<<" "
			  <<pClusterP->getIdMcHit(4)<<" "
			  <<endm;
	  pClusterP    = barrel->mWafers[j]->getClusterP()->next(pClusterP);
	}
	gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - "
			<<barrel->mWafers[j]->getClusterN()->getSize()<<" cluster(s) on the N-side of this wafer "<< endm;  
	gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise/idMcHit[0]/idMcHit[1]/idMcHit[2]/idMcHit[3]/idMcHit[4]"<< endm;  
	StScfCluster *pClusterN = barrel->mWafers[j]->getClusterN()->first();
	while (pClusterN){
	  gMessMgr->Info()<<"St_scf_Maker::PrintClusterDetails() - "
			  <<pClusterN->getNCluster()<<" "
			  <<pClusterN->getFlag()<<" "
			  <<pClusterN->getClusterSize()<<" "
			  <<pClusterN->getFirstStrip()<<" "
			  <<pClusterN->getStripMean()<<" "
			  <<pClusterN->getTotAdc()<<" "
			  <<pClusterN->getFirstAdc()<<" "
			  <<pClusterN->getLastAdc()<<" "
			  <<pClusterN->getTotNoise()<<" "
			  <<pClusterN->getIdMcHit(0)<<" "
			  <<pClusterN->getIdMcHit(1)<<" "
			  <<pClusterN->getIdMcHit(2)<<" "
			  <<pClusterN->getIdMcHit(3)<<" "
			  <<pClusterN->getIdMcHit(4)<<" "
			  <<endm;
	  pClusterN    = barrel->mWafers[j]->getClusterN()->next(pClusterN);
	}     
      }
    }
  }
}
//_____________________________________________________________________________
void St_scf_Maker::PrintIdMctrack(St_scf_cluster *scf_cluster,int mywafer)
{
  scf_cluster_st *cluster = scf_cluster->GetTable();
  for(int j = 0 ; j < scf_cluster->GetNRows(); j++){
    if (cluster[j].id_cluster == mywafer)
      {
	cout<<"cluster id = "<<cluster[j].id<<" id_cluster = "<<cluster[j].id_cluster<<" first strip = "<<cluster[j].first_strip<<endl;
    for (int i =0 ;i < 5 ;i++)
      {
	cout<<" cluster.id_mchit["<<i<<"] = "<<cluster[j].id_mchit[i]<<" cluster.id_mctrack["<<i<<"] = "<<cluster[j].id_mctrack[i]<<endl;
      }
      }
  }
}
    //_____________________________________________________________________________
void St_scf_Maker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_scf_Maker::Finish() ... "
                               << GetName() << endm; 
  //qFile->Write();
  //qFile->Close();
  return kStOK;
}

