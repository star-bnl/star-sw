 /**************************************************************************
 * Class      : StSsdPointMaker.cxx
 **************************************************************************
 *
 * $Log: StSsdPointMaker.cxx,v $
 * Revision 1.3  2004/08/13 07:07:23  croy
 * Updates to read SSD databases
 *
 * Revision 1.2  2004/07/20 14:04:02  croy
 * Use of new database structure definitions related to SSD config
 *
 * Revision 1.1  2004/03/12 06:12:37  jeromel
 * Peer review closed. Yuri/Frank.
 *
 * Revision 1.3  2002/03/25 20:13:05  hippolyt
 * Merged the two former makers 
 *
 *
 **************************************************************************/
#include "StSsdPointMaker.h"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"                         //

#include "StSsdBarrel.hh"
#include "StSsdLadder.hh"
#include "StEvent.h"
#include "StSsdHitCollection.h"
#include "StSsdDynamicControl.h"
#include "StSsdClusterControl.h"
#include "tables/St_spa_strip_Table.h" //needed to call StTable->GetTable()
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_clusterControl_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdLaddersPosition_Table.h"
#include "tables/St_ssdSectorsPosition_Table.h"
#include "tables/St_ssdBarrelPosition_Table.h"

ClassImp(StSsdPointMaker)
//_____________________________________________________________________________
StSsdPointMaker::StSsdPointMaker(const char *name):
StMaker(name),
m_noise(0),
m_noise2(0),
m_condition_db(0),
m_dimensions(0),
m_configuration(0),
m_wafpos(0),
m_ladpos(0),
m_secpos(0),
m_barpos(0)

{
}
//_____________________________________________________________________________
StSsdPointMaker::~StSsdPointMaker(){
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Init(){
  gMessMgr->Info() << " In StSsdPointMaker::init() - " << endm;

  // database readout 
  gMessMgr->Info() << " Trying to access to databases " << endm;

  mDbMgr = StDbManager::Instance();
  mDbMgr -> setVerbose(false);             // set Verbose mode for debug

  //-> connect to the db & get an empty container
  maccess    = mDbMgr -> initConfig(dbGeometry,dbSsd);
  if (maccess) 
    {
      gMessMgr->Info() << "SSD Databases respond " << endm;
      StDbTable* slsCtrlTable = maccess -> addDbTable("slsCtrl");
      mDbMgr->fetchDbTable(slsCtrlTable);
      slsCtrl_st *control  = (slsCtrl_st*) slsCtrlTable->GetTable();
      if (!control) 
	gMessMgr->Error() << "No  access to slsCtrl table" << endm;
      else
	{
	  mDynamicControl = new StSsdDynamicControl();
	  mDynamicControl -> setNElectronInAMip(control->nElectronInAMip);
	  mDynamicControl -> setADCDynamic(control->adcDynamic);
	  mDynamicControl -> setA128Dynamic(control->a128Dynamic);
	  mDynamicControl -> setNBitEncoding(control->nbitEncoding);
	  mDynamicControl -> setNStripInACluster(control->nstripInACluster);
	  mDynamicControl -> setPairCreationEnergy(control->pairCreationEnergy);
	  mDynamicControl -> setParDiffP(control->parDiffP);
	  mDynamicControl -> setParDiffN(control->parDiffN);
	  mDynamicControl -> setParIndRightP(control->parIndRightP);
	  mDynamicControl -> setParIndRightN(control->parIndRightN);
	  mDynamicControl -> setParIndLeftP(control->parIndLeftP);
	  mDynamicControl -> setParIndLeftN(control->parIndLeftN);
	  mDynamicControl -> setDAQCutValue(control->daqCutValue);
	  mDynamicControl -> printParameters();
	}

      StDbTable* clusterCtrlTable = maccess -> addDbTable("clusterControl");
      mDbMgr->fetchDbTable(clusterCtrlTable);
      clusterControl_st *clusterCtrl  = (clusterControl_st*) clusterCtrlTable->GetTable() ;
      if (!clusterCtrl) 
	gMessMgr->Error() << "No  access to clusterControl table" << endm;
      else 
	{
	  mClusterControl = new StSsdClusterControl();
	  //	  mClusterControl -> setHighCut(clusterCtrl->highCut);  
	  mClusterControl -> setHighCut(3);  
	  mClusterControl -> setTestTolerance(clusterCtrl->testTolerance);
	  mClusterControl -> setClusterTreat(clusterCtrl->clusterTreat);
	  mClusterControl -> setAdcTolerance(clusterCtrl->adcTolerance);
	  mClusterControl -> setMatchMean(clusterCtrl->matchMean);
	  mClusterControl -> setMatchSigma(clusterCtrl->matchSigma);
	  mClusterControl -> printParameters();
	}      
      // to be replace by database reading when it will be filled.....
      St_DataSet *svtparams = GetInputDB("svt");
      St_DataSetIter       local(svtparams);
      m_noise2       = (St_ssdStripCalib     *)local("ssd/ssdStripCalib");
    }
  else // No access to databases -> read tables
    {
      // 		Create tables
      gMessMgr->Info() << " No access to databases so ...read tables" << endm;
      St_DataSet *svtparams = GetInputDB("svt");
      St_DataSetIter       local(svtparams);
      m_condition_db = (St_sdm_condition_db  *)local("ssd/sdm_condition_db");
      m_noise        = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");
      m_configuration= (St_ssdConfiguration  *)local("ssd/ssdConfiguration");
      m_wafpos       = (St_ssdWafersPosition *)local("ssd/ssdWafersPosition");
      m_noise2       = (St_ssdStripCalib     *)local("ssd/ssdStripCalib");
      m_dimensions   = (St_ssdDimensions     *)local("ssd/ssdDimensions");

      St_slsCtrl *control;
      control = (St_slsCtrl *)local("ssd/slsCtrl");
      if (!control) {
	gMessMgr->Error() << "No  access to slsCtrl table" << endm;
      }  

      St_clusterControl  *clusterCtrl;
      clusterCtrl = (St_clusterControl *)local("ssd/clusterControl");
      if (!clusterCtrl) {
	gMessMgr->Error() << "No  access to clusterControl table" << endm;
      }   
      if (!m_condition_db) {
	gMessMgr->Error() << "No  access to condition database" << endm;
      }   
      if (!m_noise) {
	gMessMgr->Error() << "No  access to noise condition" << endm;
      }
      if (!m_dimensions) {
	gMessMgr->Error() << "No  access to ssdDimensions table" << endm;
      }
      if (!m_configuration) {
	gMessMgr->Error() << "No  access to ssdConfiguration table" << endm;
      }
      if (!m_wafpos) {
	gMessMgr->Error() << "No  access to ssdWafersPosition table" << endm;
      }

      // Replace tables for dynamic parameters with default values
      mDynamicControl = new StSsdDynamicControl(control);
      mDynamicControl->printParameters();
      // Replace tables for control parameters
      mClusterControl = new StSsdClusterControl(clusterCtrl);
      mClusterControl->setHighCut(3);
      mClusterControl->printParameters();
      // End of Setting Cluster Control parameters
      if ((!mDynamicControl)||(!mClusterControl)) {
	gMessMgr->Error() << "No  access to control parameters" << endm;
      } 
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

// 		Create SCM histograms
  matchisto = new TH2S("matchingHisto","Matching Adc (1p-1n)",50,0,1000,50,0,1000);
  matchisto->SetXTitle("PSide ADC count");
  matchisto->SetYTitle("NSide ADC count");
  matchisto->SetZTitle("(1p-1n) hits");

  matchisto->SetTitleOffset(2,"X");
  matchisto->SetTitleOffset(2,"Y");
//   matchisto->SetTitleOffset(-1,"Z");

  matchisto->SetLabelSize(0.03,"X");
  matchisto->SetLabelSize(0.03,"Y");
  matchisto->SetLabelSize(0.03,"Z");

  matchisto->SetNdivisions(5,"X");
  matchisto->SetNdivisions(5,"Y");
  matchisto->SetNdivisions(10,"Z");
  orthoproj = new TH1S("ProjectionOrtho","Perfect Matching Deviation",80,-80,80);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In StSsdPointMaker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 
  St_spa_strip *spa_strip = (St_spa_strip *)GetDataSet("spa_strip");
  if (!spa_strip){
    gMessMgr->Warning("StSsdPointMaker: no input (fired strip for the SSD)");
    return kStErr;
  }
  
  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt);
  mCurrentEvent = (StEvent*) GetInputDS("StEvent");
  if(mCurrentEvent) mSsdHitColl = mCurrentEvent->ssdHitCollection();
  else              mSsdHitColl = 0;
  
  StDbTable* configTable = maccess -> addDbTable("ssdConfiguration");
  mDbMgr->fetchDbTable(configTable);
  ssdConfiguration_st *config  = (ssdConfiguration_st*) configTable->GetTable() ;
  if (!config) 
    gMessMgr->Error() << "No  access to ssdConfiguration database" << endm;
  
  StDbTable* positionTable = maccess -> addDbTable("ssdWafersPosition");
  mDbMgr->fetchDbTable(positionTable);
  ssdWafersPosition_st *position  = (ssdWafersPosition_st*) positionTable->GetTable() ;
  if (!position) 
    gMessMgr->Error() << "No  access to ssdWafersPosition database" << endm;

  StDbTable* dimensionsTable = maccess -> addDbTable("ssdDimensions");
  mDbMgr->fetchDbTable(dimensionsTable);
  ssdDimensions_st *dimensions  = (ssdDimensions_st*) dimensionsTable->GetTable() ;
  if (!dimensions) 
    gMessMgr->Error() << "No  access to ssdDimensions database" << endm;
  
  if ((!dimensions)||(!config)){
    gMessMgr->Error() << "No geometry or configuration parameters " << endm;
    return kStErr;
  }

  cout<<"#################################################"<<endl;
  cout<<"####     START OF NEW SSD POINT MAKER        ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StSsdBarrel *mySsd = new StSsdBarrel(dimensions,config);
  //mySsd->initLadders(m_wafpos); 
  mySsd->initLadders(position);
  int stripTableSize = mySsd->readStripFromTable(spa_strip);
  cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
  //  mySsd->writeNoiseToFile(spa_strip);
  mySsd->sortListStrip();
  //int noiseTableSize = mySsd->readNoiseFromTable(m_noise,mDynamicControl);
  int noiseTableSize = mySsd->readNoiseFromTable(m_noise2,mDynamicControl);
  cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
  int nClusterPerSide[2];
  nClusterPerSide[0] = 0;
  nClusterPerSide[1] = 0;
  mySsd->doSideClusterisation(nClusterPerSide,mClusterControl);
  cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
  cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
  mySsd->sortListCluster();
  debugUnPeu(mySsd);
  int nPackage = mySsd->doClusterMatching(dimensions,mClusterControl);
  cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
  mySsd->convertDigitToAnalog(mDynamicControl);
  mySsd->convertUFrameToOther(dimensions);
  int nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl);
  cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
  if(mSsdHitColl) 
    cout<<"####   -> "<<mSsdHitColl->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endl;
  else 
    cout<<" ######### NO SSD HITS WRITTEN INTO CONTAINER   ####"<<endl;
  scm_spt->Purge();
  cout<<"####        END OF SSD NEW POINT MAKER       ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  if (nSptWritten) res = kStOK;
 
  if(res!=kStOK){
    gMessMgr->Warning("StSsdPointMaker: no output");
    return kStWarn;
  }

  makeScmCtrlHistograms();
  
  return kStOK;
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScfCtrlHistograms()
{
  St_DataSetIter scf_iter(m_DataSet);
  St_scf_cluster *scf_cluster = 0;
  scf_cluster = (St_scf_cluster *) scf_iter.Find("scf_cluster"); 

// 		Fill histograms
  if (scf_cluster->GetNRows()){
    Int_t clustSide   = 0;  // pside = 0 et nside = 1 
    scf_cluster_st *dClus = scf_cluster->GetTable();
    Float_t convAdcToE = (mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip())/(pow(2.0,mDynamicControl->getNBitEncoding()));
    for (Int_t iScf = 0; iScf < scf_cluster->GetNRows(); iScf++, dClus++)
      {
	clustSide = ((dClus->id_cluster/10000)-(dClus->id_cluster/100000)*10);
	if(!clustSide)
	  {
	    noisDisP->Fill(dClus->noise_count/dClus->n_strip);
	    snRatioP->Fill((dClus->adc_count*dClus->n_strip)/dClus->noise_count);
	    stpClusP->Fill(dClus->n_strip);
	    totChrgP->Fill(convAdcToE*dClus->adc_count);
	  }
	else
	  {
	    noisDisN->Fill(dClus->noise_count/dClus->n_strip);
	    snRatioN->Fill((dClus->adc_count*dClus->n_strip)/dClus->noise_count);
	    stpClusN->Fill(dClus->n_strip);
	    totChrgN->Fill(convAdcToE*dClus->adc_count);
	  }
      }
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::writeScfCtrlHistograms()
{  

  ScfCtrlFile = new TFile("event/scfCtrl_histos.root","RECREATE");
  
  noisDisP->Write();
  snRatioP->Write();
  stpClusP->Write();
  totChrgP->Write();
  
  noisDisN->Write();
  snRatioN->Write();
  stpClusN->Write();
  totChrgN->Write();
    
  ScfCtrlFile->Close();
  
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScmCtrlHistograms()
{
  St_DataSetIter scm_iter(m_DataSet);
  St_scm_spt *scm_spt = 0;
  scm_spt = (St_scm_spt *) scm_iter.Find("scm_spt"); 

// 		Fill histograms 
  if (scm_spt->GetNRows()){
    scm_spt_st *dSpt = scm_spt->GetTable();
    Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getNBitEncoding())/(mDynamicControl->getPairCreationEnergy()*mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip());
    for (Int_t iScm = 0; iScm < scm_spt->GetNRows(); iScm++, dSpt++)
      {
	if (dSpt->id_match == 11)// case 11  		    
	  {
	    Float_t a = 0, b = 0;
	    a = convMeVToAdc*(dSpt->de[0]+dSpt->de[1]);
	    b = convMeVToAdc*(dSpt->de[0]-dSpt->de[1]);
	    matchisto->Fill(a,b);
	    orthoproj->Fill((b-a)/TMath::Sqrt(2.));
	  }
      }
//     matchisto->Draw();
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::writeScmCtrlHistograms()
{
  ScmCtrlFile = new TFile("event/scmCtrl_histos.root","RECREATE");

  matchisto->Write();
  orthoproj->Write();

  ScmCtrlFile->Close();
}

void StSsdPointMaker::debugUnPeu(StSsdBarrel *mySsd)
{
  int monladder,monwafer;
  monladder=12;
  monwafer=9;
  mySsd->debugUnPeu(monladder,monwafer);
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In StSsdPointMaker::Finish() ... "
                               << GetName() << endm; 
  writeScfCtrlHistograms();
  writeScmCtrlHistograms();
  return kStOK;
}

