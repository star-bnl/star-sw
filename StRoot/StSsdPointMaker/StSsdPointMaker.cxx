// $Id: StSsdPointMaker.cxx,v 1.31 2007/02/02 20:24:15 bouchet Exp $
//
// $Log: StSsdPointMaker.cxx,v $
// Revision 1.31  2007/02/02 20:24:15  bouchet
// WriteStripTuple method added, WriteScmTuple method updated
//
// Revision 1.30  2007/02/02 17:46:58  bouchet
// Few changes for the new Logger
//
// Revision 1.29  2007/01/16 18:01:52  bouchet
// Replace printf,cout,gMessMgr with LOG statements
//
// Revision 1.28  2006/10/16 16:27:49  bouchet
// Unify classes ; Methods for all classes (StSsdStrip, StSsdCluster, StSsdPoint) are now in StSsdUtil
//
// Revision 1.27  2006/09/15 21:03:14  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.26  2005/12/31 01:43:22  perev
// Mack/Upack simplified
//
// Revision 1.25  2005/12/20 10:35:51  lmartin
// ReadStrip method updated and some cosmetic changes
//
// Revision 1.24  2005/12/20 09:23:35  lmartin
// ssdStripCalib table read from the mysql db
//
// Revision 1.23  2005/09/30 14:28:30  lmartin
// add a 0 to myTime if GetTime()<100000
//
// Revision 1.22  2005/09/26 15:49:54  bouchet
// adding a method to the poInt_t maker to check which ssdStripCalib is picked
//
// Revision 1.21  2005/08/11 13:51:39  lmartin
// PrintStripDetails, PrintPackageDetails and PrintPointDetails methods added
//
// Revision 1.20  2005/06/24 10:19:46  lmartin
// preventing crashes if ssdStripCalib is missing
//
// Revision 1.19  2005/06/16 14:29:22  bouchet
// no more makeSsdPedestalHistograms() method
//
// Revision 1.18  2005/06/14 12:09:15  bouchet
// add a histo for the pedestal and new name of the class : SsdPoint
//
// Revision 1.14  2005/06/07 16:24:47  lmartin
// InitRun returns kStOk
//
// Revision 1.13  2005/06/07 12:04:46  reinnart
// Make Stuff moved to Initrun
//
// Revision 1.12  2005/06/07 11:55:08  reinnart
// Initrun and good database connection
//
// Revision 1.11  2005/04/25 14:13:23  bouchet
// new method makeScfCtrlHistograms and makeScmCtrlHistograms and Clusternoise is coded as a float
//
// Revision 1.10  2005/04/23 08:56:20  lmartin
// physics and pedestal data processing separated
//
// Revision 1.9  2005/03/23 16:07:26  lmartin
// PrintClusterSummary and PrintPointSummary methods added
//
// Revision 1.8  2005/03/22 13:46:43  lmartin
// PrintStripSummary method added
//
// Revision 1.7  2005/03/22 10:38:51  lmartin
// HighCut value taken from the db
//
// Revision 1.6  2005/03/18 13:35:40  lmartin
// Partly missing cvs header added
//
// Revision 1.5  2005/03/18 10:16:34  lmartin
// positionSize argument added to the initLadders method
//
// Revision 1.4  2004/11/04 15:10:19  croy
// use the IAttr(".histos") to control histogramming and modification of the SsdHitCollection creation
//
// Revision 1.3  2004/08/13 07:07:23  croy
// Updates to read SSD databases
//
// Revision 1.2  2004/07/20 14:04:02  croy
// Use of new database structure definitions related to SSD config
//
// Revision 1.1  2004/03/12 06:12:37  jeromel
// Peer review closed. Yuri/Frank.
//
// Revision 1.3  2002/03/25 20:13:05  hippolyt
// Merged the two former makers 
//
//
#include "StSsdPointMaker.h"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "StChain.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "TNtuple.h"

#include "StSsdUtil/StSsdPoint.hh"
#include "StSsdUtil/StSsdPackage.hh"
#include "StSsdUtil/StSsdCluster.hh"
#include "StSsdUtil/StSsdStripList.hh"
#include "StSsdUtil/StSsdClusterList.hh"
#include "StSsdUtil/StSsdPointList.hh"
#include "StSsdUtil/StSsdPackageList.hh"
#include "StSsdUtil/StSsdWafer.hh"
#include "StSsdUtil/StSsdLadder.hh"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdUtil/StSsdStrip.hh"
#include "tables/St_spa_strip_Table.h" 
#include "tables/St_ssdPedStrip_Table.h"
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
#include "tables/St_ssdStripCalib_Table.h"

#include "StEvent.h"
#include "StSsdHitCollection.h"

ClassImp(StSsdPointMaker)

//_____________________________________________________________________________
  Int_t StSsdPointMaker::Init(){
  LOG_INFO << "Init() : Defining the histograms" << endm;
  
  if (IAttr(".histos")) {
    noisDisP = new TH1F("Noise_p","Noise Distribution",250,0,25);
    snRatioP = new TH1F("SN_p","Signal/Noise (p)",200,0,200);
    stpClusP = new TH1F("NumberOfStrips_p","Strips per Cluster",8,0,8);
    totChrgP = new TH1F("ChargeElectron_p","Total Cluster Charge",100,0,300000);
    
    noisDisN = new TH1F("Noise_n","Noise Distribution",250,0,25);
    snRatioN = new TH1F("SN_n","Signal/Noise",200,0,200);
    stpClusN = new TH1F("NumberOfStrips_n","Strips per Cluster",8,0,8);
    totChrgN = new TH1F("ChargeElectron_n","Total Cluster Charge",100,0,300000);
    
    // 		Create SCM histograms
    matchisto = new TH2S("matchingHisto","Matching Adc (1p-1n)",500,0,1000,500,0,1000);
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

    orthoproj = new TH1S("ProjectionOrtho","Perfect Matching Deviation",320,-80,80);
    
    kind = new TH1S("kind","Kind of hits",11,0,11);
    kind->SetXTitle("kind");
    kind->SetYTitle("entries");
    kind->SetTitleOffset(2,"X");
    kind->SetTitleOffset(2,"Y");
    
    matchisto_1 = new TH2S("matchingHisto_1","Matching Adc (1p-1n) for ladder 1",50,0,1000,50,0,1000);
    matchisto_1->SetXTitle("PSide ADC count");
    matchisto_1->SetYTitle("NSide ADC count");
    matchisto_1->SetZTitle("(1p-1n) hits");

    matchisto_2 = new TH2S("matchingHisto_2","Matching Adc (1p-1n) for ladder 2",50,0,1000,50,0,1000);
    matchisto_2->SetXTitle("PSide ADC count");
    matchisto_2->SetYTitle("NSide ADC count");
    matchisto_2->SetZTitle("(1p-1n) hits");

    matchisto_3 = new TH2S("matchingHisto_3","Matching Adc (1p-1n) for ladder 3",50,0,1000,50,0,1000);
    matchisto_3->SetXTitle("PSide ADC count");
    matchisto_3->SetYTitle("NSide ADC count");
    matchisto_3->SetZTitle("(1p-1n) hits");

    matchisto_4 = new TH2S("matchingHisto_4","Matching Adc (1p-1n) for ladder 4",50,0,1000,50,0,1000);
    matchisto_4->SetXTitle("PSide ADC count");
    matchisto_4->SetYTitle("NSide ADC count");
    matchisto_4->SetZTitle("(1p-1n) hits");
    
    matchisto_5 = new TH2S("matchingHisto_5","Matching Adc (1p-1n) for ladder 5",50,0,1000,50,0,1000);
    matchisto_5->SetXTitle("PSide ADC count");
    matchisto_5->SetYTitle("NSide ADC count");
    matchisto_5->SetZTitle("(1p-1n) hits");

    matchisto_6 = new TH2S("matchingHisto_6","Matching Adc (1p-1n) for ladder 6",50,0,1000,50,0,1000);
    matchisto_6->SetXTitle("PSide ADC count");
    matchisto_6->SetYTitle("NSide ADC count");
    matchisto_6->SetZTitle("(1p-1n) hits");

    matchisto_7 = new TH2S("matchingHisto_7","Matching Adc (1p-1n) for ladder 7",50,0,1000,50,0,1000);
    matchisto_7->SetXTitle("PSide ADC count");
    matchisto_7->SetYTitle("NSide ADC count");
    matchisto_7->SetZTitle("(1p-1n) hits");

    matchisto_8 = new TH2S("matchingHisto_8","Matching Adc (1p-1n) for ladder 8",50,0,1000,50,0,1000);
    matchisto_8->SetXTitle("PSide ADC count");
    matchisto_8->SetYTitle("NSide ADC count");
    matchisto_8->SetZTitle("(1p-1n) hits");
    
    matchisto_9 = new TH2S("matchingHisto_9","Matching Adc (1p-1n) for ladder 9",50,0,1000,50,0,1000);
    matchisto_9->SetXTitle("PSide ADC count");
    matchisto_9->SetYTitle("NSide ADC count");
    matchisto_9->SetZTitle("(1p-1n) hits");
    
    matchisto_10 = new TH2S("matchingHisto_10","Matching Adc (1p-1n) for ladder 10",50,0,1000,50,0,1000);
    matchisto_10->SetXTitle("PSide ADC count");
    matchisto_10->SetYTitle("NSide ADC count");
    matchisto_10->SetZTitle("(1p-1n) hits");
    
    matchisto_11 = new TH2S("matchingHisto_11","Matching Adc (1p-1n) for ladder 11",50,0,1000,50,0,1000);
    matchisto_11->SetXTitle("PSide ADC count");
    matchisto_11->SetYTitle("NSide ADC count");
    matchisto_11->SetZTitle("(1p-1n) hits");

    matchisto_12 = new TH2S("matchingHisto_12","Matching Adc (1p-1n) for ladder 12",50,0,1000,50,0,1000);
    matchisto_12->SetXTitle("PSide ADC count");
    matchisto_12->SetYTitle("NSide ADC count");
    matchisto_12->SetZTitle("(1p-1n) hits");

    matchisto_13 = new TH2S("matchingHisto_13","Matching Adc (1p-1n) for ladder 13",50,0,1000,50,0,1000);
    matchisto_13->SetXTitle("PSide ADC count");
    matchisto_13->SetYTitle("NSide ADC count");
    matchisto_13->SetZTitle("(1p-1n) hits");

    matchisto_14 = new TH2S("matchingHisto_14","Matching Adc (1p-1n) for ladder 14",50,0,1000,50,0,1000);
    matchisto_14->SetXTitle("PSide ADC count");
    matchisto_14->SetYTitle("NSide ADC count");
    matchisto_14->SetZTitle("(1p-1n) hits");
    
    matchisto_15 = new TH2S("matchingHisto_15","Matching Adc (1p-1n) for ladder 15",50,0,1000,50,0,1000);
    matchisto_15->SetXTitle("PSide ADC count");
    matchisto_15->SetYTitle("NSide ADC count");
    matchisto_15->SetZTitle("(1p-1n) hits");

    matchisto_16 = new TH2S("matchingHisto_16","Matching Adc (1p-1n) for ladder 16",50,0,1000,50,0,1000);
    matchisto_16->SetXTitle("PSide ADC count");
    matchisto_16->SetYTitle("NSide ADC count");
    matchisto_16->SetZTitle("(1p-1n) hits");

    matchisto_17 = new TH2S("matchingHisto_17","Matching Adc (1p-1n) for ladder 17",50,0,1000,50,0,1000);
    matchisto_17->SetXTitle("PSide ADC count");
    matchisto_17->SetYTitle("NSide ADC count");
    matchisto_17->SetZTitle("(1p-1n) hits");

    matchisto_18 = new TH2S("matchingHisto_18","Matching Adc (1p-1n) for ladder 18",50,0,1000,50,0,1000);
    matchisto_18->SetXTitle("PSide ADC count");
    matchisto_18->SetYTitle("NSide ADC count");
    matchisto_18->SetZTitle("(1p-1n) hits");
    
    matchisto_19 = new TH2S("matchingHisto_19","Matching Adc (1p-1n) for ladder 19",50,0,1000,50,0,1000);
    matchisto_19->SetXTitle("PSide ADC count");
    matchisto_19->SetYTitle("NSide ADC count");
    matchisto_19->SetZTitle("(1p-1n) hits");

    matchisto_20 = new TH2S("matchingHisto_20","Matching Adc (1p-1n) for ladder 20",50,0,1000,50,0,1000);
    matchisto_20->SetXTitle("PSide ADC count");
    matchisto_20->SetYTitle("NSide ADC count");
    matchisto_20->SetZTitle("(1p-1n) hits");

    flag = 0;             // flag=0->the tuple are not filled
    //DeclareNtuple(&flag); // flag=1->the tuple are filled :this lign to decomment or not
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::InitRun(Int_t runumber)
{
  //    mDbMgr = StDbManager::Instance();
  //    mDbMgr->setVerbose(false);

  //    maccess = mDbMgr->initConfig(dbGeometry,dbSsd);


  TDataSet *DbConnector = GetDataBase("Geometry/ssd");

  if (DbConnector) 
    {
      LOG_INFO << "InitRun : SSD Databases respond " << endm;
      St_slsCtrl* slsCtrlTable = (St_slsCtrl*) DbConnector->Find("slsCtrl");
      slsCtrl_st*      control      = (slsCtrl_st*) slsCtrlTable->GetTable();
      if (!control){
	LOG_ERROR << "InitRun : No access to slsCtrl table" << endm;}
      else
	{
	  mDynamicControl = new StSsdDynamicControl();
	  mDynamicControl -> setnElectronInAMip(control->nElectronInAMip);
	  mDynamicControl -> setadcDynamic(control->adcDynamic);
	  mDynamicControl -> seta128Dynamic(control->a128Dynamic);
	  mDynamicControl -> setnbitEncoding(control->nbitEncoding);
	  mDynamicControl -> setnstripInACluster(control->nstripInACluster);
	  mDynamicControl -> setpairCreationEnergy(control->pairCreationEnergy);
	  mDynamicControl -> setparDiffP(control->parDiffP);
	  mDynamicControl -> setparDiffN(control->parDiffN);
	  mDynamicControl -> setparIndRightP(control->parIndRightP);
	  mDynamicControl -> setparIndRightN(control->parIndRightN);
	  mDynamicControl -> setparIndLeftP(control->parIndLeftP);
	  mDynamicControl -> setparIndLeftN(control->parIndLeftN);
	  mDynamicControl -> setdaqCutValue(control->daqCutValue);
	  mDynamicControl -> printParameters();
	}
      St_clusterControl* clusterCtrlTable = (St_clusterControl*) DbConnector->Find("clusterControl");
      clusterControl_st *clusterCtrl  = (clusterControl_st*) clusterCtrlTable->GetTable() ;
      if (!clusterCtrl){
	LOG_ERROR << "InitRun : No access to clusterControl table" << endm;}
      else 
	{
	  mClusterControl = new StSsdClusterControl();
	  mClusterControl -> setHighCut(clusterCtrl->highCut);  
	  mClusterControl -> setTestTolerance(clusterCtrl->testTolerance);
	  mClusterControl -> setClusterTreat(clusterCtrl->clusterTreat);
	  mClusterControl -> setAdcTolerance(clusterCtrl->adcTolerance);
	  mClusterControl -> setMatchMean(clusterCtrl->matchMean);
	  mClusterControl -> setMatchSigma(clusterCtrl->matchSigma);
	  mClusterControl -> printParameters();
	}    
      
      //      TDataSet *svtparams = GetInputDB("svt");
      //      TDataSetIter local(svtparams);
      //      m_noise2       = (St_ssdStripCalib*)local("ssd/ssdStripCalib");
      
      TDataSet *CalibDbConnector = GetDataBase("Calibrations/ssd");
      if (!CalibDbConnector) {LOG_ERROR <<"InitRun: Can not found the calibration db.."<<endm;}
      else
	m_noise2 = (St_ssdStripCalib*) CalibDbConnector->Find("ssdStripCalib");
      
      if (!m_noise2){
        LOG_ERROR << "InitRun : No access to ssdStripCalib - will use the default noise and pedestal values" << endm;}
      if(m_noise2){
	LOG_WARN<<"InitRun : printing few pedestal/noise values"<<endm;
	Read_Strip(m_noise2); 
      }
      // Get once the information for configuration, wafersposition and dimensions
      St_ssdConfiguration* configTable = (St_ssdConfiguration*) DbConnector->Find("ssdConfiguration"); 
      config  = (ssdConfiguration_st*) configTable->GetTable() ;
      if (!config) {
        LOG_FATAL << "InitRun : No access to ssdConfiguration database" << endm;
	return kStFatal;
      }
      
      position = (St_ssdWafersPosition*) DbConnector->Find("ssdWafersPosition");
      if (!position) {
        LOG_FATAL << "InitRun : No access to ssdWafersPosition database" << endm;
	return kStFatal;
      }
      
      St_ssdDimensions* dimensionsTable = (St_ssdDimensions*) DbConnector->Find("ssdDimensions");
      dimensions  = (ssdDimensions_st*) dimensionsTable->GetTable() ;
      if (!dimensions) 
        {LOG_ERROR << "InitRun : No access to ssdDimensions database" << endm;}
      
      if ((!dimensions)||(!config)){
        LOG_ERROR << "InitRun : No geometry or configuration parameters " << endm;
        return kStErr;
      } 
    }
  else // No access to databases -> read tables
    {
      LOG_ERROR << "InitRun : No connection to the database in StSsdPointMaker" << endm;
    }
  return kStOk;
}

//_____________________________________________________________________________

void StSsdPointMaker::DeclareNtuple(Int_t *flag){
  mFile = new TFile("PhysicsFile.root","RECREATE");
  string varlist2 = "pulseP:pulseN:ladder:wafer:case:xg:yg:zg:flag";
  mHitNtuple     = new TNtuple("PhysNTuple","Physics Ntuple",varlist2.c_str());
  nFile = new TFile("Clusters.root","RECREATE");
  string varlist3 = "side:ladder:wafer:nstrip:snratio:noise:first_strip:TotAdc";
  nHitNtuple     = new TNtuple("ClusTuple","Clusters Ntuple",varlist3.c_str()); 
  qFile = new TFile("Strips_in_hits.root","RECREATE");
  string varlist4     = "side:ladder:wafer:nstrip:pedestal:signal:noise:snratio";
  qHitNtuple          = new TNtuple("Strips_hits","Strips in hitsNtuple",varlist4.c_str());
  *flag =  1;
}

//_____________________________________________________________________________
Int_t StSsdPointMaker::Make()
{
  LOG_DEBUG << Form("Make : begin")<< endm;
    // 		Create output tables
  Int_t res = 0; 
  char* myLabel  = new char[100];
  char* myTime = new char[100]; 
  char* myDate = new char[100];
  
  if (GetTime()>99999)
    sprintf(myTime,"%d",GetTime());
  else
    sprintf(myTime,"0%d",GetTime());
  sprintf(myDate,"%d%s",GetDate(),".");
  sprintf(myLabel,"%s%s",myDate,myTime);
  
  // two different tables can exist (physics data or pedestal data)
  
  TDataSet *SpaStrip = GetDataSet("SpaStrip");
  if (! SpaStrip) {
    LOG_ERROR << "Make : no input data set, wrong chain option" << endm;
    return kStErr;
  }
  St_spa_strip *spa_strip = dynamic_cast<St_spa_strip *> (SpaStrip->Find("spa_strip"));
  St_ssdPedStrip *spa_ped_strip = dynamic_cast<St_ssdPedStrip *> (SpaStrip->Find("ssdPedStrip"));
  
  if (!spa_strip || spa_strip->GetNRows()==0){
    {
      LOG_WARN << "Make : no input (fired strip for the SSD)"<<endm;
      LOG_WARN <<"Make : looking for a pedestal/noise tables"<<endm;
    }
    if (!spa_ped_strip || spa_ped_strip->GetNRows()==0) {
      LOG_WARN<<"Make : no pedestal/noise data..."<<endm;
      return kStWarn;
    }
    else 
      { LOG_WARN<<"Make : pedestal/noise data found : "<<spa_ped_strip->GetNRows()<<endm;}
  }
  
  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt); 
  
  St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",5000);//09/13
  m_DataSet->Add(scf_cluster);
  
  mCurrentEvent = (StEvent*) GetInputDS("StEvent");
  if(mCurrentEvent) 
    {
      mSsdHitColl = mCurrentEvent->ssdHitCollection();
      if (!mSsdHitColl) {
	LOG_WARN << "Make : The SSD hit collection does not exist  - creating a new one" << endm;
	mSsdHitColl = new StSsdHitCollection;
	mCurrentEvent->setSsdHitCollection(mSsdHitColl);
      }
    }
  else              
    mSsdHitColl = 0;
  
  LOG_INFO<<"#################################################"<<endm;
  LOG_INFO<<"####     START OF NEW SSD POINT MAKER        ####"<<endm;
  LOG_INFO<<"####        SSD BARREL INITIALIZATION        ####"<<endm;
  LOG_INFO<<"####          BEGIN INITIALIZATION           ####"<<endm; 
  StSsdBarrel *mySsd = new StSsdBarrel(dimensions,config);
  //mySsd->initLadders(m_wafpos); 
  mySsd->initLadders(position);
  //The full SSD object is built only if we are processing physics data
  if((! spa_ped_strip || spa_ped_strip->GetNRows()==0) && (spa_strip->GetNRows()!=0))
    {
      Int_t stripTableSize = mySsd->readStripFromTable(spa_strip);
      LOG_INFO<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endm;
      mySsd->sortListStrip();
      PrintStripSummary(mySsd);
      Int_t noiseTableSize = 0;      
      if (!m_noise2) 
	{LOG_WARN << "Make : No pedestal and noise values (ssdStripCalib table missing), will use default values" <<endm;}
      else
	noiseTableSize = mySsd->readNoiseFromTable(m_noise2,mDynamicControl);
      LOG_INFO<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endm;
      Int_t nClusterPerSide[2];
      nClusterPerSide[0] = 0;
      nClusterPerSide[1] = 0;
      mySsd->doSideClusterisation(nClusterPerSide,mClusterControl);
      LOG_INFO<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endm;
      LOG_INFO<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endm;
      mySsd->sortListCluster();
      
      Int_t nClusterWritten = mySsd->writeClusterToTable(scf_cluster,spa_strip);
      LOG_INFO<<"####   -> "<<nClusterWritten<<" CLUSTERS WRITTEN INTO TABLE       ####"<<endm;
      PrintClusterSummary(mySsd);
      //PrintStripDetails(mySsd,8310);
      //PrintClusterDetails(mySsd,8310); 
      makeScfCtrlHistograms(mySsd);
      //debugUnPeu(mySsd);
      Int_t nPackage = mySsd->doClusterMatching(dimensions,mClusterControl);
      LOG_INFO<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endm;
      mySsd->convertDigitToAnalog(mDynamicControl);
      mySsd->convertUFrameToOther(dimensions);
      PrintPointSummary(mySsd);
      
      //Int_t nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl);
      /*
	for(Int_t i=1;i<=20;i++)
	{
	for(Int_t j=1;j<=16;j++)
	{
	PrintStripDetails(mySsd,7000+(100*j)+i);
	PrintClusterDetails(mySsd,7000+(100*j)+i);
	PrintPointDetails(mySsd,7000+(100*j)+i);
	PrintPackageDetails(mySsd,7000+(100*j)+i);
	}
	}
      */
      Int_t nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl,scf_cluster);
      LOG_INFO<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endm;
      
      if(mSsdHitColl) {
	LOG_INFO<<"####   -> "<<mSsdHitColl->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endm;}
      else {
	LOG_INFO<<" ######### NO SSD HITS WRITTEN INTO CONTAINER   ####"<<endm;
	scm_spt->Purge();
	LOG_INFO<<"####        END OF SSD NEW POINT MAKER       ####"<<endm;
	LOG_INFO<<"#################################################"<<endm;
	makeScmCtrlHistograms(mySsd);
      }
      if(flag==1){ 
	WriteStripTuple(mySsd);
	WriteScfTuple(mySsd);
	WriteScmTuple(mySsd);
      }
      if (nSptWritten) res = kStOK;
    }
  else
    {
      if((spa_strip->GetNRows()==0)&&(spa_ped_strip && spa_ped_strip->GetNRows()!=0))
	{ 
	  LOG_INFO <<"###### WRITING SSD PEDESTAL HISTOGRAMS##########"<<endm;
	  mySsd->writeNoiseToFile(spa_ped_strip,myLabel);
	}
    }
  delete mySsd;
  
  if(res!=kStOK){
    LOG_WARN <<"Make : no output" << endm;;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScfCtrlHistograms(StSsdBarrel *mySsd)
{
  
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t found;
  Float_t convAdcToE = (mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip())/(pow(2.0,mDynamicControl->getnbitEncoding()));
  found=0;
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	  StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	  while (pClusterP)
	    {  
	      stpClusP->Fill(pClusterP->getClusterSize());
	      totChrgP->Fill(convAdcToE*pClusterP->getTotAdc());
	      noisDisP->Fill(pClusterP->getTotNoise()/pClusterP->getClusterSize());
	      snRatioP->Fill((pClusterP->getTotAdc()*pClusterP->getClusterSize())/pClusterP->getTotNoise());
	      pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);	
	    }
	    StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	    while (pClusterN)
	      {
		stpClusN->Fill(pClusterN->getClusterSize());
		totChrgN->Fill(convAdcToE*pClusterN->getTotAdc());
		noisDisN->Fill(pClusterN->getTotNoise()/(3e-33+pClusterN->getClusterSize()));
		snRatioN->Fill((pClusterN->getTotAdc()*pClusterN->getClusterSize())/(3e-33+pClusterN->getTotNoise()));	
		pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
	      }	  
      }
    }
}


//_____________________________________________________________________________

void StSsdPointMaker::makeScmCtrlHistograms(StSsdBarrel *mySsd)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t found;
  Int_t conversion[11]={11,12,21,13,31,221,222,223,23,32,33};
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  found=0;
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	  //LOG_INFO() <<"StSsdPointMaker::PrintPointDetails() - No hit in this wafer "<< endm;  
	}
	else {
	  StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
	  while (pSpt){
	    if (pSpt->getNMatched() == 11)// case 11  		    
	      {
		Float_t a = 0, b = 0;
		a = convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1));
		b = convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1));
		matchisto->Fill(a,b);
		orthoproj->Fill((b-a)/TMath::Sqrt(2.));
		switch(i+1)
		  {
		  case 1:
		    {
		      matchisto_1->Fill(a,b);
		      break;
		    } 
		  case 2:
		    {
		      matchisto_2->Fill(a,b);
		      break;
		    } 
		  case 3:
		    {
		      matchisto_3->Fill(a,b);
		      break;
		    } 
		  case 4:
		    {
		      matchisto_4->Fill(a,b);
		      break;
		    } 
		  case 5:
		    {
		      matchisto_5->Fill(a,b);
		      break;
		    } 
		  case 6:
		    {
		      matchisto_6->Fill(a,b);
		      break;
		    } 
		  case 7:
		    {
		      matchisto_7->Fill(a,b);
		      break;
		    } 
		  case 8:
		    {
		      matchisto_8->Fill(a,b);
		      break;
		    } 
		  case 9:
		    {
		      matchisto_9->Fill(a,b);
		      break;
		    } 
		  case 10:
		    {
		      matchisto_10->Fill(a,b);
		      break;
		    } 
		  case 11:
		    {
		      matchisto_11->Fill(a,b);
		      break;
		    } 
		  case 12:
		    {
		      matchisto_12->Fill(a,b);
		      break;
		    } 
		  case 13:
		    {
		      matchisto_13->Fill(a,b);
		      break;
		    } 
		  case 14:
		    {
		      matchisto_14->Fill(a,b);
		      break;
		    } 
		  case 15:
		    {
		      matchisto_15->Fill(a,b);
		      break;
		    } 
		  case 16:
		    {
		      matchisto_16->Fill(a,b);
		      break;
		    } 
		  case 17:
		    {
		      matchisto_17->Fill(a,b);
		      break;
		    } 
		  case 18:
		    {
		      matchisto_18->Fill(a,b);
		      break;
		    } 
		  case 19:
		    {
		      matchisto_19->Fill(a,b);
		      break;
		    } 
		  case 20:
		    {
		      matchisto_20->Fill(a,b);
		      break;
		    } 
		  }
	      }
	    
	    for(Int_t k=0;k<=11;k++)
	      {
		if(pSpt->getNMatched()==conversion[k])
		  {
		    kind->Fill(k);
		  }
	      }
	    
	    pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
	  } 
	}
      }
    }
}


 //_____________________________________________________________________________

void StSsdPointMaker::PrintStripSummary(StSsdBarrel *mySsd)
{
  Int_t ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  Int_t ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySsd->mLadders[i]->mWafers[j]->getStripN()->getSize();
      }
    }
  
  LOG_INFO <<"PrintStripSummary : Number of raw data in the SSD" << endm;
  LOG_INFO << "PrintStripSummary : Active Ladders : ";
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  LOG_INFO << "PrintStripSummary : Counts (p-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountP[i];
    }
  *gMessMgr<<endm;
  LOG_INFO << "PrintStripSummary : Counts (n-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountN[i];
    }
  *gMessMgr<<endm;
}

//_____________________________________________________________________________
void StSsdPointMaker::debugUnPeu(StSsdBarrel *mySsd)
{
  Int_t monladder,monwafer;
  monladder=7;
  monwafer=6;
  mySsd->debugUnPeu(monladder,monwafer);
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintClusterSummary(StSsdBarrel *mySsd)
{
  Int_t ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  Int_t ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize();
      }
    }
  
  LOG_INFO <<"PrintClusterSummary : Number of clusters in the SSD" << endm;
  LOG_INFO << "PrintClusterSummary : Active Ladders : ";
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (p-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountP[i];
    }
  *gMessMgr<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (n-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountN[i];
    }
  *gMessMgr<<endm;
}
//_____________________________________________________________________________
void StSsdPointMaker::PrintPointSummary(StSsdBarrel *mySsd)
{
  Int_t ladderCount[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  Int_t ladderCount11[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCount[i]=ladderCount[i]+mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize();
	StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
	while (pSpt){	
	  if (pSpt->getNMatched()==11) ladderCount11[i]++;
	  pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
	}
      }
    }
  
  LOG_INFO<<"PrintPointSummary : Number of hits in the SSD" << endm;
  LOG_INFO<< "PrintPointSummary : Active Ladders : ";
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  LOG_INFO << "PrintPointSummary : Counts         : ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCount[i];
    }
  *gMessMgr<<endm;
  LOG_INFO << "PrintPointSummary : Counts  (11)   : ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCount11[i];
    }
  *gMessMgr<<endm;
}
//_____________________________________________________________________________
void StSsdPointMaker::WriteStripTuple(StSsdBarrel *mySsd)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	StSsdStrip *pStripP = mySsd->mLadders[i]->mWafers[j]->getStripP()->first();
	while (pStripP){
	  
	  Strips_hits[0] = 0;
	  Strips_hits[1] = i+1;
	  Strips_hits[2] = j+1;
	  Strips_hits[3] = pStripP->getNStrip();
	  Strips_hits[4] = pStripP->getPedestal();
	  Strips_hits[5] = pStripP->getDigitSig();
	  Strips_hits[6] = pStripP->getSigma();
	  Strips_hits[7] = (float)(pStripP->getDigitSig()/pStripP->getSigma());
	  qHitNtuple->Fill(Strips_hits);
	  pStripP    = mySsd->mLadders[i]->mWafers[j]->getStripP()->next(pStripP);
	}
	StSsdStrip *pStripN = mySsd->mLadders[i]->mWafers[j]->getStripN()->first();
	while (pStripN){
	  Strips_hits[0] = 1;
	  Strips_hits[1] = i+1;
	  Strips_hits[2] = j+1;
	  Strips_hits[3] = pStripN->getNStrip();
	  Strips_hits[4] = pStripN->getPedestal();
	  Strips_hits[5] = pStripN->getDigitSig();
	  Strips_hits[6] = pStripN->getSigma();
	  Strips_hits[7] = (float)(pStripN->getDigitSig()/pStripN->getSigma());
	  qHitNtuple->Fill(Strips_hits);	   
	  pStripN    = mySsd->mLadders[i]->mWafers[j]->getStripN()->next(pStripN);
	}
      }
    }
}

//_____________________________________________________________________________
void StSsdPointMaker::WriteScfTuple(StSsdBarrel *mySsd)
{

  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t found;
  found=0;
    for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	while (pClusterP)
	  {   
		ClusterNtuple[0]=0;
		ClusterNtuple[1]=i+1;
		ClusterNtuple[2]=j+1;
		ClusterNtuple[3]=pClusterP->getClusterSize();
		ClusterNtuple[4]=((pClusterP->getTotAdc()*pClusterP->getClusterSize())/pClusterP->getTotNoise());
		ClusterNtuple[5]=pClusterP->getTotNoise()/pClusterP->getClusterSize();
		ClusterNtuple[6]=pClusterP->getFirstStrip();
		ClusterNtuple[7]=pClusterP->getTotAdc();
		pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);
		nHitNtuple->Fill(ClusterNtuple);	
	      }
	    StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	    while (pClusterN)
	      {	
		ClusterNtuple[0]=1;
		ClusterNtuple[1]=i+1;
		ClusterNtuple[2]=j+1;
		ClusterNtuple[3]=pClusterN->getClusterSize();
		ClusterNtuple[4]=((pClusterN->getTotAdc()*pClusterN->getClusterSize())/pClusterN->getTotNoise());
		ClusterNtuple[5]=pClusterN->getTotNoise()/pClusterN->getClusterSize();
		ClusterNtuple[6]=pClusterN->getFirstStrip();
		ClusterNtuple[7]=pClusterN->getTotAdc();	
		pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
		nHitNtuple->Fill(ClusterNtuple);
	      }	  
	  }
      }
    }
//_____________________________________________________________________________
void StSsdPointMaker::WriteScmTuple(StSsdBarrel *mySsd)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t found;
  Int_t conversion[11]={11,12,21,13,31,221,222,223,23,32,33}; 
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  found=0;
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	  if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	  }
	  else {
	    StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
	    while (pSpt){
	      Float_t a = 0, b = 0;
	      a = convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1));
	      b = convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1));
	      hitNtuple[0]=a;
	      hitNtuple[1]=b;
	      hitNtuple[2]=i+1;
	      hitNtuple[3]=j+1;
	      for(Int_t k=0;k<=11;k++)
		{
		  if(pSpt->getNMatched()==conversion[k])
		    {
		      hitNtuple[4]=k; 
		    }
		    }
	      hitNtuple[5]=pSpt->getXg(0);
	      hitNtuple[6]=pSpt->getXg(1);
	      hitNtuple[7]=pSpt->getXg(2);
	      hitNtuple[8]=pSpt->getFlag();
	      mHitNtuple->Fill(hitNtuple);		 
	      pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		} 
	    }
	  }
      }
    }


//_____________________________________________________________________________
void StSsdPointMaker::PrintStripDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;
  LOG_DEBUG <<"PrintStripDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side strip informations
          if (mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize()==0) {
            LOG_DEBUG <<"PrintStripDetails() - No strip on the P-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintStripDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize()<<" strip(s) on the P-side of this wafer "<< endm;  
            LOG_DEBUG<<"PrintStripDetails() - Strip/Adc/Ped/Noise"<< endm;  
            StSsdStrip *pStripP = mySsd->mLadders[i]->mWafers[j]->getStripP()->first();
            while (pStripP){
              LOG_DEBUG<<"PrintStripDetails() - "
                              <<pStripP->getNStrip()<<" "
                              <<pStripP->getDigitSig()<<" "
                              <<pStripP->getPedestal()<<" "
                              <<pStripP->getSigma()<<" "
                              <<endm;  
              pStripP    = mySsd->mLadders[i]->mWafers[j]->getStripP()->next(pStripP);
            }
	  }
          //Looking for the N-side strip informations
          if (mySsd->mLadders[i]->mWafers[j]->getStripN()->getSize()==0) {
            LOG_DEBUG <<"PrintStripDetails() - No strip on the N-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintStripDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getStripN()->getSize()<<" strip(s) on the N-side of this wafer "<< endm;  
            LOG_DEBUG <<"StSsdPointMaker::PrintStripDetails() - Strip/Adc/Ped/Noise"<< endm;  
            StSsdStrip *pStripN = mySsd->mLadders[i]->mWafers[j]->getStripN()->first();
            while (pStripN){
              LOG_DEBUG<<"PrintStripDetails() - "
                              <<pStripN->getNStrip()<<" "
                              <<pStripN->getDigitSig()<<" "
                              <<pStripN->getPedestal()<<" "
                              <<pStripN->getSigma()<<" "
                              <<endm;  
              pStripN    = mySsd->mLadders[i]->mWafers[j]->getStripN()->next(pStripN);
            }     
          }
        }
      }
    }

  if (found==0) {LOG_DEBUG <<"PrintStripDetails() - Wafer not found !!!"<<endm;}
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintClusterDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;
  LOG_DEBUG <<"PrintClusterDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side cluster informations
          if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
            LOG_DEBUG <<"PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintClusterDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<" cluster(s) on the P-side of this wafer "<< endm;  
            LOG_DEBUG<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
            while (pClusterP){
              LOG_DEBUG<<"PrintClusterDetails() - "
                              <<pClusterP->getNCluster()<<" "
                              <<pClusterP->getFlag()<<" "
                              <<pClusterP->getClusterSize()<<" "
                              <<pClusterP->getFirstStrip()<<" "
                              <<pClusterP->getStripMean()<<" "
                              <<pClusterP->getTotAdc()<<" "
                              <<pClusterP->getFirstAdc()<<" "
                              <<pClusterP->getLastAdc()<<" "
			      <<pClusterP->getTotNoise()<<" "
                              <<endm;  
              pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);
            }
	  }
          //Looking for the N-side cluster informations
          if (mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
            LOG_DEBUG <<"PrintClusterDetails() - No cluster on the N-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintClusterDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<" cluster(s) on the N-side of this wafer "<< endm;  
            LOG_DEBUG<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
            while (pClusterN){
              LOG_DEBUG<<"PrintClusterDetails() - "
                              <<pClusterN->getNCluster()<<" "
                              <<pClusterN->getFlag()<<" "
                              <<pClusterN->getClusterSize()<<" "
                              <<pClusterN->getFirstStrip()<<" "
                              <<pClusterN->getStripMean()<<" "
                              <<pClusterN->getTotAdc()<<" "
                              <<pClusterN->getFirstAdc()<<" "
                              <<pClusterN->getLastAdc()<<" "
			      <<pClusterN->getTotNoise()<<" "
                              <<endm;  
              pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
            }     
          }
        }
      }
    }

  if (found==0){ LOG_DEBUG <<"PrintClusterDetails() - Wafer not found !!!"<<endm; }
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPackageDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;

  found=0;
  LOG_DEBUG <<"PrintPackageDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()==0) {
            LOG_DEBUG <<"PrintPackageDetails() - No package in this wafer "<< endm;  
          }
          else {
            LOG_DEBUG <<"PrintPackageDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()<<" package(s) in this wafer "<< endm;  
            LOG_DEBUG <<"PrintPackageDetails() - Package/Kind/Size"<< endm;  
            StSsdPackage *pPack = mySsd->mLadders[i]->mWafers[j]->getPackage()->first();
            while (pPack){
              LOG_DEBUG<<"PrintPackageDetails() - "<<pPack->getNPackage()<<" "
                              <<pPack->getKind()<<" "
                              <<pPack->getSize()<<" "<<endm;
              for (Int_t k=0;k<pPack->getSize();k++) {
                LOG_DEBUG<<"PrintPackageDetails() - "<<k<<" "<<pPack->getMatched(k)<<" "<<pPack->getMatched(k)->getNCluster()<<endm;
              }
              pPack    = mySsd->mLadders[i]->mWafers[j]->getPackage()->next(pPack);
            }     
          }
        }
      }
    }

  if (found==0){ LOG_DEBUG <<"PrintPackageDetails() - Wafer not found !!!"<<endm;}
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPointDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;

  found=0;
  LOG_DEBUG <<"PrintPointDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
            LOG_DEBUG <<"PrintPointDetails() - No hit in this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintPointDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()<<" hit(s) in this wafer "<< endm; 
 
            LOG_DEBUG<<"PrintPointDetails() - Hit/Flag/NMatched/IdClusP/IdClusN/idMcHit[0]/idMcHit[1]/idMcHit[2]/idMcHit[3]/idMcHit[4]/Xg[0]/Xg[1]/Xg[2]/Xl[0]/Xl[1]/Xl[2]"<<endm;  
            StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
            while (pSpt){
              LOG_DEBUG<<"PrintPointDetails() - "
                              <<pSpt->getNPoint()<<" "
                              <<pSpt->getFlag()<<" "
                              <<pSpt->getNMatched()<<" "
                              <<pSpt->getIdClusterP()<<" "
                              <<pSpt->getIdClusterN()<<" "
			      <<pSpt->getNMchit(0)<<" "
			      <<pSpt->getNMchit(1)<<" "
			      <<pSpt->getNMchit(2)<<" "
			      <<pSpt->getNMchit(3)<<" "
			      <<pSpt->getNMchit(4)<<" "
			      <<pSpt->getXg(0)    <<" "
			      <<pSpt->getXg(1)    <<" "
			      <<pSpt->getXg(2)    <<" "
			      <<pSpt->getXl(0)    <<" "
			      <<pSpt->getXl(1)    <<" "
			      <<pSpt->getXl(2)    <<" "
                              <<endm;  
              pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
            }     
          }
        }
      }
    }

  if (found==0) {LOG_DEBUG <<"PrintPointDetails() - Wafer not found !!!"<<endm; }
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintInfo()
{
  if (Debug()==true) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StSsdPointMaker::Read_Strip(St_ssdStripCalib *strip_calib)
{
    ssdStripCalib_st *noise = strip_calib->GetTable();

  Int_t  mSsdLayer = 7;

  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t iOver = 0;
  Int_t iZero = 0;
  Int_t iUnder = 0;
  Int_t iGood = 0;
  for (Int_t i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
	iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	if (iLad==11 && iWaf==8 && nStrip <10) {
	  LOG_INFO<<"ReadStrip: iLad,idWaf,nStrip,iSide,pedestal,rms = "<<iLad
			  <<" "<<idWaf
			  <<" "<<nStrip
			  <<" "<<iSide
			  <<" "<<(float)(noise[i].pedestals)
			  <<" "<<(float)(noise[i].rms)<<endm;
	iGood++;
	}
      }
      else {
	if (noise[i].id<0) iUnder++;
	else {
	  if (noise[i].id==0) iZero++;
	  else iOver++;
	}
      }
    }
  LOG_INFO<<"ReadStrip: Number of rows in the table : "<<strip_calib->GetNRows()<<endm;
  LOG_INFO<<"ReadStrip: Number of good id : "<<iGood<<endm;
  LOG_INFO<<"ReadStrip: Number of id = 0  : "<<iZero<<endm;
  if (iUnder>0){
    LOG_WARN <<"ReadStrip: Number of underf  : "<<iUnder<<endm;}
  if (iOver>0){
    LOG_WARN <<"ReadStrip: Number of overf   : "<<iOver<<endm;}
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Finish() {
  LOG_DEBUG << Form("Finish() ...") << endm;
    if (flag)
      {
    mFile->Write();
    mFile->Close();  
    nFile->Write();
    nFile->Close();
    qFile->Write();
    qFile->Close();  
      }
  return kStOK;
}

