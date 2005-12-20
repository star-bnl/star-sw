// $Id: StSsdPointMaker.cxx,v 1.24 2005/12/20 09:23:35 lmartin Exp $
//
// $Log: StSsdPointMaker.cxx,v $
// Revision 1.24  2005/12/20 09:23:35  lmartin
// ssdStripCalib table read from the mysql db
//
// Revision 1.23  2005/09/30 14:28:30  lmartin
// add a 0 to myTime if GetTime()<100000
//
// Revision 1.22  2005/09/26 15:49:54  bouchet
// adding a method to the point maker to check which ssdStripCalib is picked
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
#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "TNtuple.h"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"                         //

#include "StSsdBarrel.hh"
#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "StSsdStrip.hh"
#include "StSsdStripList.hh"
#include "StSsdCluster.hh"
#include "StSsdClusterList.hh"
#include "StSsdPointList.hh"
#include "StSsdPoint.hh"
#include "StSsdPackageList.hh"
#include "StSsdPackage.hh"
#include "StEvent.h"
#include "StEventInfo.h"
#include "StRunInfo.h"
#include "StSsdHitCollection.h"
#include "StSsdDynamicControl.h"
#include "StSsdClusterControl.h"
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
  gMessMgr->Info() << "In StSsdPointMaker::init() - " << endm;

  // database readout 
  gMessMgr->Info() << "Trying to access to databases " << endm;

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
Int_t StSsdPointMaker::InitRun(int runumber)
{
  //    mDbMgr = StDbManager::Instance();
  //    mDbMgr->setVerbose(false);

  //    maccess = mDbMgr->initConfig(dbGeometry,dbSsd);


  St_DataSet *DbConnector = GetDataBase("Geometry/ssd");

  if (DbConnector) 
    {
      gMessMgr->Info() << "SSD Databases respond " << endm;
      St_slsCtrl* slsCtrlTable = (St_slsCtrl*) DbConnector->Find("slsCtrl");
      slsCtrl_st*      control      = (slsCtrl_st*) slsCtrlTable->GetTable();
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
    
      St_clusterControl* clusterCtrlTable = (St_clusterControl*) DbConnector->Find("clusterControl");
      clusterControl_st *clusterCtrl  = (clusterControl_st*) clusterCtrlTable->GetTable() ;
      if (!clusterCtrl) 
	gMessMgr->Error() << "No  access to clusterControl table" << endm;
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

      //      St_DataSet *svtparams = GetInputDB("svt");
      //      St_DataSetIter local(svtparams);
      //      m_noise2       = (St_ssdStripCalib*)local("ssd/ssdStripCalib");

      St_DataSet *CalibDbConnector = GetDataBase("Calibrations/ssd");
      if (!CalibDbConnector) gMessMgr->Error()<<"StSsdPointMaker::InitRun: Can not found the calibration db.."<<endm;
      else
	m_noise2 = (St_ssdStripCalib*) CalibDbConnector->Find("ssdStripCalib");

      if (!m_noise2) 
        gMessMgr->Error() << "No  access to ssdStripCalib - will use the default noise and pedestal values" << endm;
      if(m_noise2){
      gMessMgr->Warning("StSsdPointMaker: looking for a pedestal/noise tables");
      Read_Strip(m_noise2); 
      }
      // Get once the information for configuration, wafersposition and dimensions
      St_ssdConfiguration* configTable = (St_ssdConfiguration*) DbConnector->Find("ssdConfiguration");
      config  = (ssdConfiguration_st*) configTable->GetTable() ;
      if (!config) 
        gMessMgr->Error() << "No  access to ssdConfiguration database" << endm;
      //mConfig = new StSsdConfig();
  
      St_ssdWafersPosition* positionTable = (St_ssdWafersPosition*) DbConnector->Find("ssdWafersPosition");
      positionSize = 0;
      position  = (ssdWafersPosition_st*) positionTable->GetTable() ;
      if (!position) 
        gMessMgr->Error() << "No  access to ssdWafersPosition database" << endm;
      else
        positionSize= positionTable->GetNRows();

      St_ssdDimensions* dimensionsTable = (St_ssdDimensions*) DbConnector->Find("ssdDimensions");
      dimensions  = (ssdDimensions_st*) dimensionsTable->GetTable() ;
      if (!dimensions) 
        gMessMgr->Error() << "No  access to ssdDimensions database" << endm;
  
      if ((!dimensions)||(!config)){
        gMessMgr->Error() << "No geometry or configuration parameters " << endm;
        return kStErr;
      }

    }
  else // No access to databases -> read tables
    {
	gMessMgr->Error() << "No connection to the database in StSsdPointMaker" << endm;
    }


  return kStOk;
}

//_____________________________________________________________________________

void StSsdPointMaker::DeclareNtuple(int *flag){
  mFile = new TFile("PhysicsFile.root","RECREATE");
  string varlist2 = "pulseP:pulseN:ladder:wafer:case:xg:yg:zg";
  mHitNtuple     = new TNtuple("PhysNTuple","Physics Ntuple",varlist2.c_str());
  nFile = new TFile("Clusters.root","RECREATE");
  string varlist3 = "side:ladder:wafer:nstrip:snratio:noise:first_strip:TotAdc";
  nHitNtuple     = new TNtuple("ClusTuple","Clusters Ntuple",varlist3.c_str());

  *flag =  1;
}

//_____________________________________________________________________________
Int_t StSsdPointMaker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In StSsdPointMaker::Make() ... "
                               << GetName() << endm;
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
  St_spa_strip *spa_strip = (St_spa_strip *)GetDataSet("spa_strip");
  St_ssdPedStrip *spa_ped_strip = (St_ssdPedStrip *)GetDataSet("ssdPedStrip");
  if (!spa_strip || spa_strip->GetNRows()==0){
    gMessMgr->Warning("StSsdPointMaker: no input (fired strip for the SSD)");
    gMessMgr->Warning("StSsdPointMaker: looking for a pedestal/noise tables");
    if (!spa_ped_strip || spa_ped_strip->GetNRows()==0) {
      gMessMgr->Warning("StSsdPointMaker: no pedestal/noise data...");
      return kStWarn;
    }
    else 
      gMessMgr->Warning()<<"StSsdPointMaker: pedestal/noise data found : "<<spa_ped_strip->GetNRows()<<endm;
  }

  
  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt); 
  //St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",5000);//22/10
  //m_DataSet->Add(scf_cluster);
  mCurrentEvent = (StEvent*) GetInputDS("StEvent");
  if(mCurrentEvent) 
    {
      mSsdHitColl = mCurrentEvent->ssdHitCollection();
      if (!mSsdHitColl) {
	gMessMgr->Warning("StSsdPointMaker::Make : The SSD hit collection does not exist  - creating a new one");
	mSsdHitColl = new StSsdHitCollection;
	mCurrentEvent->setSsdHitCollection(mSsdHitColl);
      }
    }
  else              
    mSsdHitColl = 0;
  
  cout<<"#################################################"<<endl;
  cout<<"####     START OF NEW SSD POINT MAKER        ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StSsdBarrel *mySsd = new StSsdBarrel(dimensions,config);
  //mySsd->initLadders(m_wafpos); 
  mySsd->initLadders(position,positionSize);
  //The full SSD object is built only if we are processing physics data
  if((spa_ped_strip->GetNRows()==0) && (spa_strip->GetNRows()!=0))
    {
      int stripTableSize = mySsd->readStripFromTable(spa_strip);
      cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
      mySsd->sortListStrip();
      PrintStripSummary(mySsd);
      int noiseTableSize = 0;      
      if (!m_noise2) 
	gMessMgr->Warning("StSsdPointMaker::Make : No pedestal and noise values (ssdStripCalib table missing), will use default values");
      else
	noiseTableSize = mySsd->readNoiseFromTable(m_noise2,mDynamicControl);
      cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
      int nClusterPerSide[2];
      nClusterPerSide[0] = 0;
      nClusterPerSide[1] = 0;
      mySsd->doSideClusterisation(nClusterPerSide,mClusterControl);
      cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
      cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
      mySsd->sortListCluster();
      PrintClusterSummary(mySsd);
      //      PrintStripDetails(mySsd,7406);
      //      PrintClusterDetails(mySsd,7406); 
      makeScfCtrlHistograms(mySsd);
      //debugUnPeu(mySsd);
      int nPackage = mySsd->doClusterMatching(dimensions,mClusterControl);
      cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
      mySsd->convertDigitToAnalog(mDynamicControl);
      mySsd->convertUFrameToOther(dimensions);
      PrintPointSummary(mySsd);
      int nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl);
      cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
      if(mSsdHitColl) 
	cout<<"####   -> "<<mSsdHitColl->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endl;
      else 
	cout<<" ######### NO SSD HITS WRITTEN INTO CONTAINER   ####"<<endl;
      scm_spt->Purge();
      cout<<"####        END OF SSD NEW POINT MAKER       ####"<<endl;
      cout<<"#################################################"<<endl;
      makeScmCtrlHistograms(mySsd);

      if(flag==1){ 
	WriteScfTuple(mySsd);
	WriteScmTuple(mySsd);
      }
      if (nSptWritten) res = kStOK;
    }
  else
    {
    if((spa_strip->GetNRows()==0)&&(spa_ped_strip->GetNRows()!=0))
      { 
	cout<<"###### WRITING SSD PEDESTAL HISTOGRAMS##########"<<endl;
	mySsd->writeNoiseToFile(spa_ped_strip,myLabel);
      }
    }
  delete mySsd;
 
  if(res!=kStOK){
    gMessMgr->Warning("StSsdPointMaker: no output");
    return kStWarn;
  }

  
  return kStOK;
}

//_____________________________________________________________________________
void StSsdPointMaker::makeScfCtrlHistograms(StSsdBarrel *mySsd)
{

  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  int found;
  Float_t convAdcToE = (mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip())/(pow(2.0,mDynamicControl->getNBitEncoding()));
  found=0;
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	//if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	  //gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
	//}
	//else {
	  StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	  while (pClusterP)
	    {  
	      stpClusP->Fill(pClusterP->getClusterSize());
	      totChrgP->Fill(convAdcToE*pClusterP->getTotAdc());
	      noisDisP->Fill(pClusterP->getTotNoise()/pClusterP->getClusterSize());
	      snRatioP->Fill((pClusterP->getTotAdc()*pClusterP->getClusterSize())/pClusterP->getTotNoise());
	      pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);	
	    }
	  //if (mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
	    //gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - No cluster on the N-side of this wafer "<< endm;  
	  //}
	  //else {
	    StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	    while (pClusterN)
	      {
		stpClusN->Fill(pClusterN->getClusterSize());
		totChrgN->Fill(convAdcToE*pClusterN->getTotAdc());
		noisDisN->Fill(pClusterN->getTotNoise()/pClusterN->getClusterSize());
		snRatioN->Fill((pClusterN->getTotAdc()*pClusterN->getClusterSize())/pClusterN->getTotNoise());	
		pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
	      }	  
	  }
	}
}


//_____________________________________________________________________________

void StSsdPointMaker::makeScmCtrlHistograms(StSsdBarrel *mySsd)
{
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  int found;
  int conversion[11]={11,12,21,13,31,221,222,223,23,32,33};
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getNBitEncoding())/(mDynamicControl->getPairCreationEnergy()*mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip());
  found=0;
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	  //gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - No hit in this wafer "<< endm;  
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
	    
	    for(int k=0;k<=11;k++)
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
  int ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySsd->mLadders[i]->mWafers[j]->getStripN()->getSize();
      }
    }
  
  gMessMgr->Info() <<"StSsdPointMaker::PrintStripSummary/Number of raw data in the SSD" << endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintStripSummary/Active Ladders : ";
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintStripSummary/Counts (p-side): ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountP[i];
    }
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintStripSummary/Counts (n-side): ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountN[i];
    }
  *gMessMgr<<endm;
}

//_____________________________________________________________________________
void StSsdPointMaker::debugUnPeu(StSsdBarrel *mySsd)
{
  int monladder,monwafer;
  monladder=7;
  monwafer=6;
  mySsd->debugUnPeu(monladder,monwafer);
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintClusterSummary(StSsdBarrel *mySsd)
{
  int ladderCountN[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int ladderCountP[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize();
      }
    }
  
  gMessMgr->Info() <<"StSsdPointMaker::PrintClusterSummary/Number of clusters in the SSD" << endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintClusterSummary/Active Ladders : ";
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintClusterSummary/Counts (p-side): ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountP[i];
    }
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintClusterSummary/Counts (n-side): ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCountN[i];
    }
  *gMessMgr<<endm;
}
//_____________________________________________________________________________
void StSsdPointMaker::PrintPointSummary(StSsdBarrel *mySsd)
{
  int ladderCount[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  int ladderCount11[20]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} ;
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	ladderCount[i]=ladderCount[i]+mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize();
	StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
	while (pSpt){	
	  if (pSpt->getNMatched()==11) ladderCount11[i]++;
	  pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
	}
      }
    }
  
  gMessMgr->Info() <<"StSsdPointMaker::PrintPointSummary/Number of hits in the SSD" << endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintPointSummary/Active Ladders : ";
  for (int i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr<<i+1;
    }
  
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintPointSummary/Counts         : ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCount[i];
    }
  *gMessMgr<<endm;
  gMessMgr->Info() << "StSsdPointMaker::PrintPointSummary/Counts  (11)   : ";
  for (int i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      gMessMgr->width(5);
      *gMessMgr <<ladderCount11[i];
    }
  *gMessMgr<<endm;
}

//_____________________________________________________________________________
void StSsdPointMaker::WriteScfTuple(StSsdBarrel *mySsd)
{

  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  int found;
  found=0;
    for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	//Looking for the P-side cluster informations
	//  if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	    //  gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
	// }
	// else {
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
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  int found;
  int conversion[11]={11,12,21,13,31,221,222,223,23,32,33}; 
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getNBitEncoding())/(mDynamicControl->getPairCreationEnergy()*mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip());
  found=0;
  // gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	  if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	    //   gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - No hit in this wafer "<< endm;  
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
	      for(int k=0;k<=11;k++)
		{
		  if(pSpt->getNMatched()==conversion[k])
		    {
		      hitNtuple[4]=k; 
		    }
		    }
	      hitNtuple[5]=pSpt->getXg(0);
	      hitNtuple[6]=pSpt->getXg(1);
	      hitNtuple[7]=pSpt->getXg(2);
	      mHitNtuple->Fill(hitNtuple);		 
	      pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		} 
	    }
	  }
      }
    }


//_____________________________________________________________________________
void StSsdPointMaker::PrintStripDetails(StSsdBarrel *mySsd, int mywafer)
{
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  int found;
  gMessMgr->Info() <<"StSsdPointMaker::PrintStripDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side strip informations
          if (mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize()==0) {
            gMessMgr->Info() <<"StSsdPointMaker::PrintStripDetails() - No strip on the P-side of this wafer "<< endm;  
          }
          else {
            gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getStripP()->getSize()<<" strip(s) on the P-side of this wafer "<< endm;  
            gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - Strip/Adc/Ped/Noise"<< endm;  
            StSsdStrip *pStripP = mySsd->mLadders[i]->mWafers[j]->getStripP()->first();
            while (pStripP){
              gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - "
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
            gMessMgr->Info() <<"StSsdPointMaker::PrintStripDetails() - No strip on the N-side of this wafer "<< endm;  
          }
          else {
            gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getStripN()->getSize()<<" strip(s) on the N-side of this wafer "<< endm;  
            gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - Strip/Adc/Ped/Noise"<< endm;  
            StSsdStrip *pStripN = mySsd->mLadders[i]->mWafers[j]->getStripN()->first();
            while (pStripN){
              gMessMgr->Info()<<"StSsdPointMaker::PrintStripDetails() - "
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

  if (found==0) gMessMgr->Info() <<"StSsdPointMaker::PrintStripDetails() - Wafer not found !!!"<<endm;  
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintClusterDetails(StSsdBarrel *mySsd, int mywafer)
{
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  int found;
  gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side cluster informations
          if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
            gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
          }
          else {
            gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<" cluster(s) on the P-side of this wafer "<< endm;  
            gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
            while (pClusterP){
              gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - "
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
            gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - No cluster on the N-side of this wafer "<< endm;  
          }
          else {
            gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - "
                            <<mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<" cluster(s) on the N-side of this wafer "<< endm;  
            gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
            while (pClusterN){
              gMessMgr->Info()<<"StSsdPointMaker::PrintClusterDetails() - "
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

  if (found==0) gMessMgr->Info() <<"StSsdPointMaker::PrintClusterDetails() - Wafer not found !!!"<<endm;  
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPackageDetails(StSsdBarrel *mySsd, int mywafer)
{
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  int found;

  found=0;
  gMessMgr->Info() <<"StSsdPointMaker::PrintPackageDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()==0) {
            gMessMgr->Info() <<"StSsdPointMaker::PrintPackageDetails() - No package in this wafer "<< endm;  
          }
          else {
            gMessMgr->Info() <<"StSsdPointMaker::PrintPackageDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()<<" package(s) in this wafer "
<< endm;  
            gMessMgr->Info() <<"StSsdPointMaker::PrintPackageDetails() - Package/Kind/Size"<< endm;  
            StSsdPackage *pPack = mySsd->mLadders[i]->mWafers[j]->getPackage()->first();
            while (pPack){
              gMessMgr->Info()<<"StSsdPointMaker::PrintPackageDetails() - "<<pPack->getNPackage()<<" "
                              <<pPack->getKind()<<" "
                              <<pPack->getSize()<<" "<<endm;
              for (int k=0;k<pPack->getSize();k++) {
                gMessMgr->Info()<<"StSsdPointMaker::PrintPackageDetails() - "<<k<<" "<<pPack->getMatched(k)<<" "<<pPack->getMatched(k)->getNCluster()<<endm;
              }
              pPack    = mySsd->mLadders[i]->mWafers[j]->getPackage()->next(pPack);
            }     
          }
        }
      }
    }

  if (found==0) gMessMgr->Info() <<"StSsdPointMaker::PrintPackageDetails() - Wafer not found !!!"<<endm;  
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPointDetails(StSsdBarrel *mySsd, int mywafer)
{
  int LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  int found;

  found=0;
  gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (LadderIsActive[i]>0) {
      for (int j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
            gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - No hit in this wafer "<< endm;  
          }
          else {
            gMessMgr->Info()<<"StSsdPointMaker::PrintPointDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()<<" hit(s) in this wafer "<< endm; 
 
            gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - Hit/Flag/NMatched/IdClusP/IdClusN"<< endm;  
            StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
            while (pSpt){
              gMessMgr->Info()<<"StSsdPointMaker::PrintPointDetails() - "
                              <<pSpt->getNPoint()<<" "
                              <<pSpt->getFlag()<<" "
                              <<pSpt->getNMatched()<<" "
                              <<pSpt->getIdClusterP()<<" "
                              <<pSpt->getIdClusterN()<<" "
                              <<endm;  
              pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
            }     
          }
        }
      }
    }

  if (found==0) gMessMgr->Info() <<"StSsdPointMaker::PrintPointDetails() - Wafer not found !!!"<<endm;  
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StSsdPointMaker::Read_Strip(St_ssdStripCalib *strip_calib)
{
    ssdStripCalib_st *noise = strip_calib->GetTable();

  int  mSsdLayer = 7;

  int idWaf  = 0;
  int iWaf   = 0;
  int iLad   = 0;
  int nStrip = 0;
  int iSide  = 0;
  for (int i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id/100000.);
      idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
      iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
      //mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip, iSide, noise[i].pedestals, noise[i].rms, dynamicControl);
             if (iLad==11 && iWaf==8 && nStrip <10) 
      	cout<<"iLad,idWaf,nStrip,iSide,pedestal,rms = "<<iLad
      	    <<" "<<idWaf
      	    <<" "<<nStrip
      	    <<" "<<iSide
	    <<" "<<(float)(noise[i].pedestals)
      	    <<" "<<(float)(noise[i].rms)<<endl;
    }
}

//_____________________________________________________________________________
Int_t StSsdPointMaker::Finish() {
    if (Debug()) gMessMgr->Debug() << "In StSsdPointMaker::Finish() ... "
				 << GetName() << endm;
    if (flag)
      {
    mFile->Write();
    mFile->Close();  
    nFile->Write();
    nFile->Close(); 
      }
  return kStOK;
}

