// $Id: StSsdPointMaker.cxx,v 1.70 2017/04/26 20:13:03 perev Exp $
//
// $Log: StSsdPointMaker.cxx,v $
// Revision 1.70  2017/04/26 20:13:03  perev
// Hide m_DataSet
//
// Revision 1.69  2015/08/06 17:46:53  smirnovd
// Removed unused local variables
//
// Revision 1.68  2015/05/21 18:49:50  bouchet
// arrayIndex out of bounds fixed
//
// Revision 1.67  2015/05/21 02:54:00  perev
// bug #3106 By mistake, fixing memory leak was removed by me line mySsd->Reset()
// Now I put it back
// Victor
//
// Revision 1.66  2015/05/19 19:11:18  perev
// Avoid leak by useng standard array instead of new and delete
//
// Revision 1.65  2015/05/19 13:44:17  bouchet
// ticket 3105 + warning fixed when printing the size of gain table
//
// Revision 1.64  2015/05/15 18:31:10  bouchet
// possible infinite loop fixed
//
// Revision 1.63  2012/11/07 21:54:37  fisyak
// Remove check for .histos
//
// Revision 1.62  2009/02/18 21:31:22  bouchet
// fix bug for printing the number of hits/ladder and filling histograms
//
// Revision 1.61  2008/10/20 19:32:39  bouchet
// use of new writePointToContainer method for the hit quality calculation
//
// Revision 1.60  2008/07/16 21:01:57  bouchet
// calculation of hits quality removed : call of default writePointToContainer
//
// Revision 1.59  2008/05/20 03:05:54  bouchet
// fix improper STAR logger(#1185) ; thanks to Valeri
//
// Revision 1.58  2008/05/07 22:45:24  bouchet
// add mcEvent dependence for embedding
//
// Revision 1.57  2008/04/15 21:05:22  bouchet
// remove latest change
//
// Revision 1.56  2008/04/12 14:20:38  bouchet
// Add a switch to use constant noise and pedestal ; remove some printing
//
// Revision 1.55  2008/01/15 13:48:58  bouchet
// Set a default value for uninitialized variable
//
// Revision 1.54  2008/01/11 10:39:39  bouchet
// add method to read the Wafer configuration table
//
// Revision 1.53  2007/09/25 13:40:46  bouchet
// Use m_Mode to switch between pedestals used in real data/simulation ; move some message to DEBUG
//
// Revision 1.52  2007/08/20 06:47:37  bouchet
// ssdStripCalib table taken for simulation
//
// Revision 1.51  2007/07/14 14:29:44  bouchet
// forget the Debug condition for the declaration of the tuples
//
// Revision 1.50  2007/07/14 13:52:16  bouchet
// add method to fill with default pedestal/noise values if no table is found
//
// Revision 1.49  2007/07/13 06:19:43  bouchet
// display of number of reconstructed hits corrected
//
// Revision 1.48  2007/07/12 17:07:18  bouchet
// add switch to read old ssdStripCalib Table and new ssdNoise Table
//
// Revision 1.47  2007/07/02 20:01:03  bouchet
// bug fixed for the normalization of reconstruction efficiency histos
//
// Revision 1.46  2007/07/01 16:18:41  bouchet
// add a normalization for the reconstruction efficiency histograms
//
// Revision 1.45  2007/06/23 04:53:50  bouchet
// add 0's to Timestamp which size is less than 6 digits
//
// Revision 1.44  2007/06/19 18:30:24  bouchet
// Add a method to evaluate the reconstruction efficiency (defined as the ratio of the number of matched clusters with all reconstructed clusters) ; some clean-up
//
// Revision 1.43  2007/06/19 01:19:15  bouchet
// cosmetic changes
//
// Revision 1.42  2007/04/28 17:56:58  perev
// Redundant StChain.h removed
//
// Revision 1.41  2007/04/17 05:09:25  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.40  2007/04/11 22:45:22  perev
// 1/0 avoided
//
// Revision 1.39  2007/03/27 23:15:09  bouchet
// Add a switch to use the gain calibration
//
// Revision 1.38  2007/03/27 18:30:04  fisyak
// recover lost access to ssdStripCalib table
//
// Revision 1.37  2007/03/21 17:19:12  fisyak
// use TGeoHMatrix for coordinate transformation, eliminate ssdWafersPostion, ake NTuples only for Debug()>1
//
// Revision 1.36  2007/03/08 23:04:42  bouchet
// add WriteMatchedStrips() method : fill the characteristics of the strips from matched clusters ; Small change for the writing of tuples
//
// Revision 1.35  2007/03/01 22:19:21  bouchet
// add a protection when ssdStripCalib is filled with empty values
//
// Revision 1.34  2007/02/21 20:36:17  bouchet
// add a method WriteMatchedClusters :\ instead of WriteScfTuple() method that fill all the reconstructed clusters,\ this one store the clusters associated to the hits
//
// Revision 1.33  2007/02/15 14:40:27  bouchet
// bug fixed for filling makeScmCtrlHistograms() method
//
// Revision 1.32  2007/02/14 11:49:12  bouchet
// Added control histograms and updated the Cluster and Point Tuple
//
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
#include "tables/St_ssdGainCalibWafer_Table.h"
#include "tables/St_ssdNoise_Table.h"
#include "tables/St_ssdWaferConfiguration_Table.h"
#include "StEvent.h"
#include "StSsdHitCollection.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "TMath.h"
ClassImp(StSsdPointMaker);
  
//_____________________________________________________________________________
Int_t StSsdPointMaker::Init(){
  LOG_INFO << "Init() : Defining the histograms" << endm;
  noisDisP  = new TH1F("Noise_p","Noise Distribution",250,0,25);
  snRatioP  = new TH1F("SN_p","Signal/Noise (p)",200,0,200);
  stpClusP  = new TH1F("NumberOfStrips_p","Strips per Cluster",8,0,8);
  totChrgP  = new TH1F("ChargeElectron_p","Total Cluster Charge",100,0,300000);
  ClusNvsClusP  = new TH2S("ClusNvsClusP","Number of clusters on the n-side vs Number of clusters on the p-side",200,0,200,200,0,200);
  ClusNvsClusP->SetXTitle("Number of p-Side Clusters");
  ClusNvsClusP->SetYTitle("Number of n-Side Clusters");
  noisDisN  = new TH1F("Noise_n","Noise Distribution",250,0,25);
  snRatioN  = new TH1F("SN_n","Signal/Noise",200,0,200);
  stpClusN  = new TH1F("NumberOfStrips_n","Strips per Cluster",8,0,8);
  totChrgN  = new TH1F("ChargeElectron_n","Total Cluster Charge",100,0,300000);
  ClustMapP = new TH2S("ClustMapP","Number of clusters on the p-side per wafer and ladder",20,0,20,16,0,16);
  ClustMapP->SetXTitle("Ladder id");
  ClustMapP->SetYTitle("Wafer id");
  ClustMapN = new TH2S("ClustMapN","Number of clusters on the n-side per wafer and ladder",20,0,20,16,0,16);
  ClustMapN->SetXTitle("Ladder id");
  ClustMapN->SetYTitle("Wafer id");
  MatchedClusterP = new TH2F("MatchedClusterP","#frac{# clusters matched}{# clusters reconstructed} , wafers on p-side",20,1,21,16,1,17);
  MatchedClusterP->SetXTitle("Ladder id");
  MatchedClusterP->SetYTitle("Wafer id");
  MatchedClusterN = new TH2F("MatchedClusterN","#frac{# clusters matched}{# clusters reconstructed} , wafers on n-side",20,1,21,16,1,17);
  MatchedClusterN->SetXTitle("Ladder id");
  MatchedClusterN->SetYTitle("Wafer id");
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
  
  TString Title;
  Char_t *Name = new Char_t[20];
  Title ="Matching Adc (1p-1n) for ladder";
  for(Int_t ii=0;ii<20;ii++)    {
    Title = Form("Matching Adc (1p-1n) for ladder = %i",ii+1);
    sprintf(Name,"%s%d","matchingHisto_",ii);
    matchisto_[ii] =  new TH2S(Name,Title,500,0,1000, 500, 0, 1000);
    matchisto_[ii]->SetXTitle("PSide ADC count");
    matchisto_[ii]->SetYTitle("NSide ADC count");
    matchisto_[ii]->SetZTitle("(1p-1n) hits");
  }
  if (Debug()>1) DeclareNtuple();
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::InitRun(Int_t runumber) {
  //    mDbMgr = StDbManager::Instance();
  //    mDbMgr->setVerbose(false);
  
  //    maccess = mDbMgr->initConfig(dbGeometry,dbSsd);
  mode= gStSsdDbMaker->GetMode();
  LOG_INFO <<"m_Mode = " << mode << endm;
  NEvent         = 0;
  UseCalibration = 1;
  UseWaferConfig = 1;
  LOG_INFO<<Form("UseCalibration =%d UseWaferTable = %d",UseCalibration,UseWaferConfig)<<endm;
  St_slsCtrl* slsCtrlTable = (St_slsCtrl*) GetDataBase("Geometry/ssd/slsCtrl");
  if(! slsCtrlTable){LOG_ERROR << "InitRun : No access to slsCtrl table" << endm;}
  else  {
    mDynamicControl = new StSsdDynamicControl();
    slsCtrl_st*      control      = (slsCtrl_st*) slsCtrlTable->GetTable();
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
  St_clusterControl* clusterCtrlTable = (St_clusterControl*) GetDataBase("Geometry/ssd/clusterControl");
  if (!clusterCtrlTable) {LOG_ERROR << "InitRun : No access to clusterControl table" << endm;}
  else {
    mClusterControl = new StSsdClusterControl();
    clusterControl_st *clusterCtrl  = (clusterControl_st*) clusterCtrlTable->GetTable() ;
    mClusterControl -> setHighCut(clusterCtrl->highCut);  
    mClusterControl -> setTestTolerance(clusterCtrl->testTolerance);
    mClusterControl -> setClusterTreat(clusterCtrl->clusterTreat);
    mClusterControl -> setAdcTolerance(clusterCtrl->adcTolerance);
    mClusterControl -> setMatchMean(clusterCtrl->matchMean);
    mClusterControl -> setMatchSigma(clusterCtrl->matchSigma);
    mClusterControl -> printParameters();
  }    
  year = (GetDate()/10000)-2000;
  LOG_DEBUG <<Form("TimeStamp is %d Year is =%d\n",GetDate(),year)<<endm;
  switch(mode)
    {
    case 1: {
      m_noise2 = (St_ssdStripCalib*) GetDataBase("Calibrations/ssd/ssdStripCalib");
      if (!m_noise2) {LOG_ERROR << "InitRun : No access to ssdStripCalib - will use the default noise and pedestal values" << endm;} 
      else { 
	LOG_INFO<<"InitRun for simu : old Table (ssdStripCalib) is used"<<endm; 
      } 
      break;
    }
    case 0 :{
      if(year<7){
	m_noise2 = (St_ssdStripCalib*) GetDataBase("Calibrations/ssd/ssdStripCalib");
	if (!m_noise2) {LOG_ERROR << "InitRun : No access to ssdStripCalib - will use the default noise and pedestal values" << endm;}
	else {
	  LOG_INFO<<"InitRun for real data : old Table(ssdStripCalib) is used"<<endm;
	}
      }
      else {
	m_noise3 = (St_ssdNoise*)GetDataBase("Calibrations/ssd/ssdNoise");
	if (!m_noise3) {LOG_ERROR << "InitRun : No access to ssdNoise - will use the default noise and pedestal values" << endm;}
	else{
	  LOG_INFO << "InitRun for real data : new Table(ssdNoise) is used" << endm;}
      }
      break;
    }
    default : {printf("no real data nor simu");}
    }
  (UseCalibration==1)?FillCalibTable():FillDefaultCalibTable();
  (UseWaferConfig==1)?FillWaferTable():FillDefaultWaferTable();
  /*
    Init arrays for the reconstruction efficiency
  */
  for(Int_t ii=0 ;ii<20;ii++)
    {
      for(Int_t jj=0;jj<16;jj++)
	{
	  ratioP[ii][jj] = 0;
	  ratioN[ii][jj] = 0;
	}
    }
  return kStOk;
}
//_____________________________________________________________________________
void StSsdPointMaker::DeclareNtuple(){
  TFile *f = GetTFile();
  if (f){
    f->cd();
    string varlist2 = "pulseP:pulseN:ladder:wafer:case:xg:yg:zg:flag:idClusP:idClusN:position_0:position_1:xl:yl";
    mHitNtuple      = new TNtuple("PhysNTuple","Physics Ntuple",varlist2.c_str());
    string varlist3 = "side:ladder:wafer:nstrip:snratio:noise:first_strip:TotAdc:FirstAdc:LastAdc:TotNoise";
    nHitNtuple      = new TNtuple("ClusTuple","All Clusters stored",varlist3.c_str()); 
    string varlist4 = "side:ladder:wafer:nstrip:pedestal:signal:noise:snratio";
    qHitNtuple      = new TNtuple("Strips","All Strips stored",varlist4.c_str());
    pHitNtuple      = new TNtuple("ClustupleIn","Clusters in hits",varlist3.c_str()); 
    rHitNtuple      = new TNtuple("StripsIn","Strips in hits",varlist4.c_str()); 
  }
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Make()
{
  LOG_DEBUG << Form("Make : begin")<< endm;
  // 		Create output tables
  Int_t res = 0; 
  char myLabel[100];
  char myTime[100]; 
  char myDate[100];
  if (GetTime()<999)
    sprintf(myTime,"000%d",GetTime());
  else
    if ((GetTime()<9999)&&(GetTime()>999))
      sprintf(myTime,"00%d",GetTime());
    else
      if ((GetTime()<99999)&&(GetTime()>9999))
	sprintf(myTime,"0%d",GetTime());
      else 
	sprintf(myTime,"%d",GetTime());
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
  AddData(scm_spt); 
  
  St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",5000);//09/13
  AddData(scf_cluster);
  
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
  StSsdBarrel *mySsd =gStSsdDbMaker->GetSsd();
  mySsd->setClusterControl(mClusterControl);
  //The full SSD object is built only if we are processing physics data
  if((! spa_ped_strip || spa_ped_strip->GetNRows()==0) && (spa_strip->GetNRows()!=0))
    {
      Int_t stripTableSize = mySsd->readStripFromTable(spa_strip);
      LOG_INFO<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endm;
      mySsd->sortListStrip();
      PrintStripSummary(mySsd);
      noiseTableSize = 0; 
      noiseTableSize = ReadNoiseTable(mySsd,year);
      LOG_INFO<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endm;
      Int_t nClusterPerSide[2];
      nClusterPerSide[0] = 0;
      nClusterPerSide[1] = 0;
      mySsd->doSideClusterisation(nClusterPerSide,WafStatus);
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
      Int_t nPackage = mySsd->doClusterMatching(CalibArray);
      LOG_INFO<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endm;
      mySsd->convertDigitToAnalog(mDynamicControl);
      mySsd->convertUFrameToOther();
      PrintPointSummary(mySsd);
      //Int_t nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl);
      if(Debug()){
	for(Int_t i=1;i<=20;i++)
	  {
	    for(Int_t j=1;j<=16;j++)
	      {
		//PrintStripDetails(mySsd,7000+(100*j)+i);
		//PrintClusterDetails(mySsd,7000+(100*j)+i);
		//PrintPointDetails(mySsd,7000+(100*j)+i);
		//PrintPackageDetails(mySsd,7000+(100*j)+i);
	      }
	  }
      }
      //get McEvent here
      Int_t nSptWritten = 0;
      StMcEvent* mcEvent = 0;
      mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
      if(mcEvent)
	{	
	  LOG_DEBUG << " mcEvent exists " << endm;
	  nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl,scf_cluster,spa_strip,mDynamicControl,mcEvent);
	}
      else{
	nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl,scf_cluster);
      }
      LOG_INFO<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endm;
      if(mSsdHitColl){
	if (mSsdHitColl->numberOfHits()>0) {
	  NEvent++;
	  LOG_INFO<<"####   -> "<<mSsdHitColl->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endm;
	  makeScmCtrlHistograms(mySsd);
	  EvaluateEfficiency(mySsd);
	  NormalizeEfficiency();
	  scm_spt->Purge();
	}
	else {
	  LOG_INFO<<" ######### NO SSD HITS WRITTEN INTO CONTAINER   ####"<<endm;
	}
      }
      LOG_INFO<<"####        END OF SSD NEW POINT MAKER       ####"<<endm;
      LOG_INFO<<"#################################################"<<endm;
      if(Debug() >1){
	if (qHitNtuple) WriteStripTuple(mySsd);
	if (nHitNtuple) WriteScfTuple(mySsd);
	if (mHitNtuple) WriteScmTuple(mySsd);
	if (rHitNtuple) WriteMatchedStrips(mySsd);
	if (pHitNtuple) WriteMatchedClusters(mySsd);
      }
      if (nSptWritten) res = kStOK;
    }
  else
    {
      if((spa_strip->GetNRows()==0)&&(spa_ped_strip && spa_ped_strip->GetNRows()!=0))
	{ 
	  LOG_INFO <<"###### WRITING SSD PEDESTAL HISTOGRAMS##########"<<endm;
	  if(year<7){
	    mySsd->writeNoiseToFile(spa_ped_strip,myLabel);}
	  else{mySsd->writeNewNoiseToFile3(spa_ped_strip,myLabel);//new method
	    printf("done\n");
	  }
	}
    }
 //clear stuff 	 
    mySsd->Reset(); 	 
    if(res!=kStOK){
    LOG_WARN <<"Make : no output" << endm;;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScfCtrlHistograms(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Float_t convAdcToE = (mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip())/(pow(2.0,mDynamicControl->getnbitEncoding()));
  Int_t ClustersP_tot = 0;
  Int_t ClustersN_tot = 0;
  Int_t pSize         = 0;
  Int_t nSize         = 0;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	StSsdClusterList *pList = mySsd->mLadders[i]->mWafers[j]->getClusterP();
	pSize = pList->getSize();
	ClustersP_tot+= pSize;
	StSsdClusterList *nList = mySsd->mLadders[i]->mWafers[j]->getClusterN();
	nSize = nList->getSize();
	ClustersN_tot+= nSize;
	ClusNvsClusP->Fill(pSize,nSize);
	ClustMapP->Fill(i,j,pSize);
	ClustMapN->Fill(i,j,nSize);
	pSize = 0;
	nSize = 0;
	StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	while (pClusterP)
	  {  
	    stpClusP->Fill(pClusterP->getClusterSize());
	    totChrgP->Fill(convAdcToE*pClusterP->getTotAdc());
	    if (pClusterP->getClusterSize()>0) 
	      noisDisP->Fill(pClusterP->getTotNoise()/pClusterP->getClusterSize());
	    if (pClusterP->getTotNoise()>0) 
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
  LOG_DEBUG <<"totclusters P="<<ClustersP_tot<<" totclusters N="<<ClustersN_tot<<endm;
}
//_____________________________________________________________________________

void StSsdPointMaker::makeScmCtrlHistograms(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t conversion[11]={11,12,21,13,31,221,222,223,23,32,33};
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	  //LOG_INFO <<"PrintPointDetails() - No hit in this wafer "<< endm;  
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
		matchisto_[i]->Fill(a,b);
	      }
	    
	    for(Int_t k=0;k<11;k++)
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
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintStripSummary : Counts (p-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountP[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintStripSummary : Counts (n-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountN[i];
    }
  LOG_DEBUG<<endm;
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
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (p-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountP[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (n-side): ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountN[i];
    }
  LOG_DEBUG<<endm;
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
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintPointSummary : Counts         : ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCount[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintPointSummary : Counts  (11)   : ";
  for (Int_t i=0;i<20;i++)
    if (mySsd->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCount11[i];
    }
  LOG_DEBUG<<endm;
}
//_____________________________________________________________________________
void StSsdPointMaker::WriteStripTuple(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
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
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	while (pClusterP)
	  {   
	    ClusterNtuple[0] = 0;
	    ClusterNtuple[1] = i+1;
	    ClusterNtuple[2] = j+1;
	    ClusterNtuple[3] = pClusterP->getClusterSize();
	    ClusterNtuple[4] = ((pClusterP->getTotAdc()*pClusterP->getClusterSize())/pClusterP->getTotNoise());
	    ClusterNtuple[5] = pClusterP->getTotNoise()/pClusterP->getClusterSize();
	    ClusterNtuple[6] = pClusterP->getFirstStrip();
	    ClusterNtuple[7] = pClusterP->getTotAdc();
	    ClusterNtuple[8] = pClusterP->getFirstAdc();
	    ClusterNtuple[9] = pClusterP->getLastAdc();
	    ClusterNtuple[10]= pClusterP->getTotNoise();
	    pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);
	    nHitNtuple->Fill(ClusterNtuple);	
	  }
	StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	while (pClusterN)
	  {	
	    ClusterNtuple[0] = 1;
	    ClusterNtuple[1] = i+1;
	    ClusterNtuple[2] = j+1;
	    ClusterNtuple[3] = pClusterN->getClusterSize();
	    ClusterNtuple[4] = ((pClusterN->getTotAdc()*pClusterN->getClusterSize())/pClusterN->getTotNoise());
	    ClusterNtuple[5] = pClusterN->getTotNoise()/pClusterN->getClusterSize();
	    ClusterNtuple[6] = pClusterN->getFirstStrip();
	    ClusterNtuple[7] = pClusterN->getTotAdc();
	    ClusterNtuple[8] = pClusterN->getFirstAdc();
	    ClusterNtuple[9] = pClusterN->getLastAdc();
	    ClusterNtuple[10]= pClusterN->getTotNoise();	
	    pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
	    nHitNtuple->Fill(ClusterNtuple);
	  }	  
      }
    }
}
//_____________________________________________________________________________
void StSsdPointMaker::WriteScmTuple(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t conversion[11]={11,12,21,13,31,221,222,223,23,32,33}; 
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
	}
	else {
	  StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
	  while (pSpt){
	    Float_t a = 0, b = 0;
	    a = convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1));
	    b = convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1));
	    hitNtuple[0] = a;
	    hitNtuple[1] = b;
	    hitNtuple[2] = i+1;
	    hitNtuple[3] = j+1;
	    for(Int_t k=0;k<11;k++)
	      {
		if(pSpt->getNMatched()==conversion[k])
		  {
		    hitNtuple[4]=k; 
		  }
	      }
	    hitNtuple[5] = pSpt->getXg(0);
	    hitNtuple[6] = pSpt->getXg(1);
	    hitNtuple[7] = pSpt->getXg(2);
	    hitNtuple[8] = pSpt->getFlag();
	    
	    Int_t IdP = pSpt->getIdClusterP();
	    Int_t IdN = pSpt->getIdClusterN();
	    
	    StSsdClusterList *currentListP_j = mySsd->mLadders[i]->mWafers[j]->getClusterP();
	    StSsdCluster     *cluster_P_j   = currentListP_j->first();
	    while(cluster_P_j)
	      {
		if(cluster_P_j->getNCluster()==IdP) 
		  break;
		cluster_P_j = currentListP_j->next(cluster_P_j);
	      }
	    
	    StSsdClusterList *currentListN_j = mySsd->mLadders[i]->mWafers[j]->getClusterN();
	    StSsdCluster *cluster_N_j       = currentListN_j->first();
	    while(cluster_N_j)
	      {
		if(cluster_N_j->getNCluster()==IdN) 
		  break;
		cluster_N_j = currentListN_j->next(cluster_N_j);
	      }
	    
	    hitNtuple[9] = cluster_P_j->getStripMean();
	    hitNtuple[10]= cluster_N_j->getStripMean();
	    hitNtuple[11] = pSpt->getPositionU(0);
	    hitNtuple[12] = pSpt->getPositionU(1);
	    hitNtuple[13] = pSpt->getXl(0);
	    hitNtuple[14] = pSpt->getXl(1);
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
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found = 0 ;
  LOG_DEBUG <<"PrintStripDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
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
            LOG_DEBUG<<"PrintStripDetails() - Strip/Adc/Ped/Noise/Analog"<< endm;  
            StSsdStrip *pStripP = mySsd->mLadders[i]->mWafers[j]->getStripP()->first();
            while (pStripP){
              LOG_DEBUG<<"PrintStripDetails() - "
		       <<pStripP->getNStrip()<<" "
		       <<pStripP->getDigitSig()<<" "
		       <<pStripP->getPedestal()<<" "
		       <<pStripP->getSigma()<<" "
		       <<pStripP->getAnalogSig()<<" "
		       <<endm;  
	      for(Int_t e=0;e<5;e++){printf("e=%d idMcHit=%d idMcTrack=%d\n",e,pStripP->getIdMcHit(e),pStripP->getIdMcTrack(e));}
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
            LOG_DEBUG <<"StSsdPointMaker::PrintStripDetails() - Strip/Adc/Ped/Noise/Analog"<< endm;  
            StSsdStrip *pStripN = mySsd->mLadders[i]->mWafers[j]->getStripN()->first();
            while (pStripN){
              LOG_DEBUG<<"PrintStripDetails() - "
		       <<pStripN->getNStrip()<<" "
		       <<pStripN->getDigitSig()<<" "
		       <<pStripN->getPedestal()<<" "
		       <<pStripN->getSigma()<<" "
		       <<pStripN->getAnalogSig()<<" "
		       <<endm;  
	      for(Int_t e=0;e<5;e++){printf("e=%d idMcHit=%d idMcTrack=%d\n",e,pStripN->getIdMcHit(e),pStripN->getIdMcTrack(e));}
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
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found = 0;
  LOG_INFO <<"PrintClusterDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side cluster informations
          if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
            LOG_INFO <<"PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintClusterDetails() - "
		    <<mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<" cluster(s) on the P-side of this wafer "<< endm;  
            LOG_INFO<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
            while (pClusterP){
              LOG_INFO<<"PrintClusterDetails() - "
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
	      for(Int_t e=0;e<5;e++){printf("e=%d idMcHit=%d \n",e,pClusterP->getIdMcHit(e));}
              pClusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);
            }
	  }
          //Looking for the N-side cluster informations
          if (mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
            LOG_INFO <<"PrintClusterDetails() - No cluster on the N-side of this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintClusterDetails() - "
		    <<mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<" cluster(s) on the N-side of this wafer "<< endm;  
            LOG_INFO<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSsdCluster *pClusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
            while (pClusterN){
              LOG_INFO<<"PrintClusterDetails() - "
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
	      for(Int_t e=0;e<5;e++){printf("e=%d idMcHit=%d \n",e,pClusterN->getIdMcHit(e));}
              pClusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
            }     
          }
        }
      }
    }
  if (found==0){ LOG_INFO <<"PrintClusterDetails() - Wafer not found !!!"<<endm; }
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPackageDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;
  found=0;
  LOG_INFO <<"PrintPackageDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()==0) {
            LOG_INFO <<"PrintPackageDetails() - No package in this wafer "<< endm;  
          }
          else {
            LOG_INFO <<"PrintPackageDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPackage()->getSize()<<" package(s) in this wafer "<< endm;  
            LOG_INFO <<"PrintPackageDetails() - Package/Kind/Size"<< endm;  
            StSsdPackage *pPack = mySsd->mLadders[i]->mWafers[j]->getPackage()->first();
            while (pPack){
              LOG_INFO<<"PrintPackageDetails() - "<<pPack->getNPackage()<<" "
		      <<pPack->getKind()<<" "
		      <<pPack->getSize()<<" "<<endm;
              for (Int_t k=0;k<pPack->getSize();k++) {
                LOG_INFO<<"PrintPackageDetails() - "<<k<<" "<<pPack->getMatched(k)<<" "<<pPack->getMatched(k)->getNCluster()<<endm;
              }
              pPack    = mySsd->mLadders[i]->mWafers[j]->getPackage()->next(pPack);
            }     
          }
        }
      }
    }
  if (found==0){ LOG_INFO <<"PrintPackageDetails() - Wafer not found !!!"<<endm;}
}

//_____________________________________________________________________________
void StSsdPointMaker::PrintPointDetails(StSsdBarrel *mySsd, Int_t mywafer)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1} ;
  Int_t found;
  found=0;
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  LOG_INFO <<"PrintPointDetails() - Wafer "<<mywafer<< endm;  
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
        if (mySsd->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
            LOG_INFO <<"PrintPointDetails() - No hit in this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintPointDetails() - "<<mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()<<" hit(s) in this wafer "<< endm; 
 
            LOG_INFO<<"PrintPointDetails() - Hit/Flag/NMatched/IdClusP/IdClusN/idMcHit[0]/idMcHit[1]/idMcHit[2]/idMcHit[3]/idMcHit[4]/Xg[0]/Xg[1]/Xg[2]/Xl[0]/Xl[1]/Xl[2]/a/b"<<endm;  
            StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
            while (pSpt){
	      Float_t a = 0, b = 0;
	      a = convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1));
	      b = convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1));
              LOG_INFO<<"PrintPointDetails() - "
		      <<pSpt->getNPoint()    <<" "
		      <<pSpt->getFlag()      <<" "
		      <<pSpt->getNMatched()  <<" "
		      <<pSpt->getIdClusterP()<<" "
		      <<pSpt->getIdClusterN()<<" "
		      <<pSpt->getNMchit(0)   <<" "
		      <<pSpt->getNMchit(1)   <<" "
		      <<pSpt->getNMchit(2)   <<" "
		      <<pSpt->getNMchit(3)   <<" "
		      <<pSpt->getNMchit(4)   <<" "
		      <<pSpt->getXg(0)       <<" "
		      <<pSpt->getXg(1)       <<" "
		      <<pSpt->getXg(2)       <<" "
		      <<pSpt->getXl(0)       <<" "
		      <<pSpt->getXl(1)       <<" "
		      <<pSpt->getXl(2)       <<" "
		      <<a                    <<" "
		      <<b                    <<" "
		      <<endm;
	      printf("pulseP =%f pulseN = %f\n",a,b);  
              pSpt    = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
            }     
          }
        }
      }
    }
  if (found==0) {LOG_INFO <<"PrintPointDetails() - Wafer not found !!!"<<endm; }
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
  Int_t  mSsdLayer = 7;
  LOG_INFO << "Read_Strip : printing few pedestal/noise values " << endm;
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t iZero  = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t iOver  = 0;
  Int_t iUnder = 0;
  Int_t iGood  = 0;
  for (Int_t i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
	iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	if (iLad==11 && iWaf==8 && nStrip <10) {
	  LOG_DEBUG<<"ReadStrip: iLad,idWaf,nStrip,iSide,pedestal,rms = "<<iLad
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
void StSsdPointMaker::Read_Strip(St_ssdNoise *strip)
{
  ssdNoise_st *noise = strip->GetTable();
  LOG_INFO << " ReadStrip : printing few pedestal/noise values "<< endm;
  Int_t iLad              = 0;
  for (Int_t i = 0 ; i < strip->GetNRows(); i++)
    {
      iLad = noise[i].id/16;
      Int_t idWaf = noise[i].id;
      if (idWaf==1)
	{
	  for(Int_t nStrip=0;nStrip<10;nStrip++)
	    {
	      LOG_DEBUG<<"ReadStrip: iLad,idWaf,nStrip,rmsP,rmsN = "<<iLad+1
		      <<" "<<idWaf+1
		      <<" "<<nStrip
		      <<" "<<(int)(noise[i].rmsp[nStrip])
		      <<" "<<(int)(noise[i].rmsn[nStrip])<<endm;
	    }
	}
    }  
  LOG_INFO<<"ReadStrip: Number of rows in the table : "<<strip->GetNRows()<<endm;
 }
//_____________________________________________________________________________

void StSsdPointMaker::WriteMatchedStrips(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	LOG_DEBUG << " in ladder= "<<i << " wafer = "<<j<<endm;
	
	if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	}
	else{
	  LOG_DEBUG << " Size of the cluster P list = " << mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<endm;
	  StSsdCluster *pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	  while (pclusterP)
	    {  	
	      Int_t IdP = pclusterP->getNCluster();
	      //LOG_DEBUG << " we are looking for clusterId= " << pclusterP->getNCluster()<< endm;
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		if (pclusterP!=mySsd->mLadders[i]->mWafers[j]->getClusterP()->last())
		  pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		Int_t stop = 0;
		while ((pSpt)&&(stop==0)){
		  if(IdP==pSpt->getIdClusterP()) 
		    {
		      StSsdStrip *pStripP     = mySsd->mLadders[i]->mWafers[j]->getStripP()->first();
		      StSsdStrip *pStripLastP = mySsd->mLadders[i]->mWafers[j]->getStripP()->last();
		      while ((pStripP->getNStrip()!=pStripLastP->getNStrip())&&(pStripP->getNStrip()!=pclusterP->getFirstStrip())){
			pStripP    = mySsd->mLadders[i]->mWafers[j]->getStripP()->next(pStripP);
		      }
		      Int_t stripInCluster = pclusterP->getClusterSize();
		      Int_t k = 0;
		      while(k<stripInCluster){
			LOG_DEBUG<<"PrintStripDetails() - "
				 << k << " "
				 <<pStripP->getNStrip()<<" "
				 <<pStripP->getDigitSig()<<" "
				 <<pStripP->getPedestal()<<" "
				 <<pStripP->getSigma()<<" "
				 <<endm;
			StripsIn[0] = 0;
			StripsIn[1] = i+1;
			StripsIn[2] = j+1;
			StripsIn[3] = pStripP->getNStrip();
			StripsIn[4] = pStripP->getPedestal();
			StripsIn[5] = pStripP->getDigitSig();
			StripsIn[6] = pStripP->getSigma();
			StripsIn[7] = (float)(pStripP->getDigitSig()/pStripP->getSigma());
			rHitNtuple->Fill(StripsIn);  
			k++;
			stop=1;// 1 cluster matched to 1 hit
			pStripP    = mySsd->mLadders[i]->mWafers[j]->getStripP()->next(pStripP);
		      }
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
	      }
	      pclusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);
	    }
	}
	if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	}
	else{
	  LOG_DEBUG << " Size of the cluster N list = " << mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<endm;
	  StSsdCluster *pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	  while (pclusterN)
	    {  	
	      Int_t IdN = pclusterN->getNCluster();
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		if (pclusterN!=mySsd->mLadders[i]->mWafers[j]->getClusterN()->last())
		  pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		Int_t stop = 0;
		while ((pSpt)&&(stop==0)){
		  if(IdN==pSpt->getIdClusterN()) 
		    {
		      StSsdStrip *pStripN     = mySsd->mLadders[i]->mWafers[j]->getStripN()->first();
		      StSsdStrip *pStripLastN = mySsd->mLadders[i]->mWafers[j]->getStripN()->last();
		      while ((pStripN->getNStrip()!=pStripLastN->getNStrip())&&(pStripN->getNStrip()!=pclusterN->getFirstStrip())){
			pStripN    = mySsd->mLadders[i]->mWafers[j]->getStripN()->next(pStripN);
		      }
		      Int_t stripInClusterN = pclusterN->getClusterSize();
		      Int_t kk = 0;
		      while(kk<stripInClusterN){
			LOG_DEBUG<<"PrintStripDetails() - "
				 << kk << " "
				 <<pStripN->getNStrip()<<" "
				 <<pStripN->getDigitSig()<<" "
				 <<pStripN->getPedestal()<<" "
				 <<pStripN->getSigma()<<" "
				 <<endm;
			StripsIn[0] = 1;
			StripsIn[1] = i+1;
			StripsIn[2] = j+1;
			StripsIn[3] = pStripN->getNStrip();
			StripsIn[4] = pStripN->getPedestal();
			StripsIn[5] = pStripN->getDigitSig();
			StripsIn[6] = pStripN->getSigma();
			StripsIn[7] = (float)(pStripN->getDigitSig()/pStripN->getSigma());
			rHitNtuple->Fill(StripsIn);  
			kk++;
			stop=1;// 1 cluster matched to 1 hit
			pStripN    = mySsd->mLadders[i]->mWafers[j]->getStripN()->next(pStripN);
		      }
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
	      }
	      pclusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);
	    }
	}
      }
    }
}
//_____________________________________________________________________________
void StSsdPointMaker::WriteMatchedClusters(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t clusP = 0;
  Int_t clusN = 0;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	LOG_DEBUG << " in ladder= "<<i << " wafer = "<<j<<endm;
	if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	}
	else{
	  LOG_DEBUG << " Size of the cluster P list = " << mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<endm;
	  StSsdCluster *pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	  while (pclusterP)
	    {  	
	      LOG_DEBUG << " clusterId= " << pclusterP->getNCluster()<< endm;
	      Int_t IdP = pclusterP->getNCluster();
	      LOG_DEBUG << " we are looking for clusterId= " << pclusterP->getNCluster()<< endm;
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);  
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		while (pSpt){
		  if(IdP==pSpt->getIdClusterP())
		    {
		      clusP++;
		      LOG_DEBUG << "ok found the corresponding hit to this cluster id = "<<pSpt->getIdClusterP()<<endm; 
		      ClustupleIn[0] = 0;
		      ClustupleIn[1] = i+1;
		      ClustupleIn[2] = j+1;
		      ClustupleIn[3] = pclusterP->getClusterSize();
		      ClustupleIn[4] = ((pclusterP->getTotAdc()*pclusterP->getClusterSize())/pclusterP->getTotNoise());
		      ClustupleIn[5] = pclusterP->getTotNoise()/pclusterP->getClusterSize();
		      ClustupleIn[6] = pclusterP->getFirstStrip();
		      ClustupleIn[7] = pclusterP->getTotAdc();
		      ClustupleIn[8] = pclusterP->getFirstAdc();
		      ClustupleIn[9] = pclusterP->getLastAdc();
		      ClustupleIn[10]= pclusterP->getTotNoise();
		      pHitNtuple->Fill(ClustupleIn);
		      //break;
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
		pclusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);
	      }
	    }
	  clusP = 0;
	}
	if (mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
	}
	else{
	  LOG_DEBUG << " Size of the cluster N list = " << mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<endm;
	  StSsdCluster *pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	  while (pclusterN)
	    {  	
	      //LOG_DEBUG << " clusterId= " << pclusterN->getNCluster()<< endm;
	      Int_t IdN = pclusterN->getNCluster();
	      //LOG_DEBUG << " we are looking for clusterId= " << pclusterN->getNCluster()<< endm;
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);  
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		while (pSpt){
		  if(IdN==pSpt->getIdClusterN())
		    {
		      clusN++;
		      LOG_DEBUG << "ok found the corresponding hit to this cluster id = "<<pSpt->getIdClusterN()<<endm; 
		      ClustupleIn[0] = 1;
		      ClustupleIn[1] = i+1;
		      ClustupleIn[2] = j+1;
		      ClustupleIn[3] = pclusterN->getClusterSize();
		      ClustupleIn[4] = ((pclusterN->getTotAdc()*pclusterN->getClusterSize())/pclusterN->getTotNoise());
		      ClustupleIn[5] = pclusterN->getTotNoise()/pclusterN->getClusterSize();
		      ClustupleIn[6] = pclusterN->getFirstStrip();
		      ClustupleIn[7] = pclusterN->getTotAdc();
		      ClustupleIn[8] = pclusterN->getFirstAdc();
		      ClustupleIn[9] = pclusterN->getLastAdc();
		      ClustupleIn[10]= pclusterN->getTotNoise();
		      pHitNtuple->Fill(ClustupleIn);
		      //break;
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
		pclusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);
	      }
	    }
	  clusN = 0;
	}
      }
    }
  LOG_DEBUG << "Number of p-side clusters in hits = " << clusP << endm ;
  LOG_DEBUG << "Number of n-side clusters in hits = " << clusN << endm ;
}
//_____________________________________________________________________________
void StSsdPointMaker::EvaluateEfficiency(StSsdBarrel *mySsd)
{
  //Int_t LadderIsActive[20]={1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  Int_t clusP = 0;
  Int_t clusN = 0;
  for (Int_t i=0;i<20;i++) 
    if (mySsd->isActiveLadder(i)>0) {
      for (Int_t j=0; j<mySsd->mLadders[i]->getWaferPerLadder();j++) {
	if (mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
	}
	else{
	  StSsdCluster *pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->first();
	  while (pclusterP)
	    {  	
	      Int_t IdP = pclusterP->getNCluster();
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		pclusterP = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);  
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		while (pSpt){
		  if(IdP==pSpt->getIdClusterP())
		    {
		      clusP++;
		      //break;
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
		pclusterP    = mySsd->mLadders[i]->mWafers[j]->getClusterP()->next(pclusterP);
	      }
	    }
	  ratioP[i][j]+=(float)clusP/mySsd->mLadders[i]->mWafers[j]->getClusterP()->getSize();
	  clusP = 0;
	}
	if (mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
	}
	else{
	  StSsdCluster *pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->first();
	  while (pclusterN)
	    {  	
	      Int_t IdN = pclusterN->getNCluster();
	      if (mySsd->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) { 
		pclusterN = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);  
	      }
	      else {
		StSsdPoint *pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->first();
		while (pSpt){
		  if(IdN==pSpt->getIdClusterN())
		    {
		      clusN++;
		      //break;
		    }
		  pSpt = mySsd->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
		}
		pclusterN    = mySsd->mLadders[i]->mWafers[j]->getClusterN()->next(pclusterN);
	      }
	    }
	  ratioN[i][j]+=(float)clusN/mySsd->mLadders[i]->mWafers[j]->getClusterN()->getSize();
	  clusN = 0;
	}
      }
    }
}
//_____________________________________________________________________________
void StSsdPointMaker::NormalizeEfficiency(){
  Double_t NClusP = 0;
  Double_t NClusN = 0;
  for(Int_t ii=0 ;ii< 20 ;ii++)
    {
      for(Int_t jj=0 ;jj<16;jj++)
	{
	  NClusP = ratioP[ii][jj];
	  //LOG_INFO<<Form("NEvent=%d side P ladder=%d wafer=%d bin content=%f\n",NEvent,ii,jj,NClusP)<<endm;
	  if(NEvent!=0){
	    NClusP = NClusP/NEvent;
	    MatchedClusterP->SetBinContent(ii+1,jj+1,NClusP);}
	  NClusN = ratioN[ii][jj];
	  if(NEvent!=0){
	    //LOG_INFO<<Form("NEvent=%d side N ladder=%d wafer=%d bin content=%f\n",NEvent,ii,jj,NClusN)<<endm;
	  NClusN = NClusN/NEvent;
	  MatchedClusterN->SetBinContent(ii+1,jj+1,NClusN);}
	}
    }
}
//_____________________________________________________________________________
void StSsdPointMaker::FillCalibTable(){
  mGain          = (St_ssdGainCalibWafer*)GetDataBase("Calibrations/ssd/ssdGainCalibWafer"); 
  if(mGain){ 
    ssdGainCalibWafer_st *g  = mGain->GetTable() ;
    Int_t size = mGain->GetNRows();
    LOG_INFO<<Form("Size of gain table = %d",(int)mGain->GetNRows())<<endm;
    for(Int_t i=0; i<size;i++){
      LOG_DEBUG<<Form(" Print entry %d : ladder=%d gain =%lf wafer=%d",i,g[i].nLadder,g[i].nGain,g[i].nWafer)<<endm;
      CalibArray[i] = g[i].nGain;
    }
  }
  else { 
    LOG_WARN << "InitRun : No access to Gain Calib - will use the default gain" << endm;
    LOG_WARN << "We will use the default table" <<endm;
    for(Int_t i=0; i<320;i++){
      CalibArray[i] = 1;
    }
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::FillDefaultCalibTable(){
  LOG_INFO << " The calibration gain will not be used." << endm;
  for(Int_t i=0; i<320;i++){
    CalibArray[i] = 1;
    //LOG_INFO << Form("wafer=%d gain=%f",i,CalibArray[i])<<endm; 
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::FillWaferTable(){
  mWafConfig    = (St_ssdWaferConfiguration*) GetDataBase("Calibrations/ssd/ssdWaferConfiguration"); 
  if(mWafConfig){ 
    ssdWaferConfiguration_st *g  = mWafConfig->GetTable() ;
    Int_t size = mWafConfig->GetNRows();
    for(Int_t i=0; i<size;i++){
      LOG_DEBUG<<Form(" Print entry=%d : ladder=%d wafer=%d status=%d",i,g[i].nLadder,g[i].nWafer,g[i].nStatus)<<endm;
      WafStatus[g[i].nLadder][g[i].nWafer] = g[i].nStatus; 
    }
  } 
  else { 
    LOG_WARN << "InitRun : No access to Wafer Config - will use the default wafer config" << endm;
    LOG_WARN << "We will use the default table" <<endm;
    for(Int_t i=0; i<20;i++){
      for(Int_t j=0; j<16;j++){ 
	WafStatus[i][j] = 1;
      }
    }
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::FillDefaultWaferTable(){
  LOG_INFO << " The wafer configuration table will not be used." << endm;
  for(Int_t i=0; i<20;i++){ 
    for(Int_t j=0; j<16;j++){  
      WafStatus[i][j] = 1; 
      LOG_DEBUG << Form("wafer=%d gain=%f",i,CalibArray[i])<<endm; 
    }
  }
}
//_____________________________________________________________________________

Int_t StSsdPointMaker::ReadNoiseTable(StSsdBarrel *mySsd,Int_t year){
  Int_t noiseTableSize = 0; 
  
  //ssdStripCalib is used for year <2007 and for the simulation
  if((year<7)||(mode==1))
    {
      if (!m_noise2)
	{
	  LOG_WARN << "Make : No pedestal and noise values (ssdStripCalib table missing), will use default values" <<endm;
	  noiseTableSize = mySsd->readNoiseDefault(mDynamicControl);
	}
      else
	{
	  if(Debug()) {Read_Strip(m_noise2);}
	  noiseTableSize = mySsd->readNoiseFromTable(m_noise2,mDynamicControl);
	}
    }
  else if (year>=7){
    if(!m_noise3)
      {
	LOG_WARN << "Make : No pedestal and noise values (ssdNoise table missing), will use default values" << endm;
	noiseTableSize = mySsd->readNoiseDefault(mDynamicControl);
      }
    else
      if(m_noise3)
	{
	  if (Debug()){Read_Strip(m_noise3);}
	  noiseTableSize = mySsd->readNoiseFromTable(m_noise3,mDynamicControl);
	}
  }
  return noiseTableSize;  
}
//____________________________________________________________________________
Int_t StSsdPointMaker::Finish() {
  LOG_INFO << Form("Finish()") << endm;
   return kStOK;
}

