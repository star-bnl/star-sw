/***************************************************************************
 *
 * $Id: StSvtMonitor.cc,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtMonitor.cc,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include <fstream>

#include "TOrdCollection.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TGraph.h"
#include "TStyle.h"
#include "TPaveText.h"
#include "TStopwatch.h"
#include "StTFourierTransform.h"

#include "StEventTypes.h"
#include "StarClassLibrary/StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridPixels2.hh"
#include "StSvtClassLibrary/StSvtHybridStat.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtHybridHist.hh"
#include "StSvtHybridHist2D.hh"
#include "StSvtHybridHistAnalog.hh"
#include "StSvtHybridGraph.hh"
#include "StSvtHist.hh"
#include "StSvtHist2D.hh"
#include "StSvtHistAnalog.hh"
#include "StSvtGraph.hh"
#include "StSvtMonitor.hh"

// In the future, get this information from Data base
#define N_ANODES 240
#define N_TIMEBINS 128
#define MAX_NUMBER_OF_EVENTS 10000
#define MAX_NUMBER_OF_HISTOGRAMS 100
#define N_BINS_ANODES 240
#define N_BINS_TIME 128
#define N_WAFERS 216
#define N_HYBRIDS 432
#define ADC_THRESHOLD 50
// end

#define kFAST 0

TH1F* outFourier = new TH1F("fftAmpl","amplitude",250,0.,1.);
TH1F* phaseFourier = new TH1F("fftPhase","phase",250,0.,1.);

//int defaultListHist[] = {1,2,3,4,5,6,7,8,9,10,11,12,51,52,53,54,101,102,103,104,201,202,107,108,111,112,113,114,211,212,117,118,121,122,123,124,221,222,127,128,151,251,252,253,254,255};
int defaultListHist[] = {1,2,3,4,7,8,51,52,53,54,55,56,57,58,101,102,103,104,107,108,151,152,153,154,201,202,251,252,253,254,255,301,302,303,304,351,352,353,354,355,1000};

ClassImp(StSvtMonitor)

StSvtMonitor::StSvtMonitor(const char* config, int* listHist, const int maxHist)
{
  cout << "monitor constructor : " << this << endl;
  

  //Initialize some variables with default values
  mConfig = (char*)config;
  mEvents = 0;
  mStartTime = 0;
  mNPixels = 0;
  mScaleRms = 1;

  barrelID = 1;
  ladderID = 1;
  waferID = 1;
  hybridID = 1;

  histID = 1;

  kZSSTAT = kFALSE;
  kRAWSTAT = kFALSE;
  kPEDSTAT = kFALSE;
  kCMNSTAT = kFALSE;
  kEVENTSTAT = kFALSE;
  kPIXELSTAT = kFALSE;
  kGLOBALSTAT = kFALSE;
  kSAVEMEM = kTRUE;
  kFIRSTORD  = kTRUE;
  kSECONDORD = kFALSE;
  kPEDCAP  = kFALSE;
  kPEDTIME = kTRUE;
  kPRIMARY = kTRUE;
  kGLOBAL  = kFALSE;
  kTOTALSTAT = kFALSE;

  if (mConfig) {

    if ( !strncmp(mConfig, "SYST", strlen("SYST"))) {
      barrelID = 3;
      ladderID = 3;
    }
    if ( !strncmp(mConfig, "Y1L", strlen("Y1L")) ) {
      barrelID = 3;
      ladderID = 2;
    }
          
    //Create pointer to pedestal and cmn
    pedestal = NULL;
    pedestal2ndOrd = NULL;
    cmn = new StSvtHybridCollection(mConfig);

    // book histograms    
    if (listHist)
      bookHist(listHist,maxHist);
    else
      bookHist(defaultListHist,sizeof(defaultListHist)/sizeof(int));

    // Initialize statistics variables
    initStatistics();     
  }

  event = 0;
}

StSvtMonitor::~StSvtMonitor()
{
  //delete event;

  delete histograms;

  //delete pedestal;
  delete cmn;
}

StSvtMonitor::StSvtMonitor(const StSvtMonitor &m)
  // copy constructor
{}

void StSvtMonitor::initStatistics()
{
  if (!event)
    event = new StSvtData(mConfig);

  if (kZSSTAT) {
    if (!meanAdc) meanAdc = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
    if (!meanSqAdc) meanSqAdc = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
    if (!nEvents) nEvents = new int[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];      
  }
  else {
    if (meanAdc) delete meanAdc;
    if (meanSqAdc) delete meanSqAdc;
    if (nEvents) delete nEvents;
    meanAdc = NULL;
    meanSqAdc = NULL;
    nEvents = NULL;
  }

  if (kRAWSTAT) {
    if (!accumAdcRaw) accumAdcRaw = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
    if (!accumAdcSqRaw) accumAdcSqRaw = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
  }
  else {
    if (accumAdcRaw) delete accumAdcRaw;
    if (accumAdcSqRaw) delete accumAdcSqRaw;
    accumAdcRaw = NULL;
    accumAdcSqRaw = NULL;
  }

  if (kPEDSTAT) {
    if (!accumAdcPed) accumAdcPed = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
    if (!accumAdcSqPed) accumAdcSqPed = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
  }
  else {
    if (accumAdcPed) delete accumAdcPed;
    if (accumAdcSqPed) delete accumAdcSqPed;
    accumAdcPed = NULL;
    accumAdcSqPed = NULL;
  }

  if (kCMNSTAT) {
    if (!accumAdcCMN) accumAdcCMN = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
    if (!accumAdcSqCMN) accumAdcSqCMN = new float[event->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS];
  }
  else {
    if (accumAdcCMN) delete accumAdcCMN;
    if (accumAdcSqCMN) delete accumAdcSqCMN;
    accumAdcCMN = NULL;
    accumAdcSqCMN = NULL;
  }

  if (kEVENTSTAT) {
    if (kRAWSTAT) {
      if (!meanEventRaw) meanEventRaw = new StSvtHybridCollection(mConfig);
      if (!rmsEventRaw) rmsEventRaw = new StSvtHybridCollection(mConfig);
    }
    if (kPEDSTAT) {
      if (!meanEventPed) meanEventPed = new StSvtHybridCollection(mConfig);
      if (!rmsEventPed) rmsEventPed = new StSvtHybridCollection(mConfig);
    }
    if (kCMNSTAT) {
      if (!rmsEventCMN) meanEventCMN = new StSvtHybridCollection(mConfig);
      if (!rmsEventCMN) rmsEventCMN = new StSvtHybridCollection(mConfig);
    }
    if (!nEvent) nEvent = new StSvtHybridCollection(mConfig);
  }
  else {
    if (meanEventRaw) delete meanEventRaw;
    if (rmsEventRaw) delete rmsEventRaw;
    if (meanEventPed) delete meanEventPed;
    if (rmsEventPed) delete rmsEventPed;
    if (meanEventCMN) delete meanEventCMN;
    if (rmsEventCMN) delete rmsEventCMN;
    if (nEvent) delete nEvent;
    meanEventRaw = NULL;
    rmsEventRaw = NULL;
    meanEventPed = NULL;
    rmsEventPed = NULL;
    meanEventCMN = NULL;
    rmsEventCMN = NULL;
    nEvent = NULL;
  }

  if (kPIXELSTAT) {
    nPixelsArray = new TArrayF(MAX_NUMBER_OF_EVENTS);
    nEventsArray = new TArrayF(MAX_NUMBER_OF_EVENTS);
  }
  else {
    if (nPixelsArray) delete nPixelsArray;
    if (nEventsArray) delete nEventsArray;
    nPixelsArray = NULL;
    nEventsArray = NULL;
  }

  resetBuffers();
  kTOTALSTAT = kZSSTAT || kRAWSTAT || kPEDSTAT || kCMNSTAT || kEVENTSTAT || kPIXELSTAT || kGLOBALSTAT;
}

void StSvtMonitor::setStatistics()
{
  if (event) {
    mEvents++;
    mNPixels = 0;
    
    TStopwatch test; 
    test.Start();
    
    if (!kFAST) {
      cout << "Filling Statistics..." << endl;
      if (kTOTALSTAT) fillSvtStat();
      cout << "Done!" << endl;
    }
    
    cout << mEvents << " event(s) in buffer!" << endl;
    test.Stop();
    cout << "CPU Time = " << test.CpuTime() << "  Real Time = " << test.RealTime() << endl;
  }
  else
    cout << "NO event to set statistics!!! Get one..." << endl;
}

int StSvtMonitor::getEventNumber()
{
  if (event)
    return event->getEventNumber();
  else
    return 0;
}

TH1* StSvtMonitor::getHist()
{
  int index_hyb, indexHist;

  indexHist = getHistIndex(histID);
  if (indexHist < 0) return 0;

  const char* className = histograms->At(indexHist)->ClassName();
  if ( strncmp(className, "StSvtHist", strlen("StSvtHist")) &&
       strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) )
    return 0;

  StSvtHist* svtHist;
  StSvtHybridHist* hybridHist;

  svtHist = (StSvtHist*)histograms->At(indexHist) ;
  if (svtHist) {
    index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridHist = (StSvtHybridHist*)svtHist->at(index_hyb);
    return hybridHist->getHist() ;
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return 0;
  }
}

StSvtHist* StSvtMonitor::getSvtHist(int id)
{
  int indexHist;

  if (id)
    indexHist = getHistIndex(id);
  else
    indexHist = getHistIndex(histID);
  if (indexHist < 0) return 0;

  const char* className = histograms->At(indexHist)->ClassName();
  if ( strncmp(className, "StSvtHist", strlen("StSvtHist")) &&
       strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) )
    return 0;

  return (StSvtHist*)histograms->At(indexHist);
}

TGraph* StSvtMonitor::getGraph()
{
  int index_hyb, indexGraph;

  indexGraph = getHistIndex(histID);
  if (indexGraph < 0) return 0;

  const char* className = histograms->At(indexGraph)->ClassName();
  if ( strncmp(className, "StSvtGraph", strlen("StSvtGraph")) )
    return 0;

  StSvtGraph* svtGraph;
  StSvtHybridGraph* hybridGraph;

  svtGraph = (StSvtGraph*)histograms->At(indexGraph) ;
  if (svtGraph) {
    index_hyb = svtGraph->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridGraph = (StSvtHybridGraph*)svtGraph->at(index_hyb);
    return hybridGraph->getGraph() ;
  }
  else {
    cout << "Not a valid GRAPH!!!" << endl;
    return 0;
  }
}

StSvtHybridGraph* StSvtMonitor::getHybridGraph()
{
  const char* className = histograms->At(getHistIndex(histID))->ClassName();
  if ( strncmp(className, "StSvtGraph", strlen("StSvtGraph")) )
    return 0;

  int index_hyb, indexGraph;

  StSvtGraph* svtGraph;
  StSvtHybridGraph* hybridGraph;

  indexGraph = getHistIndex(histID);
  if (indexGraph < 0) return 0;

  svtGraph = (StSvtGraph*)histograms->At(indexGraph) ;
  if (svtGraph) {
    index_hyb = svtGraph->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridGraph = (StSvtHybridGraph*)svtGraph->at(index_hyb);
    return hybridGraph;
  }
  else {
    cout << "Not a valid GRAPH!!!" << endl;
    return 0;
  }
}

StSvtGraph* StSvtMonitor::getSvtGraph()
{
  const char* className = histograms->At(getHistIndex(histID))->ClassName();
  if ( strncmp(className, "StSvtGraph", strlen("StSvtGraph")) )
    return 0;

  int indexGraph;

  indexGraph = getHistIndex(histID);
  if (indexGraph < 0) return 0;

  return (StSvtGraph*)histograms->At(indexGraph);
}

//*****************************************************************************

void StSvtMonitor::bookHist(int* listHist, const int maxHist)
{
  //Create container of histograms
  histograms = new TOrdCollection;
  histIDArray = new int[maxHist];
  for (int i=0; i<maxHist; i++)
    histIDArray[i] = 0;

  char* config = mConfig;
  if (kSAVEMEM)
    config = "LADDER";

  //Define and Add histograms to collection
  for (int i=0;i<maxHist;i++) {

    switch(listHist[i]) {

    case 1:
      addHistToCollection(new StSvtHist(config,"Raw","Raw Adc",256,-0.5,255.5),1);
      ((StSvtHist*)histograms->At(getHistIndex(1)))->setXTitle("ADC (channel)");
      break;

    case 2:
      addHistToCollection(new StSvtHist2D(config,"Raw2D","Raw Adc",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),2);
      ((StSvtHist2D*)histograms->At(getHistIndex(2)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(2)))->setYTitle("Time Bin");
      break;

    case 3:
      addHistToCollection(new StSvtHist(config,"Ped","Ped Sub Adc",256,-127.5,127.5),3);
      ((StSvtHist*)histograms->At(getHistIndex(3)))->setXTitle("ADC-Pedestal (channel)");
      break;

    case 4:
      addHistToCollection(new StSvtHist2D(config,"Ped2D","Ped Sub Adc",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),4);
      ((StSvtHist2D*)histograms->At(getHistIndex(4)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(4)))->setYTitle("Time Bin");
      break;

    case 5:
      addHistToCollection(new StSvtHist(config,"CMN","Ped Sub Adc",256,-127.5,127.5),5);
      ((StSvtHist*)histograms->At(getHistIndex(5)))->setXTitle("ADC-Pedestal-CMN (channel)");
      break;

    case 6:
      addHistToCollection(new StSvtHist2D(config,"CMN2D","Ped CMN Sub Adc",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),6);
      ((StSvtHist2D*)histograms->At(getHistIndex(6)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(6)))->setYTitle("Time Bin");
      break;

    case 7:
      addHistToCollection(new StSvtHist(config,"Time","Average Over Anodes",N_BINS_TIME,-0.5,127.5),7);
      ((StSvtHist*)histograms->At(getHistIndex(7)))->setXTitle("Time Bin");
      break;

    case 8:
      addHistToCollection(new StSvtHist(config,"Anode","Average Over Time",N_BINS_ANODES,0.5,240.5),8);
      ((StSvtHist*)histograms->At(getHistIndex(8)))->setXTitle("Anode Number");
      break;

    case 10:
      addHistToCollection(new StSvtHist2D(config,"Projected Tracks","Projected Tracks",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),10);
      ((StSvtHist2D*)histograms->At(getHistIndex(10)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(10)))->setYTitle("Time Bin");
      ((StSvtHist2D*)histograms->At(getHistIndex(10)))->setMarkerStyle(5);
      ((StSvtHist2D*)histograms->At(getHistIndex(10)))->setMarkerSize(3);
      ((StSvtHist2D*)histograms->At(getHistIndex(10)))->setLineWidth(2);
      break;
      
    case 51:
      addHistToCollection(new StSvtHist("BARREL","NPixPed","Number of Pixels above Pedestal",14,0.5,14.5),51);
      ((StSvtHist*)histograms->At(getHistIndex(51)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(51)))->setYTitle("Number of Pixels (%)");
      break;

    case 52:
      addHistToCollection(new StSvtHist("BARREL","NPixThresh","Number of Pixels above ADC threshold",14,0.5,14.5),52);
      ((StSvtHist*)histograms->At(getHistIndex(52)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(52)))->setYTitle("Number of Pixels (%)");
      break;

    case 53:
      addHistToCollection(new TH1F("RawAll","Raw Adc All hybrids",256,-0.5,255.5),53);
      ((TH1F*)histograms->At(getHistIndex(53)))->SetXTitle("ADC (channel)");
      break;

    case 54:
      addHistToCollection(new StSvtHist("SVT","NPixPedHyb","Number of Pixels above Pedestal per Hybrid",224,0.5,224.5),54);
      ((StSvtHist*)histograms->At(getHistIndex(54)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(54)))->setYTitle("Number of Pixels (%)");
      break;

    case 55:
      addHistToCollection(new StSvtHist("SVT","NPixThreshHyb","Number of Pixels above ADC threshold per Hybrid",224,0.5,224.5),55);
      ((StSvtHist*)histograms->At(getHistIndex(55)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(55)))->setYTitle("Number of Pixels (%)");
      break;

    case 56:
      addHistToCollection(new StSvtHist("SVT","NPixPedHalf","Number of Pixels above Pedestal per Half Ladder",32,0.5,32.5),56);
      ((StSvtHist*)histograms->At(getHistIndex(56)))->setXTitle("Half Ladder");
      ((StSvtHist*)histograms->At(getHistIndex(56)))->setYTitle("Number of Pixels (%)");
      break;

    case 57:
      addHistToCollection(new StSvtHist("SVT","NPixThreshHalf","Number of Pixels above ADC threshold per Half Ladder",32,0.5,32.5),57);
      ((StSvtHist*)histograms->At(getHistIndex(57)))->setXTitle("Half Ladder");
      ((StSvtHist*)histograms->At(getHistIndex(57)))->setYTitle("Number of Pixels (%)");
      break;

    case 58:
      addHistToCollection(new TH1F("fft","amplitude",250,0.,25.),58);
      ((TH1F*)histograms->At(getHistIndex(58)))->SetXTitle("frequency (MHz)");
      break;

    case 101:
      addHistToCollection(new StSvtHist2D(config,"Average","Mean Over Events (RAW ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),101);
      ((StSvtHist2D*)histograms->At(getHistIndex(101)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(101)))->setYTitle("Time Bin");
      break;

    case 102:
      addHistToCollection(new StSvtHist2D(config,"RMS","RMS Over Events (RAW ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),102);
      ((StSvtHist2D*)histograms->At(getHistIndex(102)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(102)))->setYTitle("Time Bin");
      break;

    case 103:
      addHistToCollection(new StSvtHistAnalog(config,"AveAll","Mean Over Events (All Pixels)",384,-127.5,255.5),103);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(103)))->setXTitle("Mean Value(channel)");
      break;

    case 104:
      addHistToCollection(new StSvtHistAnalog(config,"RMSAll","RMS Over Events (All Pixels)",250,0.,50.),104);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(104)))->setXTitle("RMS (channel)");
      break;

    case 107:
      addHistToCollection(new StSvtGraph(mConfig),107);
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setTitle("Mean(Raw) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setXTitle("Event Number");
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setYTitle("Mean Value (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setDefaultMaximum(256);
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setDefaultMinimum(0);
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(107)))->setMinimum();
      break;

    case 108:
      addHistToCollection(new StSvtGraph(mConfig),108);
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setTitle("RMS(Raw) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setXTitle("Event Number");
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setYTitle("RMS (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setDefaultMaximum(50);
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setDefaultMinimum(0);
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(108)))->setMinimum();
      break;
      
    case 109:
      addHistToCollection(new StSvtGraph(mConfig),109);
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setTitle("Time of Laser vs Event");
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setXTitle("Event #");
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setYTitle("Time of Laser");
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setDefaultMaximum(128);
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setDefaultMinimum(0);
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(109)))->setMinimum();
      break;

    case 111:
      addHistToCollection(new StSvtHist2D(config,"Average_PED","Mean Over Events (Ped Sub ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),111);
      ((StSvtHist2D*)histograms->At(getHistIndex(111)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(111)))->setYTitle("Time Bin");
      break;

    case 112:
      addHistToCollection(new StSvtHist2D(config,"RMS_PED","RMS Over Events (Ped Sub ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),112);
      ((StSvtHist2D*)histograms->At(getHistIndex(112)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(112)))->setYTitle("Time Bin");
      break;

    case 113:
      addHistToCollection(new StSvtHistAnalog(config,"AveAll_PED","Mean Over Events (All Pixels)",384,-127.5,255.5),113);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(113)))->setXTitle("Mean Value(channel)");
      break;

    case 114:
      addHistToCollection(new StSvtHistAnalog(config,"RMSAll_PED","RMS Over Events (All Pixels)",250,0.,10.),114);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(114)))->setXTitle("RMS (channel)");
      break;

    case 117:
      addHistToCollection(new StSvtGraph(mConfig),117);
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setTitle("Mean(Ped Sub) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setXTitle("Time (hours)");
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setYTitle("Mean Value (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setDefaultMaximum(10);
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setDefaultMinimum(-10);
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(117)))->setMinimum();
      break;

    case 118:
      addHistToCollection(new StSvtGraph(mConfig),118);
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setTitle("RMS(Ped Sub) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setXTitle("Time (hours)");
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setYTitle("RMS (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setDefaultMaximum(20);
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setDefaultMinimum(0);
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(118)))->setMinimum();
      break;
      
    case 121:
      addHistToCollection(new StSvtHist2D(config,"Average_CMN","Mean Over Events (Ped and CMN Sub ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),121);
      ((StSvtHist2D*)histograms->At(getHistIndex(121)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(121)))->setYTitle("Time Bin");
      break;

    case 122:
      addHistToCollection(new StSvtHist2D(config,"RMS_CMN","RMS Over Events (Ped and CMN Sub ADC)",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),122);
      ((StSvtHist2D*)histograms->At(getHistIndex(122)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(122)))->setYTitle("Time Bin");
      break;

    case 123:
      addHistToCollection(new StSvtHistAnalog(config,"AveAll_CMN","Mean Over Events (All Pixels)",384,-127.5,255.5),123);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(123)))->setXTitle("Mean Value(channel)");
      break;

    case 124:
      addHistToCollection(new StSvtHistAnalog(config,"RMSAll_a_CMN","RMS Over Events (All Pixels)",250,0.,10),124);
      ((StSvtHistAnalog*)histograms->At(getHistIndex(124)))->setXTitle("RMS (channel)");
      break;

    case 127:
      addHistToCollection(new StSvtGraph(mConfig),127);
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setTitle("Mean(CMN Sub) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setXTitle("Time (hours)");
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setYTitle("Mean Value (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setDefaultMaximum(10);
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setDefaultMinimum(-10);
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(127)))->setMinimum();
      break;

    case 128:
      addHistToCollection(new StSvtGraph(mConfig),128);
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setTitle("RMS(CMN Sub) vs Time");
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setXTitle("Time (hours)");
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setYTitle("RMS (channel)");
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setDefaultMaximum(20);
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setDefaultMinimum(0);
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setMaximum();
      ((StSvtGraph*)histograms->At(getHistIndex(128)))->setMinimum();
      break;

    case 151:
      addHistToCollection(new TH1F("RMSAllHyb","RMS Raw Adc All hybrids",250,0.,50.),151);
      ((TH1F*)histograms->At(getHistIndex(151)))->SetXTitle("RMS (channel)");
      break;
      
    case 152:
      addHistToCollection(new StSvtHist("BARREL","RMSLadder","RMS Ped per ladder",14,0.5,14.5),152);
      ((StSvtHist*)histograms->At(getHistIndex(152)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(152)))->setYTitle("RMS (channel)");
      break;

    case 153:
      addHistToCollection(new StSvtHist("SVT","RMSBarrel","RMS Ped per barrel",32,0.5,32.5),153);
      ((StSvtHist*)histograms->At(getHistIndex(153)))->setXTitle("Half Ladder");
      ((StSvtHist*)histograms->At(getHistIndex(153)))->setYTitle("RMS (channel)");
      break;

    case 154:
      addHistToCollection(new StSvtHist("SVT","RMSHybBarrel","RMS Ped per barrel",224,0.5,224.5),154);
      ((StSvtHist*)histograms->At(getHistIndex(154)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(154)))->setYTitle("RMS (channel)");
      break;

    case 201:
      addHistToCollection(new StSvtHist(mConfig,"OverAve","Mean Over Pixels (All Events)",384,-127.5,255.5),201);
      ((StSvtHist*)histograms->At(getHistIndex(201)))->setXTitle("Mean Value(channel)");
      break;

    case 202:
      addHistToCollection(new StSvtHist(mConfig,"OverRMS","RMS Over Pixels (All Events)",100,0.,100),202);
      ((StSvtHist*)histograms->At(getHistIndex(202)))->setXTitle("RMS (channel)");
      break;

    case 211:
      addHistToCollection(new StSvtHist(mConfig,"OverAve_PED","Mean Over Pixels (All Events)",384,-127.5,255.5),211);
      ((StSvtHist*)histograms->At(getHistIndex(211)))->setXTitle("Mean Value(channel)");
      break;

    case 212:
      addHistToCollection(new StSvtHist(mConfig,"OverRMS_PED","RMS Over Pixels (All Events)",100,0.,100),212);
      ((StSvtHist*)histograms->At(getHistIndex(212)))->setXTitle("RMS (channel)");
      break;

    case 221:
      addHistToCollection(new StSvtHist(mConfig,"OverAve_CMN","Mean Over Pixels (All Events)",384,-127.5,255.5),221);
      ((StSvtHist*)histograms->At(getHistIndex(221)))->setXTitle("Mean Value(channel)");
      break;

    case 222:
      addHistToCollection(new StSvtHist(mConfig,"OverRMS_CMN","RMS Over Pixels (All Events)",100,0.,100),222);
      ((StSvtHist*)histograms->At(getHistIndex(222)))->setXTitle("RMS (channel)");
      break;

    case 251:
      //addHistToCollection(new StSvtHist("BARREL","NPixPedM","Number of Pixels above Pedestal",14,0.5,14.5),251);
      addHistToCollection(new StSvtHist("SVT","NPixPedM","Number of Pixels above Pedestal",224,0.5,224.5),251);
      ((StSvtHist*)histograms->At(getHistIndex(251)))->setXTitle("Hybrid Number");
      ((StSvtHist*)histograms->At(getHistIndex(251)))->setYTitle("Number of Pixels (%)");
      break;

    case 252:
      addHistToCollection(new StSvtHist("BARREL","NPix1000M","Number of Pixels above ADC threshold",14,0.5,14.5),252);
      ((StSvtHist*)histograms->At(getHistIndex(252)))->setXTitle("Hybrid Number");
      ((StSvtHist*)histograms->At(getHistIndex(252)))->setYTitle("Number of Pixels (%)");
      break;

    case 253:
      addHistToCollection(new StSvtHist("BARREL","MeanAnode","Mean Anode Number",14,0.5,14.5),253);
      ((StSvtHist*)histograms->At(getHistIndex(253)))->setXTitle("Hybrid Number");
      ((StSvtHist*)histograms->At(getHistIndex(253)))->setYTitle("Mean Anode");
      break;

    case 254:
      addHistToCollection(new StSvtHist("BARREL","MeanTimeBucket","Mean Time Bucket",14,0.5,14.5),254);
      ((StSvtHist*)histograms->At(getHistIndex(254)))->setXTitle("Hybrid Number");
      ((StSvtHist*)histograms->At(getHistIndex(254)))->setYTitle("Mean Time Bucket");
      break;

    case 301:
      addHistToCollection(new StSvtHist2D(config,"Pedestal","Pedestals",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),301);
      ((StSvtHist2D*)histograms->At(getHistIndex(301)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(301)))->setYTitle("Time Bin");
      break;

    case 302:
      addHistToCollection(new StSvtHist2D(config,"RMSPedestal","RMS Pedestals",N_BINS_ANODES,0.5,240.5,N_BINS_TIME,-0.5,127.5),302);
      ((StSvtHist2D*)histograms->At(getHistIndex(302)))->setXTitle("Anode Number");
      ((StSvtHist2D*)histograms->At(getHistIndex(302)))->setYTitle("Time Bin");
      break;

    case 303:
      addHistToCollection(new StSvtHist(config,"PedHyb","Pedestal per hybrid",256,-0.5,255.5),303);
      ((StSvtHist*)histograms->At(getHistIndex(303)))->setXTitle("RMS (channel)");
      break;

    case 304:
      addHistToCollection(new StSvtHist(config,"RMSPedHyb","RMS Ped per hybrid",25,0.,25.),304);
      ((StSvtHist*)histograms->At(getHistIndex(304)))->setXTitle("RMS (channel)");
      break;

    case 351:
      addHistToCollection(new TH1F("RMSPedAllHyb","RMS Pedestal All hybrids",25,0.,25.),351);
      ((TH1F*)histograms->At(getHistIndex(351)))->SetXTitle("RMS (channel)");
      break;

    case 352:
      addHistToCollection(new StSvtHist("BARREL","RMSPedLadder","RMS Ped per ladder",14,0.5,14.5),352);
      ((StSvtHist*)histograms->At(getHistIndex(352)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(352)))->setYTitle("RMS (channel)");
      break;

    case 353:
      addHistToCollection(new StSvtHist("SVT","RMSPedBarrel","RMS Ped per half ladder",32,0.5,32.5),353);
      ((StSvtHist*)histograms->At(getHistIndex(353)))->setXTitle("Half Ladder");
      ((StSvtHist*)histograms->At(getHistIndex(353)))->setYTitle("RMS (channel)");
      break;

    case 354:
      addHistToCollection(new StSvtHist("SVT","RMSPedHybBarrel","RMS Ped per barrel",224,0.5,224.5),354);
      ((StSvtHist*)histograms->At(getHistIndex(354)))->setXTitle("Hybrid");
      ((StSvtHist*)histograms->At(getHistIndex(354)))->setYTitle("RMS (channel)");
      break;

    case 355:
      addHistToCollection(new TH1F("PedAllHyb","Pedestal All hybrids",256,-0.5,255.5),355);
      ((TH1F*)histograms->At(getHistIndex(355)))->SetXTitle("RMS (channel)");
      break;

    case 1000:
      addHistToCollection(new TGraph(),1000);
      break;
      
    default:
      break;
    }
  }
}

void StSvtMonitor::fillHist(const char* fileName)
{
  //Initialize variables
  int anodeID, nAnodes, length, nSeq, iseq, time, timeSeq, capacitor, nSCAZero, status, index_hyb, indexHist;
  int* anodelist;
  StSequence* Seq;
  Int_t bin;
  float meanValue, meanSqValue, rmsValue, fPixelPed;
  int index_pixel, ihybrid, adc, nPixelPed;
  Bool_t first;
  char append[100], temp[100];
  Text_t* title;
  TString className, config;
  
  StSvtHist2D* svtHist;
  StSvtHybridHist2D* hybridHist;
  StSvtHybridData* data;

  StSvtHybridPed* ped_temp;
  StSvtHybridPixels2* ped_temp2;
  StSvtHybridPed* rms_ped_temp;
  StSvtHybridPixels* cmn_temp;

  float ped, rmsped, common, averageRms;

  int total_adc_anode[N_ANODES+1] = {0};
  int counter_anode[N_ANODES+1] = {0};
  int total_adc_time[N_TIMEBINS] = {0};
  int counter_time[N_TIMEBINS] = {0};

  ofstream file(fileName);

  // Fill hybrid stat if in FAST mode
  if ((histID > 100) && (kFAST) && (!fileName))
    fillHybridStat();

  if (event)
    index_hyb = event->getHybridIndex(barrelID, ladderID, waferID, hybridID);
  else {
    cout << "Not a valid event pointer!!" << endl;
    return;
  }
  if (index_hyb < 0) {
    cout << "Not a valid HYBRID!!";
    return;
  }

  if (event) {
    data = (StSvtHybridData*)event->at(index_hyb);
    if (!data) {
      cout << "No valid data in event!!";
      return;
    }
  }

  //  cout << "getSCAZero = " << (int)data->getSCAZero() << ", getTimeZero = " << (int)data->getTimeZero() << endl;

  indexHist = getHistIndex(histID);

  if (indexHist >= 0) {
    className = TString(histograms->At(indexHist)->ClassName());
    if ((className != "TH1F") && (className != "TH2F")  && (className != "TGraph")) {

      svtHist = (StSvtHist2D*)histograms->At(indexHist);
      config = TString(svtHist->getConfiguration());

      if (svtHist) {
	if (kSAVEMEM  && (className != "StSvtGraph") && (config != "BARREL") && (config != "SVT")) {
	  title = svtHist->getTitle();
	  strcpy(temp,title);      
	  sprintf(append," - Barrel #%d, Ladder #%d, Wafer #%d, Hybrid #%d",barrelID,ladderID,waferID,hybridID);
	  strcat(temp,append);
	  hybridHist = (StSvtHybridHist2D*)svtHist->at(svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID));
	  hybridHist->getHist()->SetTitle(temp);
	}
	else
	  hybridHist = (StSvtHybridHist2D*)svtHist->at(svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID));
      }
      else
	return;
    }
  }
  else {
    cout << "No histogram with ID = " << histID << endl;
    return;
  }

  //
  // get Pedestal
  //
  if (kFIRSTORD) {
    if(pedestal)
      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
    else
      ped_temp = NULL;
  }

  if (kSECONDORD) {
    if (pedestal2ndOrd)
      ped_temp2 = (StSvtHybridPixels2*)pedestal2ndOrd->at(index_hyb);
    else
      ped_temp2 = NULL;
  }

  //
  // get Pedestal RMS
  //
  if(rmsPedestal)
    rms_ped_temp = (StSvtHybridPed*)rmsPedestal->at(index_hyb);
  else
    rms_ped_temp = NULL;

  //
  // get Common Mode Noise
  //
  if (cmn)
    cmn_temp = (StSvtHybridPixels*)cmn->at(index_hyb);
  else
    cmn_temp = NULL;

  // *************************************************
  // Loop through pixels with data for THIS HYBRID
  // *************************************************

  if (histID < 50) {

  anodelist = NULL;
  nAnodes = data->getAnodeList(anodelist);
  nSCAZero = data->getSCAZero();

  first = kTRUE;

  for (int ianode=0;ianode<nAnodes;ianode++) {

    anodeID = anodelist[ianode];
    Seq = NULL;
    nSeq = 0;
    
    status = data->getSequences(anodeID,nSeq,Seq);
    
    for (iseq=0;iseq<nSeq;iseq++) {

  // fill the histograms
	
      length = Seq[iseq].length;
  	  
      for (timeSeq=0; timeSeq<length; timeSeq++) {

	time = Seq[iseq].startTimeBin + timeSeq;

	capacitor = time + nSCAZero;
	if (capacitor > 127)
	  capacitor -= 128;		

	index_pixel = getPixelIndex(index_hyb,anodeID,time);

	if (kFIRSTORD) {
	  if (ped_temp) {
	    if (kPEDCAP)
	      ped = ped_temp->getPixelContent(anodeID, capacitor);
	    else if (kPEDTIME)
	      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	  }
	  else
	    ped = 0;
	}

	else if (kSECONDORD) {
	  if (ped_temp2) {
	    ped_temp = (StSvtHybridPed*)ped_temp2->getSvtHybridPixels(capacitor);
	  }
	
	  if (ped_temp) {
	    ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	    if (!ped) {
	      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	      if (ped_temp) 
		ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	      else
		ped = 0;
	    }
	  }
	  else {
	    ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	    if (ped_temp) 
	      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	    else
	      ped = 0;
	  }	  
	}
	
	//if ((anodeID==120) && (time==64))
	//  cout << "ped = " << ped << endl;

	if (rms_ped_temp)
	  rmsped = (rms_ped_temp->At(rms_ped_temp->getPixelIndex(anodeID, time)))/mScaleRms;
	else
	  rmsped = 0;

	//if ((anodeID==120) && (time==64))
	//  cout << "rmsped = " << rmsped << endl;

	if (cmn_temp)
	  common = cmn_temp->At(cmn_temp->getPixelIndex(anodeID, time));
	else
	  common = 0;

	switch(histID) {

	case 1:
	  if (fileName) {
	    cout << "Not a 2D histo !" << endl;
	    return;
	  }
	  (TH1F*)hybridHist->getHist()->Fill(Seq[iseq].firstAdc[timeSeq]);	    
	  break;

	case 2:
	  if (fileName)
	    file << anodeID << "  " << time << "  " << (int)Seq[iseq].firstAdc[timeSeq] << endl;
	  else
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,time,Seq[iseq].firstAdc[timeSeq]);
	  break;
    
	case 3:
	  if (fileName) {
	    cout << "Not a 2D histo !" << endl;
	    return;
	  }
	  (TH1F*)hybridHist->getHist()->Fill(Seq[iseq].firstAdc[timeSeq]-ped);
	  break;

	case 4:

	  //if ((anodeID == 120) && (time == 64))
	  //  cout << "ped = " << ped << endl;

	  if (fileName)
	    file << anodeID << "  " << time << "  " << (int)(Seq[iseq].firstAdc[timeSeq]-ped) << endl;
	  else
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,time,(Seq[iseq].firstAdc[timeSeq]-ped));
	  break;
	    	
	case 5:
	  if (fileName) {
	    cout << "Not a 2D histo !" << endl;
	    return;
	  }
	  (TH1F*)hybridHist->getHist()->Fill(Seq[iseq].firstAdc[timeSeq]-ped-common);
	  break;

	case 6:
	  if (fileName)
	    file << anodeID << "  " << time << "  " << (int)(Seq[iseq].firstAdc[timeSeq]-ped-common) << endl;
	  else
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,time,(Seq[iseq].firstAdc[timeSeq]-ped-common));
	  break;
	    	
	case 7:
	  if (fileName) {
	    cout << "Not a 2D histo !" << endl;
	    return;
	  }
	  if (Seq[iseq].firstAdc[timeSeq] > 0) {
	    total_adc_time[time] += (long)(Seq[iseq].firstAdc[timeSeq]-ped); 
	    counter_time[time]++;
	  }
	  break;	  

	case 8:
	  if (fileName) {
	    cout << "Not a 2D histo !" << endl;
	    return;
	  }
	  if (Seq[iseq].firstAdc[timeSeq] > 0) {
	    total_adc_anode[anodeID] += (long)(Seq[iseq].firstAdc[timeSeq]-ped); 
	    counter_anode[anodeID]++;
	  }
	  break;
	  
	default:
	  break;
	} // end of switch
      }	// end of time loop 
    } // end of sequence loop
  } // end of anode loop
  } // if (histID < 50)

  //
  //Post-loop fill histograms
  //
  switch(histID){

  case 7:    
    if (fileName) {
      cout << "Not a 2D histo !" << endl;
      return;
    }
    for (time = 0;time<N_TIMEBINS;time++) {
      if (counter_time[time] != 0) {
	bin = ((TH1F*)hybridHist->getHist())->GetBin(time);
	meanValue = (float)total_adc_time[time]/(float)counter_time[time];
	((TH1F*)hybridHist->getHist())->SetBinContent(bin,meanValue);
      }
    }
    break;

  case 8:
    if (fileName) {
      cout << "Not a 2D histo !" << endl;
      return;
    }
    for (anodeID=1;anodeID<=N_ANODES;anodeID++) {
      if (counter_anode[anodeID] != 0) {
	bin = ((TH1F*)hybridHist->getHist())->GetBin(anodeID);
	meanValue = (float)total_adc_anode[anodeID]/(float)counter_anode[anodeID];
	((TH1F*)hybridHist->getHist())->SetBinContent(bin,meanValue);
      }
    }
    break;

  case 10:
    fillHistProjTracks((TH2F*)hybridHist->getHist());
    break;

  default:
    break;
  }

  // *************************************************
  // Loop through ALL pixels for THIS HYBRID
  // *************************************************

  if ((histID > 100) && (histID < 150)) {

  for (int anodeID = 1; anodeID <= N_ANODES;anodeID++) {
    for (int time=0; time < N_TIMEBINS;time++) {

      index_pixel = getPixelIndex(index_hyb,anodeID,time);
  
      switch(histID){
	
      case 101:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kRAWSTAT)
	  meanValue = accumAdcRaw[index_pixel]/(float)mEvents;	    
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << meanValue << endl;
	else {
	  /*
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	    first = kFALSE;
	  }
	  */
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,meanValue);
	}
	break;

      case 102:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else  if (kRAWSTAT){
	  meanValue = accumAdcRaw[index_pixel]/(float)mEvents;
	  meanSqValue = accumAdcSqRaw[index_pixel]/(float)mEvents;
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << rmsValue << endl;
	else {
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	    first = kFALSE;
	  }
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,rmsValue);
	}
	break;
	
      case 103:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kRAWSTAT)
	  meanValue = accumAdcRaw[index_pixel]/(float)mEvents;	    
	
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	    first = kFALSE;
	}
	
	((StSvtHybridHistAnalog*)hybridHist)->Fill(meanValue,anodeID);
	
	break;
	
      case 104:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else if (kRAWSTAT) {
	  meanValue = accumAdcRaw[index_pixel]/(float)mEvents;
	  meanSqValue = accumAdcSqRaw[index_pixel]/(float)mEvents;
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	  first = kFALSE;
	}
	
	//if ((capacitor > 1) && (capacitor < 127) && (time > 1)) {
	((StSvtHybridHistAnalog*)hybridHist)->Fill(rmsValue,anodeID);
	//}	  
	
	break;
	
      case 107:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	    return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,meanEventRaw);
	break;
	
      case 108:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,rmsEventRaw);
	break;
	
      case 111:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kPEDSTAT)
	  meanValue = accumAdcPed[index_pixel]/(float)mEvents;	    
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << meanValue << endl;
	else {
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	    first = kFALSE;
	  }
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,meanValue);
	}
	break;
	
      case 112:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else if (kPEDSTAT) {
	  meanValue = accumAdcPed[index_pixel]/(float)mEvents;
	  meanSqValue = accumAdcSqPed[index_pixel]/(float)mEvents;
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << rmsValue << endl;
	else {
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	    first = kFALSE;
	  }
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,rmsValue);
	}
	break;
	
      case 113:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kPEDSTAT)
	  meanValue = accumAdcPed[index_pixel]/(float)mEvents;	    

	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	  first = kFALSE;
	}

	((StSvtHybridHistAnalog*)hybridHist)->Fill(meanValue,anodeID);
	
	break;
	
      case 114:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else if (kPEDSTAT) {
	  meanValue = accumAdcPed[index_pixel]/((float)mEvents);
	  meanSqValue = accumAdcSqPed[index_pixel]/((float)mEvents);
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if ((anodeID == 120) && (time == 64)) {
	  cout << "rmsValue = " << rmsValue << endl;
	  cout << "meanSqValue = " << meanSqValue << endl;
	  cout << "meanValue = " << meanValue << endl;
	}

	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	  first = kFALSE;
	}

	//if ((capacitor > 1) && (capacitor < 127) && (time > 1)) {
	((StSvtHybridHistAnalog*)hybridHist)->Fill(rmsValue,anodeID);
	//}
	break;
	
      case 117:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,meanEventPed);
	break;

      case 118:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,rmsEventPed);
	break;

      case 121:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kCMNSTAT)
	  meanValue = accumAdcCMN[index_pixel]/(float)mEvents;	    
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << meanValue << endl;
	else {
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	    first = kFALSE;
	  }
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,meanValue);
	}
	break;
	
      case 122:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else if (kCMNSTAT) {
	  meanValue = accumAdcCMN[index_pixel]/(float)mEvents;
	  meanSqValue = accumAdcSqCMN[index_pixel]/(float)mEvents;
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if (fileName)
	  file << anodeID << "  " << time << "  " << rmsValue << endl;
	else {
	  if (first) {
	    ((TH2F*)hybridHist->getHist())->Reset();
	      first = kFALSE;
	  }
	  (TH2F*)hybridHist->getHist()->Fill(anodeID,time,rmsValue);
	}
	break;
	
      case 123:
	meanValue = 0;
	if (kZSSTAT)
	  meanValue = meanAdc[index_pixel];
	else if (kCMNSTAT)
	  meanValue = accumAdcCMN[index_pixel]/(float)mEvents;	    
	
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	    return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	  first = kFALSE;
	}
	
	((StSvtHybridHistAnalog*)hybridHist)->Fill(meanValue,anodeID);
	
	break;

      case 124:
	meanValue = 0;
	meanSqValue = 0;
	if (kZSSTAT) {
	  meanValue = meanAdc[index_pixel];
	  meanSqValue = meanSqAdc[index_pixel];
	}
	else if (kCMNSTAT) {
	  meanValue = accumAdcCMN[index_pixel]/(float)mEvents;
	  meanSqValue = accumAdcSqCMN[index_pixel]/(float)mEvents;
	}
	rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (first) {
	  ((StSvtHybridHistAnalog*)hybridHist)->Reset();
	  first = kFALSE;
	}
	
	((StSvtHybridHistAnalog*)hybridHist)->Fill(rmsValue,anodeID);
	
	break;
	
      case 127:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	  return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,meanEventCMN);
	break;
	
      case 128:
	if (fileName) {
	  cout << "Not a 2D histo !" << endl;
	    return;
	}
	if (kEVENTSTAT)
	  ((StSvtHybridGraph*)hybridHist)->Fill(mEvents,nEvent,rmsEventCMN);
	break;

      default:
	break;
      } // end of switch
    } // end of time loop
  } // end of anode loop
  } // if ((histID > 100) && (histID < 150))


  // *************************************************
  // Fill other histos
  // *************************************************

  switch (histID) {

  case 51:
  case 52:
    for (int wafer=1;wafer <= event->getNumberOfWafers(barrelID);wafer++) {
      for (int hybrid=1;hybrid <= event->getNumberOfHybrids();hybrid++) {

	index_hyb = event->getHybridIndex(barrelID, ladderID, wafer, hybrid);
	if (index_hyb < 0) continue;

	data = (StSvtHybridData*)event->at(index_hyb);

	anodelist = NULL;
	nAnodes = data->getAnodeList(anodelist);
	nPixelPed = 0;

	for (int ianode=0;ianode<nAnodes;ianode++) {

	  anodeID = anodelist[ianode];
	  Seq = NULL;
	  nSeq = 0;
    
	  status = data->getSequences(anodeID,nSeq,Seq);
    
	  for (iseq=0;iseq<nSeq;iseq++) {
	    length = Seq[iseq].length;	    
	    for (timeSeq=0; timeSeq<length; timeSeq++) {
	      time = Seq[iseq].startTimeBin + timeSeq;

	      adc = Seq[iseq].firstAdc[timeSeq];
	      if (histID == 51) {
		if (adc>0)
		  nPixelPed++;
	      }
	      else if (histID == 52) {
		if (adc>ADC_THRESHOLD)
		  nPixelPed++;
	      }
	    }
	  }
	}
	ihybrid = (wafer-1)*2+hybrid;
	fPixelPed = (float)nPixelPed/30720.;
	((TH1F*)hybridHist->getHist())->Fill(ihybrid,fPixelPed);
      }
    }
    break;

  case 53:
    for (int barrel=1; barrel <= event->getNumberOfBarrels(); barrel++) {
      for (int ladder=1; ladder <= event->getNumberOfLadders(barrel); ladder++) {
	for (int wafer=1; wafer <= event->getNumberOfWafers(barrel); wafer++) {
	  for (int hybrid=1; hybrid <= event->getNumberOfHybrids(); hybrid++) {

	    index_hyb = event->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index_hyb < 0) continue;
	    
	    data = (StSvtHybridData*)event->at(index_hyb);
	    
	    anodelist = NULL;
	    nAnodes = data->getAnodeList(anodelist);
	    
	    for (int ianode=0;ianode<nAnodes;ianode++) {
	      
	      anodeID = anodelist[ianode];
	      Seq = NULL;
	      nSeq = 0;
	      
	      status = data->getSequences(anodeID,nSeq,Seq);
	      
	      for (iseq=0;iseq<nSeq;iseq++) {
		length = Seq[iseq].length;	    
		for (timeSeq=0; timeSeq<length; timeSeq++) {
		  time = Seq[iseq].startTimeBin + timeSeq;
		  
		  adc = (int)Seq[iseq].firstAdc[timeSeq];
		  ((TH1F*)histograms->At(getHistIndex(51)))->Fill(adc);
		}
	      }
	    }
	  }
	}
      }
    }
    break;

  case 54:
  case 55:
  case 56:
  case 57:
    ihybrid = 1;
    nPixelPed = 0;
    for (int ladder=1; ladder<= event->getNumberOfLadders(barrelID);ladder++) {
      for (int wafer=1;wafer <= event->getNumberOfWafers(barrelID);wafer++) {
	for (int hybrid=1;hybrid <= event->getNumberOfHybrids();hybrid++) {
	  
	  index_hyb = event->getHybridIndex(barrelID, ladder, wafer, hybrid);
	  if (index_hyb < 0) continue;
	  
	  data = (StSvtHybridData*)event->at(index_hyb);
	  
	  anodelist = NULL;
	  nAnodes = data->getAnodeList(anodelist);
	  
	  for (int ianode=0;ianode<nAnodes;ianode++) {
	    
	    anodeID = anodelist[ianode];
	    Seq = NULL;
	    nSeq = 0;
	    
	    status = data->getSequences(anodeID,nSeq,Seq);
	    
	    for (iseq=0;iseq<nSeq;iseq++) {
	      length = Seq[iseq].length;	    
	      for (timeSeq=0; timeSeq<length; timeSeq++) {
		time = Seq[iseq].startTimeBin + timeSeq;
		
		adc = (int)Seq[iseq].firstAdc[timeSeq];
		if ((histID == 54) || (histID == 56)) {
		  if (adc>0)
		    nPixelPed++;
		}
		else if ((histID == 55) || (histID == 57)) {
		  if (adc>ADC_THRESHOLD)
		    nPixelPed++;
		}
	      }
	    }
	  }

	  fPixelPed = (float)nPixelPed/30720.;

	  if ((histID == 54) || (histID == 55)) {
	    ((TH1F*)hybridHist->getHist())->Fill(ihybrid++,fPixelPed);
	    nPixelPed = 0;
	  }
	}
	
	if ((histID == 56) || (histID == 57)) {
	  if ((barrelID == 3) && (ladder%2 == 0) && (wafer == 4)) {
	    fPixelPed = (float)nPixelPed/(30720.*4.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,fPixelPed);
	    nPixelPed = 0;
	  }
	  else if ((barrelID == 3) && (ladder%2 != 0) && (wafer == 3)) {
	    fPixelPed = (float)nPixelPed/(30720.*3.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,fPixelPed);
	    nPixelPed = 0;
	  }
	  else if ((barrelID != 3) && (wafer == event->getNumberOfWafers(barrelID)/2)) {
	    fPixelPed = (float)nPixelPed/(30720.*(float)event->getNumberOfWafers(barrelID)/2.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,fPixelPed);
	    nPixelPed = 0;
	  }	 
      
	  if ((barrelID == 3) && (ladder%2 == 0) && (wafer == event->getNumberOfWafers(barrelID))) {
	    fPixelPed = (float)nPixelPed/(30720.*3.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),fPixelPed);
	    nPixelPed = 0;
	  }
	  else if ((barrelID == 3) && (ladder%2 != 0) && (wafer == event->getNumberOfWafers(barrelID))) {
	    fPixelPed = (float)nPixelPed/(30720.*4.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),fPixelPed);
	    nPixelPed = 0;
	  }
	  else if (barrelID != 3 && (wafer == event->getNumberOfWafers(barrelID))) {
	    fPixelPed = (float)nPixelPed/(30720.*(float)event->getNumberOfWafers(barrelID)/2.*(float)event->getNumberOfHybrids());
	    ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),fPixelPed);
	    nPixelPed = 0;
	  }
	}
      }
    }
    break;

  case 151:
    for (int barrel=1; barrel <= event->getNumberOfBarrels();barrel++) {
      for (int ladder=1; ladder <= event->getNumberOfLadders(barrel);ladder++) {
	for (int wafer=1;wafer <= event->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid=1;hybrid <= event->getNumberOfHybrids();hybrid++) {
	  
	    index_hyb = event->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index_hyb < 0) continue;

	    for (int anode=1;anode<=N_ANODES;anode++) {
	      for (int time=0;time<N_TIMEBINS;time++) {	      

		index_pixel = getPixelIndex(index_hyb,anode,time);

		meanValue = 0;
		meanSqValue = 0;
		if (kZSSTAT) {
		  meanValue = meanAdc[index_pixel];
		  meanSqValue = meanSqAdc[index_pixel];
		}
		else  if (kRAWSTAT){
		  meanValue = accumAdcRaw[index_pixel]/(float)mEvents;
		  meanSqValue = accumAdcSqRaw[index_pixel]/(float)mEvents;
		}

		rmsValue = sqrt(meanSqValue - meanValue*meanValue);
		((TH1F*)histograms->At(getHistIndex(151)))->Fill(rmsValue);
	      }
	    }
	  }
	}
      }
    }
    break;

  case 152:
    for (int wafer=1;wafer <= event->getNumberOfWafers(barrelID);wafer++) {
      for (int hybrid=1;hybrid <= event->getNumberOfHybrids();hybrid++) {

	index_hyb = event->getHybridIndex(barrelID, ladderID, wafer, hybrid);
	if (index_hyb < 0) continue;

	averageRms = 0.;
	for (int anode=1;anode<=N_ANODES;anode++) {
	  for (int time=0;time<N_TIMEBINS;time++) {	      

	    index_pixel = getPixelIndex(index_hyb,anode,time);

	    meanValue = 0;
	    meanSqValue = 0;
	    if (kZSSTAT) {
	      meanValue = meanAdc[index_pixel];
	      meanSqValue = meanSqAdc[index_pixel];
	    }
	    else  if (kRAWSTAT){
	      meanValue = accumAdcRaw[index_pixel]/(float)mEvents;
	      meanSqValue = accumAdcSqRaw[index_pixel]/(float)mEvents;
	    }

	    rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	    averageRms += rmsValue;
	  }
	}
	averageRms /= N_ANODES*N_TIMEBINS;
	int ihybrid = (wafer-1)*2+hybrid;
	((TH1F*)hybridHist->getHist())->Fill(ihybrid,averageRms);
      }
    }
    break;
   
  case 153:    
  case 154:
    ihybrid = 1;
    averageRms = 0;
    for (int ladder=1; ladder<= event->getNumberOfLadders(barrelID);ladder++) {
      for (int wafer=1;wafer <= event->getNumberOfWafers(barrelID);wafer++) {
	for (int hybrid=1;hybrid <= event->getNumberOfHybrids();hybrid++) {
	  
	  index_hyb = event->getHybridIndex(barrelID, ladder, wafer, hybrid);
	  if (index_hyb < 0) continue;
	  
	  for (int anode=1;anode<=N_ANODES;anode++) {
	    for (int time=0;time<N_TIMEBINS;time++) {	      

	      index_pixel = getPixelIndex(index_hyb,anode,time);
	      
	      meanValue = 0;
	      meanSqValue = 0;
	      if (kZSSTAT) {
		meanValue = meanAdc[index_pixel];
		meanSqValue = meanSqAdc[index_pixel];
	      }
	      else  if (kRAWSTAT){
		meanValue = accumAdcRaw[index_pixel]/(float)mEvents;
		meanSqValue = accumAdcSqRaw[index_pixel]/(float)mEvents;
	      }
	      
	      rmsValue = sqrt(meanSqValue - meanValue*meanValue);
	      averageRms += rmsValue;
	    }
	  }

	  if (histID == 154) {
	    averageRms /= N_ANODES*N_TIMEBINS;
	    ((TH1F*)hybridHist->getHist())->Fill(ihybrid++,averageRms);
	    averageRms = 0;
	  }
	}
	
	if (histID == 153) {
	  if ((barrelID == 3) && (ladder%2 == 0) && (wafer == 4)) {
	    averageRms /= N_ANODES*N_TIMEBINS*4.*(float)event->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }
	  else if ((barrelID == 3) && (ladder%2 != 0) && (wafer == 3)) {
	    averageRms /= N_ANODES*N_TIMEBINS*3.*(float)event->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }
	  else if ((barrelID != 3) && (wafer == event->getNumberOfWafers(barrelID)/2)) {
	    averageRms /= N_ANODES*N_TIMEBINS*
	      (float)event->getNumberOfWafers(barrelID)/2.*(float)event->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }	 
	}
      }

      if (histID == 153) {
	if ((barrelID == 3) && (ladder%2 == 0)) {
	  averageRms /= N_ANODES*N_TIMEBINS*3.*(float)event->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
	else if ((barrelID == 3) && (ladder%2 != 0)) {
	  averageRms /= N_ANODES*N_TIMEBINS*4.*(float)event->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
	else if (barrelID != 3) {
	  averageRms /= N_ANODES*N_TIMEBINS*
	    (float)event->getNumberOfWafers(barrelID)/2.*(float)event->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+event->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
      }
    }
    break;
    
  default:
    break;
  }

  if (fileName)
    cout << "ADC values written to " << fileName << endl;
  file.close();
}

void StSvtMonitor::fillHistPed(const char* fileName)
{
  //Initialize variables
  int anodeID, nAnodes, length, nSeq, iseq, time, timeSeq, capacitor, nSCAZero, index_hyb, indexHist;
  int* anodelist;
  int index_pixel, ihybrid;
  Bool_t first;
  char append[100], temp[100];
  Text_t* title;
  TString className, config;
  float averageRms;

  StSvtHist2D* svtHist;
  StSvtHybridHist2D* hybridHist;

  StSvtHybridPed* ped_temp;
  StSvtHybridPixels2* ped_temp2;
  StSvtHybridPed* rms_ped_temp;

  float ped, rmsped;

  ofstream file(fileName);

  if (pedestal)
    index_hyb = pedestal->getHybridIndex(barrelID, ladderID, waferID, hybridID);
  else if (rmsPedestal)
    index_hyb = rmsPedestal->getHybridIndex(barrelID, ladderID, waferID, hybridID);
  else {
    cout << "Not a valid pedestal pointer!!" << endl;
    return;
  }
  if (index_hyb < 0) {
    cout << "Not a valid HYBRID!!";
    return;
  }

  indexHist = getHistIndex(histID);

  if (indexHist >= 0) {
    className = TString(histograms->At(indexHist)->ClassName());

    if ((className != "TH1F") && (className != "TH2F")  && (className != "TGraph")) {

      svtHist = (StSvtHist2D*)histograms->At(indexHist);
      config = TString(svtHist->getConfiguration());

      if (svtHist) {
	if (kSAVEMEM  && (className != "StSvtGraph") && (config != "BARREL") && (config != "SVT")) {
	  title = svtHist->getTitle();
	  strcpy(temp,title);      
	  sprintf(append," - Barrel #%d, Ladder #%d, Wafer #%d, Hybrid #%d",barrelID,ladderID,waferID,hybridID);
	  strcat(temp,append);
	  hybridHist = (StSvtHybridHist2D*)svtHist->at(svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID));
	  hybridHist->getHist()->SetTitle(temp);
	}
	else
	  hybridHist = (StSvtHybridHist2D*)svtHist->at(svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID));
      }
      else
	return;
    }
  }
  else {
    cout << "No histogram with ID = " << histID << endl;
    return;
  }

  //cout << "pedestal = " << pedestal << endl;

  if (kFIRSTORD) {
    if(pedestal)
      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
    else
      ped_temp = NULL;
  }

  if (kSECONDORD) {
    if (pedestal2ndOrd)
      ped_temp2 = (StSvtHybridPixels2*)pedestal2ndOrd->at(index_hyb);
    else
      ped_temp2 = NULL;
  }

  //cout << "pedTemp = " << ped_temp << endl;

  if(rmsPedestal)
    rms_ped_temp = (StSvtHybridPed*)rmsPedestal->at(index_hyb);
  else
    rms_ped_temp = NULL;

  //cout << "rms_ped_temp = " << rms_ped_temp << endl;

  anodelist = NULL;
  nAnodes = 240;

  first = kTRUE;

  for (int ianode=0;ianode<nAnodes;ianode++) {

    anodeID = ianode + 1;
    nSeq = 1;

    for (iseq=0;iseq<nSeq;iseq++) {

  // fill the histograms
	
      length = 128;
  	  
      for (timeSeq=0; timeSeq<length; timeSeq++) {

	time = timeSeq;

	capacitor = time + nSCAZero;
	if (capacitor > 127)
	  capacitor -= 128;		
	
	index_pixel = getPixelIndex(index_hyb,anodeID,time);

	if (kFIRSTORD) {
	  if (ped_temp) {
	    if (kPEDCAP)
	      ped = ped_temp->getPixelContent(anodeID, capacitor);
	    else if (kPEDTIME)
	      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	  }
	  else
	    ped = 0;
	}

	else if (kSECONDORD) {
	  if (ped_temp2) {
	    ped_temp = (StSvtHybridPed*)ped_temp2->getSvtHybridPixels(capacitor);
	  }
	
	  if (ped_temp) {
	    ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	    if (!ped) {
	      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	      if (ped_temp) 
		ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	      else
		ped = 0;
	    }
	  }
	  else {
	    ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	    if (ped_temp) 
	      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	    else
	      ped = 0;
	  }	  
	}
	
	//if ((anodeID==120) && (time==64))
	//  cout << "ped = " << ped << endl;

	if (rms_ped_temp)
	  rmsped = (rms_ped_temp->At(rms_ped_temp->getPixelIndex(anodeID, time)))/mScaleRms;
	else
	  rmsped = 0;

	switch(histID) {
	  
	case 301:
	  if (fileName)
	    file << anodeID << "  " << time << "  " << ped << endl;
	  else if (kPEDCAP)
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,capacitor,ped);
	  else if (kPEDTIME)
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,time,ped);
	  break;

	case 302:
	  if (fileName)
	    file << anodeID << "  " << time << "  " << rmsped << endl;
	  else
	    (TH2F*)hybridHist->getHist()->Fill(anodeID,time,rmsped);
	  break;

	case 303:
	  (TH1F*)hybridHist->getHist()->Fill(ped);
	  break;

	case 304:
	  (TH1F*)hybridHist->getHist()->Fill(rmsped);
	  break;

	default:
	  break;
	}
      }	    	  
    }
  }


  // some post-loop histogram filling

  switch(histID) {

  case 351:
    if (!rmsPedestal) break;
    for (int barrel=1;barrel <= rmsPedestal->getNumberOfBarrels();barrel++) {
      for (int ladder=1;ladder <= rmsPedestal->getNumberOfLadders(barrel);ladder++) {
	for (int wafer=1;wafer <= rmsPedestal->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid=1;hybrid <= rmsPedestal->getNumberOfHybrids();hybrid++) {
	    
	    index_hyb = rmsPedestal->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index_hyb < 0) continue;
	    
	    if(rmsPedestal)
	      rms_ped_temp = (StSvtHybridPed*)rmsPedestal->at(index_hyb);
	    else
	      rms_ped_temp = NULL;
	    
	    if (rms_ped_temp) {
	      
	      for (int anode=1;anode<=N_ANODES;anode++) {
		for (int time=0;time<N_TIMEBINS;time++) {
		  
		  rmsped = (rms_ped_temp->At(rms_ped_temp->getPixelIndex(anode, time)))/mScaleRms;
		  if (getHistIndex(351) > 0)
		    if (((TH1F*)histograms->At(getHistIndex(351)))->GetEntries() < (rmsPedestal->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS))
		      ((TH1F*)histograms->At(getHistIndex(351)))->Fill(rmsped);
		}
	      }
	    }
	  }
	}
      }
    }
    break;

  case 352:
    if (!rmsPedestal) break;
    for (int wafer=1;wafer <= rmsPedestal->getNumberOfWafers(barrelID);wafer++) {
      for (int hybrid=1;hybrid <= rmsPedestal->getNumberOfHybrids();hybrid++) {

	index_hyb = rmsPedestal->getHybridIndex(barrelID, ladderID, wafer, hybrid);
	if (index_hyb < 0) continue;

	rms_ped_temp = (StSvtHybridPed*)rmsPedestal->at(index_hyb);
	averageRms = 0.;
	for (int anode=1;anode<=N_ANODES;anode++) {
	  for (int time=0;time<N_TIMEBINS;time++) {	      
	    rmsped = (rms_ped_temp->At(rms_ped_temp->getPixelIndex(anode, time)))/mScaleRms;
	    averageRms += rmsped;
	  }
	}
	averageRms /= N_ANODES*N_TIMEBINS;
	int ihybrid = (wafer-1)*2+hybrid;
	((TH1F*)hybridHist->getHist())->Fill(ihybrid,averageRms);
      }
    }
    break;
   
  case 353:
  case 354:
    if (!rmsPedestal) break;
    ihybrid = 1;
    averageRms = 0;
    for (int ladder=1; ladder<= rmsPedestal->getNumberOfLadders(barrelID);ladder++) {
      for (int wafer=1;wafer <= rmsPedestal->getNumberOfWafers(barrelID);wafer++) {
	for (int hybrid=1;hybrid <= rmsPedestal->getNumberOfHybrids();hybrid++) {
	  
	  index_hyb = rmsPedestal->getHybridIndex(barrelID, ladder, wafer, hybrid);
	  if (index_hyb < 0) continue;
	  
	  rms_ped_temp = (StSvtHybridPed*)rmsPedestal->at(index_hyb);
	  for (int anode=1;anode<=N_ANODES;anode++) {
	    for (int time=0;time<N_TIMEBINS;time++) {	      
	      rmsped = (rms_ped_temp->At(rms_ped_temp->getPixelIndex(anode, time)))/mScaleRms;
	      averageRms += rmsped;
	    }
	  }

	  if (histID == 354) {
	    averageRms /= N_ANODES*N_TIMEBINS;
	    ((TH1F*)hybridHist->getHist())->Fill(ihybrid++,averageRms);
	    averageRms = 0;
	  }
	}
	
	if (histID == 353) {

	  if ((barrelID == 3) && (ladder%2 == 0) && (wafer == 4)) {
	    averageRms /= N_ANODES*N_TIMEBINS*4.*(float)rmsPedestal->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }
	  else if ((barrelID == 3) && (ladder%2 != 0) && (wafer == 3)) {
	    averageRms /= N_ANODES*N_TIMEBINS*3.*(float)rmsPedestal->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }
	  else if ((barrelID != 3) && (wafer == rmsPedestal->getNumberOfWafers(barrelID)/2)) {
	    averageRms /= N_ANODES*N_TIMEBINS*
	    (float)rmsPedestal->getNumberOfWafers(barrelID)/2.*(float)rmsPedestal->getNumberOfHybrids();
	    ((TH1F*)hybridHist->getHist())->Fill(ladder,averageRms);
	    averageRms = 0.;
	  }	 
	}
      }

      if (histID == 353) {
	if ((barrelID == 3) && (ladder%2 == 0)) {
	  averageRms /= N_ANODES*N_TIMEBINS*3.*(float)rmsPedestal->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+rmsPedestal->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
	else if ((barrelID == 3) && (ladder%2 != 0)) {
	  averageRms /= N_ANODES*N_TIMEBINS*4.*(float)rmsPedestal->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+rmsPedestal->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
	else if (barrelID != 3) {
	  averageRms /= N_ANODES*N_TIMEBINS*
	    (float)rmsPedestal->getNumberOfWafers(barrelID)/2.*(float)rmsPedestal->getNumberOfHybrids();
	  ((TH1F*)hybridHist->getHist())->Fill(ladder+rmsPedestal->getNumberOfLadders(barrelID),averageRms);
	  averageRms = 0.;
	}
      }
    }
    break;
       
  case 355:
    if (!pedestal) break;
    for (int barrel=1;barrel <= pedestal->getNumberOfBarrels();barrel++) {
      for (int ladder=1;ladder <= pedestal->getNumberOfLadders(barrel);ladder++) {
	for (int wafer=1;wafer <= pedestal->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid=1;hybrid <= pedestal->getNumberOfHybrids();hybrid++) {
	    
	    index_hyb = pedestal->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index_hyb < 0) continue;
	    
	    if(pedestal)
	      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	    else
	      ped_temp = NULL;
	    
	    if (ped_temp) {
	      
	      for (int anode=1;anode<=N_ANODES;anode++) {
		for (int time=0;time<N_TIMEBINS;time++) {
		  
		  ped = ped_temp->At(ped_temp->getPixelIndex(anode, time));
		  if (getHistIndex(355) > 0)
		    if (((TH1F*)histograms->At(getHistIndex(355)))->GetEntries() < (pedestal->getTotalNumberOfHybrids()*N_ANODES*N_TIMEBINS))
		      ((TH1F*)histograms->At(getHistIndex(355)))->Fill(ped);
		}
	      }
	    }
	  }
	}
      }
    }
    break;

  default:
    break;
  }
  
  if (fileName)
    cout << "ADC values written to " << fileName << endl;
  file.close();
}

void StSvtMonitor::fillSvtStat()
{
  int b, l, w, h;
  float time, pixels;
  TGraph* graph;

  b = barrelID;
  l = ladderID;
  w = waferID;
  h = hybridID;
  
  if (kCMNSTAT)
    cmnCalc();

  for (int ibarrel=0;ibarrel < event->getNumberOfBarrels();ibarrel++) {

    barrelID = ibarrel + 1;

    for (int iladder=0;iladder < event->getNumberOfLadders(ibarrel+1);iladder++) {

      ladderID = iladder + 1;

      for (int iwafer=0;iwafer < event->getNumberOfWafers(ibarrel+1);iwafer++) {

	waferID = iwafer + 1;
	
	for (int ihybrid=0;ihybrid < event->getNumberOfHybrids();ihybrid++) {

	  hybridID = ihybrid + 1;
    
	  fillHybridStat();
 	}
      }
    }
  }

  // Fill pixel stat after loop
  if (kPIXELSTAT) {
    pixels = mNPixels/13271040.;
    cout << "Overall occupancy = " << pixels << endl;
  }

  if (kPIXELSTAT) {
    pixels = mNPixels/13271040.;
    nPixelsArray->AddAt(pixels,mEvents-1);
    if (mEvents == 1)
      mStartTime = event->getUnixTime();
    //time = (float)(event->getUnixTime() - mStartTime)/3600;
    time = (float)(event->getEventNumber());
    nEventsArray->AddAt(time,mEvents-1);
    //nEventsArray->AddAt(mEvents,mEvents-1);
    if (getHistIndex(1000) > 0) {
      graph = new TGraph(mEvents,nEventsArray->GetArray(),nPixelsArray->GetArray());    
      graph->SetTitle("Number of Pixels vs Event");
      graph->SetMaximum(.2);
      graph->SetMinimum(0.);
      histograms->AddAt(graph,getHistIndex(1000));
    }
  }
  
  barrelID = b;
  ladderID = l;
  waferID = w;
  hybridID = h;
}

void StSvtMonitor::fillHybridStat()
{
  int anodeID, time, timeSeq, capacitor, nSCAZero;
  int nAnodes, nSeq, iseq, status, ihybrid;
  int* anodelist;
  int index_pixel, index_hyb;
  float adc, ped=0, common=0, content;
  float totalAdcRaw=0, totalAdcSqRaw=0, totalAdcPed=0, totalAdcSqPed=0, totalAdcCMN=0, totalAdcSqCMN=0;

  float meanValue, rmsValue;

  StSvtHybridPed* ped_temp;
  StSvtHybridPixels2* ped_temp2;
  StSvtHybridPixels* cmn_temp;
  StSvtHybridData* data;

  int nPixelPed=0, nPixelADC=0;
  float fPixelPed=0., fPixelADC=0.;

  float total_adc_anode = 0;
  float total_adc_time = 0;
  float total_adc=0, meanAnode=0, meanTime=0;

  index_hyb = event->getHybridIndex(barrelID, ladderID, waferID, hybridID);
  data = (StSvtHybridData*)event->at(index_hyb);

  if (!data) return;

  anodelist = NULL;
  nAnodes = data->getAnodeList(anodelist);
  nSCAZero = data->getSCAZero();

  if ((kPEDSTAT) || (kCMNSTAT)) {
    if (kFIRSTORD) {
      if(pedestal)
	ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
      else
	ped_temp = NULL;
    }    
    if (kSECONDORD) {
      if (pedestal2ndOrd)
	ped_temp2 = (StSvtHybridPixels2*)pedestal2ndOrd->at(index_hyb);
      else
	ped_temp2 = NULL;
    }	  
  }

  if (kCMNSTAT) {
    if (cmn)
      cmn_temp = (StSvtHybridPixels*)cmn->at(index_hyb);
    else
      cmn_temp = NULL;
  }

  for (int ianode=0;ianode<nAnodes;ianode++) {
    
    anodeID = anodelist[ianode];
    StSequence* Seq = NULL;
    nSeq = 0;

    status = data->getSequences(anodeID,nSeq,Seq);
    
    for (iseq=0;iseq<nSeq;iseq++) {
            
      for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {
	
	time = Seq[iseq].startTimeBin + timeSeq;

	capacitor = time + nSCAZero;
	if (capacitor > 127)
	  capacitor -= 128;			

	index_pixel = getPixelIndex(index_hyb,anodeID,time);

	if ((kPEDSTAT) || (kCMNSTAT)){
	  if (kFIRSTORD) {
	    if (ped_temp) {
	      if (kPEDCAP)
		ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, capacitor));
	      else if (kPEDTIME)
		ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	    }
	    else
	      ped = 0;
	  }	 
	  else if (kSECONDORD) {
	    if (ped_temp2) {
	      ped_temp = (StSvtHybridPed*)ped_temp2->getSvtHybridPixels(capacitor);
	    }
	    
	    if (ped_temp) {
	      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	      if (!ped) {
		ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
		if (ped_temp) 
		  ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
		else
		  ped = 0;
	      }
	    }
	    else {
	      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	      if (ped_temp) 
		ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
	      else
		ped = 0;
	    }	  
	  }
	}

	if (kCMNSTAT) {
	  if (cmn_temp)
	    common = cmn_temp->At(cmn_temp->getPixelIndex(anodeID, time));
	  else
	    common = 0;		       
	}

	// Raw data

	if (kZSSTAT) {
	  adc = Seq[iseq].firstAdc[timeSeq];
	  if (adc) {
	    meanAdc[index_pixel] = (meanAdc[index_pixel]*((float)(nEvents[index_pixel]))+adc)/((float)(nEvents[index_pixel]+1));
	    meanSqAdc[index_pixel] = (meanSqAdc[index_pixel]*((float)(nEvents[index_pixel]))+adc*adc)/((float)(nEvents[index_pixel]+1));
	    nEvents[index_pixel]++;
	  }
	}
	if (kRAWSTAT) {
	  adc = Seq[iseq].firstAdc[timeSeq];
	  accumAdcRaw[index_pixel] += adc;
	  accumAdcSqRaw[index_pixel] += adc*adc;

	  if (kEVENTSTAT) {
	    
	    //if ((anodeID == 120) && (time == 64))
	    //  totalAdcRaw = adc;
	    
	    totalAdcRaw += adc;
	    totalAdcSqRaw += adc*adc;
	  }
	}

	// Ped subtracted
	if (kPEDSTAT) {
	  adc = (float)Seq[iseq].firstAdc[timeSeq] - ped;
	  accumAdcPed[index_pixel] += adc;
	  accumAdcSqPed[index_pixel] += adc*adc;
	
	  if (kEVENTSTAT) {
	    totalAdcPed += adc;
	    totalAdcSqPed += adc*adc;
	  }
	}

	// Common mode noise subtracted
	if (kCMNSTAT) {
	  adc = Seq[iseq].firstAdc[timeSeq] - ped - common;
	  accumAdcCMN[index_pixel] += adc;
	  accumAdcSqCMN[index_pixel] += adc*adc;
		
	  if (kEVENTSTAT) {
	    totalAdcCMN += adc;
	    totalAdcSqCMN += adc*adc;
	  }
	}

	// Pixel statistics
	if (kPIXELSTAT) {
	  adc = Seq[iseq].firstAdc[timeSeq];
	  if (adc>0) {
	    nPixelPed++;
	    mNPixels++;
	  }
	  if (adc>ADC_THRESHOLD)
	    nPixelADC++;

	  total_adc_time += time*adc; 
	  total_adc_anode += anodeID*adc; 
	  total_adc += adc; 
	}
      }
    }	
  }	      

  if (kPIXELSTAT) {

    int h = histID;

    if (nPixelPed) {
      fPixelPed = (float)nPixelPed/30720.;

      histID = 251;
      if (getHist()) {
	//ihybrid = (waferID-1)*2 + hybridID;
	ihybrid = (ladderID-1)*2*event->getNumberOfWafers(barrelID)+(waferID-1)*2 + hybridID;
	content = getHist()->GetBinContent(ihybrid);
	fPixelPed = (content*((float)mEvents-1) + fPixelPed)/(float)mEvents;
	getHist()->SetBinContent(ihybrid,fPixelPed);
      }
    }
    
    if (nPixelADC) {
      fPixelADC = (float)nPixelADC/30720.;

      histID = 252;
      if (getHist()) {
	ihybrid = (waferID-1)*2 + hybridID;
	content = getHist()->GetBinContent(ihybrid);
	fPixelADC = (content*((float)mEvents-1) + fPixelADC)/(float)mEvents;
	getHist()->SetBinContent(ihybrid,fPixelADC);
      }
    }

    histID = 253;
    meanAnode = (float)total_adc_anode/total_adc;
    if (getHist()) {
      ihybrid = (waferID-1)*2 + hybridID;
      content = getHist()->GetBinContent(ihybrid);
      meanAnode = ((float)content*((float)mEvents-1) + meanAnode)/(float)mEvents;
      getHist()->SetBinContent(ihybrid,meanAnode);
    }

    histID = 254;
    meanTime = (float)total_adc_time/total_adc;
    ihybrid = (waferID-1)*2 + hybridID;
    if (getHist()) {
      ihybrid = (waferID-1)*2 + hybridID;
      content = getHist()->GetBinContent(ihybrid);
      meanTime = ((float)content*((float)mEvents-1) + meanTime)/(float)mEvents;
      getHist()->SetBinContent(ihybrid,meanTime);
    }

    histID = h;
  }

  if (kEVENTSTAT) {
    
    int h = histID;

    if (kRAWSTAT) {
      fillEventStat(totalAdcRaw, totalAdcSqRaw, meanEventRaw, rmsEventRaw);

      histID = 201;
      meanValue = ((TArrayF*)meanEventRaw->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(meanValue);

      histID = 202;
      rmsValue = ((TArrayF*)rmsEventRaw->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(rmsValue);
    }
    
    if (kPEDSTAT) {
      fillEventStat(totalAdcPed, totalAdcSqPed, meanEventPed, rmsEventPed);
      
      histID = 211;
      meanValue = ((TArrayF*)meanEventPed->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(meanValue);
      
      histID = 212;
      rmsValue = ((TArrayF*)rmsEventPed->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(rmsValue);
    }

    if (kCMNSTAT) {
      fillEventStat(totalAdcCMN, totalAdcSqCMN, meanEventCMN, rmsEventCMN);

      histID = 221;
      meanValue = ((TArrayF*)meanEventCMN->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(meanValue);
      
      histID = 222;
      rmsValue = ((TArrayF*)rmsEventCMN->at(index_hyb))->At(mEvents-1);
      if (getHist())
	getHist()->Fill(rmsValue);
    }

    histID = h;
  }
}

void StSvtMonitor::fillEventStat(float totalAdc, float totalAdcSq, 
				 StSvtHybridCollection* meanColl, StSvtHybridCollection* rmsColl)
{
  //  int sum, sumSQ, count;
  float mean, meanSQ, rms, time;
  int index_hyb;

  TArrayF* mean_array=NULL;
  TArrayF* rms_array=NULL;
  TArrayF* n_array=NULL;

  //mean = totalAdc;
  mean = totalAdc/(N_ANODES*N_TIMEBINS);
  meanSQ = totalAdcSq/(N_ANODES*N_TIMEBINS);

  rms = sqrt(meanSQ - mean*mean);

  index_hyb = event->getHybridIndex(barrelID, ladderID, waferID, hybridID);

  mean_array = (TArrayF*)meanColl->at(index_hyb);
  rms_array = (TArrayF*)rmsColl->at(index_hyb);
  n_array = (TArrayF*)nEvent->at(index_hyb);
    
  if (!mean_array)
    mean_array = new TArrayF(MAX_NUMBER_OF_EVENTS);
  mean_array->AddAt(mean,mEvents-1);

  if (!rms_array)
    rms_array = new TArrayF(MAX_NUMBER_OF_EVENTS);
  rms_array->AddAt(rms,mEvents-1);
    
  if (!n_array)
    n_array = new TArrayF(MAX_NUMBER_OF_EVENTS);
  if (mEvents == 1)
    mStartTime = event->getUnixTime();
  //time = (float)(event->getUnixTime() - mStartTime)/3600;
  time = (float)(event->getEventNumber());
  n_array->AddAt(time,mEvents-1);
    
  meanColl->put_at((TObject*)mean_array,index_hyb);
  rmsColl->at(index_hyb)= (TObject*)rms_array;
  nEvent->at(index_hyb)= (TObject*)n_array;
}

void StSvtMonitor::fillHistProjTracks(TH2F* hist)
{
    if (! stEvent) return; // If no event, we're done
    //    int evtID = stEvent->id();

    StPrimaryVertex* primVert = stEvent->primaryVertex();
    StThreeVectorF primPos;

    if (primVert) {
      primPos = primVert->position();
      cout << "z = " << primPos.z() << endl;
    }
    
    // OK, we've got the event. Process it.

    StSPtrVecTrackNode& theNodes = stEvent->trackNodes();
    StTrack* track;
    StPhysicalHelixD helix;
    StTrackFitTraits trackFit;

    double s, x, y, z;
    double pt, pz, p, eta;
    StThreeVectorD r0(0.,0.,0.);
    StThreeVectorD r(0.,10.4,0.);
    StThreeVectorD n(0.,1.,0.);
    StThreeVectorF momentum;
    int hybrid, wafer;
    float time, anode, s0, x0, z0, t0=0, vDriftVeloc=0.625, xShift=0, zShift=0;
    int nPrim=0, nPrimGood=0;

    for (StSPtrVecTrackNodeIterator i = theNodes.begin(); i!=theNodes.end();i++) {
      nPrim += (*i)->entries(primary);

      if (kGLOBAL)
	track = (*i)->track(global);
      else if (kPRIMARY)
	track = (*i)->track(primary);

      if (!track) continue;

      helix = track->geometry()->helix();
      momentum = track->geometry()->momentum();
      trackFit = track->fitTraits();

      if ((kPRIMARY) && ((track->flag()<0) || (trackFit.numberOfFitPoints()<10))) continue;
      nPrimGood += (*i)->entries(primary);

      s = helix.pathLength(r,n);
      s0 = helix.pathLength(r0,n);

      //if ((s < 0) || (s > 1000)) continue;
      if ((s < s0) || (s > 1000)) continue;

      x = helix.x(s);
      y = helix.y(s);
      z = helix.z(s);

      pt = sqrt(momentum.x()*momentum.x() + momentum.y()*momentum.y());
      pz = momentum.z();
      p = sqrt(pt*pt + pz*pz);
      eta = 0.5*log((p+pz)/(p-pz));

      hybrid = 0;
      wafer = 0;

      if ((x > 0.) && (x < 3.)) {
	hybrid = 2;
	x0 = 3.0 + xShift;
      }
      else if ((x < 0.) && (x > -3.)) {
	hybrid = 1;
	x0 = -3.0 + xShift;
      }

      if ((z > -22.05) && (z < -15.75)) {
	wafer = 1;
	if (hybrid == 1)
	  z0 = -21.9 + zShift;
	else if (hybrid == 2)
	  z0 = -15.9 + zShift;
      }
      else if ((z > -15.75) && (z < -9.45)) {
	wafer = 2;
	if (hybrid == 1)
	  z0 = -15.6 + zShift;
	else if (hybrid == 2)
	  z0 = -9.6 + zShift;
      }
      else if ((z > -9.45) && (z < -3.15)) {
	wafer = 3;
	if (hybrid == 1)
	  z0 = -9.3 + zShift;
	else if (hybrid == 2)
	  z0 = -3.3 + zShift;
      }
      else if ((z > -3.15) && (z < 3.15)) {
	wafer = 4;
	if (hybrid == 1)
	  z0 = -3.0 + zShift;
	else if (hybrid == 2)
	  z0 = 3.0 + zShift;
      }
      else if ((z > 3.15) && (z < 9.45)) {
	wafer = 5;
	if (hybrid == 1)
	  z0 = 3.3 + zShift;
	else if (hybrid == 2)
	  z0 = 9.3 + zShift;
      }
      else if ((z > 9.45) && (z < 15.75)) {
	wafer = 6;
	if (hybrid == 1)
	  z0 = 9.6 + zShift;
	else if (hybrid == 2)
	  z0 = 15.6 + zShift;
      }
      else if ((z > 15.75) && (z < 22.05)) {
	wafer = 7;
	if (hybrid == 1)
	  z0 = 15.9 + zShift;
	else if (hybrid == 2)
	  z0 = 21.9 + zShift;
      }

      
      if (hybrid == 1) {
	time = ((x-x0)/vDriftVeloc + t0)/0.04;
	anode = (z-z0)/0.025;
      }
      else if (hybrid == 2) {
	time = ((x0-x)/vDriftVeloc + t0)/0.04;
	anode = (z0-z)/0.025;
      }

      if ((wafer == waferID) && (hybrid == hybridID)) {
	cout << "x = " << x << ", z = " << z << endl;
	hist->Fill(anode,time);
      }          
    }
}

void StSvtMonitor::fillFourier(TH1F* input, char* option)
{
  StTFourierTransform* fft;
  float scale=0.;

  if (getHistIndex(58) > 0)
    fft->Fft(input,outFourier,phaseFourier,3,127);

  if (TString(option) == "X") 
    scale = 2.5;
  else if (TString(option) == "Y")
    scale = 25.;

  ((TH1F*)histograms->At(getHistIndex(58)))->Reset();
  for (int i=1;i<=outFourier->GetNbinsX();i++) {
    Stat_t x1=outFourier->GetBinContent(i);
    Axis_t x2=outFourier->GetBinCenter(i);
    if (x1 > 0)
      ((TH1F*)histograms->At(getHistIndex(58)))->Fill(x2*scale,x1);
  }
}

//*****************************************************************************

void StSvtMonitor::reBin(int nBinsX, int nBinsY )
{
  int index_hyb, indexHist;

  StSvtHist* svtHist;
  StSvtHybridHist* hybridHist;
  TH1* hist;

  resetHist(histID);
  indexHist = getHistIndex(histID);
  if (indexHist < 0) return;

  svtHist = (StSvtHist*)histograms->At(indexHist) ;
  if (svtHist) {
    index_hyb = svtHist->getHybridIndex(barrelID, ladderID, waferID, hybridID);
    hybridHist = (StSvtHybridHist*)svtHist->at(index_hyb);
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return;
  }
    
  const char* className = histograms->At(indexHist)->ClassName();

  if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) ) {
    hist = (TH2F*)hybridHist->getHist() ;
    hist->SetBins(nBinsX,0.5,240.5,nBinsY,-0.5,127.5);    
  }
  else if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) { 
    cout << "Not a 2D histogram! Cannot rebin..." << endl;
    return;
  }  
}

void StSvtMonitor::reBinAllHybrids(int nBinsX, int nBinsY)
{
  int indexHist;
  StSvtHist* svtHist;

  resetHist(histID);
  indexHist = getHistIndex(histID);
  if (indexHist < 0) return;

  svtHist = (StSvtHist*)histograms->At(indexHist) ;

  if (svtHist) {

    const char* className = histograms->At(indexHist)->ClassName();

    if ( !strncmp(className, "StSvtHist2D", strlen("StSvtHist2D")) ) {
      ((StSvtHist2D*)svtHist)->setBins(nBinsX,0.5,240.5,nBinsY,-0.5,127.5);    
    }
    else if ( !strncmp(className, "StSvtHist", strlen("StSvtHist")) ) { 
      cout << "Not a 2D histogram! Cannot rebin..." << endl;
      return;
    }  
  }
  else {
    cout << "Not a valid HISTOGRAM!!!" << endl;
    return;
  }
}

void StSvtMonitor::reBinAllHistos(int nBinsX, int nBinsY)
{
  int hist;

  hist = histID;

  for (int i=0;i<histograms->GetSize();i++) {
    histID = getHistID(i);
    reBinAllHybrids(nBinsX,nBinsY);
  }

  histID = hist;
}

//*****************************************************************************
void StSvtMonitor::fillAllHistos()
{
  //Helens
  int hist;

  hist = histID;

  for (int i=0;i<histograms->GetSize();i++) {
    histID = getHistID(i);
    fillHist();
  }

  histID = hist;
}

//*****************************************************************************

void StSvtMonitor::resetHist(int ID)
{

  int indexHist;

  cout << "ID = " << ID  << endl;

  if (!ID) {
    for (int i=0;i<histograms->GetSize();i++) {
      if ((getHistID(i) < 200) || (getHistID(i) > 300)) {
	TString className = TString(histograms->At(i)->ClassName());
	if ((className == "TH1F") || (className == "TH2F"))
	  ((TH1F*)histograms->At(i))->Reset();
	//else if (className == "TGraph") {
	//  if (((TGraph*)histograms->At(i))->GetHistogram())
	//    ((TGraph*)histograms->At(i))->GetHistogram()->Reset();
	//}
	else if (className != "TGraph" && className != "StSvtGraph")
	  ((StSvtHist*)histograms->At(i))->reset();    
      }
    }
  }

  else if (ID < 0) {
    for (int i=0;i<histograms->GetSize();i++) {
      if ((getHistID(i) < 50) || ((getHistID(i) > 100) && (getHistID(i) < 150)) || ((getHistID(i) > 300) && (getHistID(i) < 350))) {
	TString className = TString(histograms->At(i)->ClassName());
	if ((className == "TH1F") || (className == "TH2F"))
	  ((TH1F*)histograms->At(i))->Reset();
	//else if (className == "TGraph") {
	//  if (((TGraph*)histograms->At(i))->GetHistogram())
	//    ((TGraph*)histograms->At(i))->GetHistogram()->Reset();
	//}
	else if (className != "TGraph" && className != "StSvtGraph")
	  ((StSvtHist*)histograms->At(i))->reset();    
      }
    }
  }

  else {
    indexHist = getHistIndex(ID);
    if (indexHist < 0) return;
    TString className = TString(histograms->At(indexHist)->ClassName());
    if ((className == "TH1F") || (className == "TH2F"))
      ((TH1F*)histograms->At(indexHist))->Reset();
    //else if (className == "TGraph") {
    //  if (((TGraph*)histograms->At(indexHist))->GetHistogram())
    //((TGraph*)histograms->At(indexHist))->GetHistogram()->Reset();
    //}
    else if (className != "TGraph" && className != "StSvtGraph")
      ((StSvtHist*)histograms->At(indexHist))->reset();
  }
  
  cout << "Single Event Histograms reset!" << endl;
}

void StSvtMonitor::resetPed()
{
  for (int i=0;i<histograms->GetSize();i++) {
    if (getHistID(i) > 300) {
      TString className = TString(histograms->At(i)->ClassName());
      if ((className == "TH1F") || (className == "TH2F"))
	((TH1F*)histograms->At(i))->Reset();
      else if (className == "TGraph")
	continue;
      else
	((StSvtHist*)histograms->At(i))->reset();    
    }
  }

  pedestal = NULL;

  cout << "Pedestals reset!" << endl;
}

void StSvtMonitor::resetPedRMS()
{
  for (int i=0;i<histograms->GetSize();i++) {
    if (getHistID(i) > 300) {
      TString className = TString(histograms->At(i)->ClassName());
      if ((className == "TH1F") || (className == "TH2F"))
	((TH1F*)histograms->At(i))->Reset();
      else if (className == "TGraph")
	continue;
      else 
	((StSvtHist*)histograms->At(i))->reset();    
    }
  }

  rmsPedestal = NULL;

  cout << "RMS reset!" << endl;
}

void StSvtMonitor::resetBuffers()
{
  if (event) resetStatistics();

  mEvents = 0;

  for (int i=0;i<histograms->GetSize();i++) {
    
    if ((getHistID(i) > 100) && (getHistID(i) < 300)) {
      TString className = TString(histograms->At(i)->ClassName());
      
      if ((className == "StSvtHist2D") ||
	  (className == "StSvtHist") ||
	  (className == "StSvtHistAnalog")) {
	((StSvtHist*)histograms->At(i))->reset();
      }
      else if (className == "TH2F")
	((TH2F*)histograms->At(i))->Reset(); 
      else if (className == "TH1F")
	((TH1F*)histograms->At(i))->Reset(); 
    }
  }

  cout << "Buffers and Multi Events Histograms reset!" << endl;
}

void StSvtMonitor::resetStatistics()
{
    int index = 0;
    if (kPIXELSTAT)
      mNPixels = 0;
    
    for (int ibarrel=0; ibarrel < event->getNumberOfBarrels(); ibarrel++) {
      for (int iladder=0; iladder < event->getNumberOfLadders(ibarrel+1); iladder++) {
	for (int iwafer=0; iwafer < event->getNumberOfWafers(ibarrel+1); iwafer++) {
	  for (int ihybrid=0; ihybrid < event->getNumberOfHybrids(); ihybrid++) {
	    
	    if (event->getHybridIndex(ibarrel+1, iladder+1, iwafer+1, ihybrid+1) < 0) continue;
	    
	    for (int ianode=0; ianode < N_ANODES; ianode++) {
	      for (int time=0; time < N_TIMEBINS; time++) {
		
		if (kZSSTAT) {
		  meanAdc[index] = 0;
		  meanSqAdc[index] = 0; 
		  nEvents[index] = 0;
		}
		if (kRAWSTAT) {
		  accumAdcRaw[index] = 0;
		  accumAdcSqRaw[index] = 0;
		}
		if (kPEDSTAT) {
		  accumAdcPed[index] = 0;
		  accumAdcSqPed[index] = 0;
		}
		if (kCMNSTAT) {
		  accumAdcCMN[index] = 0;
		  accumAdcSqCMN[index] = 0;
		}
		index++;
	      }
	    }
	    
	  }
	}
      }
    }
    cout << "Statistics reset!" << endl;
}

//************************************************************************************************

void StSvtMonitor::cmnCalc()
{
  int anodeID, nAnodes, nSeq, iseq, time, timeSeq, capacitor, nSCAZero, status, index_hyb;
  int* anodelist = NULL;
  float ped, common;

  if (mEvents == 0) {
    cout << "NO events to calculate Common Mode Noise! Get one..." << endl;
    return;
  }

  cout << "Calculating Common Mode Noise..." << endl;

  StSvtHybridPed* ped_temp;
  StSvtHybridPixels2* ped_temp2;
  StSvtHybridPixels* cmn_temp;
  StSvtHybridData* data;

  int b, l, w, h;

  b = barrelID;
  l = ladderID;
  w = waferID;
  h = hybridID;
  
  for (int ibarrel=0;ibarrel<event->getNumberOfBarrels();ibarrel++) {

    barrelID = ibarrel + 1;

    for (int iladder=0;iladder<event->getNumberOfLadders(ibarrel+1);iladder++) {

      ladderID = iladder + 1;

      for (int iwafer=0;iwafer<event->getNumberOfWafers(ibarrel+1);iwafer++) {

	waferID = iwafer + 1;
	
	for (int ihybrid=0;ihybrid<event->getNumberOfHybrids();ihybrid++) {

	  hybridID = ihybrid + 1;
	  if (event->getHybridIndex(barrelID, ladderID, waferID, hybridID) < 0) continue;

	  float total_Adc[N_TIMEBINS] = {0};
	  int counter_time[N_TIMEBINS] = {0};
  
	  index_hyb = event->getHybridIndex(barrelID, ladderID, waferID, hybridID);

	  data = (StSvtHybridData*)event->at(index_hyb);
	  if (!data) continue;

	  if (kFIRSTORD) {
	    if(pedestal)
	      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
	    else
	      ped_temp = NULL;
	    //	    if (!ped_temp) {
	    //	      ped_temp = new StSvtHybridPed(barrelID, ladderID, waferID, hybridID);
	    //	      pedestal->at(index_hyb)=ped_temp;
	    //	    }
	  }
	  
	  if (kSECONDORD) {
	    if (pedestal2ndOrd)
	      ped_temp2 = (StSvtHybridPixels2*)pedestal2ndOrd->at(index_hyb);
	    else
	      ped_temp2 = 0;
	    //	    if (!ped_temp2) {
	    //	      ped_temp2 = new StSvtHybridPixels2(barrelID, ladderID, waferID, hybridID);
	    //	      pedestal2ndOrd->at(index_hyb)=ped_temp2;
	    //	    }
	  }

	  cmn_temp = (StSvtHybridPixels*)cmn->at(index_hyb);
	  if (!cmn_temp) {
	    cmn_temp = new StSvtHybridPixels(barrelID, ladderID, waferID, hybridID);
	    cmn->at(index_hyb)=cmn_temp;
	  }

	  anodelist = NULL;
	  nAnodes = data->getAnodeList(anodelist);
	  nSCAZero = data->getSCAZero();

	  for (int ianode=0;ianode<nAnodes;ianode++) {
	    
	    anodeID = anodelist[ianode];
	    StSequence* Seq = NULL;
	    nSeq = 0;
	    
	    status = data->getSequences(anodeID,nSeq,Seq);
	    
	    for (iseq=0;iseq<nSeq;iseq++) {
	  	  
	      for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {

		time = Seq[iseq].startTimeBin + timeSeq;

		capacitor = time + nSCAZero;
		if (capacitor > 127)
		  capacitor -= 128;		
		
		if (kFIRSTORD) {
		  
		  if (ped_temp) {
		    if (kPEDCAP)
		      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, capacitor));
		    else if (kPEDTIME)
		      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
		  }
		  else
		    ped = 0;
		}
		
		else if (kSECONDORD) {
		  
		  if (ped_temp2) {
		    ped_temp = (StSvtHybridPed*)ped_temp2->getSvtHybridPixels(capacitor);
		  }
		  
		  if (ped_temp) {
		    ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
		    if (!ped) {
		      ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
		      if (ped_temp) 
			ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
		      else
			ped = 0;
		    }
		  }
		  else {
		    ped_temp = (StSvtHybridPed*)pedestal->at(index_hyb);
		    if (ped_temp) 
		      ped = ped_temp->At(ped_temp->getPixelIndex(anodeID, time));
		    else
		      ped = 0;
		  }	  
		}	      
	      
		total_Adc[time] += Seq[iseq].firstAdc[timeSeq] - ped;
		counter_time[time]++;
	      }  
	    }
	  }
	
	  for (time=0;time<N_TIMEBINS;time++) {

	    if (counter_time[time] != 0)
	      common = (float)(total_Adc[time]/counter_time[time]);
	    else
	      common = 0;

	    for (anodeID=1;anodeID<N_ANODES;anodeID++) 
	      cmn_temp->AddAt(common,cmn_temp->getPixelIndex(anodeID, time));
	  }
      
	}
      }
    }
  }

  barrelID = b;
  ladderID = l;
  waferID = w;
  hybridID = h;

  cout << "Done with CMN!" << endl;
}

//*****************************************************************************

int StSvtMonitor::getPixelIndex(int indexHyb, int anodeID, int time)
{
  int index = (indexHyb)*N_ANODES*N_TIMEBINS + (anodeID-1)*N_TIMEBINS + time;
  return index;
}

void StSvtMonitor::addHistToCollection(TH1F* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(TH2F* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(TGraph* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(StSvtHist* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(StSvtHist2D* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(StSvtHistAnalog* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

void StSvtMonitor::addHistToCollection(StSvtGraph* h, int ID)
{
  histograms->Add(h);
  histIDArray[histograms->IndexOf(h)] = ID;
}

int StSvtMonitor::getHistIndex(int ID)
{
  for (int index = 0;index<MAX_NUMBER_OF_HISTOGRAMS;index++) {
    if (histIDArray[index] == ID) return index;
  }

  //if got here, there is some problem
  return -1;
}

int StSvtMonitor::getHistID(int index)
{
  if (index < MAX_NUMBER_OF_HISTOGRAMS)
    return histIDArray[index];
  else return -1;
}
