/***************************************************************************
 *
 * $Id: StSvtMonitor.hh,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtMonitor.hh,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#ifndef STSVTMONITOR_HH
#define STSVTMONITOR_HH

#include "TObject.h"

class TOrdCollection;
class TCanvas;
class TH1;
class TH1F;
class TH2F;
class TGraph;
class TArrayF;

class StSvtHybridStat;
class StSvtHybridCollection;
class StSvtHist;
class StSvtHist2D;
class StSvtHistAnalog;
class StSvtHybridGraph;
class StSvtGraph;

class StSvtData;

class StEvent;

class StSvtMonitor : public TObject
{
public:
  StSvtMonitor(const char* config, int* listHist=0, const int maxHist=0);
  ~StSvtMonitor();
  StSvtMonitor(const StSvtMonitor&);	// copy constructor

  //set hybrid and histogram (to be handle) identity
  void setBarrelID(int b) {barrelID = b;}
  void setLadderID(int l) {ladderID = l;}
  void setWaferID(int w) {waferID = w;}
  void setHybridID(int h) {hybridID = h;}
  void setHybridID(int b, int l, int w, int h) {barrelID = b; ladderID = l; waferID = w; hybridID = h;}
  void setHistID(int hist) {histID = hist;}

  int getBarrelID() {return barrelID;}
  int getLadderID() {return ladderID;}
  int getWaferID() {return waferID;}
  int getHybridID() {return hybridID;}
  int getHistID() {return histID;}

  // set keys
  void setkRaw(Bool_t key) {kRAWSTAT = key;initStatistics();}
  void setkZS(Bool_t key) {kZSSTAT = key;initStatistics();}
  void setkPedSub(Bool_t key) {kPEDSTAT = key;initStatistics();}
  void setkCMNSub(Bool_t key) {kCMNSTAT = key;initStatistics();}
  void setkEventStat(Bool_t key) {kEVENTSTAT = key;initStatistics();}
  void setkPixelStat(Bool_t key) {kPIXELSTAT = key;initStatistics();}
  void setkGlobalStat(Bool_t key) {kGLOBALSTAT = key;initStatistics();}
  void setkSaveMem(Bool_t key) {kSAVEMEM = key;initStatistics();}
  void setkPedTime(Bool_t key) {kPEDTIME = key; kPEDCAP = !key;initStatistics();}
  void setkPedCap(Bool_t key) {kPEDCAP = key; kPEDTIME = !key;}
  void setkPed1stOrd(Bool_t key) {kFIRSTORD = key; kSECONDORD = !key;initStatistics();}
  void setkPed2ndOrd(Bool_t key) {kSECONDORD = key; kFIRSTORD = !key;initStatistics();}
  void setkPrimary(Bool_t key) {kPRIMARY = key; kGLOBAL = !key;initStatistics();}
  void setkGlobal(Bool_t key) {kGLOBAL = key; kPRIMARY = !key;initStatistics();}

  Bool_t getkRaw() {return kRAWSTAT;}
  Bool_t getkPedSub() {return kPEDSTAT;}
  Bool_t getkCMNSub() {return kCMNSTAT;}
  Bool_t getkEventStat() {return kEVENTSTAT;}
  Bool_t getkPixelStat() {return kPIXELSTAT;}
  Bool_t getkGlobalStat() {return kGLOBALSTAT;}
  Bool_t getkSaveMem() {return kSAVEMEM;}
  Bool_t getkPedTime() {return kPEDTIME;}
  Bool_t getkPedCap() {return kPEDCAP;}
  Bool_t getkPed1stOrd() {return kFIRSTORD;}
  Bool_t getkPed2ndOrd() {return kSECONDORD;}

  // retrieve histos and graphs
  TOrdCollection*   getHistograms(){return histograms;}
  TH1*              getHist();
  StSvtHist*        getSvtHist(int id=0);
  TGraph*           getGraph();
  StSvtHybridGraph* getHybridGraph();
  StSvtGraph*       getSvtGraph();

  //control actions
  void initStatistics();
  void setEvent(StSvtData* evt){event = evt;}
  void setPedestal(StSvtHybridCollection* ped){pedestal = ped;} 
  void setPedestal2ndOrd(StSvtHybridCollection* ped){pedestal2ndOrd = ped;}
  void setRMSPedestal(StSvtHybridCollection* ped){rmsPedestal = ped;} 
  void setStatistics();
  void setRMSFactor(int factor){mScaleRms = factor;}
  void resetHist(int ID=0);
  void resetPed();
  void resetPedRMS();
  void resetBuffers();
  void resetStatistics();
  void incrementEvent(){mEvents++;}

  void setStEvent(StEvent* evt){stEvent = evt;}

  // Retrieve event
  StSvtData* getEvent(){return event;}
  int getnEvents(){return mEvents;}

  //Common Mode Noise
  void cmnCalc();
  int  getPixelIndex(int indexHybrid, int anodeID, int time);

  //histograming actions
  void bookHist(int* listHist, const int maxHist);
  void addHistToCollection(TH1F* h, int ID);  
  void addHistToCollection(TH2F* h, int ID);  
  void addHistToCollection(TGraph* h, int ID);  
  void addHistToCollection(StSvtHist* h, int ID);  
  void addHistToCollection(StSvtHist2D* h, int ID);  
  void addHistToCollection(StSvtHistAnalog* h, int ID);
  void addHistToCollection(StSvtGraph* h, int ID);  
  int  getHistIndex(int ID);
  int  getHistID(int index);
  int  getEventNumber();
  int  getEventsInBuffer(){return mEvents;}
  void fillHist(const char* fileName=0);
  void fillHistPed(const char* fileName=0);
  void fillSvtStat();
  void fillHybridStat();
  void fillEventStat(float totalAdc, float totalAdcSq, 
		     StSvtHybridCollection* meanColl, StSvtHybridCollection* rmsColl);
  void fillHistProjTracks(TH2F* hist);
  void fillFourier(TH1F* hist, char* option);

  void reBin(int nBinsX, int nBinsY);
  void reBinAllHybrids(int nBinsX, int nBinsY);
  void reBinAllHistos(int nBinsX, int nBinsY);

  void fillAllHistos();  //! Helens

private:

  //Configuration type
  char* mConfig;         //!

  //Event Object
  StSvtData* event;      //!

  //StEvent Object
  StEvent* stEvent;      //!

  // Number of events
  int mEvents;

  // Number of pixels per event
  int mNPixels;

  //Start Time for time evolution plots
  int mStartTime;

  //pedestal and common mode noise
  StSvtHybridCollection* pedestal;         //!
  StSvtHybridCollection* pedestal2ndOrd;   //!
  StSvtHybridCollection* rmsPedestal;      //!
  StSvtHybridCollection* cmn;              //!

  int mScaleRms;

  //statistical variables
  float* meanAdc;              //!
  float* meanSqAdc;            //!
  int*   nEvents;              //!
  float* accumAdcRaw;          //!
  float* accumAdcSqRaw;        //!
  float* accumAdcPed;          //!
  float* accumAdcSqPed;        //!
  float* accumAdcCMN;          //!
  float* accumAdcSqCMN;        //!
  StSvtHybridCollection* meanEventRaw;       //!
  StSvtHybridCollection* rmsEventRaw;        //!
  StSvtHybridCollection* meanEventPed;       //!
  StSvtHybridCollection* rmsEventPed;        //!
  StSvtHybridCollection* meanEventCMN;       //!
  StSvtHybridCollection* rmsEventCMN;        //!
  StSvtHybridCollection* nEvent;             //!
  TArrayF* nPixelsArray;
  TArrayF* nEventsArray;

  //histogram buffer
  TOrdCollection *histograms;   //!

  // ID's
  int barrelID;
  int ladderID;
  int waferID;
  int hybridID;

  int histID;
  int* histIDArray;             //!

  // keys
  Bool_t kRAWSTAT;
  Bool_t kPEDSTAT;
  Bool_t kCMNSTAT;
  Bool_t kEVENTSTAT;
  Bool_t kPIXELSTAT;
  Bool_t kZSSTAT;
  Bool_t kGLOBALSTAT;
  Bool_t kTOTALSTAT;
  Bool_t kSAVEMEM;
  Bool_t kFIRSTORD;
  Bool_t kSECONDORD;
  Bool_t kPEDCAP;
  Bool_t kPEDTIME;
  Bool_t kPRIMARY;
  Bool_t kGLOBAL;

  ClassDef(StSvtMonitor,1)
};

#endif
