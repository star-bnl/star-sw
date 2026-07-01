/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to hold the mudst pointers and data that is needed for analysis with MuDsts and FCS classes. An instance of this data structure can be passed to objects inherting from #StMuFcsVirtualAna and this structure is filled with #StFwdAnaDataMaker. It also contains some static methods that can be used for processing the data and to compute the Transverse Single Spin Assymetry (TSSA)

  DESCRIPTION
  Contains all relevant pointers and data for mudst analysis with the FCS. Also contains a #TTree which can be turned on and off to save the event, point, and point pair information. It also contains a vector of triggers you want to include in your analysis and other variables related to the trigger information. Contains a simple #PolData struct to fill and hold polarization data and a map  of key->value pairing of filldata->#PolData. Also, contains variables related to cuts you may want to apply. All these "options" should be set before calling methods in the STAR maker chain. Also contains many useful static function calls that can be used in #StMuFcsVirtualAna objects or analysis and drawing macros.

  CAVEATS
  1. The trigger matching only works if you have taken the time to create a text file that contains a list of all the FCS triggers and their starting and ending run numbers. It's format must be "TriggerName OfflineId StartRun EndRun". This text file must then be specified to #StFwdAnaDataMaker

  2. The polarization file should be taken from https://wiki.bnl.gov/rhicspin/Results_(Polarimetry) and saved as a text file. You need to manually remove any incomplete rows

  LOG
  @[January 9, 2026]  > Copied from *StMuFcsPi0TreeMaker* and modified to keep only the relevant data pointers
  @[January 21, 2026] > Copied members of *StFcsRun22TriggerMap* to here so don't need an extra pointer. Limited number of pointers to only the main relevant ones and the rest is accessed through getter functions
  @[June 8, 2026] > Changed #FcsPi0Candidate to #FcsPairCandidate, added #mEvent as an internal event counter
  @[June 30, 2026] > Changed name from StMuFcsAnaData to StFwdAnaData to be consistent with new naming scheme which is more general for STAR forward analysis software. Changed the related StMuFcs* names to the new StFwdData* names. Moved #mNTrig, #mTriggers, and #mUseVertex to #StFwdDataEvent. Changed #mEvtInfo to #mEvtData to be consistent with name change of FcsEventInfo to StFwdDataEvent. Changed mPi0Tree to #mDataTree and changed "Pi0" branch name to "Pair"
  @[July 1, 2026] > Added #getPolData() with no arguments which returns the polarization data based on the fill number in #StFwdDataEvent, made some getter functions const. Made variables that cut on energy, vertex, etc protected so they must be set and gotten with functions
*/


#ifndef STFWDANA_STFWDANADATA_HH
#define STFWDANA_STFWDANADATA_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
#include "TColor.h"
#include "TString.h"
#include "TPolyLine.h"
#include "TEllipse.h"
#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TH1F.h"
#include "TH3.h"
#include "TLegend.h"
#include "TF1.h"
#include "TGeoPolygon.h"
#include "TGraphErrors.h"
#include "TRandom3.h"
#include "TCanvas.h"

//STAR Headers
#include "StEnumerations.h"
#include "StMaker.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerId.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsPoint.h"
#include "StEpdDbMaker/StEpdDbMaker.h"
#include "StEpdHitMaker/StEpdHitMaker.h"

#include "StSpinPool/StFwdData/StFwdDataEvent.h"
#include "StSpinPool/StFwdData/StFwdDataFcs.h"

class StEpdGeom;
class StFwdAnaDataMaker;

/*Simple data struct to hold polarization information from file only important values are kept*/
struct PolData
{
  Int_t mFillNum = 0;              //! Fill Number
  Int_t mBeamEn = 0;               //! Beam energy
  Int_t mStartTime = 0;            //! Start time of fill
  Double_t mBlueP0 = 0;            //! Blue beam intial polarization in %
  Double_t mBlueErrP0 = 0;         //! Blue beam intial polarization error in %
  Double_t mBluedPdT = 0;          //! Blue beam polarization decay in %/hour
  Double_t mBlueErrdPdT = 0;       //! Blue beam polarization decay error in %/hour
  Double_t mYellowP0 = 0;          //! Yellow beam intial polarization in %
  Double_t mYellowErrP0 = 0;       //! Yellow beam intial polarization error in %
  Double_t mYellowdPdT = 0;        //! Yellow beam polarization decay in %/hour
  Double_t mYellowErrdPdT = 0;     //! Yellow beam polarization decay error in %/hour

  void Print() const;
};

class StFwdAnaData : public TObject {
public:
  friend class StFwdAnaDataMaker;
  
  StFwdAnaData();
  virtual ~StFwdAnaData();

  //static const short NENERGYBIN = 8;     ///< Number of energy bins
  static const short NXFBIN = 8;         ///< Number of x_F bins (should match energy binning)
  static const short NPHIBIN = 24;       ///< Number of phi bins

  static const Double_t xfbins[NXFBIN+1];// = {0, 0.04, 0.08, 0.12, 0.16, 0.2, 0.24, 0.3, 0.5}; //@[August 4, 2025] > New xF binning

  //STAR classes needed to access various STAR and event data
  StEvent*             event()     { return mStEvent; }
  StMuDstMaker*        muDstMkr()  { return mMuDstMkr; }
  StMuDst*             muDst()     { return mMuDst; }
  StMuEvent*           muEvent()   { return mMuEvent; }
  const StTriggerData* trigData()  { return mTrigData; }
  StRunInfo*           runInfo()   { return mRunInfo; }
  StSpinDbMaker*       spinDbMkr() { return mSpinDbMkr; }
  StFcsDb*             fcsDb()     { return mFcsDb; }
  StEpdGeom*           epdGeom()   { return mEpdGeom; }
  StMuFcsCollection*   fcsColl()   { return mMuFcsColl; }
  void                 epdColl(TClonesArray*& epdmucoll, StEpdCollection*& epdcoll);  ///< Since the EPD hits coming from the MuDst and the EPD hits coming from StEpdHitMaker are of different types this is the cheapest solution I could find to return two types. Just check which one is nonzero and use that one. @[January 21, 2026] > [This solution was suggested here](https://cplusplus.com/forum/beginner/101409/). Another suggest solution was to return a pair but this also requires checking for which one is nonzero. This check can be performed with ternary operator as can be seen in #StMuFcsAnaEpdCheck. Its a bit messy but its too late to change the EPD code to give the same return types for either case.

  static Int_t MakeGraph(TFile* file, TObjArray* grapharr, TGraph*& graph, const char* name, const char* title );
  static Int_t MakeGraph(TFile* file, TObjArray* grapharr, TGraphErrors*& graph, const char* name, const char* title );

  static std::vector<Double_t> ProjectToEpd(Double_t xfcs, Double_t yfcs, Double_t zfcs, Double_t zvertex);
  static void AddHistStatsOneline( TLegend* HistLeg, const TH1* h1, const std::string &title="" );
  static void DoTssaAna( TH1* npi0[][2], TH1* h1_rawasymVphi[][StFwdAnaData::NXFBIN] );    ///< Compute asymmetry from npi0 array of [blue,yellow][spin up,down] and return an array of raw assymtries by [blue,yellow][energybin]
  static void DoTssaFit(TH1* h1_rawasymVphi[][StFwdAnaData::NXFBIN], TH1* h1_bluepoldata, TH1* h1_yellowpoldata, TH1* h1_AnResult[]);
  static void DoPi0Fits(TH3* mH1F_invmass, TH1* hist_proj[] );
  static void DoBgCorrectedAn(TH1* h1_invmass_en[], TH1* h1_an_inc, TH1* h1_an_bg, TH1* h1_anresult );  
  static Double_t pol4bg(Double_t* x, Double_t* par);      ///< disjoint 4th order polynomial to exclude signal range
  static Double_t skewgaus(Double_t*x, Double_t* par);     ///< Skewed Gaussian
  static Double_t pol4skewgaus(Double_t*x, Double_t* par); ///< fourth order polynomial + skewed Gaussian
  static Int_t GetColor(Double_t Value, Double_t MinVal, Double_t MaxVal);

  void resetEvent(); ///< Reset those variables relevant to the next event

  Double_t vertexCutLow()const{ return mVertexCutLow; }
  Double_t vertexCutHigh()const{ return mVertexCutHigh; }
  Double_t enCut()const{ return mEnCut; }
  Double_t epdNmipCut()const{ return mEpdNmipCut; }

  void setVertexCutLow(Double_t vlow){  mVertexCutLow = vlow;  }
  void setVertexCutHigh(Double_t vhigh){ mVertexCutHigh = vhigh; }
  void setEnCut(Double_t encut){ mEnCut = encut; }
  void setEpdNmipCut(Double_t nmipcut){ mEpdNmipCut = nmipcut; }
  
  void setRandomSeed(ULong_t seed){ mSpinRndm.SetSeed(seed); }

  UInt_t getRandomSeed()const{ return mSpinRndm.GetSeed(); }

  void setTreeOnBit(UShort_t bitmap){ mTreeOnBitMap = bitmap; }
  void setEventBit(bool val=1);
  void setPhotonOn(bool val=1);
  void setPhPairOn(bool val=1);

  UShort_t checkTreeOnBit() const { return mTreeOnBitMap; }
  bool isEventOn() const;
  bool isPhotonOn() const;
  bool isPhPairOn() const;

  PolData* getPolData() const;                            //Get polarization data based on fill number in #StFwdDataEvent
  PolData* getPolData(Int_t fillnum) const;               //Get polarization data based on specific fill number
  Double_t randomNum(){ return mSpinRndm.Rndm(); }        //random number between 0 and 1
  
  TTree* getTree()const{ return mDataTree; }
  Int_t fillTree()const{ if(mDataTree!=0){return mDataTree->Fill();} return 0; }
  Int_t writeTree(const char* name=nullptr,Int_t option=0,Int_t bufsize=0)const{ if(mDataTree!=0){return mDataTree->Write(name,option,bufsize);} return 0;}
  Int_t writeTree(const char* name=nullptr,Int_t option=0,Int_t bufsize=0){ if(mDataTree!=0){return mDataTree->Write(name,option,bufsize);} return 0;}
  Int_t getTreeEntries()const{ if(mDataTree!=0){return mDataTree->GetEntriesFast();} return 0; }

  ULong_t getEventNum()const{ return mEvent; }
  StFwdDataEvent* getEvtData()const{ return mEvtData; }
  Int_t getEntry(Int_t ientry){ return mDataTree->GetEntry(ientry); }
  Int_t getNTrig()const{ return mEvtData->mNTrig; }
  const Int_t* getTrig()const{ return mEvtData->mTriggers; }
  Int_t getTrig(Int_t itrig)const{ return mEvtData->mTriggers[itrig]; }
  Float_t fcsPtThr(Int_t trigid) const;                               ///< Get Pt threshold for a given offline trigger id
  Float_t fcsPtThrAsym(Int_t trigid) const;                           ///< Get Pt threshold asymetric for a given offline trigger id
  bool exceedTrigPt(Double_t checkpt);    //Function to check if a particle "candidate"'s transverse momentum (pt) exceeds the trigger threshold for the run number loaded into the data

  TClonesArray* getPhArr()const{ return mPhArr; }
  Int_t getNPhoton()const{ return mPhArr->GetEntriesFast(); }
  StFcsPhotonCandidate* getPhoton(Int_t iph)const{ return dynamic_cast<StFcsPhotonCandidate*>(mPhArr->UncheckedAt(iph)); }

  TClonesArray* getPhPairArr()const{ return mPhPairArr; }
  Int_t getNPhPair()const{ return mPhPairArr->GetEntriesFast(); }
  StFcsPairCandidate* getPhPair(Int_t ipi0)const{ return dynamic_cast<StFcsPairCandidate*>(mPhPairArr->UncheckedAt(ipi0)); }
  //#ifndef __CINT__
  //void SetTrigs(const char* trigname,...);//{ mTargetTrig.emplace_back(trigname); }
  //template<typename... Args>
  //void SetTrigs(Args... restargs){ SetTrigs(restargs...); } //function to set trigger ids to use. Does not check for repetition so need to be a good user
  //#endif
  void AddTrig(const char* trigname ){ mTargetTrig.emplace_back(trigname); }  //function to set trigger ids to use. Does not check for repetition so need to be a good user
  void setIgnoreTrig(bool value=true){ mIgnoreTrig = value; }
  bool ignoreTrig()const{ return mIgnoreTrig; }
  
  int sizeOfFcsTriggers() const;                                          ///< Return size of #mListOfFcsTriggers
  const char* fcsTriggerName(int idx) const;                              ///< Return the trigger name in #mListOfFcsTriggers for a given index (#idx)
  const char* fcsTrigNameFromId(Int_t trigidtomatch, Int_t runnumber) const;  ///< Check #mAllFcsTrigRanges for a key #trigidtomatch to find any #TrigRange matches. If not found return "NF" (not found) and if found then check if #runnumber is valid, if so return the name of the trigger; otherwise return "NF".

  virtual void Print(const char* opt="") const;  //"e" for event, "t" for trigger, "g" for photon, "p" for photon pair, "a" for all

  std::vector<std::string> mTargetTrig;     ///< For Target Trigger ID
  
  //bool mReadMuDst;   ///< flag to check if reading from mudst or not (This can be used to turn off populating event info)
  //bool mReadSim;     ///< flag to check if reading mudst from simulations
  bool mValidTrigFound = false;       ///< (false) Found a trigger that matches the ones specified or #mIgnoreTrig is on
  bool mEmTrigFound = false;          ///< (false) Found a trigger that mathces one of the FCS EM triggers
  // Variables to keep track if a given trigger was found in an event and can be used to fill the trigger sepcific histograms. The logic is that less than 0 means trigger was not fired. greater than or equal to 0 means trigger was fired. If trigger is fired then the value gets set to the value that corresponds to the index of a histogram array to be used later for filling. The 0 index is for all triggers
  short mTrigEm0 = -1;            ///< Gets set to 1 if EM0 or EM0_tpc trigger was fired
  short mTrigEm1 = -1;            ///< Gets set to 2 if EM1 or EM1_tpc trigger was fired
  short mTrigEm2 = -1;            ///< Gets set to 3 if EM2 or EM2_tpc trigger was fired
  short mTrigEm3 = -1;            ///< Gets set to 4 if EM3 or EM3_tpc trigger was fired


protected:
  ULong_t mEvent = 0;                       ///< Internal event counter that can be used to tag and keep track of events

  Double_t mVertexCutLow = -150.0;          ///< (-150.0) Variables for z vertex cuts at low end in cm
  Double_t mVertexCutHigh = 150.0;          ///< (150.0) Variables for z vertex cuts at high end in cm
  Double_t mEnCut = 1.0;                    ///< (1.0) Energy Cut for #StFcsPhotonCandidates
  Double_t mEpdNmipCut = 0.4;               ///< (0.4) Cut on EPD nmip to classify cluster or point as charged or uncharged
  
  bool mIgnoreTrig = false;                 ///< (false) flag to check if ignoring triggers or not
  UShort_t mTreeOnBitMap = 0x7;             ///< (0x7) Turn on or off branches in the pi0 tree. first bit is events, second bit is photon branch, third bit is pi0 branch. Turn on all branches by default

  //Mutable Makers that need to be changed in Make
  StEvent* mStEvent = 0;
  StMuDstMaker* mMuDstMkr = 0;
  StMuDst* mMuDst = 0;
  StMuEvent* mMuEvent = 0;
  const StTriggerData* mTrigData = 0;
  StRunInfo* mRunInfo = 0;
  StMuFcsCollection* mMuFcsColl = 0;
  StEpdHitMaker* mEpdHitMkr = 0;
  TClonesArray* mMuEpdHits = 0;
  StEpdCollection* mEpdColl = 0;

  //Called in InitRun() shouldn't be changed unless InitRun() is called again
  StFcsDb* mFcsDb = 0;
  StSpinDbMaker* mSpinDbMkr = 0;
  StEpdGeom* mEpdGeom = 0;

  //Initialized in Init() but will be modified and reset by Make calls
  TTree* mDataTree = 0;                  ///< #TTree with desired branches
  StFwdDataEvent* mEvtData = 0;           ///< #StFwdDataEvent object for TTree
  //For trigger branch of tree
  TClonesArray* mPhArr = 0;             ///< #TClonesArray of all #StFcsPhotonCandidate
  TClonesArray* mPhPairArr = 0;         ///< Array of #StFcsPhotonCandidate pairs to be used later in analysis

  void loadTree(TFile* file);   ///< Attempts to read the TTree from a file
  void makeTree(TFile* file);   ///< Creates a new TTree deleting old one if it exists

  Int_t ReadPolFile(const char* filename);        ///< Function to read the polarization data file that is custom made for this class

  //Copied from StFcsRun22TriggerMap
  //Simple struct to hold the tigger ranges when reading the file
  struct TrigRange{
    TrigRange();
    TrigRange(const char* name, Int_t trigid, Int_t startrun, Int_t endrun);
    std::string mName;                 ///< Name of the trigger
    Int_t mOfflineTrigId;              ///< Offline trigger id
    Int_t mStartRun;                   ///< Starting run number (inclusive)
    Int_t mEndRun;                     ///< Ending run number (inclusive)
    Float_t mPtThreshold;              ///< P_T==E_T threshold for trigger
    Float_t mPtThresholdAsym;          ///< Secondary trigger threshold for asymmetric P_T triggers, -1 if it doesn't exist
    bool ValidRun(Int_t runnum) const; ///< check if a given runnum falls into the range
  };

  void readFcsTrigTxtFile(const char* filename);                      ///< Process the text file and fill #mListOfFcsTriggers and #mAllFcsTrigRanges

  std::vector<std::string> mListOfFcsTriggers;       ///< All found names of the triggers. This just makes it easier to recall unique trigger names
  std::map<Int_t,TrigRange> mAllFcsTrigRanges;       ///< Map of the unique offline trigger Ids to all the found ranges for those triggers
  
private:
  TRandom3 mSpinRndm;                 ///< Spin state randomizer
  std::map<Int_t,PolData*> mPolarizationData;   ///< Map of polarization data from file with fill number as key to quickly look up from data structure

  void SetTriggerPtThresholds(TrigRange& input);  ///< Copied a map of trigger name to its corresponding P_T==E_T thresholds from https://www.star.bnl.gov/protected/spin/akio/fcs/run22trg.html on November 25, 2024. All values will be in GeV. Called when filling #mAllFcsTrigRanges

  ClassDef(StFwdAnaData,1)
};

#endif

