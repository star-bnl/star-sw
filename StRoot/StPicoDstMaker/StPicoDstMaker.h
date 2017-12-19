#ifndef StPicoDstMaker_h
#define StPicoDstMaker_h
#include <vector>
#include "StChain/StMaker.h"
#include "StPicoDstMaker/StPicoArrays.h"
#include "StPicoDstMaker/StPicoBbcFiller.h"
#include "StPicoDstMaker/StPicoEpdFiller.h"
#include "StPicoDstMaker/StPicoFmsFiller.h"

class TClonesArray;
class TChain;
class TFile;
class TTree;
class StMuDst;
class StMuTrack;
class StEmcCollection;
class StEmcPosition;
class StEmcGeom;
class StEmcRawHit;
class StPicoDst;
class StPicoEvent;



class StPicoDstMaker : public StMaker
{
public:
  enum PicoIoMode {IoWrite=1, IoRead=2};
  enum PicoVtxMode {NotSet=0, Default=1, Vpd=2, VpdOrDefault=3};

  StPicoDstMaker(char const* name = "PicoDst");
  //  StPicoDstMaker(PicoIoMode ioMode, char const* fileName = "", char const* name = "PicoDst");
  StPicoDstMaker(Int_t ioMode, char const* fileName = "", char const* name = "PicoDst");
  virtual ~StPicoDstMaker();

  virtual Int_t Init();
  virtual Int_t InitRun(Int_t const runnumber);
  virtual Int_t Make();
  virtual void  Clear(Option_t* option = "");
  virtual Int_t Finish();

  void printArrays();

  /// Enables or disables branches matching a simple regex pattern in reading mode
  void SetStatus(char const* branchNameRegex, int enable);

  /// Returns null pointer if no StPicoDst
  StPicoDst* picoDst();
  /// In read mode, returns pointer to the chain of .picoDst.root files
  TChain* chain();
  /// Returns pointer to the current TTree, the top level io structure
  TTree* tree();

  /// Sets the split level for the file and all branches. Please refer to the ROOT manual (http://root.cern.ch) for more info
  void setSplit(int = 99);
  /// Sets the buffer size for all branches.
  void setBufferSize(int = 65536 * 4);
  /// Sets the compression level for the file and all branches. 0 means no compression, 9 is the higher compression level.
  void setCompression(int comp = 9);

  void setVtxMode(const PicoVtxMode vtxMode);
  void SetGoodTriggers(const Char_t *trigList=0); 
  void SetMaxTrackDca(Double_t cut = 50);
  void SetMaxVertexTransError(Double_t cut = 0.0050);
  void SetVxXYrange(Double_t xmin = -0.3, Double_t xmax = 0., Double_t ymin = -0.27, Double_t ymax = -0.13);
  void SetVxZrange(Double_t zmin = -70, Double_t zmax = 70.);
  void SetVxRmax(Double_t rmax = 2);
private:

  void streamerOff();

  void openWrite();
  void write();
  void closeWrite();
  Int_t openRead();
  void  read();
  void closeRead();
  void setBranchAddresses(TChain*);

  void buildEmcIndex();
  void initEmc();
  void finishEmc();

  Bool_t initMtd(Int_t const runnumber);

  void clearArrays();
  void createArrays();

  Int_t MakeRead();
  Int_t MakeWrite();

  //for reading
  void fillEventHeader() const; //changes "global" variable, not this maker

  //for writing
  Int_t fillTracks(); // iok != 0 skip event
  void fillEvent();
  void fillEmcTrigger();
  void fillMtdTrigger();
  void fillBTowHits();
  void fillBTofHits();
  void fillMtdHits();

 /**
  * Returns various measurements by the BEMC and BSMD detectors corresponding to
  * a given global track.
  *
  * param[in]   t        A global track
  * param[out]  id >= 0  Indicates that a BEMC tower matching track t has been found
  * param[out]  adc      The largest ADC value of a tower in the BEMC cluster matching track t
  * param[out]  ene[0]   The highest energy tower in the BEMC cluster matching track t
  * param[out]  ene[1]   The total energy of the BEMC cluster containing the matching tower
  * param[out]  ene[2]   The energy deposited in the (closest) BEMC tower matching track t
  * param[out]  ene[3]   The energy deposited in the second closest BEMC tower matching track t
  * param[out]  ene[4]   The energy deposited in the third closest BEMC tower matching track t
  * param[out]  d[0]     The distance [cm] along z from the track t projection onto BSMD to the BEMC cluster matching track t
  * param[out]  d[1]     The distance [rad] along phi similar to d[0]
  * param[out]  d[2]     The distance [rad] along eta between the track t projection onto BEMC and the matched BEMC tower center
  * param[out]  d[3]     The distance [rad] along phi similar to d[2]
  * param[out]  nep[0]   The number of eta strips in the BSMD cluster corresponding to the BEMC cluster matching track t
  * param[out]  nep[1]   The number of phi strips in the BSMD cluster corresponding to the BEMC cluster matching track t
  * param[out]  towid[]  Unique ids of the three BEMC towers identified for ene[2], ene[3], and ene[4]
  */
  Bool_t getBEMC(const StMuTrack* t, int* id, int* adc, float* ene, float* d, int* nep, int* towid);
  int  setVtxModeAttr();

  /// Selects a primary vertex from `muDst` vertex collection according to the
  /// vertex selection mode `mVtxMode` specified by the user.
  Bool_t selectVertex();

  /// A pointer to the main input source containing all muDst `TObjArray`s
  /// filled from corresponding muDst branches
  StMuDst*  mMuDst;

  /// A pointer to the main input/outpur picoDst structure containing all `TObjArray`s
  StPicoDst*  mPicoDst;

  StEmcCollection* mEmcCollection;
  StEmcPosition*   mEmcPosition;
  StEmcGeom*       mEmcGeom[4];
  StEmcRawHit*     mEmcIndex[4800];
  Float_t   mTpcVpdVzDiffCut;

  Float_t    mBField;

  PicoVtxMode mVtxMode;

  TString   mInputFileName;        //! *.list - MuDst or picoDst
  TString   mOutputFileName;       //! FileName
  TFile*    mOutputFile;

  TChain*   mChain;
  TTree*    mTTree;

  int mEventCounter;

  /// Parameters to control the storage of picoDst tree in write mode. Have no
  /// effect when reading .picoDst.root files
  ///@{
  int mSplit;
  int mCompression;
  int mBufferSize;
  ///@}

  // MTD map from backleg to QT
  Int_t  mModuleToQT[30][5];        // Map from module to QT board index
  Int_t  mModuleToQTPos[30][5];     // Map from module to the position on QA board
  Int_t  mQTtoModule[8][8];         // Map from QT board to module
  Int_t  mQTSlewBinEdge[8][16][8];  // Bin Edge for QT slewing correction
  Int_t  mQTSlewCorr[8][16][8];     // QT Slewing correction

  TClonesArray*   mPicoArrays[StPicoArrays::NAllPicoArrays];
  Char_t          mStatusArrays[StPicoArrays::NAllPicoArrays];

  StPicoBbcFiller  mBbcFiller;
  StPicoEpdFiller  mEpdFiller;
  StPicoFmsFiller  mFmsFiller;
  static Double_t  fgerMax;
  static Double_t  fgdca3Dmax; 
  static vector<Int_t> fGoodTriggerIds; 
  static Double_t  fgVxXmin, fgVxXmax, fgVxYmin, fgVxYmax;
  static Double_t  fgVxZmin, fgVxZmax, fgVxRmax;
  ClassDef(StPicoDstMaker, 0)
};


inline StPicoDst* StPicoDstMaker::picoDst() { return mPicoDst; }
inline TChain* StPicoDstMaker::chain() { return mChain; }
inline TTree* StPicoDstMaker::tree() { return mTTree; }
inline void StPicoDstMaker::setSplit(int split) { mSplit = split; }
inline void StPicoDstMaker::setCompression(int comp) { mCompression = comp; }
inline void StPicoDstMaker::setBufferSize(int buf) { mBufferSize = buf; }
inline void StPicoDstMaker::setVtxMode(const PicoVtxMode vtxMode) { mVtxMode = vtxMode; }

#endif
