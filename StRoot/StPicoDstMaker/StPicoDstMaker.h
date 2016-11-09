#ifndef StPicoDstMaker_h
#define StPicoDstMaker_h

#include <vector>
#include <utility>

#include "TClonesArray.h"

#include "StChain/StMaker.h"
#include "StPicoDstMaker/StPicoArrays.h"

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
  StPicoDstMaker(PicoIoMode ioMode, char const* fileName = "", char const* name = "PicoDst");
  virtual ~StPicoDstMaker();

  virtual Int_t Init();
  virtual Int_t InitRun(Int_t const runnumber);
  virtual Int_t Make();
  virtual void  Clear(Option_t* option = "");
  virtual Int_t Finish();

  void printArrays();
  void SetStatus(char const* arrType, int status);

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

  void setVtxMode(PicoVtxMode);

protected:

  void streamerOff();

  void openWrite();
  void write();
  void closeWrite();
  Int_t openRead();
  void  read();
  void setBranchAddresses();
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

  void fillTracks();
  void fillEvent();
  void fillEmcTrigger();
  void fillMtdTrigger();
  void fillBTOWHits();
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
  bool getBEMC(StMuTrack* t, int* id, int* adc, float* ene, float* d, int* nep, int* towid);
  int  setVtxModeAttr();

  /// Selects a primary vertex from `muDst` vertex collection according to the
  /// vertex selection mode `mVtxMode` specified by the user.
  bool selectVertex();

  StMuDst*   mMuDst;
  StEmcCollection* mEmcCollection;
  StEmcPosition*   mEmcPosition;
  StEmcGeom*       mEmcGeom[4];
  StEmcRawHit*     mEmcIndex[4800];
  StPicoDst* mPicoDst;
  Float_t    mBField;

  PicoVtxMode mVtxMode;

  TString   mInputFileName;        //! *.list - MuDst or picoDst
  TString   mOutputFileName;       //! FileName
  TFile*    mOutputFile;

  TChain*   mChain;
  TTree*    mTTree;

  int mEventCounter;
  int mSplit;
  int mCompression;
  int mBufferSize;

  // MTD map from backleg to QT
  Int_t  mModuleToQT[30][5];        // Map from module to QT board index
  Int_t  mModuleToQTPos[30][5];     // Map from module to the position on QA board
  Int_t  mQTtoModule[8][8];         // Map from QT board to module
  Int_t  mQTSlewBinEdge[8][16][8];  // Bin Edge for QT slewing correction
  Int_t  mQTSlewCorr[8][16][8];     // QT Slewing correction

  TClonesArray*   mPicoArrays[StPicoArrays::NAllPicoArrays];
  char            mStatusArrays[StPicoArrays::NAllPicoArrays];

  ClassDef(StPicoDstMaker, 0)
};

inline StPicoDst* StPicoDstMaker::picoDst() { return mPicoDst; }
inline TChain* StPicoDstMaker::chain() { return mChain; }
inline TTree* StPicoDstMaker::tree() { return mTTree; }
inline void StPicoDstMaker::setSplit(int split) { mSplit = split; }
inline void StPicoDstMaker::setCompression(int comp) { mCompression = comp; }
inline void StPicoDstMaker::setBufferSize(int buf) { mBufferSize = buf; }
inline void StPicoDstMaker::setVtxMode(PicoVtxMode const vtxMode) { mVtxMode = vtxMode; }
#endif
