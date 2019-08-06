/**
 * \class StPicoDstMaker
 * \brief Class that converts MuDst into PicoDst
 *
 * The class allows the conversion from MuDst to PicoDst
 * using several options important options:
 *
 * \par Vertex selection criteria (PicoVtxMode)
 * Since MuDst stored several vertices and PicoDst allows one to
 * keep only one, the vertex selection criteria have been proposed.
 * Currently, next options are available:
 *   a) Default - select the first primary vertex (with index = 0)
 *   b) VpdOrDefault - uses the  Default option at first, and then
 *      if BTOF info is available checks for the Vpd option.
 *   c) Vpd - checks if BTof info is available then loops over primary
 *      vertices and checks if |Vz - VpdVz|<mTpcVpdVzDiffCut. The first
 *      vertex that satisfies the cut is selected.
 * Default is NotSet. In this case the program execution will be terminated.
 * Has to be explicitly set.
 *
 * \par Vertex position (mTpcVpdVzDiffCut)
 * There is a possibility to cut on the vertex position requiring
 * the difference of vertex position along the beam direction (z) reconstructed
 * in TPC and estimated via VPD by the absolute value to be less then mTpcVpdVzDiffCut.
 * Default is 6 cm.
 *
 * \par Saving covariance matrix (PicoCovMtxMode)
 * In order to store covariance matrix for each track one should specify
 * PicoCovMtxMode with one of two words:
 *   a) PicoCovMtxSkip - skip and not write covariance matrix
 *   b) PicoCovMtxWrite - store covariance matrices. If covariance matrix
 *      does not exist, then it is filled with zeros
 * Default is PicoCovMtxSkip.
 *
 *\par Saving BEmc SMD hits (PicoBEmcSmdMode)
 * To save the BEmc SMD eta/phi hits near a BHT2/3 trigger tower, one should specify
 * PicoBEmcSmdMode with one of two words:
 *   a) PicoBEmcSmdSkip - skip and don't write SMD hits for BHT2/3 triggers
 *   b) PicoBEmcSmdWrite - save BEmc SMD hits within 1.5 tower radius of every BHT3
 * Default is PicoBEmcSmdSkip.
 *
 * Additional information can be found here:
 * <a href="https://drupal.star.bnl.gov/STAR/blog/gnigmat/picodst-format">The PicoDst format</a>
 */

#ifndef StPicoDstMaker_h
#define StPicoDstMaker_h

// StChain headers
#include "StChain/StMaker.h"

// PicoDst headers
#include "StPicoEvent/StPicoArrays.h"
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

//_________________
class StPicoDstMaker : public StMaker {

 public:

  /// Write/Read mode: 1-write, 2-read
  enum PicoIoMode {IoWrite=1, IoRead=2};
  /// Primary vertex selection mode: 0-NotSet, 1-Default, 2-Vpd, 3-VpdOrDefault
  enum PicoVtxMode {NotSet=0, Default=1, Vpd=2, VpdOrDefault=3};
  /// Write or not write covariance matrix: 0-skip, 1-write
  enum PicoCovMtxMode {Skip=0, Write=1};
  /// Write or not write the BEmc SMD hits associated with a BHT2/3 trigger: 0-skip, 1-write
  enum PicoBEmcSmdMode {SmdSkip=0, SmdWrite=1};

  /// Constructor
  StPicoDstMaker(char const* name = "PicoDst");
  /// Constructor that takes most of pararmeters
  StPicoDstMaker(PicoIoMode ioMode, char const* fileName = "",
		 char const* name = "PicoDst");
  /// Destructor
  virtual ~StPicoDstMaker();

  /// Init run
  virtual Int_t InitRun(Int_t const runnumber);
  /// Standard STAR Init() function called from StChain
  virtual Int_t Init();
  /// Standard STAR Make() function called from StChain
  virtual Int_t Make();
  /// Clear
  virtual void  Clear(Option_t* option = "");
  /// Standard STAR Finish() function called from StChain
  virtual Int_t Finish();

  /// Print pico arrays
  void printArrays();

  /// Enables or disables branches matching a simple regex pattern in reading mode
  void SetStatus(char const* branchNameRegex, int enable);

  /// Returns null pointer if no StPicoDst
  StPicoDst* picoDst()      { return mPicoDst; }
  /// In read mode, returns pointer to the chain of .picoDst.root files
  TChain* chain()           { return mChain; }
  /// Returns pointer to the current TTree, the top level io structure
  TTree* tree()             { return mTTree; }

  /// Sets the split level for the file and all branches.
  /// Please refer to the ROOT manual (http://root.cern.ch) for more info
  void setSplit(int = 99);
  /// Sets the buffer size for all branches.
  void setBufferSize(int = 65536 * 4);
  /// Sets the compression level for the file and all branches. 0 means no compression,
  /// 9 is the higher compression level.
  void setCompression(int comp = 9);

  /// Set vertex selection mode
  void setVtxMode(const PicoVtxMode vtxMode)              { mVtxMode = vtxMode; }
  /// Set to write or not to write covariant matrix
  void setCovMtxMode(const PicoCovMtxMode covMtxMode)     { mCovMtxMode = covMtxMode; }
  /// Set to write or not write BEmc Smd hits
  void setBEmcSmdMode(const PicoBEmcSmdMode bemcSmdMode)  { mBEmcSmdMode = bemcSmdMode; }

 private:

  /// Turn-off ROOT streamers
  void streamerOff();

  /// Open file for writing
  void openWrite();
  /// Write information
  void write();
  /// Close file where the information has been written
  void closeWrite();
  /// Open file to read
  Int_t openRead();
  /// Read information
  void  read();
  /// Close file which was read
  void closeRead();
  /// Set branch addresses
  void setBranchAddresses(TChain*);

  /// Build EMC indexes
  void buildEmcIndex();
  /// Initialize EMC related arrays
  void initEmc();
  /// Finish EMC
  void finishEmc();

  /// Initialize MTD information
  Bool_t initMtd(Int_t const runnumber);

  /// Clear arrays
  void clearArrays();
  /// Create arrays
  void createArrays();

  Int_t MakeRead();
  Int_t MakeWrite();

  /// Fill event header info
  void fillEventHeader() const;

  /// Fill track information
  void fillTracks();
  /// Fill event information
  void fillEvent();
  /// Fill EMC trigger information
  void fillEmcTrigger();
  /// Fill MTD trigger
  void fillMtdTrigger();
  /// Fill BEMC tower information
  void fillBTowHits();
  /// Fill BTOF information
  void fillBTofHits();
  /// Fill MTD information
  void fillMtdHits();
  /// Fill EPD hit information
  void fillEpdHits();
  /// Fill BBC hit information
  void fillBbcHits();
  /// Fill ETOF information
  void fillETofHits();

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
  /// Set vertex mode attributes
  Int_t  setVtxModeAttr();
  /// Set covariance matrix mode attributes
  Int_t  setCovMtxModeAttr();
  /// Set BEmc Smd mode attributes
  Int_t  setBEmcSmdModeAttr();

  /// Selects a primary vertex from `muDst` vertex collection according to the
  /// vertex selection mode `mVtxMode` specified by the user.
  Bool_t selectVertex();
  /// VpdVz-Vz cut value. Default is 5 cm.
  Float_t   mTpcVpdVzDiffCut;

  /// A pointer to the main input source containing all muDst `TObjArray`s
  /// filled from corresponding muDst branches
  StMuDst*  mMuDst;

  /// A pointer to the main input/outpur picoDst structure containing all `TObjArray`s
  StPicoDst*  mPicoDst;

  /// Pointer to the ECM collection
  StEmcCollection* mEmcCollection;
  /// Pointer to the EMC position
  StEmcPosition*   mEmcPosition;
  /// Pointer to the EMC geometry
  StEmcGeom*       mEmcGeom[4];
  /// Pointer to the array of BEMC tower hits
  StEmcRawHit*     mEmcIndex[4800];

  /// Magnetic field of the current event
  Float_t    mBField;

  /// Vertex selection mode
  PicoVtxMode mVtxMode;
  /// Covariant matrix not write/write mode
  PicoCovMtxMode mCovMtxMode;
  /// BEmc Smd not write/write mode
  PicoBEmcSmdMode mBEmcSmdMode;

  /// *.list - MuDst or picoDst
  TString   mInputFileName;
  /// Output file name
  TString   mOutputFileName;
  /// Pointer to the output file
  TFile*    mOutputFile;

  /// Pointer to the chain
  TChain*   mChain;
  /// Pointer to the TTree with picoDst
  TTree*    mTTree;

  /// Event counter
  int mEventCounter;

  /// Splitting level of ROOT file
  int mSplit;
  /// Compression level
  int mCompression;
  /// Size of the buffer
  int mBufferSize;

  /// Map from module to QT board index
  Int_t  mModuleToQT[30][5];
  /// Map from module to the position on QA board
  Int_t  mModuleToQTPos[30][5];
  /// Map from QT board to module
  Int_t  mQTtoModule[8][8];
  /// Bin Edge for QT slewing correction
  Int_t  mQTSlewBinEdge[8][16][8];
  /// QT Slewing correction
  Int_t  mQTSlewCorr[8][16][8];

  /// Pointer to the pico arrays
  TClonesArray*   mPicoArrays[StPicoArrays::NAllPicoArrays];
  /// Status arrays
  char            mStatusArrays[StPicoArrays::NAllPicoArrays];

  /// FMS filler
  StPicoFmsFiller  mFmsFiller;

  /// Get CVS status
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPicoDstMaker.h,v 1.25 2019/08/06 21:40:19 gnigmat Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;
  }

  ClassDef(StPicoDstMaker, 0)
};

inline void StPicoDstMaker::setSplit(int split) { mSplit = split; }
inline void StPicoDstMaker::setCompression(int comp) { mCompression = comp; }
inline void StPicoDstMaker::setBufferSize(int buf) { mBufferSize = buf; }

#endif
