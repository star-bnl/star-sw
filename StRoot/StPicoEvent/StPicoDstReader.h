/**
 * \class StPicoDstReader
 * \brief Allows to read picoDst file(s)
 *
 * This class allows to read picoDst.root file or a list of files
 * that contain picoDst and sets up pointers to the picoDst, and
 * certain TClonesArrays that keep Event, Track, BTofHit, etc...
 * One can also turn on or off certain branches using the
 * SetStatus method.
 *
 * \author Grigory Nigmatkulov
 * \date May 28, 2018
 */

#ifndef StPicoDstReader_h
#define StPicoDstReader_h

// ROOT headers
#include "TChain.h"
#include "TTree.h"
#include "TFile.h"
#include "TString.h"
#include "TClonesArray.h"

// PicoDst headers
#include "StPicoDst.h"
#include "StPicoEvent.h"
#include "StPicoArrays.h"

//_________________
class StPicoDstReader : public TObject {

 public:

  /// Constructor that takes either picoDst file or file that
  /// contains a list of picoDst.root files
  StPicoDstReader(const Char_t* inFileName);
  /// Destructor
  ~StPicoDstReader();

  /// Return a pointer to picoDst (return NULL if no dst is found)
  StPicoDst *picoDst()    { return mPicoDst; }
  /// Return pointer to the chain of .picoDst.root files
  TChain *chain()         { return mChain; }
  /// Return pointer to the current TTree
  TTree *tree()           { return mTree; }

  /// Set enable/disable branch matching when reading picoDst
  void SetStatus(const Char_t* branchNameRegex, Int_t enable);

  /// Calls openRead()
  void Init();
  /// Read next event in the chain
  Bool_t readPicoEvent(Long64_t iEvent);
  /// Close files and finilize
  void Finish();

 private:

  /// Name of the inputfile (or of the inputfiles.list)
  TString mInputFileName;

  /// Turn off streamers
  void streamerOff();

  /// Create TClonesArray of pico classes and set them to picoDst
  void createArrays();
  /// Clear all TClonesArrays with picoDst classes
  void clearArrays();
  /// Set adresses of picoArrays and their statuses (enable/disable) to chain
  void setBranchAddresses(TChain *chain);

  /// Pointer to the input/output picoDst structure
  StPicoDst *mPicoDst;
  /// Pointer to the chain
  TChain *mChain;
  /// Pointer to the current tree
  TTree *mTree;

  /// Event counter
  Int_t mEventCounter;

  /// Pointers to pico arrays
  TClonesArray *mPicoArrays[StPicoArrays::NAllPicoArrays];
  /// Status of pico arrays
  Char_t        mStatusArrays[StPicoArrays::NAllPicoArrays];

  ClassDef(StPicoDstReader, 0)
};

#endif
