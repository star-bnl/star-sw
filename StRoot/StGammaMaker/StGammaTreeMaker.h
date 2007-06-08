#ifndef __StGammaTreeMaker_h__
#define __StGammaTreeMaker_h__

#include "StMaker.h"
#include "TString.h"

class TTree;
class TFile;
class StGammaEvent;

class StGammaTreeMaker : public StMaker
{

 public:
  StGammaTreeMaker( const Char_t *name="gtmaker" );
  ~StGammaTreeMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  Int_t Finish();

  /// Sets the output file for the TTree.  Defaults into .hist branch
  void SetFile( TFile *file ){ mGammaFile = file; }
  /// Sets a pointer to an existing TTree.  Default creates at Init().
  void SetTree( TTree *tree ){ mGammaTree = tree; }
  /// Sets the filename (unless a file has been passeed above)
  void SetFilename( const Char_t *fname ){ mFilename=fname; }

  TFile *file(){ return mGammaFile; }
  TTree *tree(){ return mGammaTree; }

  /// create a gamma event
  StGammaEvent *event(){ return mGammaEvent; }

 private:
 protected:

  TTree *mGammaTree;
  TFile *mGammaFile;

  StGammaEvent *mGammaEvent;
  TString mFilename;

  ClassDef(StGammaTreeMaker,1);

};

#endif
