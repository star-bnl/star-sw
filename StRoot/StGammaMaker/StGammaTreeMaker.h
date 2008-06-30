#ifndef __StGammaTreeMaker_h__
#define __StGammaTreeMaker_h__

#include "StMaker.h"
#include "TString.h"

class TTree;
class TFile;
class StGammaEvent;
class StGammaTreeMaker;

#include <vector>

class StGammaTreeVersion : public TNamed
{

 public:
  StGammaTreeVersion( const Char_t *name="version", const Char_t *title="versioning information for StGammaMaker chain"):TNamed(name,title){ /* nada */ };
  ~StGammaTreeVersion(){ /* nada */ };

  void print();

 private:
 protected:

  std::vector<TString> mMakerTags;   
  std::vector<TString> mStorageTags;

  friend class StGammaTreeMaker;
  
  ClassDef(StGammaTreeVersion,1);

};
 
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

  virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StGammaTreeMaker.h,v 1.4 2008/06/30 16:34:15 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}

 private:
 protected:

  TTree *mGammaTree;
  TFile *mGammaFile;

  StGammaEvent *mGammaEvent;
  TString mFilename;

  StGammaTreeVersion mVersion; /// code versioning information

  ClassDef(StGammaTreeMaker,1);

};

#endif
