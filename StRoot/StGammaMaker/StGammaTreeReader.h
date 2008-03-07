#ifndef __StGammaTreeReader_h__
#define __StGammaTreeReader_h__

#include "StMaker.h"
#include "StGammaEvent.h"

#include "TChain.h"
#include "TString.h"
#include <map>

class StGammaTreeReader : public StMaker
{

 public:

  StGammaTreeReader(const Char_t *name="mGammaTree", const Char_t *bname="gammas");
  ~StGammaTreeReader(){ /* nada */ };

  /// Add a file to the list of files in the chain being processed
  void chainFile( const Char_t *name, const Char_t *matches=".root" );

  StGammaEvent *event(){ return mEvent; }

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  /// Returns the total number of entries for this chain of events
  Long64_t getNumberOfEntries(){ return mChain->GetEntries(); }

  /// Retusn a specified entry number
  Int_t    getEvent(Long64_t entry); // read in by event
  /// Returns a specified event for a specified run (do not add files after first 
  /// call to this function).
  Int_t    getEvent(Int_t runNumber, Int_t eventNumber );

  /// Pointer to the chain itself
  TChain *chain(){ return mChain; }

  void Test();

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StGammaTreeReader.h,v 1.1 2008/03/07 14:21:16 jwebb Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
  }


 private:
 protected:

  Int_t index;
  Int_t mNumberOfFiles;
  Bool_t mIndexed;      /**< true if index has been built, false otherwise */
  void   buildIndex();
  std::map<Long64_t,Long64_t> mIndex;

  Long64_t key( Int_t run, Int_t event );

  TChain *mChain;       /**< real events chain */
  StGammaEvent *mEvent; /**< gamma event */
  TString mBranchName;  /**< name of the branch where StGammaEvent is stored */

  void    treeDetails();
  Bool_t  mFirst;

  ClassDef(StGammaTreeReader,1);


};

#endif
