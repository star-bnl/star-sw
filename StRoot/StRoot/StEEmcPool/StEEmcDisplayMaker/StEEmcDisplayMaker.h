#ifndef __StEEmcDisplayMaker_h__
#define __StEEmcDisplayMaker_h__

#include "StMaker.h"
#include "StEEmcDisplay.h"

#include <vector>

class TTree;
class TFile;

class StEEmcA2EMaker;
class StEEmcGenericClusterMaker;
class StEEmcGenericPointMaker;
class StEEmcPi0Maker;

class StEEmcDisplayMaker : public StMaker
{

 public:
  StEEmcDisplayMaker( const Char_t *name="eeDisp" );
  ~StEEmcDisplayMaker(){ /* nada */ }

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  TTree *tree(){ return mTree; } /**< returns a pointer to the TTree */
  TFile *file(){ return mFile; } /**< returns a pointer to the output file */

  TFile *setFile( TFile *file ); /**< sets the output file to be an externally supplied TFile.  User responsible for ->Write(). */
  TFile *setFile( const Char_t *name, const Option_t *opt="RECREATE"); /**< creates a new TFile.  ->Write() when maker Finish(). */

  void adc2energy( StEEmcA2EMaker *a ){ mEEanalysis=a; } /**< sets pointer to adc-to-energy maker */
  void clusters( StEEmcGenericClusterMaker *c ){ mEEclusters = c; } /**< sets pointer to cluster maker */
  void points( StEEmcGenericPointMaker *p ){ mEEpoints = p; } /**< sets pointer to point maker */
  void pi0s( StEEmcPi0Maker *p ){ mEEpairs = p; } /**< sets pointer to pi0 maker */

  void addTrigger( Int_t t ){ mTriggerList.push_back(t); } /**< adds trigger to the list of triggers to save */
  void setCheckTrigger( Bool_t c=true ){ mCheckTrigger=c; } /**< set true to require a trigger in the list specified with addTrigger */
 
 private:
 protected:

  TFile *mFile;//!
  Bool_t mFileLocal;
  TTree *mTree;//!
  
  StEEmcDisplay *mDisplay; /**< data structure for event display */

  StEEmcA2EMaker            *mEEanalysis; /**< adc to energy maker */
  StEEmcGenericClusterMaker *mEEclusters; /**< cluster maker */
  StEEmcGenericPointMaker   *mEEpoints;   /**< point maker */
  StEEmcPi0Maker            *mEEpairs;    /**< pi0 maker */

  std::vector<Int_t> mTriggerList; /**< list of trigger ids */
  Int_t checkTrigger(); /**< determines if trigger appears in trigger list */
  const Char_t *triggerList(); /**< returns space sep. list of triggers from trigger list */
  Bool_t mCheckTrigger;

  ClassDef(StEEmcDisplayMaker,1);

};

#endif
