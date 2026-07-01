/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to serve as an abstract virtual base class so I have a common set of functions that can be called in #StFwdAnaDataMaker. All analysis modules (hence the 'Ana') should inherit from this class.

  DESCRIPTION
  Contains two abstract virtual methods #LoadHists() and #DoMake(). #LoadHists() is used to create histograms and add them to a #HistManager object. #DoMake() is where the logic of that subalgorithm should go

  LOG
  @[January 12, 2026] > First instance
  @[July 1, 2026] > Changed name from StMuFcsVirtualAna to #StFwdAnaVirtual

*/


#ifndef STFWDANA_STFWDANAVIRTUAL_HH
#define STFWDANA_STFWDANAVIRTUAL_HH

#include "TObject.h"
#include "TFile.h"

#include "StSpinPool/StFwdData/HistManager.h"
#include "StSpinPool/StFwdAna/StFwdAnaData.h"

class StFwdAnaVirtual : public TObject
{
public:
  StFwdAnaVirtual();
  virtual ~StFwdAnaVirtual();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* data) = 0;  ///< Function for creating and adding histograms to the hist manager object. Even though hist manager has its own internal file handler the argument is more of a flag to determine if hist manager will load histograms from the file passed in or create `new` ones. This has to happen this way because of ROOT handles file saving
  virtual Int_t DoMake(StFwdAnaData* data) = 0;  ///< Different "Ana" modules will do different things in Make and should be put in this virtual function. This becomes effectively the "Make" function

  ClassDef(StFwdAnaVirtual,0)

};

#endif

