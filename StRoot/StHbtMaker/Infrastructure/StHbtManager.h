/***************************************************************************
 *
 * $Id: StHbtManager.h,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The Manager is the top-level object that coordinates activities
 *   and performs event, particle, and pair loops, and checks the
 *   various Cuts of the Analyses in its AnalysisCollection
 *
 ***************************************************************************
 *
 * $Log: StHbtManager.h,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtManager_hh
#define StHbtManager_hh

#include <string>

#include "StHbtMaker/Infrastructure/StHbtAnalysisCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StChain/StMaker.h"

class StHbtManager{

private:
  StHbtAnalysisCollection* mAnalysisCollection;
  StHbtEventReader*        mEventReader;

public:
  StHbtManager();
  ~StHbtManager();

  // Gets and Sets...
  StHbtAnalysisCollection* AnalysisCollection();
  StHbtEventReader* EventReader();
  void SetEventReader(StHbtEventReader*);

  void AddAnalysis(StHbtAnalysis*);

  void Init();
  void ProcessEvent();
  void Finish();

  string Report();

  ClassDef(StHbtManager, 0)

};

inline StHbtAnalysisCollection* StHbtManager::AnalysisCollection(){return mAnalysisCollection;}
inline StHbtEventReader* StHbtManager::EventReader(){return mEventReader;}
inline void StHbtManager::SetEventReader(StHbtEventReader* reader){mEventReader = reader;}

inline void StHbtManager::AddAnalysis(StHbtAnalysis* anal){mAnalysisCollection->push_back(anal);}

#endif

