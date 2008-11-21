#ifndef StPi0DataMaker_StPi0DataMaker_H
#define StPi0DataMaker_StPi0DataMaker_H

class TH1F;
class TDataSet;

#include <StMaker.h>
class StEvent;
class StMcEvent;
class StMcTrack;
class StEmcRawHit;
class StEmcCluster;
class StEmcPoint;
class StEmcGeom;
class StEmcPosition;
class StEmcDecoder;
class StBemcTables;

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
class TMyPointTreeData;

#ifndef __CINT__
// Hide this from CINT - it does not like templates
#include "EventMixer.h"
#endif
#include "StPi0DataMakerUtil.h"

class StPi0DataMaker : public StMaker {
public:
  typedef StMaker inherited;
#ifdef __CINT__
  // Use dummy classes for CINT instead of the real templates, mixer will not be available in command line
  typedef class _dummyMixer {public: typedef const Int_t &const_list_reference; Int_t dummy;} TEventMixer;
  typedef class _dummyMixerParameters {public: Int_t dummy;} TEventMixerParameters;
#else
  // Hide this from CINT - it does not like templates
  typedef TMyPointTreeData point_data;
  typedef TMixer<point_data> TEventMixer;
  typedef TMixerParameters<TEventMixer> TEventMixerParameters;
#endif

  StPi0DataMaker(const Char_t *name = "StPi0DataMaker");
  virtual ~StPi0DataMaker();
  virtual Int_t Init();
  virtual void  Clear(Option_t *option = "");
  virtual Int_t Make();
  virtual Int_t Finish();
  
  TMyDataAnalysisSettings settings;

  ClassDef(StPi0DataMaker, 1)

protected:
  void combineCandidates(TEventMixer::const_list_reference points1, TEventMixer::const_list_reference points2, TDataSet *array);

  StEmcGeom *mEmcGeom;
  StEmcGeom *mPsdGeom;
  StEmcGeom *mSmdeGeom;
  StEmcGeom *mSmdpGeom;
  StEmcPosition *mEmcPosition;
  StEmcDecoder *mEmcDecoder;
  StBemcTables *mBemcTables;

  TEventMixerParameters *mEventMixer;

  TDataSet *mMCGammaTreeDataArray;
  TDataSet *mMCPionTreeDataArray;
  TDataSet *mMCEtaTreeDataArray;
  TDataSet *mMCNbarTreeDataArray;
  TDataSet *mCandidateTreeDataArray;
  TDataSet *mCandidateTreeDataMixArray;
  TDataSet *mCandidateTreeDataSubmixArray;
  TDataSet *mEventTreeDataArray;
  TDataSet *mHitTreeDataArray;
  TDataSet *mClusterTreeDataArray;
  TDataSet *mPointTreeDataArray;
  TDataSet *mSMDThresholdTreeDataArray;

  TH1F* mEventSummary;
  TH1F* mTriggerSummary;

  void postData();
};

#endif
