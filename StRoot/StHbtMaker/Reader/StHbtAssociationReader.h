#ifndef StHbtAssociationReader_hh
#define StHbtAssociationReader_hh


#include <ctime>
#include "StMaker.h"
#include "StHbtMaker/Infrastructure/StHbtCheckPdgIdList.h"
#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class StHbtAssociationReader : public StHbtEventReader, public StHbtCheckPdgIdList {
  
 private:
  // pointers to other mkers
  StMaker* mTheEventMaker;        //! this is the chain where the StEventReaderMaker is
  StMaker* mTheMcEventMaker;      //! this is the chain where the StMcEventReaderMaker is
  StMaker* mTheAssociationMaker;  //! this is the chain where the StAssociationEventReaderMaker is
  //  StMaker* mTheV0Maker;       //! this is the chain where the StStrangeMuDstMaker is

  // some monitor histograms
  StHbt1DHisto* mDiffCurrent;   //! momenta diff distribution
  StHbt1DHisto* mDiff;          //! momenta diff distribution
  StHbt1DHisto* mDiffMean;      //! mean of momenta diff distribution
  StHbt1DHisto* mDiffRMS;       //! sigma of momenta diff distribution

  long              mV0;        //! Number of v0s looked at to date
  int eventNumber;
  time_t timeStamp;
  bool mPerfectPID;
  
 protected:

 public:

  StHbtAssociationReader();
  ~StHbtAssociationReader();
  
  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();

  // sets and gets for the other makers
  void SetTheEventMaker(StMaker*);
  void SetTheMcEventMaker(StMaker*);    // NOTE! this is now obsolete, as we get the maker via "GetDataSet"
                                        // but I leave it in just to not break any macros - malisa 28sep2005
                                        // You can Set it if you want, but it just doesn't do anything
  void SetTheAssociationMaker(StMaker*);
  //void SetTheV0Maker(StMaker*);
  StMaker* TheEventMaker();
  StMaker* TheMcEventMaker();
  StMaker* TheAssociationMaker();
  //StMaker* TheV0Maker();

  bool PerfectPID();
  void SetPerfectPID(bool);

  ClassDef(StHbtAssociationReader, 0)
};
    
inline void StHbtAssociationReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline void StHbtAssociationReader::SetTheMcEventMaker(StMaker* mcMaker){mTheMcEventMaker=mcMaker;}
inline void StHbtAssociationReader::SetTheAssociationMaker(StMaker* associationMaker){mTheAssociationMaker=associationMaker;}
//inline void StHbtAssociationReader::SetTheV0Maker(StMaker* theV0Maker) { mTheV0Maker = theV0Maker; };
inline StMaker* StHbtAssociationReader::TheEventMaker(){return mTheEventMaker;}
inline StMaker* StHbtAssociationReader::TheMcEventMaker(){return mTheMcEventMaker;}
inline StMaker* StHbtAssociationReader::TheAssociationMaker(){return mTheAssociationMaker;}
//inline StMaker* StHbtAssociationReader::TheV0Maker(){return mTheV0Maker;}
inline bool StHbtAssociationReader::PerfectPID(){return mPerfectPID;}
inline void StHbtAssociationReader::SetPerfectPID(bool b) {mPerfectPID=b;}


#endif


