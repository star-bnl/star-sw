#ifndef ___STAR_StEpdHitMaker
#define ___STAR_StEpdHitMaker

/*!
  StEpdHitMaker:

  \author Mike Lisa
  \date 5 Jan 2018

  \description

  This takes the StEvent and asks for the StEpdHitCollection.
  If it is not there, it creates one and fills it from the
  StTriggerData object and info from the StEpdDbMaker (database)

  \author David Kapukchyan
  \date 10 July 2024
  It can also process trigger data from Mudst data by calling #setReadMuDst(). Need to call StEventMaker to make it work right though since this maker will still fill into StEvent. This was done so StEvent will clean the collection from event to event.

*/


#include "StMaker.h"
#include <math.h>
class StEpdHit;
class StTriggerData;
class StEpdDbMaker;
class StEpdCollection;
class StEvent;

class StEpdHitMaker : public StMaker {
 public:
  /// default constructor
  //  StEpdHitMaker(const char *name="epdHit");
  StEpdHitMaker();
  virtual ~StEpdHitMaker(){};
  /// Init does nothing right now
  virtual int Init();
  /// Increments event counters
  /// Gets trigger, database, and EPD hit collection
  /// calls FillStEpdData method to make (if necessary) and fill the hit collection
  virtual int Make();
  /// Finish does nothing right now
  virtual int Finish();

  void setReadMuDst(bool value=true){ mReadMuDst=value; }
  
  /// Returns the collection of StEpdHits in the event
  StEpdCollection* GetEpdCollection();  // collection of StEpdHit objects

  /// Returns a pointer to the StTriggerData object
  const StTriggerData* GetTriggerData();

  /// Returns a pointer to the StEpdDbMaker
  StEpdDbMaker* GetEpdDbMaker();

  /// Returns a pointer to the StEvent object
  StEvent* GetStEvent(){return mStEvent;}

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}

 protected:

 private:


  int mEventCounter;              /// simple event counter
  int mTriggerEventCounter;       /// another event counter.  At the moment, it is redundant with mEventCounter
  StEpdCollection* mEpdCollection;
  const StTriggerData* mTriggerData;
  StEpdDbMaker* mEpdDbMaker;
  StEvent* mStEvent;

  bool mReadMuDst = false;

  //  static const int mNPREPOST=2;

  /// Internal function that decodes the QT data found in the StTriggerData object,
  ///   using pointers provided by the StEpdDbMaker (database).
  /// Also generates calibrated energy loss using calibrations in the database.
  /// This data is filled into the StEpdHitCollection object owned by StEvent.
  void FillStEpdData();


  ClassDef(StEpdHitMaker, 0)   //StAF chain virtual base class for Makers
};
#endif

