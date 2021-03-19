/*!
  \class StEbyET0
  
  StEbyET0 returns the event T0 time with respect to the clock
  mode = 0 : run normally
         1 : run and produce calibration ntuple

*/

#ifndef STAR_StEbyET0
#define STAR_StEbyET0

class StEvent;
class TFile;
class TNtupleD;

enum trigDetType {kVPD = 0,
                  kEPD,
                  kBBC,
                  kZDC,
                  kVPDeast,
                  kEPDeast, // for FXT
                  kBBCeast,
                  kZDCeast,
                  kVPDwest,
                  kEPDwest,
                  kBBCwest,
                  kZDCwest
                 };

class StEbyET0 {
 
 public: 
  StEbyET0();
  virtual           ~StEbyET0();
  virtual double    getT0(StEvent* event, int mode=0) {return getTime(event,mode);}
  virtual double    getTime(StEvent* event, int mode=0);
  virtual void      getTpcInfo(StEvent* event, double* info); // info must have 12 elements
  virtual void      getTriggerInfo(StEvent* event, trigDetType trigDet, double* info); // info must have 2 elements
  virtual void      finishCalib();

  static StEbyET0*  Instance() {
     if (!mInstance) mInstance = new StEbyET0();
     return mInstance;
  }

 private:
  virtual void      fillTree(StEvent* event);
  static StEbyET0* mInstance;
  int mRunId;
  int mEventId;
  float mTime;
  TNtupleD* mTree;

  ClassDef(StEbyET0,0)

};
    
#endif

//_____________________________________________________________________________
// $Id: StEbyET0.h,v 1.1 2021/03/19 01:44:47 genevb Exp $
// $Log: StEbyET0.h,v $
// Revision 1.1  2021/03/19 01:44:47  genevb
// Introduce Event-by-Event T0 corrections
//
//
//
