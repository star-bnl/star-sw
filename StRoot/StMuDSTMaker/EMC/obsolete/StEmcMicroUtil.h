/*!\class StEmcMicroUtil
\author Alexandre A. P. Suaide

This class has many utilities to convert StEvent format into
StEmcMicroEvent format.
*/
#ifndef StEmcMicroUtil_h
#define StEmcMicroUtil_h
#include "TObject.h"

#define MAXV0TRACKS 1000

class StEvent;
class StTrack;
class StEmcMicroEvent;
class StEmcMicroTrack;
class StEmcGeom;
class StEmcFilter;

class StEmcMicroUtil : public TObject
{
  protected:
    StEvent          *mStEvent;
    StEmcMicroEvent  *mMicroEvent;
    StEmcGeom        *mGeo[4];
    StEmcFilter      *mPFilter;
    StEmcFilter      *mGFilter;
    StTrack          *mV0Tracks[MAXV0TRACKS];
		Int_t            mNV0Tracks;

    Bool_t           mDoSavePrimaries;
    Bool_t           mDoSaveGlobals;
    Bool_t           mDoSaveEmc;
    Bool_t           mDoSaveFpd;
    Bool_t           mDoSaveV0;
		
		void             clearGarbage();
    
    
  public:
                     StEmcMicroUtil();
                     ~StEmcMicroUtil();
  
    void             processStEvent();                          ///< Process StEvent and convert it on StEmcMicroEvent
    void             processMicroEvent();                       ///< Process StEmcMicroEvent and convert it on StEvent
    
    void             processStEventInfo();                      ///< Process StEvent general info
    void             processStEventTracks();                    ///< Process StEvent tracks
    void             processStEventV0();                        ///< Process StEvent V0
    void             processStEventEMC();                       ///< Process StEvent EMC collection
    void             processStEventFPD();                       ///< Process StEvent FPD collection
    
    Float_t          calcDcaSigned(StTrack*);                   ///< Calcs DCA signed
    
    void             processMicroEventInfo();                   ///< Process StEmcMicroEvent general info
    void             processMicroEventTracks();                 ///< Process StEmcMicroEvent tracks
    void             processMicroEventV0();                     ///< Process StEmcMicroEvent V0
    void             processMicroEventEMC();                    ///< Process StEmcMicroEvent EMC collection
    
    void             createTrack(StTrack*,StEmcMicroTrack*);    ///< Creates StEmcMicroTrack from StTrack
    void             createTrack(StEmcMicroTrack*,StTrack*);    ///< Creates StTrack from StEmcMicroTrack
                  
    StEmcMicroEvent* getMicroEvent(StEvent*);                   ///< Returns StEmcMicroEvent
    StEvent*         getStEvent(StEmcMicroEvent*);              ///< Returns StEvent
    
    void             setPrimaryFilter(StEmcFilter *f) { mPFilter = f; } 
    void             setGlobalFilter(StEmcFilter *f)  { mGFilter = f; } 
    
    void             setSavePrimaries(Bool_t a) { mDoSavePrimaries = a; }      ///< Save or don't primary tracks. Default is kTRUE. Track selection is done by PrimaryFilter
    void             setSaveGlobals(Bool_t a)   { mDoSaveGlobals = a; }        ///< Save or don't global tracks. Default is kTRUE. Track selection is done by GlobalFilter
    void             setSaveEmc(Bool_t a)       { mDoSaveEmc = a; }            ///< Save or don't EMC data. Default is kTRUE.
    void             setSaveFpd(Bool_t a)       { mDoSaveFpd = a; }            ///< Save or don't FPD data. Default is kFALSE.
    void             setSaveV0(Bool_t a)        { mDoSaveV0 = a; }            ///< Save or don't V0 data. Default is kFALSE.
   
            
            
  ClassDef(StEmcMicroUtil,1)
};

#endif
