#ifndef StPicoNpeEvent__h
#define StPicoNpeEvent__h

/* **************************************************
 *  A specialized class for storing eventwise Npe
 *  candidates.
 *
 *  Authors:  **Kunsu OH        (kunsuoh@gmail.com)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 * **************************************************
 */

class StPicoEvent;

#include "TObject.h"
#include "TClonesArray.h"

#include "StElectronPair.h"

class StPicoNpeEvent : public TObject
{
public:
    StPicoNpeEvent();
    ~StPicoNpeEvent(){ clear("C");}
    void    clear(char const *option = "");
    void    addPicoEvent(StPicoEvent const & picoEvent);
    void    addElectronPair(StElectronPair const*);
    void    nElectrons(int);
    void    nPartners(int);
    
    Int_t   runId()   const;
    Int_t   eventId() const;
    TClonesArray const * electronPairArray()   const;
    int     nElectronPair()  const;
    int     nElectrons() const;
    int     nPartners() const;
    
private:
    // some variables below are kept in ROOT types to match the same ones in StPicoEvent
    Int_t   mRunId;           // run number
    Int_t   mEventId;         // event number
    int   mNElectronPair;       // number of stored pairs
    int   mNElectrons;
    int   mNPartners;
    
    TClonesArray*        mElectronPairArray;
    static TClonesArray* fgElectronPairArray;
    
    ClassDef(StPicoNpeEvent, 2)
};

inline void StPicoNpeEvent::nElectrons(int n) { mNElectrons = n; }
inline void StPicoNpeEvent::nPartners(int n) { mNPartners = n; }

inline TClonesArray const * StPicoNpeEvent::electronPairArray()   const { return mElectronPairArray;}
inline int   StPicoNpeEvent::nElectronPair()  const { return mNElectronPair;}
inline int   StPicoNpeEvent::nElectrons()  const { return mNElectrons;}
inline int   StPicoNpeEvent::nPartners()  const { return mNPartners;}
inline Int_t StPicoNpeEvent::runId()   const { return mRunId; }
inline Int_t StPicoNpeEvent::eventId() const { return mEventId; }
#endif
