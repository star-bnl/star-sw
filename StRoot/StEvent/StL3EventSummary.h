/***************************************************************************
 *
 * $Id: StL3EventSummary.h,v 2.1 2001/08/02 01:26:31 ullrich Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Event Summary Information
 *
 ***************************************************************************
 *
 * $Log: StL3EventSummary.h,v $
 * Revision 2.1  2001/08/02 01:26:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StL3EventSummary_hh
#define StL3EventSummary_hh

#include "StObject.h"
#include "StContainers.h"
#include "StDaqLib/L3/L3_Banks.hh"

class StL3AlgorithmInfo;

class StL3EventSummary : public StObject
{
public:
    StL3EventSummary();
    StL3EventSummary(Bank_L3_SUMD *l3sumd);
    // StL3EventSummary(const StL3EventSummary&);            use default
    // StL3EventSummary& operator=(const StL3EventSummray&); use default
    ~StL3EventSummary() {}

    unsigned int                     numberOfProcessedEvents() const;
    unsigned int                     numberOfReconstructedEvents() const;
    unsigned int                     numberOfTracks() const;
    unsigned int                     numberOfAlgorithms() const;

    bool                             unbiasedTrigger() const;
    bool                             zVertexTrigger() const;

    StPtrVecL3AlgorithmInfo&         algorithmsAcceptingEvent();
    const StPtrVecL3AlgorithmInfo&   algorithmsAcceptingEvent() const;

    StSPtrVecL3AlgorithmInfo&        algorithms();
    const StSPtrVecL3AlgorithmInfo&  algorithms() const;

    void                             addAlgorithm(StL3AlgorithmInfo*);
    void                             setNumberOfTracks(int);

private:
    UInt_t   mNumberOfProcessedEvents;
    UInt_t   mNumberReconstructedEvents;
    UInt_t   mNumberOfTracks;
    UInt_t   mNumberOfAlgorithms;
    Bool_t   mZVertexTrigger;
    Bool_t   mUnbiasedTrigger;
    StPtrVecL3AlgorithmInfo   mL3AcceptAlgorithms;
    StSPtrVecL3AlgorithmInfo  mL3Algorithms;

    ClassDef(StL3EventSummary,1)
};


inline unsigned int
StL3EventSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

inline unsigned int
StL3EventSummary::numberOfReconstructedEvents() const { return mNumberReconstructedEvents; }

inline unsigned int
StL3EventSummary::numberOfTracks() const { return mNumberOfTracks; }

inline unsigned int
StL3EventSummary::numberOfAlgorithms() const { return mNumberOfAlgorithms; }

inline bool
StL3EventSummary::unbiasedTrigger() const { return mUnbiasedTrigger; }

inline bool
StL3EventSummary::zVertexTrigger() const { return mZVertexTrigger; }

inline StPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithmsAcceptingEvent() { return mL3AcceptAlgorithms; }

inline const StPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithmsAcceptingEvent() const { return mL3AcceptAlgorithms; }

inline StSPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithms() { return mL3Algorithms; }

inline const StSPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithms() const { return mL3Algorithms; }

#endif
