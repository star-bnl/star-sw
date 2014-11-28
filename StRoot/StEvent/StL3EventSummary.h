/*!
 * \class StL3EventSummary
 * \author Christof Struck, July 2001
 */
/***************************************************************************
 *
 * $Id: StL3EventSummary.h,v 2.5 2002/02/22 22:56:49 jeromel Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Event Summary Information
 *
 ***************************************************************************
 *
 * $Log: StL3EventSummary.h,v $
 * Revision 2.5  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/11/28 18:52:57  struck
 * updated classdef version
 *
 * Revision 2.3  2001/11/14 23:29:35  struck
 * minor changes in 'unbiasedTrigger' function, trigger word added for debugging purposes
 *
 * Revision 2.2  2001/08/20 21:29:53  ullrich
 * Added method setCounters().
 *
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

    int                              numberOfProcessedEvents() const;
    int                              numberOfReconstructedEvents() const;
    unsigned int                     numberOfTracks() const;
    unsigned int                     numberOfAlgorithms() const;

    int                              unbiasedTriggerPreScale() const;

    bool                             unbiasedTrigger() const;
    bool                             zVertexTrigger() const;

    unsigned int                     l0TriggerWord() const;

    StPtrVecL3AlgorithmInfo&         algorithmsAcceptingEvent();
    const StPtrVecL3AlgorithmInfo&   algorithmsAcceptingEvent() const;

    StSPtrVecL3AlgorithmInfo&        algorithms();
    const StSPtrVecL3AlgorithmInfo&  algorithms() const;

    void                             addAlgorithm(StL3AlgorithmInfo*);
    void                             setNumberOfTracks(int);
    void                             setCounters(int, int);
    void                             setUnbiasedTrigger();
    void                             setUnbiasedTriggerPreScale(int);
    void                             setZVertexTrigger();
    void                             setL0TriggerWord(unsigned int);

private:
    Int_t    mNumberOfProcessedEvents;
    Int_t    mNumberReconstructedEvents;
    UInt_t   mNumberOfTracks;
    UInt_t   mNumberOfAlgorithms;
    Bool_t   mZVertexTrigger;
    Bool_t   mUnbiasedTrigger;
    UInt_t   mL0TriggerWord;
    Int_t    mUnbiasedPreScale;
    StPtrVecL3AlgorithmInfo   mL3AcceptAlgorithms;
    StSPtrVecL3AlgorithmInfo  mL3Algorithms;

    ClassDef(StL3EventSummary,2)
};


inline int
StL3EventSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

inline int
StL3EventSummary::numberOfReconstructedEvents() const { return mNumberReconstructedEvents; }

inline unsigned int
StL3EventSummary::numberOfTracks() const { return mNumberOfTracks; }

inline unsigned int
StL3EventSummary::numberOfAlgorithms() const { return mNumberOfAlgorithms; }

inline bool
StL3EventSummary::unbiasedTrigger() const { return mUnbiasedTrigger; }

inline bool
StL3EventSummary::zVertexTrigger() const { return mZVertexTrigger; }

inline unsigned int
StL3EventSummary::l0TriggerWord() const { return mL0TriggerWord; }

inline StPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithmsAcceptingEvent() { return mL3AcceptAlgorithms; }

inline const StPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithmsAcceptingEvent() const { return mL3AcceptAlgorithms; }

inline StSPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithms() { return mL3Algorithms; }

inline const StSPtrVecL3AlgorithmInfo&
StL3EventSummary::algorithms() const { return mL3Algorithms; }

inline int
StL3EventSummary::unbiasedTriggerPreScale() const { return mUnbiasedPreScale; }

inline void
StL3EventSummary::setCounters(int nProcessed, int nReconstructed) {
      mNumberOfProcessedEvents = nProcessed;
      mNumberReconstructedEvents = nReconstructed;
}

inline void
StL3EventSummary::setUnbiasedTrigger() { mUnbiasedTrigger = true; }

inline void
StL3EventSummary::setZVertexTrigger() { mZVertexTrigger = true; }

inline void
StL3EventSummary::setUnbiasedTriggerPreScale(int preScale) { mUnbiasedPreScale = preScale; }

inline void
StL3EventSummary::setL0TriggerWord(unsigned int triggerWord) { mL0TriggerWord = triggerWord; }

#endif
