#ifndef StPicoHFEvent__h
#define StPicoHFEvent__h

/* **************************************************
 *  Generic class storing event-wise information in HF analysis
 *  constructor takes option mode:
 *   - StPicoHFEvent::kTwoParticleDecay       ->  two particle decay at secondary vertex (A -> B + C)
 *   - StPicoHFEvent::kThreeParticleDecay     ->  three particle decay at secondary vertex (A -> B + C + d)
 *   - StPicoHFEvent::kTwoAndTwoParticleDecay ->  two particle decay at secondary vertex (A -> B + C)
 *                                                and two particle decay at tertiary vertex (C -> D + E)
 *
 *  Initial Authors:
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *          **Jochen Thaeder  (jmthader@lbl.gov)
 *
 *  ** Code Maintainer 
 *
 * **************************************************
 */

#include "TObject.h"
#include "TClonesArray.h"

class StPicoEvent;

class StHFPair;
class StHFTriplet;
class StHFQuadruplet;

class StPicoHFEvent : public TObject
{
public:
   StPicoHFEvent();
   StPicoHFEvent(unsigned int mode);
   ~StPicoHFEvent(){ clear("C");}
   void  clear(char const *option = "");
   void  addPicoEvent(StPicoEvent const & picoEvent);

   // -- add pair or triplet to the event
   void  addHFSecondaryVertexPair(StHFPair const*);
   void  addHFSecondaryVertexTriplet(StHFTriplet const*);
   void  addHFTertiaryVertexPair(StHFPair const*);
   void  addHFSecondaryVertexQuadruplet(StHFQuadruplet const*);

   // -- get array with particles from secondary and tertiary vertex
   TClonesArray const * aHFSecondaryVertices() const;
   unsigned int         nHFSecondaryVertices() const;
   TClonesArray const * aHFTertiaryVertices()  const;
   unsigned int         nHFTertiaryVertices()  const;

   // -- get variables from StPicoEvent
   Int_t runId()   const;
   Int_t eventId() const;

   // -- different event/decay modes
   enum eHFEventMode {kTwoParticleDecay, kThreeParticleDecay, kTwoAndTwoParticleDecay, kFourParticleDecay};

private:

   // -- some variables below are kept in ROOT types to match the same ones in StPicoEvent
   Int_t                mRunId;                       // run number
   Int_t                mEventId;                     // event number

   unsigned int         mNHFSecondaryVertices;        // number of stored secondary vertex candidates
   unsigned int         mNHFTertiaryVertices;         // number of stored tertiary vertex candidates

   TClonesArray*        mHFSecondaryVerticesArray;    // secondary vertex candidates
   static TClonesArray* fgHFSecondaryVerticesArray;

   TClonesArray*        mHFTertiaryVerticesArray;     // tertiary vertex candidates
   static TClonesArray* fgHFTertiaryVerticesArray;

   ClassDef(StPicoHFEvent, 1)
};

inline TClonesArray const * StPicoHFEvent::aHFSecondaryVertices() const { return mHFSecondaryVerticesArray;}
inline unsigned int         StPicoHFEvent::nHFSecondaryVertices() const { return mNHFSecondaryVertices; }

inline TClonesArray const * StPicoHFEvent::aHFTertiaryVertices()  const { return mHFTertiaryVerticesArray;}
inline unsigned int         StPicoHFEvent::nHFTertiaryVertices()  const { return mNHFTertiaryVertices; }

inline Int_t StPicoHFEvent::runId()        const { return mRunId; }
inline Int_t StPicoHFEvent::eventId()      const { return mEventId; }
#endif
