#ifndef StPicoD0Event__h
#define StPicoD0Event__h

/* **************************************************
 *  A specialized class for storing eventwise D0
 *  candidates. 
 *
 *  Authors:  Xin Dong        (xdong@lbl.gov)
 *            **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include <cstddef>

class StPicoEvent;

#include "TObject.h"
#include "TClonesArray.h"
#include "StThreeVectorF.hh"

class StKaonPion;

class StPicoD0Event : public TObject
{
public:
   StPicoD0Event();
   ~StPicoD0Event(){ clear("C");}
   void    clear(char const *option = "");
   void    addPicoEvent(StPicoEvent const & picoEvent, StThreeVectorF const* kfVertex = NULL, StThreeVectorF const* pVtx = NULL);
   void    addKaonPion(StKaonPion const*);
   void    nKaons(int);
   void    nPions(int);

   Int_t   runId()   const;
   Int_t   eventId() const;
   TClonesArray const * kaonPionArray()   const;
   int     nKaonPion()  const;
   int     nKaons() const;
   int     nPions() const;
   StThreeVectorF const& pVtx() const;
   StThreeVectorF const& kfVertex() const;

private:
   // some variables below are kept in ROOT types to match the same ones in StPicoEvent
   Int_t   mRunId;           // run number
   Int_t   mEventId;         // event number
   StThreeVectorF mpVtx;
   StThreeVectorF mKfVertex;
   int   mNKaonPion;       // number of stored pairs
   int   mNKaons;
   int   mNPions;

   TClonesArray*        mKaonPionArray;
   static TClonesArray* fgKaonPionArray;

   ClassDef(StPicoD0Event, 1)
};

inline void StPicoD0Event::nKaons(int n) { mNKaons = n; }
inline void StPicoD0Event::nPions(int n) { mNPions = n; }

inline TClonesArray const * StPicoD0Event::kaonPionArray()   const { return mKaonPionArray;}
inline int   StPicoD0Event::nKaonPion()  const { return mNKaonPion;}
inline int   StPicoD0Event::nKaons()  const { return mNKaons;}
inline int   StPicoD0Event::nPions()  const { return mNPions;}
inline Int_t StPicoD0Event::runId()   const { return mRunId; }
inline Int_t StPicoD0Event::eventId() const { return mEventId; }
inline StThreeVectorF const& StPicoD0Event::kfVertex() const { return mKfVertex; }
inline StThreeVectorF const& StPicoD0Event::pVtx() const { return mpVtx; }
#endif
