/***************************************************************************
 *
 * $Id: StEmcHit.h,v 1.5 1999/04/30 13:16:27 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHit.h,v $
 * Revision 1.5  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/03/07 15:31:38  wenaus
 * Order constructor inits to remove g+ warnings
 *
 * Revision 1.3  1999/02/23 15:25:55  ullrich
 * Complete Revision
 *
 * Revision 1.2  1999/01/15 22:53:36  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StEmcHit_hh
#define StEmcHit_hh
#include "StObject.h"
#include "StArray.h"
#include <iostream.h>

class StEmcHit : public StObject {
public:
    StEmcHit();
    StEmcHit(Int_t, Float_t, Float_t, Float_t);
    // StEmcHit(const StEmcHit&);
    // const StEmcHit & operator=(const StEmcHit&);
    ~StEmcHit();

    Float_t energy() const;
    Int_t   id()     const;
    Float_t phi()    const;
    Float_t eta()    const;

    void setEnergy(Float_t);
    void setEta(Float_t);
    void setId(Int_t);
    virtual void                   Print(Option_t *opt=""); // *MENU*

protected:
    Int_t   mId;
    Float_t mPhi;
    Float_t mEta;
    Float_t mEnergy;
  ClassDef(StEmcHit,1)  //StEmcHit structure
};
StCollectionDef(EmcHit)

ostream& operator<< (ostream&, const StEmcHit&);

//
//    Inline member functions
//
inline StEmcHit::StEmcHit() : mId(0), mPhi(0), mEta(0), mEnergy(0) { /* noop */ }

inline StEmcHit::StEmcHit(Int_t i, Float_t E, Float_t p, Float_t e)
    : mId(i), mPhi(p), mEta(e), mEnergy(E) { /* noop */ }

inline StEmcHit::~StEmcHit() { /* noop */ }

inline Float_t StEmcHit::energy() const { return mEnergy; }

inline Int_t StEmcHit::id() const { return mId; }

inline Float_t StEmcHit::phi() const { return mPhi; }

inline Float_t StEmcHit::eta() const { return mEta; }

inline void StEmcHit::setEnergy(Float_t val) { mEnergy = val; };

inline void StEmcHit::setId(Int_t i) { mId = i; };

inline void StEmcHit::setPhi(Float_t val) { mPhi = val; };

inline void StEmcHit::setEta(Float_t val) { mEta = val; };

#endif
