/***************************************************************************
 *
 * $Id: StEmcHit.hh,v 1.4 1999/03/07 15:31:38 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHit.hh,v $
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

#include <iostream.h>

class StEmcHit {
public:
    StEmcHit();
    StEmcHit(int, float, float, float);
    // StEmcHit(const StEmcHit&);
    // const StEmcHit & operator=(const StEmcHit&);
    ~StEmcHit();

    float energy() const;
    int   id()     const;
    float phi()    const;
    float eta()    const;

    void setEnergy(float);
    void setPhi(float);
    void setEta(float);
    void setId(int);

protected:
    int   mId;
    float mPhi;
    float mEta;
    float mEnergy;
};

ostream& operator<< (ostream&, const StEmcHit&);

//
//    Inline member functions
//
inline StEmcHit::StEmcHit() : mId(0), mPhi(0), mEta(0), mEnergy(0) { /* noop */ }

inline StEmcHit::StEmcHit(int i, float E, float p, float e)
    : mId(i), mPhi(p), mEta(e), mEnergy(E) { /* noop */ }

inline StEmcHit::~StEmcHit() { /* noop */ }

inline float StEmcHit::energy() const { return mEnergy; }

inline int StEmcHit::id() const { return mId; }

inline float StEmcHit::phi() const { return mPhi; }

inline float StEmcHit::eta() const { return mEta; }

inline void StEmcHit::setEnergy(float val) { mEnergy = val; };

inline void StEmcHit::setId(int i) { mId = i; };

inline void StEmcHit::setPhi(float val) { mPhi = val; };

inline void StEmcHit::setEta(float val) { mEta = val; };

#endif
