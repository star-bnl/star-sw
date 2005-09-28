// $Id: StMcCalorimeterHit.hh,v 2.7 2005/09/28 21:30:14 fisyak Exp $
//
// $Log: StMcCalorimeterHit.hh,v $
// Revision 2.7  2005/09/28 21:30:14  fisyak
// Persistent StMcEvent
//
// Revision 2.6  2005/01/27 23:40:46  calderon
// Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
//
// Revision 2.5  2004/01/13 21:03:34  fisyak
// Replace iostream by Stiostream.h (for icc)
//
// Revision 2.4  2003/10/08 20:17:54  calderon
// -using <iostream>, std::cout, std::ostream.
// -changes in FTPC volume Id.
//   o Causes changes in decoding of plane().
//   o sector() is added.
//   o print volumeId and sector() in the operator<<.
//
// Revision 2.3  2003/09/02 17:58:41  perev
// gcc 3.2 updates + WarnOff
//
// Revision 2.2  2000/06/06 02:58:40  calderon
// Introduction of Calorimeter classes.  Modified several classes
// accordingly.
//
// Revision 2.1  2000/05/05 14:54:19  calderon
// Initial revision
//
//                                                                           
#ifndef StMcCalorimeterHit_hh
#define StMcCalorimeterHit_hh

#include "StObject.h"
#include "Stiostream.h"

//#include "StMemoryPool.hh"

class StMcTrack;

class StMcCalorimeterHit : public StObject {
public:
    StMcCalorimeterHit();
    StMcCalorimeterHit(int, int, int, float);
    StMcCalorimeterHit(int, int, int, float, StMcTrack*);
    virtual ~StMcCalorimeterHit();
    
    int operator==(const StMcCalorimeterHit&) const;
    int operator!=(const StMcCalorimeterHit&) const;
    void operator+=(const StMcCalorimeterHit&);

    bool sameCell(const StMcCalorimeterHit&) const;

  // "Get" Methods
    virtual int                          module() const;
    virtual int                             eta() const;
    virtual int                             sub() const;
    virtual float                            dE() const;
    virtual StMcTrack*              parentTrack() const;	

  // "Set" Methods

    virtual void setModule(int);
    virtual void setEta(int);
    virtual void setSub(int);
    virtual void setdE(float);
    virtual void setParentTrack(StMcTrack*);
    
  //  void* operator new(size_t)     { return mPool.alloc(); }
  //  void  operator delete(void* p) { mPool.free(p); }
    
protected:
    int                  mModule;
    int                  mEta;
    int                  mSub;
    float                mdE;
    StMcTrack*           mParentTrack;

  //private:
  //  static StMemoryPool mPool; 
    ClassDef(StMcCalorimeterHit,1)
};

ostream&  operator<<(ostream& os, const StMcCalorimeterHit&);

inline int  StMcCalorimeterHit::module() const {return mModule; }
inline int  StMcCalorimeterHit::eta() const {return mEta; }
inline int  StMcCalorimeterHit::sub() const {return mSub; }
inline float StMcCalorimeterHit::dE()  const {return mdE; }
inline StMcTrack* StMcCalorimeterHit::parentTrack() const {return mParentTrack; }

#endif

