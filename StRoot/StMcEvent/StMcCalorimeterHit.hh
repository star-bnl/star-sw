// $Id: StMcCalorimeterHit.hh,v 2.1 2000/05/05 14:54:19 calderon Exp $
//
// $Log: StMcCalorimeterHit.hh,v $
// Revision 2.1  2000/05/05 14:54:19  calderon
// Initial revision
//
//                                                                           
#ifndef StMcCalorimeterHit_hh
#define StMcCalorimeterHit_hh

#include <iostream.h>

class StMcTrack;

class StMcCalorimeterHit {
public:
    StMcCalorimeterHit();
    StMcCalorimeterHit(int, int, int, float, StMcTrack*);
    virtual ~StMcCalorimeterHit();
    
    int operator==(const StMcCalorimeterHit&) const;
    int operator!=(const StMcCalorimeterHit&) const;
    void operator+=(const StMcCalorimeterHit&);
    void operator+=(const float&);
    

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
    
protected:
    int                  mModule;
    int                  mEta;
    int                  mSub;
    float                mdE;
    StMcTrack*           mParentTrack;
};

ostream&  operator<<(ostream& os, const StMcCalorimeterHit&);

inline int  StMcCalorimeterHit::module() const {return mModule; }
inline int  StMcCalorimeterHit::eta() const {return mEta; }
inline int  StMcCalorimeterHit::sub() const {return mSub; }
inline float StMcCalorimeterHit::dE()  const {return mdE; }
inline StMcTrack* StMcCalorimeterHit::parentTrack() const {return mParentTrack; }

#endif

