/***************************************************************************
 *
 *  StMcHit.hh
 *
 **************************************************************************/
#ifndef StMcHit_hh
#define StMcHit_hh

#include <iostream.h>
#include "StThreeVector.hh"
#include "tables/g2t_hits.h"

class StMcTrack;

class StMcHit {
public:
    StMcHit();
    StMcHit(const StThreeVector<float>&,
	  float, float,
	  StMcTrack*);
    StMcHit(g2t_hits_st*);
    // StMcHit(const StSvtHit&);                  use default
    // const StMcHit & operator=(const StMcHit&);   use default
    virtual ~StMcHit();
    
    int operator==(const StMcHit&) const;
    int operator!=(const StMcHit&) const;
    

  // "Get" Methods

    virtual const StThreeVector<float>& position() const;
    virtual float                       dE() const;
    virtual float                       dS() const;
    virtual StMcTrack*                  parentTrack() const;	

  // "Set" Methods

    virtual void setPosition(const StThreeVector<float>&);
    virtual void setdE(float);
    virtual void setdS(float);
    virtual void setParentTrack(StMcTrack*);
    
protected:
    StThreeVector<float> mPosition;
    float                mdE;
    float                mdS;
    StMcTrack*           mParentTrack;
};

ostream&  operator<<(ostream& os, const StMcHit&);

inline const StThreeVector<float>& StMcHit::position() const { return mPosition; }

inline float StMcHit::dE() const { return mdE; }

inline float StMcHit::dS() const { return mdS; }

inline StMcTrack* StMcHit::parentTrack() const { return mParentTrack; }	

#endif

