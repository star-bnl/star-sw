/***************************************************************************
 *
 *  StMcHit.hh
 *
 **************************************************************************/
#ifndef StMcHit_hh
#define StMcHit_hh

#include <iostream.h>
#include "tables/g2t_hits.h"

class StMcTrack;
class StThreeVectorF;
class StMcHit {
public:
    StMcHit();
    StMcHit(const StThreeVectorF&,
	  float, float,
	  StMcTrack*);
    StMcHit(g2t_hits_st*);
    // StMcHit(const StSvtHit&);                  use default
    // const StMcHit & operator=(const StMcHit&);   use default
    virtual ~StMcHit();
    
    int operator==(const StMcHit&) const;
    int operator!=(const StMcHit&) const;
    

  // "Get" Methods

    virtual const StThreeVectorF& position() const;
    virtual float                       dE() const;
    virtual float                       dS() const;
    virtual StMcTrack*                  parentTrack() const;	

  // "Set" Methods

    virtual void setPosition(const StThreeVectorF&);
    virtual void setdE(float);
    virtual void setdS(float);
    virtual void setParentTrack(StMcTrack*);
    
protected:
    StThreeVectorF       mPosition;
    float                mdE;
    float                mdS;
    StMcTrack*           mParentTrack;
};

ostream&  operator<<(ostream& os, const StMcHit&);

inline const StThreeVectorF& StMcHit::position() const { return mPosition; }

inline float StMcHit::dE() const { return mdE; }

inline float StMcHit::dS() const { return mdS; }

inline StMcTrack* StMcHit::parentTrack() const { return mParentTrack; }	

#endif

