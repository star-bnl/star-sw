/***************************************************************************
 *
 * $Id: StMcHit.hh,v 2.4 2000/05/05 15:25:43 calderon Exp $
 * $Log: StMcHit.hh,v $
 * Revision 2.4  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.3  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcHit_hh
#define StMcHit_hh

#include <iostream.h>
#include "StThreeVectorF.hh"

class StMcTrack;
class g2t_hits_st;

class StMcHit {
public:
    StMcHit();
    StMcHit(const StThreeVectorF&,const StThreeVectorF&,
	  float, float,
	  StMcTrack*);
    StMcHit(g2t_hits_st*);
    // StMcHit(const StSvtHit&);                  use default
    // const StMcHit & operator=(const StMcHit&);   use default
    virtual ~StMcHit();
    
    int operator==(const StMcHit&) const;
    int operator!=(const StMcHit&) const;
    

  // "Get" Methods
    virtual const StThreeVectorF&      position() const;
    virtual const StThreeVectorF& localMomentum() const;
    virtual float                            dE() const;
    virtual float                            dS() const;
    virtual StMcTrack*              parentTrack() const;	

  // "Set" Methods

    virtual void setPosition(const StThreeVectorF&);
    virtual void setLocalMomentum(const StThreeVectorF&);
    virtual void setdE(float);
    virtual void setdS(float);
    virtual void setParentTrack(StMcTrack*);
    
protected:
    StThreeVectorF       mPosition;
    StThreeVectorF       mLocalMomentum;
    float                mdE;
    float                mdS;
    StMcTrack*           mParentTrack;
};

ostream&  operator<<(ostream& os, const StMcHit&);

inline const StThreeVectorF& StMcHit::position() const { return mPosition;}

inline const StThreeVectorF& StMcHit::localMomentum() const { return mLocalMomentum;}

inline float StMcHit::dE() const { return mdE; }

inline float StMcHit::dS() const { return mdS; }

inline StMcTrack* StMcHit::parentTrack() const { return mParentTrack; }	

#endif

