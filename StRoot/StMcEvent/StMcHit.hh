/***************************************************************************
 *
 * $Id: StMcHit.hh,v 2.11 2011/10/11 01:16:40 perev Exp $
 * $Log: StMcHit.hh,v $
 * Revision 2.11  2011/10/11 01:16:40  perev
 * Comments++
 *
 * Revision 2.10  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.9  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.8  2004/01/13 21:02:51  fisyak
 * Add inheritance from StObject
 *
 * Revision 2.7  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.6  2003/09/02 17:58:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.5  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
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

#include "StObject.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"

class StMcTrack;
class g2t_hits_st;

class StMcHit : public StObject {
public:
  enum EMcHitBits {
    kMatched = BIT(23) // if hit has matched with reconstructed one
  };
    StMcHit();
    StMcHit(const StThreeVectorF& x,const StThreeVectorF& p,
		 float de, float ds, long key, long volId, StMcTrack* parent);
    StMcHit(g2t_hits_st*);
    // StMcHit(const StSvtHit&);                  use default
    // const StMcHit & operator=(const StMcHit&);   use default
    virtual ~StMcHit();
    
    int operator==(const StMcHit&) const;
    int operator!=(const StMcHit&) const;
    

  // "Get" Methods
    virtual const StThreeVectorF&      position() const { return mPosition;}
    virtual const StThreeVectorF& localMomentum() const { return mLocalMomentum;}
    virtual float                            dE() const { return mdE; }
    virtual float                            dS() const { return mdS; }
    virtual long                            key() const { return mKey; }
    virtual long                       volumeId() const { return mVolumeId; }
    virtual StMcTrack*              parentTrack() const	{ return mParentTrack; }	
  // "Set" Methods

    virtual void setPosition(const StThreeVectorF&);
    virtual void setLocalMomentum(const StThreeVectorF&);
    virtual void setdE(float);
    virtual void setdS(float);
    virtual void setKey(long);
    virtual void setVolumeId(long);
    virtual void setParentTrack(StMcTrack*);
    virtual void Print(Option_t *option="") const; // *MENU* 
    
protected:
    StThreeVectorF       mPosition;
    StThreeVectorF       mLocalMomentum;
    float                mdE;
    float                mdS;
    long                 mKey;
    long                 mVolumeId;
    StMcTrack*           mParentTrack;
  ClassDef(StMcHit,1)
};
ostream&  operator<<(ostream& os, const StMcHit&);
#endif

