/***************************************************************************
 *
 * $Id: StMcIstHit.hh,v 2.1 2004/09/14 05:00:29 calderon Exp $
 * $Log: StMcIstHit.hh,v $
 * Revision 2.1  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcIstHit_hh
#define StMcIstHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class StThreeVectorF;
class g2t_ist_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcIstHit : public StMcHit {
public:
    StMcIstHit();
  StMcIstHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcIstHit(g2t_ist_hit_st*);
    ~StMcIstHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
private:

    static StMemoryPool mPool; //!
};

ostream&  operator<<(ostream& os, const StMcIstHit&);


#endif
