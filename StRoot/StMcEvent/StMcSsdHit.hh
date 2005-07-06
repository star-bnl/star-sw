/***************************************************************************
 *
 * $Id: StMcSsdHit.hh,v 2.3 2005/07/06 20:05:28 calderon Exp $
 * $Log: StMcSsdHit.hh,v $
 * Revision 2.3  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and SSd classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcSsdHit_hh
#define StMcSsdHit_hh

#include "StMcHit.hh"
#ifdef POOL
#include "StMemoryPool.hh"
#endif

class StMcTrack;
class g2t_ssd_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcSsdHit : public StMcHit {
public:
    StMcSsdHit();
  StMcSsdHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcSsdHit(g2t_ssd_hit_st*);
    ~StMcSsdHit();

#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif
    unsigned long layer() const;  // 1 SSD layer
    unsigned long ladder() const; // 1-20 ladders
    
private:
#ifdef POOL
    static StMemoryPool mPool; //!
#endif
    ClassDef(StMcSsdHit,1)
};

ostream&  operator<<(ostream& os, const StMcSsdHit&);


#endif
