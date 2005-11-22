/***************************************************************************
 *
 * $Id: StMcPixelHit.hh,v 2.6 2005/11/22 21:44:52 fisyak Exp $
 * $Log: StMcPixelHit.hh,v $
 * Revision 2.6  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.5  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.4  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.3  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#ifndef StMcPixelHit_hh
#define StMcPixelHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_pix_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcPixelHit : public StMcHit {
public:
    StMcPixelHit();
  StMcPixelHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcPixelHit(g2t_pix_hit_st*);
    ~StMcPixelHit();
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif

    unsigned long layer() const; // 1-2
    unsigned long ladder() const; // 1-6, 1-18
  virtual void Print(Option_t *option="") const; // *MENU* 
    
private:

#ifdef POOL
    static StMemoryPool mPool; 
#endif
    ClassDef(StMcPixelHit,1)
};

ostream&  operator<<(ostream& os, const StMcPixelHit&);


#endif
