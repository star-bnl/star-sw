/***************************************************************************
 *
 * $Id: StMcIstHit.hh,v 2.9 2009/02/06 15:38:13 fisyak Exp $
 * $Log: StMcIstHit.hh,v $
 * Revision 2.9  2009/02/06 15:38:13  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.8  2006/10/23 21:13:46  calderon
 * Updates to layer(), wafer() and side() methods from Willie L.
 *
 * Revision 2.7  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.6  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.5  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.4  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.3  2005/05/12 22:38:30  potekhin
 * Willie: Added function wafer() to return wafer number (1-7,1-10,1-13 for layers 1,2,3)
 * and side() to return ladder side (1=inner,2=outer)
 *
 * Revision 2.2  2005/05/11 20:54:28  calderon
 * Added persistency: ClassImp, ClassDef and inheritance from StObject.
 *
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
class g2t_ist_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcIstHit : public StMcHit {
public:
    StMcIstHit() : StMcHit() {}
    StMcIstHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcIstHit(g2t_ist_hit_st*);
    ~StMcIstHit();
    
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif
    
    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
    // Willie: Added function wafer() to return wafer number (1-12)
    unsigned long wafer() const;
    virtual void Print(Option_t *option="") const; // *MENU* 
    
private:
    
#ifdef POOL
    static StMemoryPool mPool; 
#endif
    ClassDef(StMcIstHit,1)
};

ostream&  operator<<(ostream& os, const StMcIstHit&);


#endif
