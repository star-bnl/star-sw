/***************************************************************************
 *
 * $Id: StMcFgtHit.hh,v 2.6 2009/10/13 19:14:27 perev Exp $
 * $Log: StMcFgtHit.hh,v $
 * Revision 2.6  2009/10/13 19:14:27  perev
 * Wei-Ming update
 *
 * Revision 2.5  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.4  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.2  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#ifndef StMcFgtHit_hh
#define StMcFgtHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_fgt_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcFgtHit : public StMcHit {
public:
    StMcFgtHit() : StMcHit() {}
    StMcFgtHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcFgtHit(g2t_fgt_hit_st*);
    ~StMcFgtHit();
    
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif

    unsigned long layer() const; // 
    unsigned long quad() const; // 
  virtual void Print(Option_t *option="") const; // *MENU* 
    
private:

#ifdef POOL
    static StMemoryPool mPool; 
#endif
    ClassDef(StMcFgtHit,1)
};

ostream&  operator<<(ostream& os, const StMcFgtHit&);


#endif
