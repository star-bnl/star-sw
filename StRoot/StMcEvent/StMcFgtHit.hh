/***************************************************************************
 *
 * $Id: StMcFgtHit.hh,v 2.2 2005/07/06 20:05:28 calderon Exp $
 * $Log: StMcFgtHit.hh,v $
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
    StMcFgtHit();
    StMcFgtHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcFgtHit(g2t_fgt_hit_st*);
    ~StMcFgtHit();
    
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
private:

    static StMemoryPool mPool; //!
    ClassDef(StMcFgtHit,1)
};

ostream&  operator<<(ostream& os, const StMcFgtHit&);


#endif
