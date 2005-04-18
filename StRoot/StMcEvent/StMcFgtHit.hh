/***************************************************************************
 *
 * $Id: StMcFgtHit.hh,v 2.1 2005/04/18 20:11:33 calderon Exp $
 * $Log: StMcFgtHit.hh,v $
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
class StThreeVectorF;
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
