/***************************************************************************
 * First version of StMcHpdHit
 *
 *
 **************************************************************************/
#ifndef StMcHpdHit_hh
#define StMcHpdHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_hpd_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcHpdHit : public StMcHit {
public:
    StMcHpdHit() : StMcHit() {}
    StMcHpdHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcHpdHit(g2t_hpd_hit_st*);
    ~StMcHpdHit();
    
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif
    
    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
    virtual void Print(Option_t *option="") const; // *MENU*     
private:
    
#ifdef POOL
    static StMemoryPool mPool; 
#endif
    ClassDef(StMcHpdHit,1)
};

ostream&  operator<<(ostream& os, const StMcHpdHit&);


#endif
