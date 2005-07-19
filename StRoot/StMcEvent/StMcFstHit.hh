/***************************************************************************
 *
 * $Id: StMcFstHit.hh,v 2.4 2005/07/19 20:07:34 calderon Exp $
 * $Log: StMcFstHit.hh,v $
 * Revision 2.4  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.3  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.2  2005/05/12 22:48:09  potekhin
 * Layer, module and plane accessors from Mirko
 *
 * Revision 2.1  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 *
 **************************************************************************/
#ifndef StMcFstHit_hh
#define StMcFstHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"
#include "StThreeVector.hh"

class StMcTrack;
class g2t_fst_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcFstHit : public StMcHit {
public:
    StMcFstHit() : StMcHit() {}
    StMcFstHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcFstHit(g2t_fst_hit_st*);
    ~StMcFstHit();
    
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif
    
    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
    unsigned long plane() const; //
    unsigned long module() const;  //  
    
private:
    
#ifdef POOL
    static StMemoryPool mPool; //!
#endif
    ClassDef(StMcFstHit,1)
};

ostream&  operator<<(ostream& os, const StMcFstHit&);


#endif
