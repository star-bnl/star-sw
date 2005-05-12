/***************************************************************************
 *
 * $Id: StMcFstHit.hh,v 2.2 2005/05/12 22:48:09 potekhin Exp $
 * $Log: StMcFstHit.hh,v $
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

class StMcTrack;
class StThreeVectorF;
class g2t_fst_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcFstHit : public StMcHit {
public:
  StMcFstHit();
  StMcFstHit(const StThreeVectorF&,const StThreeVectorF&,
	     const float, const float, const long, const long, StMcTrack*);
  StMcFstHit(g2t_fst_hit_st*);
  ~StMcFstHit();

  void* operator new(size_t)     { return mPool.alloc(); }
  void  operator delete(void* p) { mPool.free(p); }

  unsigned long layer() const; // 
  unsigned long ladder() const; // 

  unsigned long plane() const; //
  unsigned long module() const;  //  
   
private:

    static StMemoryPool mPool; //!
    ClassDef(StMcFstHit,1)
};

ostream&  operator<<(ostream& os, const StMcFstHit&);


#endif
