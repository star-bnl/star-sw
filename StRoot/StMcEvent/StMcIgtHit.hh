/*!
 * \class  StMcIgtHit
 * \brief  Monte Carlo Hit class for the GEM Tracker.
 * \author Gerrit van Nieuwenhuizen, Manuel Calderon de la Barca Sanchez
 * \date   July 2005
 *
 ***************************************************************************
 *
 * $Id: StMcIgtHit.hh,v 2.4 2005/11/22 21:44:52 fisyak Exp $
 *
 ***************************************************************************
 * $Log: StMcIgtHit.hh,v $
 * Revision 2.4  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.3  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.2  2005/07/19 20:07:34  calderon
 * Addition of default constructor, including base class StMcHit constructor.
 * Bracket calls to StMemoryPool inside #ifdef.
 *
 * Revision 2.1  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 *
 **************************************************************************/
#ifndef StMcIgtHit_hh
#define StMcIgtHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_igt_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcIgtHit : public StMcHit {
public:
    StMcIgtHit() : StMcHit() {}
    StMcIgtHit(const StThreeVectorF&,const StThreeVectorF&,
	       const float, const float, const long, const long, StMcTrack*);
    StMcIgtHit(g2t_igt_hit_st*);
    ~StMcIgtHit();
    
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
    unsigned long layer() const; // 
    unsigned long ladder() const; // 
    
    // Willie: Added function wafer() to return wafer number (1-7,1-10,1-13 for layers 1,2,3)
    // and side() to return ladder side (1=inner,2=outer)
    unsigned long wafer() {return ((mVolumeId/10000)%20);}
    unsigned long side() {return (((mVolumeId%200)/100)+1);} //1=inner; 2=outer;
    virtual void Print(Option_t *option="") const; // *MENU* 
    
private:
    
    static StMemoryPool mPool; 
    ClassDef(StMcIgtHit,1)
};

ostream&  operator<<(ostream& os, const StMcIgtHit&);


#endif
