/***************************************************************************
 *
 * $Id: StMcCtbHit.hh,v 2.4 2005/09/28 21:30:14 fisyak Exp $
 * $Log: StMcCtbHit.hh,v $
 * Revision 2.4  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/07/06 20:05:28  calderon
 * Remove forward declaration of StThreeVectorF, use #include, and only in
 * StMcHit base class.  StThreeVectorF is not a class anymore, it is now
 * only a typedef, only template version of StThreeVector exists now.
 *
 * Revision 2.2  2005/01/27 23:40:46  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2003/02/19 03:29:41  calderon
 * Introduction of CTB classes to repository.
 *
 * Revision 1.0  2003/03/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.
 */
#ifndef StMcCtbHit_hh
#define StMcCtbHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_ctf_hit_st;

class StMcCtbHit : public StMcHit {
public:
    StMcCtbHit();
    StMcCtbHit(const StThreeVectorF&,const StThreeVectorF&,
	     const float, const float,  const long, const long, StMcTrack*);
    StMcCtbHit(g2t_ctf_hit_st*);
    virtual ~StMcCtbHit();
    int operator==(const StMcCtbHit&) const;
    int operator!=(const StMcCtbHit&) const;
    void get_slat_tray(unsigned int & slat, unsigned int & tray) const;
    float tof() const;
#ifdef POOL
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
#endif    
private:
#ifdef POOL
    static StMemoryPool mPool; 
#endif
    float               mTof;
    ClassDef(StMcCtbHit,1)
};

ostream&  operator<<(ostream& os, const StMcCtbHit&);

inline float
StMcCtbHit::tof() const
{
    return mTof;
}



#endif
