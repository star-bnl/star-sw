/***************************************************************************
 *
 * $Id: StMcRichHit.hh,v 2.5 2000/06/06 02:58:41 calderon Exp $
 * $Log: StMcRichHit.hh,v $
 * Revision 2.5  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.4  2000/05/26 21:42:11  calderon
 * Added volumeId() method.
 *
 * Revision 2.3  2000/05/05 15:25:44  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.2  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#ifndef StMcRichHit_hh
#define StMcRichHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class StThreeVectorF;
class g2t_rch_hit_st;

class StMcRichHit : public StMcHit {
public:
    StMcRichHit();
    StMcRichHit(const StThreeVectorF&,const StThreeVectorF&,
	     const float, const float,  const long, const long, StMcTrack*);
    StMcRichHit(g2t_rch_hit_st*);
    ~StMcRichHit();
    int operator==(const StMcRichHit&) const;
    int operator!=(const StMcRichHit&) const;

    unsigned short  pad() const;
    unsigned short  row() const;
    float           tof() const;
    
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
private:
    static StMemoryPool mPool; //!
    float               mTof;
};

ostream&  operator<<(ostream& os, const StMcRichHit&);

inline unsigned short
StMcRichHit::pad() const
{
    return (mVolumeId & 0xff);  // first 8 bits
}

inline unsigned short
StMcRichHit::row() const
{
    return ( (mVolumeId>>8) & 0xff);  // second 8 bits
}

inline float
StMcRichHit::tof() const
{
    return mTof;
}

#endif
