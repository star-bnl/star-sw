/***************************************************************************
 *
 * $Id: StMcTpcHit.hh,v 2.3 1999/12/14 07:04:49 calderon Exp $
 * $Log: StMcTpcHit.hh,v $
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:53  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTpcHit_hh
#define StMcTpcHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"

class StMcTrack;
class StThreeVectorF;
class g2t_tpc_hit_st;

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcTpcHit : public StMcHit {
public:
    StMcTpcHit();
    StMcTpcHit(const StThreeVectorF&,
	     const float, const float, StMcTrack*);
    StMcTpcHit(g2t_tpc_hit_st*);
    ~StMcTpcHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
    unsigned long sector()     const; // 1-24
    unsigned long padrow()     const; // 1-45

private:
    static StMemoryPool         mPool; //!
    long   mVolumeId;
};

ostream&  operator<<(ostream& os, const StMcTpcHit&);

inline unsigned long
StMcTpcHit::sector() const
{
    // volume Id = 10000 * some junk + 100 * sector + padrow
    return (mVolumeId%10000)/100;   //  sector=[1,24]
}

inline unsigned long
StMcTpcHit::padrow() const
{
    return (mVolumeId%100);   // padrow=[1-45]
}

#endif
