/***************************************************************************
 *
 * $Id: StMcTpcHit.hh,v 2.1 1999/11/19 19:06:33 calderon Exp $
 * $Log: StMcTpcHit.hh,v $
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
    
    void setPadCoordinate(StTpcPadCoordinate);
    void setLocalCoordinate(StTpcLocalSectorCoordinate);

    unsigned long sector()     const; // 0-23
    unsigned long padrow()     const; // 0-44
    unsigned long pad()        const; // start from zero
    unsigned long timeBucket() const; // start from zero
    const StThreeVectorF& localPosition() const;

private:
    static StMemoryPool         mPool; //!
    StTpcPadCoordinate          mPadCoordinate; //!
    StTpcLocalSectorCoordinate  mSectorTwelveCoordinate; //!
};

inline unsigned long
StMcTpcHit::sector() const
{
    return mPadCoordinate.sector()-1;   //  sector=[0,23]
}

inline unsigned long
StMcTpcHit::padrow() const
{
    return mPadCoordinate.row()-1;   // padrow=[0-44]
}

inline unsigned long
StMcTpcHit::pad() const
{
    return mPadCoordinate.pad()-1;   // start from zero
}

inline unsigned long
StMcTpcHit::timeBucket() const
{
    return mPadCoordinate.timeBucket();   // already starts from zero.
}

inline const StThreeVectorF&
StMcTpcHit::localPosition() const
{
    return mSectorTwelveCoordinate.position();
}

inline void
StMcTpcHit::setPadCoordinate(StTpcPadCoordinate val) { mPadCoordinate=val; }

inline void
StMcTpcHit::setLocalCoordinate(StTpcLocalSectorCoordinate val) { mSectorTwelveCoordinate=val; }

#endif
