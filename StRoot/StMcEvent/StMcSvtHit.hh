/***************************************************************************
 *
 * $Id: StMcSvtHit.hh,v 2.5 2000/04/18 22:55:28 calderon Exp $
 * $Log: StMcSvtHit.hh,v $
 * Revision 2.5  2000/04/18 22:55:28  calderon
 * Functions to access the volume Id
 * Added volume Id to output of operator<<
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
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
 * Revision 1.3  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcSvtHit_hh
#define StMcSvtHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class g2t_svt_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcSvtHit : public StMcHit {
public:
    StMcSvtHit();
    StMcSvtHit(const StThreeVectorF&,
	     const float, const float,
	     StMcTrack*);
    StMcSvtHit(g2t_svt_hit_st*);
    ~StMcSvtHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned long layer() const;      // layer=[1,6]
    unsigned long ladder() const;     // ladder=[1-8]
    unsigned long wafer() const;      // wafer=[1-7]
    unsigned long barrel() const;     // barrel=[1-3]
    unsigned long hybrid() const;
    long          volumeId() const;

protected:
    static StMemoryPool mPool;  //!
    long   mVolumeId;
};

ostream&  operator<<(ostream& os, const StMcSvtHit&);

inline unsigned long
StMcSvtHit::layer() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)
    return (mVolumeId)/1000;
}

inline unsigned long
StMcSvtHit::ladder() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)
    return (mVolumeId)%100 ;
}

inline unsigned long
StMcSvtHit::wafer() const
{
    // Volume Id: 1000*layer + 100*wafer + ladder (Helen, Nov 99)
    return ((mVolumeId)%1000)/100;
}

inline unsigned long
StMcSvtHit::barrel() const { return layer()/2; }

inline unsigned long
StMcSvtHit::hybrid() const { return 0; } // to be implemented

inline long
StMcSvtHit::volumeId() const { return mVolumeId; }

#endif
