/***************************************************************************
 *
 * $Id: StMcFtpcHit.hh,v 2.4 2000/01/18 20:52:31 calderon Exp $
 * $Log: StMcFtpcHit.hh,v $
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/15 20:05:48  calderon
 * corrected the comment on the numbering of the plane
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:32  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcFtpcHit_hh
#define StMcFtpcHit_hh

#include "StMcHit.hh"
#include "StMemoryPool.hh"

class StMcTrack;
class StThreeVectorF;
class g2t_ftp_hit_st;

#if !defined(ST_NO_NAMESPACES)
#endif

class StMcFtpcHit : public StMcHit {
public:
    StMcFtpcHit();
    StMcFtpcHit(const StThreeVectorF&,
	     const float, const float, StMcTrack*);
    StMcFtpcHit(g2t_ftp_hit_st*);
    ~StMcFtpcHit();

    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }

    unsigned long plane() const; // 1-20
    
private:
    long mVolumeId;

    static StMemoryPool mPool; //!
};

ostream&  operator<<(ostream& os, const StMcFtpcHit&);


#endif
