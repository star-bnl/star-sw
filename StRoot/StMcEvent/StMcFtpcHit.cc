/***************************************************************************
 *
 * $Id: StMcFtpcHit.cc,v 2.9 2003/10/08 21:21:31 calderon Exp $
 * $Log: StMcFtpcHit.cc,v $
 * Revision 2.9  2003/10/08 21:21:31  calderon
 * Added backward compatibility for plane(), in case of volumeId < 1000.
 * The sector() will return a dummy value of 99999 in this case.
 *
 * Revision 2.8  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.7  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.6  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.5  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.4  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
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
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcFtpcHit.hh"
#include "StMcTrack.hh"
#include "tables/St_g2t_ftp_hit_Table.h" 

static const char rcsid[] = "$Id: StMcFtpcHit.cc,v 2.9 2003/10/08 21:21:31 calderon Exp $";

StMemoryPool StMcFtpcHit::mPool(sizeof(StMcFtpcHit));

StMcFtpcHit::StMcFtpcHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }

StMcFtpcHit::StMcFtpcHit(g2t_ftp_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0)
{/* noop */ }

StMcFtpcHit::~StMcFtpcHit() {/* noop */ }

ostream&  operator<<(ostream& os, const StMcFtpcHit& h)
{
    os << "Position      : " << h.position() << endl; 
    os << "Local Momentum: " << h.localMomentum() << endl;
    os << "Volume Id     : " << h.volumeId() << endl;
    os << "Plane         : " << h.plane()    << endl;
    os << "Sector        : " << h.sector()   << endl;
    return os;
}

unsigned long
StMcFtpcHit::plane() const
{
    if (mVolumeId<1000) return (mVolumeId/100 - 1)*10 + (mVolumeId)%100; // for backward compatibility

    //new encoding from Maria: 1st (west) or 2nd (east) FTPC * 1000 + plane * 100 + sector
    //volume_id = 101? to 110? are the first FTPC (1-10 in StEvent), last digit is sector (below)
    //volume_id = 201? to 210? are the second FTPC (11-20 in StEvent), last digit is sector (below)
    return (mVolumeId/1000 - 1)*10 + (mVolumeId/10)%100;
}

unsigned long
StMcFtpcHit::sector() const
{
    if (mVolumeId < 1000) return 99999; // for backward compatibility
    //volume_id = 1??1 to 1??6 are the sectors in the first FTPC (1-6 in StEvent)
    //volume_id = 2??1 to 2??6 are the sectors in the second FTPC (1-6 in StEvent)
    return mVolumeId%10;
}
