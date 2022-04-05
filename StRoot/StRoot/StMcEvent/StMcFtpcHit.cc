/***************************************************************************
 *
 * $Id: StMcFtpcHit.cc,v 2.15 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcFtpcHit.cc,v $
 * Revision 2.15  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.14  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.13  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.12  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.11  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.10  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
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

static const char rcsid[] = "$Id: StMcFtpcHit.cc,v 2.15 2011/10/17 00:24:00 fisyak Exp $";

ClassImp(StMcFtpcHit);

ostream&  operator<<(ostream& os, const StMcFtpcHit& h)
{
    os << "FtpcHit" << endl;
    os << *((StMcHit *) &h);
    os << "Plane        : "  << h.plane() << endl;
    os << "Sector       : "  << h.sector() << endl;;
    return os;
}

ULong_t
StMcFtpcHit::plane() const
{
    if (mVolumeId<1000) return (mVolumeId/100 - 1)*10 + (mVolumeId)%100; // for backward compatibility

    //new encoding from Maria: 1st (west) or 2nd (east) FTPC * 1000 + plane * 100 + sector
    //volume_id = 101? to 110? are the first FTPC (1-10 in StEvent), last digit is sector (below)
    //volume_id = 201? to 210? are the second FTPC (11-20 in StEvent), last digit is sector (below)
    return (mVolumeId/1000 - 1)*10 + (mVolumeId/10)%100;
}

ULong_t
StMcFtpcHit::sector() const
{
    if (mVolumeId < 1000) return 99999; // for backward compatibility
    //volume_id = 1??1 to 1??6 are the sectors in the first FTPC (1-6 in StEvent)
    //volume_id = 2??1 to 2??6 are the sectors in the second FTPC (1-6 in StEvent)
    return mVolumeId%10;
}
//________________________________________________________________________________
void StMcFtpcHit::Print(Option_t *option) const {
  cout << "FtpcHit\t"; 
  StMcHit::Print();
  cout << "\tPlane:"   << plane()
       << " Sector:"  << sector();
}
