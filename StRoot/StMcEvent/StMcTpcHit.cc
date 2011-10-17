/***************************************************************************
 *
 * $Id: StMcTpcHit.cc,v 2.13 2011/10/17 00:24:01 fisyak Exp $
 * $Log: StMcTpcHit.cc,v $
 * Revision 2.13  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.12  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.11  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.10  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.9  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.8  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.7  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.6  2000/05/05 15:25:44  calderon
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
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StMcTpcHit.hh"

static const char rcsid[] = "$Id: StMcTpcHit.cc,v 2.13 2011/10/17 00:24:01 fisyak Exp $";

ClassImp(StMcTpcHit);
ostream&  operator<<(ostream& os, const StMcTpcHit& h)
{
    os << "TpcHit" << endl;
    os << *((StMcHit *) &h);
    os << "Sector   :" << h.sector() << endl;
    os << "PadRow   :" << h.padrow() << endl;
  return os;
}
//________________________________________________________________________________
void StMcTpcHit::Print(Option_t *option) const {
  cout << "TpcHit\t"; 
  StMcHit::Print();
  cout << "\tSector:" << sector()    
       << " PadRow:" << padrow();
}

