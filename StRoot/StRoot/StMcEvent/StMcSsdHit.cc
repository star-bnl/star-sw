/***************************************************************************
 *
 * $Id: StMcSsdHit.cc,v 2.6 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcSsdHit.cc,v $
 * Revision 2.6  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.5  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.4  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.3  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.2  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.1  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.2  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Ssd classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
#include "StMcSsdHit.hh"

static const char rcsid[] = "$Id: StMcSsdHit.cc,v 2.6 2011/10/17 00:24:00 fisyak Exp $";

ClassImp(StMcSsdHit);

ostream&  operator<<(ostream& os, const StMcSsdHit& h)
{
    os << "SsdHit" << endl;
    os << *((StMcHit *) &h);
    os << "Ladder : " << h.ladder() 
       << "\twafer : " << h.wafer() 
       << endl;
  return os;
}

//________________________________________________________________________________
void StMcSsdHit::Print(Option_t *option) const {cout << *this;}
