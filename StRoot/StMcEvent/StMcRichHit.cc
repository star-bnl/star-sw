/***************************************************************************
 *
 * $Id: StMcRichHit.cc,v 2.8 2011/10/17 00:24:00 fisyak Exp $
 * $Log: StMcRichHit.cc,v $
 * Revision 2.8  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.7  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.6  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.5  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.4  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.3  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.2  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#include "StMcRichHit.hh"

static const char rcsid[] = "$Id: StMcRichHit.cc,v 2.8 2011/10/17 00:24:00 fisyak Exp $";
ClassImp(StMcRichHit);

ostream&  operator<<(ostream& os, const StMcRichHit& h)
{
  os << "RichHit\t" << *((StMcHit *) &h)
     << "\tPad:" << h.pad()
     << " Row:" << h.row() 
     << " T. of Flight:" << h.tof();
  return os;
}
