/***************************************************************************
 *
 * $Id: StMcTofHit.cc,v 2.7 2011/10/17 00:24:01 fisyak Exp $
 * $Log: StMcTofHit.cc,v $
 * Revision 2.7  2011/10/17 00:24:01  fisyak
 * Add time of flight for hits
 *
 * Revision 2.6  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.5  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.4  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.3  2005/01/27 23:40:48  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.2  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 ***************************************************************************/
#include "StMcTofHit.hh"
#include "tables/St_g2t_ctf_hit_Table.h"
static const char rcsid[] = "$Id: StMcTofHit.cc,v 2.7 2011/10/17 00:24:01 fisyak Exp $";
ClassImp(StMcTofHit);


ostream&  operator<<(ostream& os, const StMcTofHit& h) {
    os << "TofHit\t" << endl;
    os << *((StMcHit *) &h);
    os << "T. of Flight    : " << h.tof() << endl;
    os << "path length     : " << h.sTrack() << endl;
  return os;
}
//________________________________________________________________________________
void StMcTofHit::Print(Option_t *option) const {
  cout << "TofHit\t";
  StMcHit::Print();
  cout << "\tT. of Flight: " << tof()
       << " path length: " << sTrack();
}
