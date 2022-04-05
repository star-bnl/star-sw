/***************************************************************************
 *
 * $Id: StMcPxlHit.cc,v 2.1 2013/03/25 23:50:35 perev Exp $
 * $Log: StMcPxlHit.cc,v $
 * Revision 2.1  2013/03/25 23:50:35  perev
 * Mustafa.Pxl add
 *
 *
 **************************************************************************/
#include "StMcPxlHit.hh"
#include "tables/St_g2t_pix_hit_Table.h"

static const char rcsid[] = "$Id: StMcPxlHit.cc,v 2.1 2013/03/25 23:50:35 perev Exp $";

#ifdef POOL
StMemoryPool StMcPixelHit::mPool(sizeof(StMcPixelHit));
#endif
ClassImp(StMcPxlHit);
ostream&  operator<<(ostream& os, const StMcPxlHit& h)
{
   os << "PxlHit" << endl;
   os << *((StMcHit *) &h);
   os << "Layer           : " << h.sector() << endl;
   os << "Ladder          : " << h.ladder() << endl;
   os << "Sensor          : " << h.sensor() << endl;
   return os;
}

//________________________________________________________________________________
void StMcPxlHit::Print(Option_t *option) const
{
   cout << *this << endl;
}

/***************************************************************************
 *
 * $Id: StMcPxlHit.cc,v 2.1 2013/03/25 23:50:35 perev Exp $
 * $Log: StMcPxlHit.cc,v $
 * Revision 2.1  2013/03/25 23:50:35  perev
 * Mustafa.Pxl add
 *
 * Revision 2.10  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.9  2009/02/06 15:56:46  fisyak
 * Jonathan: decoding for upgr15 geometry
 *
 * Revision 2.8  2006/11/17 16:54:58  didenko
 * fixes from Willie for upgr05
 *
 * Revision 2.7  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.6  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.5  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.4  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.2  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.1  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 *
 **************************************************************************/
