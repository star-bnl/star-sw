/***************************************************************************
 *
 * $Id: StMcHit.cc,v 2.14 2015/07/22 19:30:00 jwebb Exp $
 * $Log: StMcHit.cc,v $
 * Revision 2.14  2015/07/22 19:30:00  jwebb
 * Fix minor compiler warnings.
 *
 * Revision 2.13  2014/08/06 19:07:34  perev
 * Warnoff
 *
 * Revision 2.12  2011/10/17 00:24:00  fisyak
 * Add time of flight for hits
 *
 * Revision 2.10  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.9  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.8  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.7  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.6  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.5  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.4  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.3  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:32  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StMcHit.hh"
#include "TString.h"
static const Char_t rcsid[] = "$Id: StMcHit.cc,v 2.14 2015/07/22 19:30:00 jwebb Exp $";
ClassImp(StMcHit);
    
int StMcHit::operator==(const StMcHit& h) const
{
    return h.mKey == mKey && h.mPosition == mPosition &&
           h.mdE   == mdE && h.mdS == mdS ;
}

ostream& operator<<(ostream& os, const StMcHit& h)
{
    if (h.parentTrack())
      os << "Key, parent Key : " << Form("%5i/%5i",(int)h.key(),(int)h.parentTrack()->key()) << endl;
    else 
      os << "Key             : " << Form("%5i/undef",int(h.key())) << endl;
    
    os << "Position xyz    : " << Form("%8.2f%8.2f%8.2f",h.position().x(), h.position().y(), h.position().z()) << endl;
    os << "Local Momentum  : " << Form("%8.2f%8.2f%8.2f",h.localMomentum().x()  ,h.localMomentum().y()  ,h.localMomentum().z())  << endl;
    os << "dE              : " << Form("%8.2f keV",1e6*h.dE())  << endl;
    os << "dS              : " << Form("%8.2f cm",h.dS()) << endl;
    os << "tof             : " << Form("%8.2f ns",h.tof()*1e9) << endl;
    os << "VolId           : " << h.volumeId() << endl;
    return os;
}
//________________________________________________________________________________
void StMcHit::Print(Option_t *option) const {
  cout << *this << endl;
}
