/***************************************************************************
 *
 * $Id: StTpcPixel.cxx,v 2.6 2015/10/09 17:46:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.cxx,v $
 * Revision 2.6  2015/10/09 17:46:14  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.5  2004/08/06 15:37:43  fisyak
 * Add clster id
 *
 * Revision 2.4  2004/04/26 16:33:35  fisyak
 * Make use of StTpcPixel
 *
 * Revision 2.3  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/12/13 20:16:31  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:45:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcPixel.h"

void StTpcPixel::Print(Option_t *option) const {
      cout << option << "\t" << *this << endl;
    }

static const char rcsid[] = "$Id: StTpcPixel.cxx,v 2.6 2015/10/09 17:46:14 ullrich Exp $";
ostream& operator<< (ostream& os, const StTpcPixel& m)
{
  os << "d:" << (int) m.detector() 
     << "/s:" << (int) m.sector() 
     << "/r:" << (int) m.padrow() 
     << "/p:" << (int) m.pad() 
     << "/t:" << (int) m.timebin() 
     << "/a:" << m.adc() 
     << "/i:" << m.idTruth() 
     << "/ClId:" << m.id() 
     << endl;
    return os;
}

ClassImp(StTpcPixel)
