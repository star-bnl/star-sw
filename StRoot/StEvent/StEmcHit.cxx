/***************************************************************************
 *
 * $Id: StEmcHit.cxx,v 1.2 1999/04/28 22:27:30 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHit.cxx,v $
 * Revision 1.2  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/04/28 22:27:30  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/02/23 15:45:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcHit.h"
#include <iostream.h>
ClassImp(StEmcHit)
StCollectionImp(EmcHit)
ostream& operator<< (ostream& os, const StEmcHit& h)
{
  cout << *this << endl;
}
//______________________________________________________________________________
