/***************************************************************************
 *
 * $Id: StMwcSector.cxx,v 1.4 1999/04/28 22:27:34 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcSector.cxx,v $
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:48  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StMwcSector.h"

static const Char_t rcsid[] = "$Id: StMwcSector.cxx,v 1.4 1999/04/28 22:27:34 fisyak Exp $";

ClassImp(StMwcSector)

StCollectionImp(MwcSector)
StMwcSector::StMwcSector()
{
    mId = 0;  
    mMips = 0;
}

StMwcSector::StMwcSector(Short_t id, Float_t m)
{
    mId = id;  
    mMips = m;
}

StMwcSector::~StMwcSector() { /* noop */ }

void StMwcSector::setId(Short_t val) { mId = val; }

void StMwcSector::setMips(Float_t val) { mMips = val; }
