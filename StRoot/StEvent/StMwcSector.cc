/***************************************************************************
 *
 * $Id: StMwcSector.cc,v 1.2 1999/01/15 22:53:48 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcSector.cc,v $
 * Revision 1.2  1999/01/15 22:53:48  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StMwcSector.hh"

static const char rcsid[] = "$Id: StMwcSector.cc,v 1.2 1999/01/15 22:53:48 wenaus Exp $";

StMwcSector::StMwcSector()
{
    mId = 0;  
    mMips = 0;
}

StMwcSector::StMwcSector(short id, float m)
{
    mId = id;  
    mMips = m;
}

StMwcSector::~StMwcSector() { /* noop */ }

void StMwcSector::setId(short val) { mId = val; }

void StMwcSector::setMips(float val) { mMips = val; }
