/***************************************************************************
 *
 * $Id: StMwcSector.cc,v 1.1 1999/01/15 20:39:55 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcSector.cc,v $
 * Revision 1.1  1999/01/15 20:39:55  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StMwcSector.hh"

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
