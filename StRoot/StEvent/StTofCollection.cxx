/***************************************************************************
 *
 * $Id: StTofCollection.cxx,v 2.1 2000/12/08 03:52:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain. All ToF stuff goes here
 * except the StTofPidTraits and the StTofSoftwareMonitor.
 *
 ***************************************************************************
 *
 * $Log: StTofCollection.cxx,v $
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTofCollection.h"

static const char rcsid[] = "$Id: StTofCollection.cxx,v 2.1 2000/12/08 03:52:42 ullrich Exp $";

ClassImp(StTofCollection)
    
StTofCollection::StTofCollection() { /* noop */ }

StTofCollection::~StTofCollection() { /* noop */ }
