/***************************************************************************
 *
 * $Id: StTrsWireBinEntry.cc,v 1.1 1998/11/10 17:12:27 fisyak Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Results from transport of a single StTrsMiniChargeSegment
 *
 ***************************************************************************
 *
 * $Log: StTrsWireBinEntry.cc,v $
 * Revision 1.1  1998/11/10 17:12:27  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:27  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/03 17:29:58  lasiuk
 * added set member functions
 * added scaleNumberOfElectrons()
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsWireBinEntry.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
StTrsWireBinEntry::StTrsWireBinEntry() { /* nopt */ }
#endif

StTrsWireBinEntry::StTrsWireBinEntry(StThreeVector<double> x, float elec)
    : mPosition(x), mNumberOfElectrons(elec) {/* nopt */ }

StTrsWireBinEntry::~StTrsWireBinEntry() {/* nopt */ }

void StTrsWireBinEntry::scaleNumberOfElectrons(float fac)
{
    mNumberOfElectrons *= fac;
}

// Non-member function
ostream& operator<<(ostream& os, const StTrsWireBinEntry& entry)
{
    return os << '(' << entry.position() << ',' << entry.numberOfElectrons() << ')';
}
