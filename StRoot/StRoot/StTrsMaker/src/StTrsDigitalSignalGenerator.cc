/***************************************************************************
 *
 * $Id: StTrsDigitalSignalGenerator.cc,v 1.4 1999/02/28 20:19:32 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Responsible for initialization
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSignalGenerator.cc,v $
 * Revision 1.4  1999/02/28 20:19:32  lasiuk
 * take number of time bins from db
 * not compatible with data compression from the analogSignalGenerator
 *
 * Revision 1.3  1999/02/04 18:34:01  lasiuk
 * remove digSector from constructor;
 * add fillSector for designation
 *
 * Revision 1.2  1999/01/18 10:23:10  lasiuk
 * add StTrsDigitalSector
 *
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/04 18:51:27  lasiuk
 * initialization in base class
 * incorporate electronics db
 * sector by reference
 *
 **************************************************************************/
#include "StTrsDigitalSignalGenerator.hh"

StTrsDigitalSignalGenerator::StTrsDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : mElectronicsDb(el), mSector(sec)
{
    mNumberOfTimeBins = mElectronicsDb->numberOfTimeBins();
    PR(mNumberOfTimeBins);
}

StTrsDigitalSignalGenerator::~StTrsDigitalSignalGenerator() {/* nopt */}

void StTrsDigitalSignalGenerator::fillSector(StTrsDigitalSector* theDigitalSector)
{
    mDigitalSector = theDigitalSector;
}
