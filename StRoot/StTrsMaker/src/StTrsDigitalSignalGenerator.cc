/***************************************************************************
 *
 * $Id: StTrsDigitalSignalGenerator.cc,v 1.1 1998/11/10 17:12:24 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Responsible for initialization
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSignalGenerator.cc,v $
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
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
StTrsDigitalSignalGenerator::StTrsDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
    : mElectronicsDb(el), mSector(sec)
#include "StTrsDigitalSignalGenerator.hh"

StTrsDigitalSignalGenerator::StTrsDigitalSignalGenerator(StTpcElectronics* el, StTrsSector* sec)
void StTrsDigitalSignalGenerator::fillSector(StTrsDigitalSector* theDigitalSector)
{
    mDigitalSector = theDigitalSector;
}
