/***************************************************************************
 *
 * $Id: StMixerDigitalSignalGenerator.cc,v 1.1 2000/02/16 21:02:08 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsDigitalSignalGenerator.cc
 *
 ***************************************************************************
 *
 * Description: Responsible for initialization
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMixerDigitalSignalGenerator.hh"

StMixerDigitalSignalGenerator::StMixerDigitalSignalGenerator(StTpcElectronics* el, StMixerSector* sec)
    : mElectronicsDb(el), mSector(sec)
{
    mNumberOfTimeBins = mElectronicsDb->numberOfTimeBins();
    PR(mNumberOfTimeBins);
}

StMixerDigitalSignalGenerator::~StMixerDigitalSignalGenerator() {/* nopt */}

void StMixerDigitalSignalGenerator::fillSector(StMixerDigitalSector* theDigitalSector)
{
    mDigitalSector = theDigitalSector;
}
