/***************************************************************************
 *
 * $Id: StTrsDigitalSignalGenerator.hh,v 1.1 1998/11/10 17:12:10 fisyak Exp $
 *
 * Author: brian, October 1998 
 ***************************************************************************
 *
 * Description: Abstract class to define functionality of digital response 
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSignalGenerator.hh,v $
 * Revision 1.1  1998/11/10 17:12:10  fisyak
 * Put Brian trs versin into StRoot
 *
 *
 * Revision 1.2  1999/01/18 10:23:00  lasiuk
 * add StTrsDigitalSector
 *
 * Revision 1.1  1998/11/10 17:12:10  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/04 18:51:16  lasiuk
 * initialization in base class
 * incorporate electronics db
 * sector by reference
 *
 * Revision 1.1  1998/06/30 22:54:10  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_TRS_DIGITAL_SIGNAL_GENERATOR_HH
#include <vector>
#include "StTrsAnalogSignal.hh"
#include "StTpcElectronics.hh"
#include "StTrsSector.hh"
#include "StTrsDigitalSector.hh"

class StTrsDigitalSignalGenerator {
public:
    virtual ~StTrsDigitalSignalGenerator();

    virtual void addCorrelatedNoise() = 0;

    StTrsDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);
    
protected:
    StTpcElectronics*  mElectronicsDb;
    StTrsSector*       mSector;
protected:
    StTpcElectronics*   mElectronicsDb;
    StTrsSector*        mSector;
    StTrsDigitalSector* mDigitalSector;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
      vector<StTrsAnalogSignal>::iterator mTimeSequenceIterator;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator;
#endif
};

#endif
