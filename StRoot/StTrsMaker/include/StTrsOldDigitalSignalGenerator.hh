/***************************************************************************
 *
 * $Id: StTrsOldDigitalSignalGenerator.hh,v 1.3 2003/12/24 13:44:51 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: parameterization of the digital signal response
 *
 ***************************************************************************
 *
 * $Log: StTrsOldDigitalSignalGenerator.hh,v $
 * Revision 1.3  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.2  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/11/05 22:17:05  calderon
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 *
 * Revision 1.3  1999/02/04 18:35:17  lasiuk
 * digital sector removed from constructor;
 * fillSector() added in base class
 *
 * Revision 1.2  1999/01/18 10:25:12  lasiuk
 * add conversion code for StTrsDigitalSector
 *
 * Revision 1.1  1998/11/10 17:12:11  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/04 18:51:17  lasiuk
 * initialization in base class
 * incorporate electronics db
 * sector by reference
 *
 * Revision 1.1  1998/06/30 22:54:09  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_FAST_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_TRS_FAST_DIGITAL_SIGNAL_GENERATOR_HH

#include <Stiostream.h>
#include "StTrsDigitalSignalGenerator.hh"

class StTrsOldDigitalSignalGenerator : public StTrsDigitalSignalGenerator {
public:
    ~StTrsOldDigitalSignalGenerator();
    //StTrsOldDigitalSignalGenerator(const StTrsOldDigitalSignalGenerator&);
    //StTrsOldDigitalSignalGenerator& operator=(const StTrsOldDigitalSignalGenerator&);

    static StTrsDigitalSignalGenerator* instance();
    static StTrsDigitalSignalGenerator* instance(StTpcElectronics*, StTrsSector*);
    
    void digitizeSignal()    ;
    void addWhiteNoise()     ;
    void addCorrelatedNoise();
    
protected:
    StTrsOldDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);

private:
    static StTrsDigitalSignalGenerator* mInstance;

    double     mSimpleConversion;
};
#endif
