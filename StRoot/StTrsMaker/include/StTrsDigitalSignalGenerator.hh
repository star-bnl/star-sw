/***************************************************************************
 *
 * $Id: StTrsDigitalSignalGenerator.hh,v 1.8 2005/09/09 22:12:48 perev Exp $
 *
 * Author: brian, October 1998 
 ***************************************************************************
 *
 * Description: Abstract class to define functionality of digital response 
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSignalGenerator.hh,v $
 * Revision 1.8  2005/09/09 22:12:48  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.7  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.6  2000/01/10 23:11:32  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.5  1999/12/08 02:10:25  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.4  1999/02/28 20:19:44  lasiuk
 * take number of time bins from db
 * not compatible with data compression from the analogSignalGenerator
 *
 * Revision 1.3  1999/02/04 18:33:48  lasiuk
 * remove digSector from constructor;
 * add fillSector for designation
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
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::vector;
#endif

#include "StTrsAnalogSignal.hh"
#include "StTpcElectronics.hh"
#include "StTrsSector.hh"
#include "StTrsDigitalSector.hh"

#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
        using std::range_error;
        using std::domain_error;
#   endif
#endif
class StTrsDigitalSignalGenerator {
public:
    virtual ~StTrsDigitalSignalGenerator();

    virtual void digitizeSignal()     = 0;
    virtual void addWhiteNoise()      = 0;
    virtual void addCorrelatedNoise() = 0;
  void         SetSectorNo(int sect) {mSectorNo = sect;}
  int          GetSectorNo() {return mSectorNo;}

    void         fillSector(StTrsDigitalSector*);
    
protected:
    //StTrsDigitalSignalGenerator();
    StTrsDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);
	
protected:
    unsigned int        mNumberOfTimeBins;
    
    StTpcElectronics*   mElectronicsDb;
    StTrsSector*        mSector;
    StTrsDigitalSector* mDigitalSector;
    int                 mSectorNo;

vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator;
};

#endif
