/***************************************************************************
 *
 * $Id: StTrsManager.hh,v 1.1 1998/11/10 17:12:11 fisyak Exp $
 *
 * Author: brian
 ***************************************************************************
 *
 * Description: Overall simulation manager.  Stores global info as well
 *              as information local to the sector during simulation.
 *
 ***************************************************************************
 *
 * $Log: StTrsManager.hh,v $
 * Revision 1.1  1998/11/10 17:12:11  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.7  1998/11/08 17:28:37  lasiuk
 * allocators for SUN
 *
 * Revision 1.6  1998/10/22 00:23:24  lasiuk
 * Oct 22
 *
 * Revision 1.5  1998/06/30 22:56:07  lasiuk
 * add signal processors
 *
 * Revision 1.4  1998/06/23 22:27:08  ullrich
 * Made readEvent() private. Called now directly from
 * processEvent().
 *
 * Revision 1.3  1998/06/04 23:16:17  lasiuk
 * add addDB() functions and data members
 *
 * Revision 1.2  1998/05/21 21:27:40  lasiuk
 * Initial revision
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_MANAGER_HH
#define ST_TRS_MANAGER_HH

#include <vector>

// SCL
#include "StGlobals.hh"

// TRS
#include "g2t_tpc_hit.hh"
#include "StTrsReader.hh"
#include "StTrsWireHistogram.hh"
#include "StTrsSector.hh"
#include "StTrsWriter.hh"
// dataBases
#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StMagneticField.hh"
#include "StTrsDeDx.hh"
// Tools
#include "StTrsChargeTransporter.hh"
#include "StTrsAnalogSignalGenerator.hh"
#include "StTrsDigitalSignalGenerator.hh"

class StTrsManager {
public:
    
    StTrsManager(int subsegments);
    virtual ~StTrsManager();
    //StTrsManager(const StTrsManager&);
    //StTrsManager& operator=(const StTrsManager&);

    // Bookeepers
    void addReader(StTrsReader*);
    void addWireHistogram(StTrsWireHistogram*);
    void addSector(StTrsSector*);

    // Tools
    void addChargeTransporter(StTrsChargeTransporter*);
    void addAnalogSignalGenerator(StTrsAnalogSignalGenerator*);
    void addDigitalSignalGenerator(StTrsDigitalSignalGenerator*);
    void addWriter(StTrsWriter*);
    
    // dataBases
    void addTpcGeometryDb(StTpcGeometry*);
    void addSlowControlDb(StTpcSlowControl*);
    void addMagneticFieldDb(StMagneticField*);
    void addGasDb(StTrsDeDx*);

    bool processEvent();
    
private:
    bool readEvent();
    
private:
    StTrsReader                  *mReader;
    StTrsWriter                  *mWriter;
    StTpcGeometry                *mTpcGeometryDb;
    StTpcSlowControl             *mSlowControlDb;
    StMagneticField              *mMagneticFieldDb;
    StTrsDeDx                    *mGasDb;
    StTrsChargeTransporter       *mChargeTransporter;
    StTrsWireHistogram           *mWireHistogram;
    StTrsSector                  *mSector;
    StTrsAnalogSignalGenerator   *mAnalogSignalGenerator;
    StTrsDigitalSignalGenerator  *mDigitalSignalGenerator;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsChargeSegment>    mData;
#else
    vector<StTrsChargeSegment, allocator<StTrsChargeSegment> >    mData;
#endif

  // Parameters
    int                           mNumberOfSubSegments;
};

#endif
