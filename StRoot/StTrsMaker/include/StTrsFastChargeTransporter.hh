/**********************************************************************
 *
 * $Id: StTrsFastChargeTransporter.hh,v 1.1 1998/11/10 17:12:10 fisyak Exp $
 *
 * Author: brian June 1, 1998
 *
 **********************************************************************
 *
 * Description:  Real class to project charge segment on pad-plane
 *
 **********************************************************************
 *
 * $Log: StTrsFastChargeTransporter.hh,v $
 * Revision 1.1  1998/11/10 17:12:10  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/02 22:47:16  lasiuk
 * Initialization in base class
 * add attachment
 * add transparency
 *
 * Revision 1.2  1998/10/22 00:23:23  lasiuk
 * Oct 22
 *
 * Revision 1.1  1998/06/04 23:32:21  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_TRS_FAST_CHARGE_TRANSPORTER_HH
#define ST_TRS_FAST_CHARGE_TRANSPORTER_HH

#include "StTrsChargeTransporter.hh"

#include "StGlobals.hh"
#include "StThreeVector.hh"
//#include "StTrsWireBinEntry.hh"
//#include "StTrsMiniChargeSegment.hh"
#include "StTpcGeometry.hh"

class StTrsFastChargeTransporter : public StTrsChargeTransporter {
public:
    ~StTrsFastChargeTransporter();
    //StTrsFastChargeTransporter(const StTrsFastChargeTransporter&);
    //StTrsFastChargeTransporter& operator=(const StTrsFastChargeTransporter&);

    static StTrsChargeTransporter* instance(StTpcGeometry*, StTpcSlowControl*, StTrsDeDx*, StMagneticField*);
    
    void    transportToWire(StTrsMiniChargeSegment&)      ;
    double  chargeAttachment(double)                 const;
    double  wireGridTransmission()                        ;

protected:
    StTrsFastChargeTransporter(StTpcGeometry*, StTpcSlowControl*, StTrsDeDx*, StMagneticField*);
    
private:
    static StTrsChargeTransporter* mInstance;
};

#endif
