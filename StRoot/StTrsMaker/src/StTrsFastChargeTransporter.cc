/**********************************************************************
 *
 * $Id: StTrsFastChargeTransporter.cc,v 1.1 1998/11/10 17:12:24 fisyak Exp $
 *
 * Author: brian June 1, 1998
 *
 **********************************************************************
 *
 * Description:  Real class to project charge segment on pad-plane
 *
 **********************************************************************
 *
 * $Log: StTrsFastChargeTransporter.cc,v $
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1999/01/18 21:02:14  lasiuk
 * frisch grid for +/-z
 *
 * Revision 1.2  1999/01/18 10:14:26  lasiuk
 * pressure in mbar
 *
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/02 22:47:03  lasiuk
 * Initialization in base class
 * add attachment
 * add transparency
 *
 * Revision 1.2  1998/10/22 00:24:23  lasiuk
 * Oct 22
 *
 * Revision 1.1  1998/06/04 23:31:58  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#include "StTrsFastChargeTransporter.hh"

StTrsChargeTransporter* StTrsFastChargeTransporter::mInstance = 0; // static data member

StTrsChargeTransporter* StTrsFastChargeTransporter::instance(StTpcGeometry* geo, StTpcSlowControl* sc, StTrsDeDx* gas, StMagneticField* mag)
{
    if(!mInstance) { // create an instance:
	mInstance = new StTrsFastChargeTransporter(geo, sc, gas, mag);
    }
    //else  do nothing

    return mInstance;
}

StTrsFastChargeTransporter::StTrsFastChargeTransporter(StTpcGeometry* geodb, StTpcSlowControl* scdb, StTrsDeDx* gasdb, StMagneticField* magdb)
    : StTrsChargeTransporter(geodb, scdb, gasdb, magdb)
{/* nopt*/ }

StTrsFastChargeTransporter::~StTrsFastChargeTransporter() {/* nopt */}

    PR(mGeomDb->frischGrid());

    // must keep time/z position of cluster! --> replace with transit distance!
    double driftLength = mGeomDb->frischGrid() - seg.position().z();
    PR(driftLength);
	cerr << "ERROR: Drift distance < 0" << endl;
	//continue; // Do something!!!
    }
    //
    // Diffusion to space out charge on the wire:
    if (mTransverseDiffusion) {
	seg.position().setX( (mGaussDistribution.shoot(seg.position().x(), (mSigmaTransverse*sqrt(driftLength)) ) ) );
	seg.position().setY( (mGaussDistribution.shoot(seg.position().y(), (mSigmaTransverse*sqrt(driftLength)) ) ) );
    }
    
    if (mLongitudinalDiffusion) {
	seg.position().setZ( (mGaussDistribution.shoot(driftLength, (mSigmaLongitudinal*sqrt(driftLength)) ) ) );
    }
    else {
    PR(seg.position());
    }

//     PR(seg.position());

    //
    // Alter charge with:
    //     - absorption (O2)
    //     - wire grid transmision

    // O2 abosorption (stat)
    if(mChargeAttachment) {
    PR(seg.charge());
    
//     PR(seg.charge());
	PR(wireGridTransmission());
    // Grid Transparency (stat)
    if(mGatingGridTransparency) {
	//PR(wireGridTransmission());
    PR(seg.charge());
    }

//     PR(seg.charge());
}

double StTrsFastChargeTransporter::wireGridTransmission()
{
    if(mDoTransparencyCalc)
    //    using namespace units;
    double suppression = 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(760*atmosphere)*driftL;
    PR(suppression);
//     PR(mO2Concentration);
    return suppression; 
//     PR(driftL);
//     PR(mSCDb->driftVelocity());
//     double suppression = 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mSCDb->driftVelocity();
//     PR(suppression);

    return 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mSCDb->driftVelocity(); 
}
