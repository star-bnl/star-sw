/**********************************************************************
 *
 * $Id: StTrsFastChargeTransporter.cc,v 1.5 1999/02/10 18:02:54 lasiuk Exp $
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
 * Revision 1.5  1999/02/10 18:02:54  lasiuk
 * debug output
 *
 * Revision 1.5  1999/02/10 18:02:54  lasiuk
 * debug output
 *
 * Revision 1.4  1999/01/28 02:47:03  lasiuk
 * drift length ambiguity
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

void StTrsFastChargeTransporter::transportToWire(StTrsMiniChargeSegment& seg)
{
    // Projection onto pad plane
    //PR(mGeomDb->frischGrid());
    double frischGrid = (seg.position().z() > 0) ?
	mGeomDb->frischGrid() : -mGeomDb->frischGrid();
//     PR(frischGrid);
    // must keep time/z position of cluster! -->
    // replace z component with transit (drift) distance!
    double driftLength = (frischGrid>0) ?
    PR(driftLength);
	seg.position().z() - frischGrid ;

//     PR(driftLength);
    if (driftLength<0) {// PROBLEMS
	cerr << "ERROR: StTrsFastChargeTransporter::transportToWire()" << endl;
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
    }
    
//     PR(seg.charge());

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
	mTransparency = transparencyCalculation();
    return mTransparency;
}

double StTrsFastChargeTransporter::chargeAttachment(double driftL) const
{
//     PR(mAttachment);
//     PR(mO2Concentration);
//     PR(.760*bar);
//     PR(driftL);
//     PR(mSCDb->driftVelocity());
//     double suppression = 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mSCDb->driftVelocity();
//     PR(suppression);

    return 1.-mAttachment*(mO2Concentration/1.e+6)*sqr(.760*bar)*driftL/mSCDb->driftVelocity(); 
}
