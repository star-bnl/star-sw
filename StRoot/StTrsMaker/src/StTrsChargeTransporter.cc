/**********************************************************************
 *
 * $Id: StTrsChargeTransporter.cc,v 1.2 1999/01/18 10:16:28 lasiuk Exp $
 *
 * Author: brian Nov 1, 1998
 *
 **********************************************************************
 *
 * Description:  Abstract class does initialization
 *
 **********************************************************************
 *
 * $Log: StTrsChargeTransporter.cc,v $
 * Revision 1.2  1999/01/18 10:16:28  lasiuk
 * units consistency
 *
 * Revision 1.2  1999/01/18 10:16:28  lasiuk
 * units consistency
 *
 * Revision 1.1  1998/11/10 17:12:23  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/08 17:05:31  lasiuk
 * boolean macros are not for C++
 *
 * Revision 1.2  1998/11/05 19:01:16  lasiuk
 * setGatingGridVoltage() added
 * transparency cacluation from db numbers
 *
 * Revision 1.1  1998/11/02 22:47:03  lasiuk
 * Initialization in base class
 * add attachment
 * add transparency
 *
 **********************************************************************/
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "PhysicalConstants.h"

#include "StTrsChargeTransporter.hh"

HepJamesRandom StTrsChargeTransporter::mEngine;
RandFlat       StTrsChargeTransporter::mFlatDistribution(mEngine);
RandGauss      StTrsChargeTransporter::mGaussDistribution(mEngine);

StTrsChargeTransporter::StTrsChargeTransporter(StTpcGeometry* geodb, StTpcSlowControl* scdb, StTrsDeDx* gasdb, StMagneticField* magdb)
    :mChargeAttachment(false), mGatingGridTransparency(false), mTransverseDiffusion(false), mLongitudinalDiffusion(false), mExB(false)
{
    mGeomDb = geodb;
    mSCDb       = scdb;
    mGasDb      = gasdb;
    mMagDb      = magdb;

    mAttachment = gasdb->attachmentCoefficient();
    mSigmaTransverse = gasdb->transverseDiffusionCoefficient();
    mSigmaLongitudinal = gasdb->longitudinalDiffusionCoefficient();
    mO2Concentration = 50.; // 50 ppm //mO2Concentration = scdb->oxygenInPPM();
    mDriftVelocity = scdb->driftVelocity();
    mGateVoltage   = -130*volt;      // scdb->gatingGridVoltage();
    mTransparency = 1.;
    
    mDoTransparencyCalc = true;
}

StTrsChargeTransporter::~StTrsChargeTransporter() {/* nopt */}

//
// GATING GRID TRANSPARENCY CALCULATION
//
void StTrsChargeTransporter::Lg()
{
    mLg =  mZGate - mGatePitch/(2*pi)*log(2*pi*mGateWireRadius/mGatePitch)-sqr(mZGate)/mDriftDistance; 
}

double StTrsChargeTransporter::transitionVoltage()
{
    double value;
    value = mDriftVoltage/mDriftDistance*(mZGate - 4*pi*mGateWireRadius*mLg/mGatePitch);
    return value;
}

double StTrsChargeTransporter::maximumTransparency()
{
    double value;
    value = mDriftVoltage/mDriftDistance*(4*pi*mGateWireRadius*mLg/mGatePitch + mZGate);
    return value;
}

void StTrsChargeTransporter::sigmaGC()
{
    mSGC = abs(4*pi*mGateWireRadius*epsilon0*mDriftVoltage/mDriftDistance/mGatePitch);
//     PR(mSGC);
}

void StTrsChargeTransporter::sigmaG()  // (needs gate voltage (data member) charge density of the gating grid
{
    mSG = epsilon0*(mGateVoltage - mZGate*mDriftVoltage/mDriftDistance)/mLg;
//     PR(mSG);
}

double StTrsChargeTransporter::sigmaGPlus()
{
    sigmaGC();
    sigmaG();
    double term1, term2, value;

    term1 = ( 1./pi*(mSGC*sqrt(1-sqr(mSG/mSGC))) );
    term2 = ( 1./pi*mSG*acos(-mSG/mSGC) ) ;
    value = term1 + term2;
    //cout << "  term1: " << term1 << " term2 " << term2 << endl;
    //cout << "  sigmaGPlus " << value << endl;

    return value;
}

double StTrsChargeTransporter::zeroGateVoltage()
{   // Transparency
    double value;
    value = 1.-mZGate/mLg;
    return value;
}

double StTrsChargeTransporter::linearOnset()
{
    double value;
    value = 1 - 4*pi*mGateWireRadius/mGatePitch;
    return value;
}

double StTrsChargeTransporter::linearTransparency()
{
    //     T(Vg=0) = zg/Lg
    //     T(Vg_linear) = 1 - 4 pi r_g/s_g
    double T1 = zeroGateVoltage();
    double T2 = linearOnset();
    double V2 = transitionVoltage();
    //double V1 = 0;
    double slope = (T2-T1)/V2;
    double eqn   = slope*(mGateVoltage) + T1;

    return eqn; 
}
double StTrsChargeTransporter::transparencyCalculation()
{
    // set a lot of variables:
    mDriftVoltage = mSCDb->driftVoltage();
//     PR(mDriftVoltage);
    
    mDriftDistance = mGeomDb->driftDistance();      // (zDriftDistance)
//     PR(mDriftDistance);
    
    mGateWireRadius = mGeomDb->gateWireRadius(); // (gateRadius)
//     PR(mGateWireRadius);
    
    mGatePitch      = mGeomDb->gatePitch();   // (gatePitch)
 //    PR(mGatePitch);
    

    mZGate = mGeomDb->outerSectorGatingGridPadPlaneSeparation() -
	mGeomDb->outerSectorFrischGridPadPlaneSeparation();
//     PR(mZGate);
 

    Lg();
    
    double trans;
    if(mGateVoltage <= maximumTransparency()) {
	trans = 1.;
    }
    else if(mGateVoltage > maximumTransparency() && mGateVoltage < transitionVoltage()) {
	double sigmaP = abs(epsilon0*mDriftVoltage/mDriftDistance);
	trans = (1-(sigmaGPlus()/sigmaP));
    }
    else if(mGateVoltage >=transitionVoltage()) {
	trans = linearTransparency();
    }

    return (trans>0) ? trans : 0;    
}
