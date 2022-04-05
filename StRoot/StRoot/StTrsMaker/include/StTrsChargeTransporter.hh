/**********************************************************************
 *
 * $Id: StTrsChargeTransporter.hh,v 1.4 2009/11/03 14:34:19 fisyak Exp $
 *
 * Author: brian June 1, 1998
 *
 **********************************************************************
 *
 * Description:  Abstract Class interface for Transporter
 *
 **********************************************************************
 *
 * $Log: StTrsChargeTransporter.hh,v $
 * Revision 1.4  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.3  2000/02/24 16:23:48  long
 * transportToWire(StTrsMiniChargeSegment&) --->transportToWire(StTrsMiniChargeSegment&,double &,double &) to calculate diffussion as a function of field
 *
 * Revision 1.2  1999/03/15 13:45:08  lasiuk
 * omegaTau is calculated assuming mobility is independent of electric field
 *
 * Revision 1.1  1998/11/10 17:12:09  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.4  1998/11/05 19:00:44  lasiuk
 * setGatingGridVoltage() added
 *
 * Revision 1.3  1998/11/02 22:47:15  lasiuk
 * Initialization in base class
 * add attachment
 * add transparency
 *
 * Revision 1.2  1998/10/22 00:23:20  lasiuk
 * Oct 22
 *
 * Revision 1.1  1998/06/04 23:32:20  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_TRS_TRANSPORTER_HH
#define ST_TRS_TRANSPORTER_HH

#include "StGlobals.hh"
#include "Randomize.h"

//TRS
#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StMagneticField.hh"
#include "StTrsDeDx.hh"
#include "StTrsMiniChargeSegment.hh"

class StTrsChargeTransporter {
public:
    StTrsChargeTransporter(StTpcGeometry*, StTpcSlowControl*, StTrsDeDx*, StMagneticField*);
    virtual ~StTrsChargeTransporter();
    //StTrsChargeTransporter(const StTrsChargeTransporter&);
    //StTrsChargeTransporter& operator=(const StTrsChargeTransporter&);

    // The Real Stuff is here...FAST or SLOW
    virtual void   transportToWire(StTrsMiniChargeSegment&,double &,double &)       = 0;
    virtual double chargeAttachment(double)                 const = 0;
    virtual double wireGridTransmission()                         = 0;

    // switches
    void setChargeAttachment(bool);
    void setGatingGridTransparency(bool);
    void setGatingGridVoltage(float);
    void setTransverseDiffusion(bool);
    void setLongitudinalDiffusion(bool);
    void setExB(bool);
  void setDriftVelocity(Double_t dv) {mDriftVelocity = dv;}

    double transparencyCalculation();
    
private:
    StTrsChargeTransporter() {/*never*/}

    // Transparency Calculations
    void   Lg();                      // Calculate characteristic Length
    double transitionVoltage();       // Onset of linear Transparency with gate voltage 
    double maximumTransparency();     // maximum Voltage for 100% Transparency
    void   sigmaGC();                 // constant
    void   sigmaG();                  // Total charge density of the gating grid
    double sigmaGPlus();              // positive charge density of GG
    double zeroGateVoltage();         // need for linearity Calc
    double linearOnset();             // Transparency at Linear onset
    double linearTransparency();      //
    //double transparencyCalculation(); // Selects the region!

protected:
    bool mChargeAttachment;
    bool mGatingGridTransparency;
    bool mTransverseDiffusion;
    bool mLongitudinalDiffusion;
    bool mExB;
    bool mDoTransparencyCalc;
    
    double mAttachment;
    double mTransparency;
    double mSigmaTransverse;
    double mSigmaLongitudinal;
    double mOmegaTau;
    double mDriftVelocity;
    double mO2Concentration; // should be from SC

    // For Transparency Calculation
    // These must come from the slow control db!!!
    // see StTrsChargeTransporter::transparencyCalculation()
    double mGateVoltage;     //  "
    double mLg;              // for Gate Calc
    double mSGC;             //  "
    double mSG;              //  "
    double mDriftVoltage;
    double mDriftDistance;
    double mGateWireRadius;
    double mGatePitch;
    double mZGate;
    
    StTpcGeometry*    mGeomDb;
    StTpcSlowControl* mSCDb;
    StTrsDeDx*        mGasDb;
    StMagneticField*  mMagDb;

protected:
    static HepJamesRandom  mEngine;
    static RandFlat        mFlatDistribution;
    static RandGauss       mGaussDistribution;
};
inline void StTrsChargeTransporter::setChargeAttachment(bool a) {mChargeAttachment = a;}
inline void StTrsChargeTransporter::setGatingGridTransparency(bool t) {mGatingGridTransparency = t;}
inline void StTrsChargeTransporter::setGatingGridVoltage(float v) {mGateVoltage = v;}
inline void StTrsChargeTransporter::setTransverseDiffusion(bool td) {mTransverseDiffusion = td;}
inline void StTrsChargeTransporter::setLongitudinalDiffusion(bool ld) {mLongitudinalDiffusion = ld;}
inline void StTrsChargeTransporter::setExB(bool eb) {mExB = eb;}
#endif
