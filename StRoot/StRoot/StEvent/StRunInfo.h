/*!
 * \class StRunInfo 
 * \author Thomas Ullrich, Sep 2001
 */
/***************************************************************************
 *
 * $Id: StRunInfo.h,v 2.8 2004/10/20 16:06:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunInfo.h,v $
 * Revision 2.8  2004/10/20 16:06:53  ullrich
 * Add variables to report on space charge and the correction mode.
 *
 * Revision 2.7  2004/07/06 23:05:26  ullrich
 * Added SVT drift velocity scaler.
 *
 * Revision 2.6  2004/01/22 23:14:07  ullrich
 * Added Rhic scaler methods (BBC).
 *
 * Revision 2.5  2002/02/25 19:32:47  ullrich
 * Added more RHIC related info.
 *
 * Revision 2.4  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2002/01/31 23:42:36  ullrich
 * Added member to hold BBC coincidence rate.
 *
 * Revision 2.2  2001/12/02 19:27:12  ullrich
 * Added new member and methods.
 *
 * Revision 2.1  2001/09/18 00:14:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StRunInfo_hh
#define StRunInfo_hh

#include "StObject.h"
#include "TString.h"
#include "StEnumerations.h"
#include <ctime>

class StRunInfo : public StObject {
public:
    StRunInfo();
    // StRunInfo(const StRunInfo&);            use default
    // StRunInfo& operator=(const StRunInfo&); use default
    virtual ~StRunInfo();

    int      runId() const;
    time_t   productionTime() const;
    TString  productionVersion() const;
    double   centerOfMassEnergy() const;
    int      beamMassNumber(StBeamDirection) const;
    float    beamEnergy(StBeamDirection) const;
    float    initialBeamIntensity(StBeamDirection) const;
    float    beamLifeTime(StBeamDirection) const;
    float    beamFillNumber(StBeamDirection) const;
    double   magneticField() const;
    double   tpcDriftVelocity(StBeamDirection) const;
    double   svtDriftVelocityScaler() const;
    
    double   zdcWestRate() const;
    double   zdcEastRate() const;
    double   zdcCoincidenceRate() const;
    double   bbcCoincidenceRate() const;
    double   backgroundRate() const;
    double   l0RateToRich() const;

    double   bbcEastRate() const;
    double   bbcWestRate() const;
    double   bbcBlueBackgroundRate() const;
    double   bbcYellowBackgroundRate() const;

    int      spaceChargeCorrectionMode() const;
    float    spaceCharge() const;

    void     setRunId(int);
    void     setProductionTime(time_t);                 
    void     setProductionVersion(const char*);   
    void     setCenterOfMassEnergy(double);             
    void     setBeamMassNumber(StBeamDirection, int);  
    void     setBeamEnergy(StBeamDirection, float);
    void     setInitialBeamIntensity(StBeamDirection, float);
    void     setBeamLifeTime(StBeamDirection, float);
    void     setBeamFillNumber(StBeamDirection, float);
    void     setMagneticField(double);                  
    void     setTpcDriftVelocity(StBeamDirection, double);
    void     setSvtDriftVelocityScaler(float);

    void     setZdcWestRate(double);
    void     setZdcEastRate(double);
    void     setZdcCoincidenceRate(double);
    void     setBbcCoincidenceRate(double);
    void     setBackgroundRate(double);
    void     setL0RateToRich(double);

    void     setBbcEastRate(double);	
    void     setBbcWestRate(double);	
    void     setBbcBlueBackgroundRate(double);
    void     setBbcYellowBackgroundRate(double);

    void     setSpaceChargeCorrectionMode(int);
    void     setSpaceCharge(float);

    
protected:
    Int_t	mRunId;
    
    UInt_t      mProductionTime;
    TString     mProductionVersion;
    
    Float_t     mCenterOfMassEnergy;
    Int_t       mBeamMassNumber[2];
    
    Double_t    mMagneticFieldZ;
    Float_t     mTpcDriftVelocity[2];
    Float_t     mSvtDriftVelocityScaler;

    Double_t    mZdcEastRate;
    Double_t    mZdcWestRate;
    Double_t    mZdcCoincidenceRate;
    Double_t    mBackgroundRate;
    Double_t    mL0RateToRich;
    Double_t    mBbcCoincidenceRate;

    Float_t     mBeamEnergy[2];
    Float_t     mInitialBeamIntensity[2];
    Float_t     mBeamLifeTime[2];
    Float_t     mBeamFillNumber[2];

    Double_t    mBbcEastRate;
    Double_t    mBbcWestRate;
    Double_t    mBbcBlueBackgroundRate;
    Double_t    mBbcYellowBackgroundRate;

    Int_t       mSpaceChargeCorrectionMode;
    Float_t     mSpaceCharge;
    
    ClassDef(StRunInfo,7)
};
#endif
