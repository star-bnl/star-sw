/*!
 * \class StRunInfo 
 * \author Thomas Ullrich, Sep 2001
 */
/***************************************************************************
 *
 * $Id: StRunInfo.h,v 2.4 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunInfo.h,v $
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

#include <ctime>
#include "StObject.h"
#include "TString.h"
#include "StEnumerations.h"

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
    int      beamCharge(StBeamDirection) const;
    double   magneticField() const;
    double   tpcDriftVelocity(StBeamDirection) const;

    double   zdcWestRate() const;
    double   zdcEastRate() const;
    double   zdcCoincidenceRate() const;
    double   bbcCoincidenceRate() const;
    double   backgroundRate() const;
    double   l0RateToRich() const;

    void     setRunId(int);
    void     setProductionTime(time_t);                 
    void     setProductionVersion(const char*);   
    void     setCenterOfMassEnergy(double);             
    void     setBeamMassNumber(StBeamDirection, int);  
    void     setBeamCharge(StBeamDirection, int);      
    void     setMagneticField(double);                  
    void     setTpcDriftVelocity(StBeamDirection, double);

    void     setZdcWestRate(double);
    void     setZdcEastRate(double);
    void     setZdcCoincidenceRate(double);
    void     setBbcCoincidenceRate(double);
    void     setBackgroundRate(double);
    void     setL0RateToRich(double); 
    
protected:
    Int_t	mRunId;
    
    UInt_t      mProductionTime;
    TString     mProductionVersion;
    
    Float_t     mCenterOfMassEnergy;
    Int_t       mBeamMassNumber[2];
    Int_t       mBeamCharge[2];
    
    Double_t    mMagneticFieldZ;
    Float_t     mTpcDriftVelocity[2];

    Double_t    mZdcEastRate;
    Double_t    mZdcWestRate;
    Double_t    mZdcCoincidenceRate;
    Double_t    mBackgroundRate;
    Double_t    mL0RateToRich;
    Double_t    mBbcCoincidenceRate;

    ClassDef(StRunInfo,3)
};
#endif
