/**********************************************************
 * $Id: StRichMaterialsDb.h,v 2.2 2001/04/10 16:56:06 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMaterialsDb.h,v $
 *  Revision 2.2  2001/04/10 16:56:06  lasiuk
 *  Change parameters to bring into line with richgeo.g and CERN.
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 **********************************************************/

#ifndef ST_RICH_MATERIALS_H
#define ST_RICH_MATERIALS_H

#include "StRichMaterialsDbInterface.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StRichMaterialsDb : public StRichMaterialsDbInterface {
public:

    static StRichMaterialsDb* getDb();
  
    double meanWavelength();
    double shortestWavelength();
    double longestWavelength();
    double meanRadiatorDepth();  
    double innerWavelength();
    double outerWavelength();
    void   setWavelengthRange(double, double);

    // C6F14    
    double indexOfRefractionOfC6F14At(double wavelength) const;
    double absorptionCoefficientOfC6F14At(double wavelength) const;
  
    // quartz 
    double indexOfRefractionOfQuartzAt(double wavelength) const;
    double absorptionCoefficientOfQuartzAt(double wavelength) const;
  
    // methane
    double indexOfRefractionOfMethaneAt(double wavelength) const;
    double absorptionCoefficientOfMethaneAt(double wavelength) const;
  
    double quantumEfficiencyOfCsIAt(double wavelength) const;
    
    double version() const;
    void   print(ostream& os = cout) const;
    
protected:
    StRichMaterialsDb();
    
private:
    void my_fill();             // fill with my own stuff
    
    static StRichMaterialsDb* p2Db;   // handle to only instance

    bool boundsCheck(double)       const;
    double convertToEnergy(double) const;
    double whichBin(double)        const;

    double mVersion;
    double mLongestWavelength;
    double mShortestWavelength;
    double mMeanWavelength;  
    double mInnerWave;
    double mOuterWave;

    double mMeanRadiatorDepth;

    static double mHc;
    vector<double> mEnergy;
    vector<double> mFreonN;
    vector<double> mFreonL;
    vector<double> mQuartzN;
    vector<double> mQuartzL;
    vector<double> mCsIQE;
    double mMethaneIndexOfRefraction;
    double mMethaneAbsCoeff;
    
    double mBinSize;
};

inline double StRichMaterialsDb::meanWavelength() { return mMeanWavelength;}
inline double StRichMaterialsDb::longestWavelength() {return mLongestWavelength;}
inline double StRichMaterialsDb::shortestWavelength() {return mShortestWavelength;}
inline double StRichMaterialsDb::meanRadiatorDepth() {return mMeanRadiatorDepth;}
inline double StRichMaterialsDb::innerWavelength() {return mInnerWave;}
inline double StRichMaterialsDb::outerWavelength() {return mOuterWave;}
inline double StRichMaterialsDb::version() const {return mVersion;}
#endif
