/*
 * Author: M. Mustafa
 *
 */
/**
   \class StPxlDigmapsSim

   \brief STAR wrapper for DIGMAPS package.

   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StPxlDigmapsSim
#define STAR_StPxlDigmapsSim

#include "StPxlUtil/StPxlConstants.h"
#include "StPxlISim.h"
#include "TVector3.h"
class StPxlDb;
class StMcPxlHit;
class StMcTrack;
class TObject;
class TRandom3;
class TF1;
class DIGPlane;
class DIGADC;
class DIGTransport;
class DIGEvent;

class StPxlDigmapsSim: public StPxlISim
{
  public:

    StPxlDigmapsSim(const Char_t *name = "pxlDigmapsSim");
    /*! \brief This class does not own any hit containers.
     *        mRandom is deleted here.
     */
    virtual ~StPxlDigmapsSim();

    virtual Int_t initRun(TDataSet const& calib_db, TObjectSet const* pxlDbDataSet, Int_t run);

    /*! \brief creates an StPxlHit object for every StMcPxlHit, and fills the
     *  hit StPxlHitCollection container.
     *
     *  Returns:
     *  kStOk: if hits have been loaded to StPxlHitCollection successfully.
     */
    virtual Int_t addPxlRawHits(StMcPxlHitCollection const& in, StPxlRawHitCollection& out);

    /*! \brief Documentation method. GetCVS can be called from the chain, providing a list
     *  of all maker versions in use.
     */
    virtual const char *GetCVS() const
    {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDigmapsSim.h,v 1.1.2.1 2017/09/11 20:15:14 dongx Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
    }

  private:
    void fillDigmapsEvent(int, StMcPxlHit const*, DIGEvent&) const;
    void calculateIncidencePositions(int, StMcPxlHit const*, TVector3&, TVector3&) const;
    float calculateDepositedEnergy(float totalLength, float betagamma) const;
    float betaGamma(StMcTrack const*) const;
    double dEdxvsBGNorm(double* x, double* par);
    bool goodPixel(int sec, int lad, int sen, int ix, int iy) const;
    bool goodSensor(int sec, int lad, int sen) const;

    TRandom3* mRndGen;
    DIGPlane* mDigPlane;
    DIGADC* mDigAdc;
    DIGTransport* mDigTransport;

    StPxlDb* mPxlDb;

    //! sensor good status range
    short mSensorGoodStatusMin;
    short mSensorGoodStatusMax;

    //! row and column good status
    short mRowColumnGoodStatus;

    //! energy landaulaw parameter
    float mEnergyLandauMean;
    float mEnergyLandauSigma;
    double mScalePar[10]; // parameters to function mdEdxvsBGNorm
    
    float mResAddX;  // additional resolution contribution from mechanical vibration and calibration precision
    float mResAddZ;  // additional resolution contribution from mechanical vibration and calibration precision
    // These two offset values are initialized per run according to the resolution above, fixed for each sensor in each run
    float mOffsetX[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder];
    float mOffsetZ[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder]; 
    TF1* mdEdxvsBGNorm;  // dEdx vs. beta*gamma function normalized to MIP - for momentum and particle species depenence
};
#endif
