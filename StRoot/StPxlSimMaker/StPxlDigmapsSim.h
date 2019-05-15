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

    /*! \brief initRun function to read in DB entries for slow simulator parameters and masking tables.
     */
    virtual int initRun(TDataSet const& calib_db, TObjectSet const* pxlDbDataSet, Int_t run);
    
    /*! \brief set own random seed if needed, default to use gRandom
     */
    void setSeed(unsigned int seed) { mOwnRndSeed = seed; }

    /*! \brief Documentation method. GetCVS can be called from the chain, providing a list
     *  of all maker versions in use.
     */
    virtual const char *GetCVS() const
    {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDigmapsSim.h,v 1.3 2018/03/15 21:37:42 dongx Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
    }

  private:
    /*! \brief main class to take MC hit, create DIGMAPS event and generate cluster pixels in DIGMAPS.
     */
    void fillDigmapsEvent(int, StMcPxlHit const*, DIGEvent&) const;
    /*! \brief calculate the in and out positions for a MC hit in the PXL epitaxial volume
     */
    void calculateIncidencePositions(int, StMcPxlHit const*, TVector3&, TVector3&) const;
    /*! \brief re-sample the deposite energy loss in PXL epitaxial volume, correct for betagamma
     *  using the PDG dependence, valid for betagamma upto 50000
     */
    float calculateDepositedEnergy(float totalLength, float betagamma) const;
    /*! \brief calculate beta*gamma for a given McTrack
     */
    float betaGamma(StMcTrack const*) const;
    /*! \brief betagamma dependence function parametrized using the dependence taken from PDG
     *  normalized to the minimum dE/dx
     */
    double dEdxvsBGNorm(double* x, double* par);
    /*! \brief apply the hotPixel masking table
     */
    bool goodPixel(int sec, int lad, int sen, int ix, int iy) const;
    /*! \brief apply the sensorStatus table
     */
    bool goodSensor(int sec, int lad, int sen) const;
    /*! \brief creates an StPxlHit object for every StMcPxlHit, and fills the
     *  hit StPxlHitCollection container.
     *
     *  Returns:
     *  kStOk: if hits have been loaded to StPxlHitCollection successfully.
     */
    virtual int addPxlRawHits(StMcPxlHitCollection const& in, StPxlRawHitCollection& out);

    //! random generator for dE/dx energy re-sampling
    TRandom3* mRndGen;
    unsigned int mOwnRndSeed;
    
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
    //
    short mHitEffMode;
    float mMomCut;      // momentum cut for momentum depenent efficiency
    float mHitEffInner; // single hit efficiency - 0.97 (best knowledge seen from ZF cosmic ray test)
    float mHitEffOuter; // single hit efficiency - 0.97 (best knowledge seen from ZF cosmic ray test)
};
#endif
