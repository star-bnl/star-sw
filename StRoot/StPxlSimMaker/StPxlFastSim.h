/*
 * $Id: StPxlFastSim.h,v 1.8 2017/10/19 19:38:17 jeromel Exp $
 *
 * Author: M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.h,v $
 * Revision 1.8  2017/10/19 19:38:17  jeromel
 * Merging PXL201709UPD back to MAIN
 *
 * Revision 1.7.8.1  2017/09/11 19:20:54  jeromel
 * No changes, checking branch
 *
 * Revision 1.7  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.5  2015/01/27 01:31:09  smirnovd
 * Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
 *
 * Revision 1.4  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.1  2013/05/12 21:43:33  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.4  2013/05/03 15:08:19  mstftsm
 *
 */

/**
   \class StPxlFastSim

   \brief Class to simulate PXL hits from Monte Carlo.

   This class has the responsibility to create StPxlHit objects and store them in
   StPxlHitCollection.

   StPxlHit is a Gaussian smeared StMcPxlHit.
   The smearing parameters are fetched from Calibrations/tracker/PixelHitError*

   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StPxlFastSim
#define STAR_StPxlFastSim

#include "StPxlISim.h"
class StRandom;
class StPxlDb;
class TObjectSet;

class StPxlFastSim: public StPxlISim
{
 public:

  /*! \brief Constructor */
  StPxlFastSim(const Char_t *name="pxlFastSim",Bool_t randomSeed=kFALSE): StPxlISim(name), mPxlDb(0), mRandom(0), mResXPix(0), mResYPix(0), mResZPix(0), mUseRandomSeed(randomSeed) {}

  /*! \brief This class does not own any hit containers.
   *        mRandom is deleted here.
  */
  ~StPxlFastSim();


  /*! \brief A random seed is passed to mRandom
   * PXL smearing resolutions (PixelHitError) are fetched from calib_db.
   *
   * returns kStOk if resolutions have been fetched successfully. kStErr otherwise.
   */
  Int_t initRun(const TDataSet& calib_db, const TObjectSet* pxlDbDataSet,const Int_t run);

   /*! \brief creates an StPxlHit object for every StMcPxlHit, and fills the
   *  hit StPxlHitCollection container.
   *
   *  Returns:
   *  kStOk: if hits have been loaded to StPxlHitCollection successfully.
  */
  Int_t addPxlHits(const StMcPxlHitCollection&, StPxlHitCollection&);

  /*! \brief Documentation method. GetCVS can be called from the chain, providing a list
   *  of all maker versions in use.
  */

 virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPxlFastSim.h,v 1.8 2017/10/19 19:38:17 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 private:
  //Routine to smear hit by resolution with gaussian, mean zero and width res.
  double distortHit(const double x, const double res, const double constraint) const;

  void localToMatser(Double_t* local,Double_t* master,Int_t sector,Int_t ladder,Int_t sensor);

 private:
  StPxlDb* mPxlDb;
  StRandom* mRandom;

  Double_t mResXPix;
  Double_t mResYPix;
  Double_t mResZPix;

  Bool_t mUseRandomSeed;
};
#endif
/*
 * $Id: StPxlFastSim.h,v 1.8 2017/10/19 19:38:17 jeromel Exp $
 *
 * Author: M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlFastSim.h,v $
 * Revision 1.8  2017/10/19 19:38:17  jeromel
 * Merging PXL201709UPD back to MAIN
 *
 * Revision 1.7.8.1  2017/09/11 19:20:54  jeromel
 * No changes, checking branch
 *
 * Revision 1.7  2015/03/13 18:45:01  perev
 * Roll back
 *
 * Revision 1.5  2015/01/27 01:31:09  smirnovd
 * Minor refactoring of StPxlFastSim::distortHit() to include a new warning for unphysical hit position
 *
 * Revision 1.4  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/03/13 17:00:19  mstftsm
 * StPxlSimMaker has a method to switch on random seed for StRandom generatos in simulators. Default is not a random seed.
 *
 * Revision 1.1  2013/05/12 21:43:33  jeromel
 * Initial revision, code peer review closed 2013/05/06
 *
 * Revision 1.4  2013/05/03 15:08:19  mstftsm
 *
 */

