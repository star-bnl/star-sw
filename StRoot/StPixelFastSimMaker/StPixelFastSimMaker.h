/*
 * $Id: StPixelFastSimMaker.h,v 1.3 2006/11/29 21:42:20 andrewar Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.h,v $
 * Revision 1.3  2006/11/29 21:42:20  andrewar
 * Update with Pixel resolution smearing.
 *
 * Revision 1.2  2006/11/28 21:29:13  wleight
 * Added smearing for Hpd and Ist and a switch to turn it on and off
 *
 * Revision 1.1  2006/02/03 20:11:57  fisyak
 * The initial revision
 *
 *
 */

/**
   \class StPixelFastSimMaker

   \brief Class to simulate Pixel and Ist hits from Monte Carlo.

   The HFT and IST hits slow simulator will evolve over time. This class
   has responsibility for creating StHit objects and storing them in the
   appropriate container. The created container is then added to the
   reconstructed event.

   Currently (Jan, 2006), we simply store the perfect Monte Carlo position
   for each hit. This will change as the simulations become more complex.
   In particular, for the HFT gaussian smearing will be introduced as a
   first approximation. For the IST, a more complicated simulator is needed
   to account for ambiguities in hit recconstruction. That is not the
   purpose of this maker.

   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StPixelFastSimMaker
#define STAR_StPixelFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

class StEvent;
class StMcEvent;
class StRandom;

class StPixelFastSimMaker : public StMaker {
 public:

  /* \brief Constructor uses standard Maker text naming convention,
     with value "PixelFastSim"*/
  StPixelFastSimMaker(const char *name="PixelFastSim") :StMaker(name){}

  /* Please note: The destructor is empty. StEvent will own any hits
     created by this maker, and is responsible for cleanup.
  */
  virtual       ~StPixelFastSimMaker();


  /* \brief This is called every event, and contains all the activity
     of making StHit objects.

     Make() creates an StRnDHit object for every MChit, and fills the
     hit container. Hit container is passed to StEvent.

     Returns kStOk always.
  */
  virtual Int_t  Make();

  /* \brief Init method is not currently used. */
  virtual int Init();

  /* \brief Clear method is not currently used. */
  virtual void Clear(Option_t *option="");

  /* \brief Finish method is not currently used. */
  virtual int Finish();
  
  /* \brief Accept method for reconstructed event. */
  virtual Bool_t accept(StEvent* event);

  /* \brief Accept method for monte carlo event. */
  virtual Bool_t accept(StMcEvent* event);

  //Routines for transforming HPD hits
  StThreeVectorF local2GlobalHpd(const StThreeVectorF& local, int ladder);
  StThreeVectorF global2LocalHpd(const StThreeVectorF& global, int ladder);

  //Routine to smear hit by resolution with gaussian, mean zero and width res
  double distortHit(double x, double res, double detLength);

  /* \brief Method for adding a gaussian smearing to the hit error vector */
  void smearGaus(StThreeVectorD &mError, double sigma1, double sigma2);

  /* \brief Documentation method. GetCVS can be called from the chain, providing a list
     of all maker versions in use.
  */
  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StPixelFastSimMaker.h,v 1.3 2006/11/29 21:42:20 andrewar Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

 protected:
  StRandom* myRandom;
  double resXHpd;
  double resZHpd;
  double tiltAngleHpd;
  double rotAngleHpd;
  double offsetAngleHpd;
  double radiusHpd;
  double waferLengthHpd;
  double ladderWidthHpd;
  int mSmear; //to turn smearing on and off
  
  ClassDef(StPixelFastSimMaker,0)   //StAF chain virtual base class for Makers
};
#endif



