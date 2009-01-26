/*
 * $Id: StPixelFastSimMaker.h,v 1.12 2009/01/26 14:50:46 fisyak Exp $
 *
 * Author: A. Rose, LBL, Y. Fisyak, BNL, M. Miller, MIT
 *
 * 
 **********************************************************
 * $Log: StPixelFastSimMaker.h,v $
 * Revision 1.12  2009/01/26 14:50:46  fisyak
 * Clean up
 *
 * Revision 1.11  2007/11/06 16:20:07  wleight
 * Digitized Pixel, removed all hit smearing, and implemented idTruth
 *
 * Revision 1.10  2007/10/18 14:25:13  didenko
 * updates for pile-up events
 *
 * Revision 1.9  2007/09/09 17:00:33  fisyak
 * Fix bug 1056
 *
 * Revision 1.8  2007/04/23 18:11:48  andrewar
 * Removed references to Hpd (includes were obsolete)
 *
 * Revision 1.7  2007/04/16 19:10:52  wleight
 * Added IST simulation (digitization but no clustering)
 *
 * Revision 1.6  2007/04/06 14:55:33  andrewar
 * Shift of HFT to face of ladder.
 *
 * Revision 1.5  2006/12/15 02:17:20  wleight
 * Ist now gets hit smearing parameters from the database
 *
 * Revision 1.4  2006/12/14 23:52:52  andrewar
 * Added Sevil's hit error db loader.
 *
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
#include <vector>
class StEvent;
class StMcEvent;
class StRandom;
class StMcPixelHitCollection;
class St_g2t_ist_hit;
class St_g2t_pix_hit;

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

 /* \brief InitRun method is not currently used. */
  virtual int InitRun( int);

  
  /* \brief Accept method for reconstructed event. */
  virtual Bool_t accept(StEvent* event);

  /* \brief Accept method for monte carlo event. */
  virtual Bool_t accept(StMcEvent* event);

  //.. load pileup hits ....
  void LoadPixPileUpHits();

  //..add PIXEL pileup into the collection
  void AddPixPileUpHit(StMcPixelHitCollection* pixHitCol);

  //Routine to smear hit by resolution with gaussian, mean zero and width res
  double distortHit(double x, double res, double detLength);

  /* \brief Method for adding a gaussian smearing to the hit error vector */
  void smearGaus(StThreeVectorD &mError, double sigma1, double sigma2);

  /* \brief Documentation method. GetCVS can be called from the chain, providing a list
     of all maker versions in use.
  */
  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StPixelFastSimMaker.h,v 1.12 2009/01/26 14:50:46 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  void shiftHit(StThreeVectorF &pos, StThreeVectorF &mom, int, int);
  int sector(int,int);
  int secLadder(int,int);
  double phiForLadder(int,int);


 protected:
  StRandom* myRandom;

  double resXIst1;
  double resZIst1;
  double resXIst2;
  double resZIst2;
  double resXIst3;
  double resZIst3;
  double resZPix;
  double resXPix;
  int mSmear; //to turn smearing on and off

 protected:
  vector<double> pileup_x; 
  vector<double> pileup_y; 
  vector<double> pileup_z; 
			
  vector<float> pileup_px;
  vector<float> pileup_py;
  vector<float> pileup_pz;

  vector<int> pileup_key;
  vector<int> pileup_vid;

  vector<float> pileup_de;
  vector<float> pileup_ds;
  
  bool pileup_on;  //.. true: there's a pile file on the disk and will do
		   //.. pileup simulation. 
		   //.. false: there's no pile up file on the disk and will 
		   //.. do regular production


  ClassDef(StPixelFastSimMaker,1)   //StAF chain virtual base class for Makers
};

struct stripHit{
	double localX;
	double e;
};

struct istStrip{
	vector<stripHit> stripHits;
	double intercept;
};

#endif



