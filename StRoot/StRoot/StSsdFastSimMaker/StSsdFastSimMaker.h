 /*
   \class StSsdFastSimMaker

   \brief Class to simulate Ssd from GEANT.

   Currently, we read in the SSD hits from the GEANT hit table, smear the hit on the wafer, and fill the SSD Hit Collection with the smeared points.
   This class conforms to the STAR StMaker standards.
*/

#ifndef STAR_StSsdFastSimMaker
#define STAR_StSsdFastSimMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StSsdUtil/StSsdBarrel.hh"
#include "StSsdUtil/StSsdWafer.hh"
class StEvent;
class StMcEvent;
class StRandom;
class TH1S;
class TH2S;
class TH2F;

class StSsdBarrel;
class St_ssdDimensions;
class St_ssdWafersPosition;
class slsCtrl_st;
class St_slsCtrl;
class StSsdBarrel;
class St_g2t_ssd_hit;
class St_g2t_svt_hit;
class St_sls_strip;
class ssdConfiguration_st;
class StSsdHit;
class StSsdHitCollection;
class StSsdFastSimMaker : public StMaker {
 private :
  StSsdBarrel           *mySsd;//!
  St_ssdDimensions      *m_dimensions;//!
  St_ssdWafersPosition  *m_positions; //!
  ssdConfiguration_st   *m_config;    //!
  St_slsCtrl            *m_ctrl;      //!
  StSsdHit              *mHit;
 public:
  StSsdFastSimMaker(const char *name="SsdFastSim");   
  /* Please note: The destructor is empty. StEvent will own any hits
     created by this maker, and is responsible for cleanup.
  */
  virtual       ~StSsdFastSimMaker();
  /* \brief This is called every event, and contains all the activity
     of making StHit objects.
     
     Make() creates an StSsdHit object for every MChit, and fills the
     hit container. Hit container is passed to StEvent.
     Returns kStOk always.
  */
  virtual Int_t Make();
  /* \brief Read in SSD Hit from Table, smear hit, and fill SSD Hit Collection */
  virtual Int_t Init();
  /* \brief Define variables and histograms */
  virtual Int_t InitRun(int);
  /* \brief Define Hit Errors and Geometry Parameters */
  virtual void Clear(Option_t *option="");
  /* \brief Finish method is not currently used. */
  virtual Int_t Finish();
  /* \brief Accept method for reconstructed event. */
  virtual Bool_t accept(StEvent* event);
  //Routine to smear hit by resolution with gaussian, mean zero and width res
  Float_t distortHit(Float_t x, double res);
  void setSsdParameters(ssdDimensions_st *geom_par);  
  void printSsdParameters();  
  Int_t     idWaferToWaferNumb(Int_t idWafer); //  idwafer = layer*1000+waf*100+ladder => waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1
  Int_t     idWaferToLadderNumb(Int_t idWafer);//  idwafer => ladder-1
  Int_t     waferNumbToIdWafer(Int_t waferNumb);// waferNumb = mNWaferPerLadder*(ladder-1) + waf - 1 => idwafer
  Int_t     idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  Int_t     IsOnWafer(Float_t x, Float_t y);
  Int_t     RemoveTriangle(Float_t x, Float_t y);
 
 private :
  ssdDimensions_st *mDimensions;
  Int_t    mSsdLayer;
  Int_t    mNLadder;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Int_t    mActiveLadders[20];
  Float_t  mDetectorLargeEdge;
  Float_t  mDetectorSmallEdge;
  Float_t  mStripPitch;
  Float_t  mTheta;
  
 protected:
  StRandom* myRandom;
  double    mResXSsd;
  double    mResZSsd;
  StSsdHitCollection   *mCol;   
  StEvent   *mEvent;
  int       mSmear;    //to turn smearing on and off
  TH1S      *WaferNumb; //id of wafer where there are hits 
  TH2S      *HitsMap;   //wafer (y_axis :1 to 16) vs ladder (x_axis: 1 to 20)
  TH1F      *dX;        //x-x(smeared) in cm
  TH1F      *dY;        //y-y(smeared) in cm
  TH1F      *dZ;        //z-z(smeared) in cm
  TH2F      *Local;
  TH1F      *Ratio;


/* \brief Documentation method. GetCVS can be called from the chain, providing a list
   of all maker versions in use. */
 virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:  $ $Id: StSsdFastSimMaker.h,v 1.5 2014/08/11 19:27:09 bouchet Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StSsdFastSimMaker,0)   //StAF chain virtual base class for Makers
};
#endif
