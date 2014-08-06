// $Id: StSsdDaqMaker.h,v 1.9 2014/08/06 11:43:42 jeromel Exp $
//
// $Log: StSsdDaqMaker.h,v $
// Revision 1.9  2014/08/06 11:43:42  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2008/04/16 20:06:30  fisyak
// rename maker to be compline with StSsdPointMaker expectations
//
// Revision 1.7  2007/04/04 01:20:33  bouchet
// Cosmetic changes to print the active ladders according to the ssdConfiguration Table
//
// Revision 1.6  2007/03/22 01:58:17  bouchet
// add a method to fill pedestal and noise of the strips in a tuple
//
// Revision 1.5  2007/02/25 18:10:58  bouchet
// Remove a histogram and add more precise histograms for the particular status of strips : count the number of strips per wafer where pedestal = 0 or 255 (adc) and rms = 0 or 255 (adc)
//
// Revision 1.4  2007/02/18 15:57:33  bouchet
// New Logger update and addition of Finish() function
//
// Revision 1.3  2005/06/16 14:27:00  bouchet
// Pedestal Histos are filled in this Maker
//
// Revision 1.2  2005/05/11 13:47:30  reinnart
// No connection between StSsdDaqMaker and StSsdDbMaker any more
//
// Revision 1.1  2005/04/15 15:11:21  lmartin
// StSsdDaqMaker
//
#ifndef STAR_StSsdDaqMaker
#define STAR_StSsdDaqMaker

/*!
 *                                                                     
 * \class  StSsdDaqMaker
 * \author Martin,Reinnarth,Bouchet
 * \date   2004/11/03
 * \brief  Class to read SSD data from DAQ files
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StSsdDaqMaker Class to read SSD data from DAQ files 
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).
class StDAQReader;
class StSsdConfig;
class St_ssdConfiguration;
class ssdConfiguration_st;
class St_spa_strip;
class St_ssdPedStrip;
class TH1F;
class TH1S;
class TH2S;
class TFile;
class TNtuple;
class StSsdDaqMaker : public StMaker {
 private:
  // Private method declaration if any
  // St_spa_strip   *spa_strip;
  // St_ssdPedStrip *ssdPedStrip;
  Float_t PedestalNTuple[10];
  TFile *pFile;
  TNtuple *pTuple;
  void DeclareNTuple(); 
  void PrintConfiguration(Int_t runumber,ssdConfiguration_st *config);
 protected:
  // Protected method if any
  StSsdConfig*  mConfig;
  TH2S *occupancy_wafer; //occupancy  per wafer for the ladders
  TH2S *occupancy_chip;  //occupancy per chip for the ladders
  TH2S *noise_chip;      //mean noise per chip
  TH2S *noise_wafer;     //mean noise per wafer
  TH2S *noise_chip_P;    //mean noise per chip of the P Side
  TH2S *noise_chip_N;    //mean noise per chip of the N Side
  TH2S *pedestal_chip;   //pedestal per chip for the ladders
  TH1F *occupancy;       //number of inactives strips per ladder
  TH2S *ped_zero_ladP;   //number of strips of p-side wafers for which pedestal = 0
  TH2S *ped_zero_ladN;   //number of strips of n-side wafers for which pedestal = 0
  TH2S *ped_high_ladP;   //number of strips of p-side wafers for which pedestal = 255
  TH2S *ped_high_ladN;   //number of strips of n-side wafers for which pedestal = 255
  TH2S *noise_zero_ladP; //number of strips of p-side wafers for which noise = 0
  TH2S *noise_zero_ladN; //number of strips of n-side wafers for which noise = 0
  TH2S *noise_high_ladP; //number of strips of p-side wafers for which noise = 255
  TH2S *noise_high_ladN; //number of strips of n-side wafers for which noise = 255
  int  mPedOut;          //to turn the fill of TNtuple on and off
 public: 
  StSsdDaqMaker(const char *name="SpaStrip");
  virtual       ~StSsdDaqMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int runumber);
  virtual Int_t Make(); 
  virtual Int_t Finish();
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSsdDaqMaker.h,v 1.9 2014/08/06 11:43:42 jeromel Exp $ built " __DATE__ " " __TIME__; 
    return cvs;
  }

  ClassDef(StSsdDaqMaker,0)   //StAF chain virtual base class for Makers
};

#endif


