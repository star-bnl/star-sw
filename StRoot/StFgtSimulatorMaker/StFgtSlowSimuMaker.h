// $Id: StFgtSlowSimuMaker.h,v 1.3 2014/08/06 11:43:13 jeromel Exp $


/*!
 * \class StFgtSlowSimuMaker        
 * \author Jan Balewski, Wei-Ming Zang
 *

 FGT slow simu code allows processing of Geant hits from simulation thrugh semi-realistic FGT detector response package and addition of real pedestals for the subset of fired strips.

Most of parameters are obtined from the dedicated DB table:
"Calibrations/fgt/fgtSimuParams"

Those are the valuse loaded to DB:

    table.param[0] =102;// fgt-simu-setup version # 

    // analog signal propagation
    table.param[1] = 9992; // meLossTab[9993-10000]=5.0E+03 to 6.0E+06 (too high) are cut off. They are replaced by meLossTab[9992]=4.78E+03 in simulation. 
    table.param[2] = 40. ; //( ions/cm) # of primary pairs produced per cm of path
    table.param[3] = 80.e-9; //  (seconds) track ignored by FGT slow sim
    table.param[4] = 0.035 ; // cm, for 2D gauss smearing, default=0.035 for FNAL
    table.param[5] = 0.005; // (GeV) track ignored by FGT slow sim
    table.param[6] = 0.017; //  (cm per 1 cm of path)
    table.param[7] = 6; // # of bins in 2D distribution to store 2D gauss

    // digitalization
    table.param[10] = 5.0;  //  drop strips below it
    table.param[11] = 1000; // in a.u. used in simu
    table.param[12] = 2.0;  // a factor making simulated ADCs comparable to 2012 pp510 data 
    table.param[13] = 0.45; // divide charge between P/R plane

    They are mapped to real variables in the InitRun()

    If DB fails to deliver reasonable params fgt-slows-simu flips to the locked state, par_badSetup!=0, and ignores FGT data for the whole job.


    Steps of data processing:

    -#  unpack_g2t_hits() : converts hardcoded geant volumes to FGT official indexing, filters every track segment passimg thrugh the sensitve GEM volume.
     The accepted track segements are stored in 6x4=24 independent, internal lists: mG2tHitList[iDisc][iQuad].push_back(aux)

    -# The analog part of the 'slow simulation' is performed independently for each quadrant, using track segemnt lists as input. The working horse is 
  responseMipModel( len, dir) - taking as input only lenght and direction of each track segment.
       *********************************************************************
       *********************************************************************
       WARN: this code treats all partiles as MIPs or drops them 
       if they  have too low. It is not bad for high pT leptons from W, but for 
       soft tracks in a jet it generates too few ion pairs and simulated 
       amplitude may be many times too small. 
       This should be improved in the next wave of FGT slow simu tinning.
       September 2011, Jan B.
       *********************************************************************
       **********************************************************************


  responseMipModel(  generates realistic distribution of primary ionization pairs along the track path, using meLossTab[]. The details are described in Frank's thesis: http://drupal.star.bnl.gov/STAR/system/files/2001-F.Simon-diploma.pdf
chapter 3. "The GEM Concept" .
 
Individual pairs are amplified according to the distance to the 1st GEM foil and their location is displaced transversely using Gauss distribution.
*** This model does not simulate 7 time bins ***
  Finally, the addHit() stores the total charge of given pair as 2D gaussion in the target high resolution histogram quadDigitizationXY, covering area of the whole quadrant with 2D bins 0.2mmx0.2mm.
This finishes the analog part of fgt-slow-simu.

  -# Digitalizaton process, executed by  projectQuad2strips(),  projects of 2D charge in to two 1D plaines of strips.  The external methods: 
  Int_t iRadID=StFgtGeom::rad2LocalStripId(r,phi,&binFrac);
  Int_t iPhiID=StFgtGeom::phi2LocalStripId(r,phi,&binFrac);
provide mapping from 2D plane location to strip ID. 
  The charge accumlated in all bins of 2D histo (above some threshold) are project in to two planes.
 *** R & Phi planes consist of strips overlaping 100%, charge sharing is set by single DB param ***    
 *** It is known the spacer grid implement in GEANT results with less charge absorbtion than for FGT as build. ***


  -# Storing of fired strips in to StFgtStrip-collection is done by  exportStripPlane2StEvent(). There is a threshold below which strips are ignoted. To help find clusters up to 3 strips after each fired strips are stored.

  The switch_addPeds allows adding pedestals to the select strips. If activated, it checks for bad pedestal and drops such strips.

   
*/
 
#ifndef STAR_StFgtSlowSimuMaker
#define STAR_StFgtSlowSimuMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtUtil/StFgtConsts.h"
#include <TVector3.h>

class St_g2t_fgt_hit;
class g2t_fgt_hit_st;
class TH2F;
class TH1F;

#include <TRandom3.h>
class StFgtDisc;
class StFgtDb;

class StFgtStripCollection;


//#define __FGT_QA_HISTO__
/* enable this switch to generate many QA histos. 
Note, you need to pass pointer to the valid  TObjArray top get them out of this maker.
In bfc.C you should have:
  StFgtSlowSimuMaker    *myMkSM =  get valid pointer....
   HList=new  TObjArray;
   myMkSM->setHList(HList); 
.....
after event loop save them:
  TString &fnameOut1=chain->GetFileOut();  TString fnameOut2=fnameOut1;
  fnameOut2.ReplaceAll(".root",".fgt");
  myMkSM->saveHisto(fnameOut2);// jan , save one maker is enough, all histos are merged
*/


///.... utility c-struct for g2t hits
struct fgt_g2t_auxil {
  TVector3 Rlab;/// hit entrance in LAB
  TVector3 Rloc, Dloc; /// entrance & path in local ref frame
  g2t_fgt_hit_st *hitPtr; /// the oryginal g2t hit
  Int_t iQuad; /// quadrant of the FGT DISK: [0-3]
};


class StFgtSlowSimuMaker : public StMaker {   
 protected:

 static const Float_t pulseShape[20];

 private:


  enum {mxH=32};
  TH1 *hA[mxH];
  TObjArray *HList;

  
  StFgtDb *fgtDb;

  /// working arrays
  TH2F *quadDigitizationXY; /// 2D digitization response of one quart, local REF   
  TH1F *quadDigitizationPhi; /// phi-strips response vector, one disk 
  TH1F *quadDigitizationRad; /// rad-strips response vector, one disk 

  /// same as working arrays, but not reset for for each quad and disk  WMZ
  TH2F *digXYAll;  /// 2D digitization response of all events , for monitoring
  TH2F *digPadcAll, *digRadcAll;/// for monitoring
  TH1F *digPAll, *digRAll; /// for monitoring

  Int_t mInpEve;
  Int_t mEventId;

  /// parameters of the simulator are set in InitRun() base don DB table

  Double_t   par_stripThreshAdc; ///  drop strips below it 
  Double_t   par_XYamplSigma; /// signal smearing in X-Y
 
  Int_t      par_cutoffOfBichel; /// cutoff for BichselELossProbHighBG.dat 
  Double_t   par_pairsPerCm ; /// # of primary pairs produced per cm of path 
  Double_t   par_trackTOFcutoff; ///  (seconds) track ignored by FGT slow sim
  Double_t   par_trackPcutoff; /// (GeV) track ignored by FGT slow sim

  Double_t   par_transDiffusionPerPath; /// to displace primary inization
  Int_t      par_binStep; /// # of bins in 2D distribution to store 2D gauss
  

  Double_t   par_2DampCutoffScale; /// in a.u. used in simu
  Double_t   par_overalGain; /// a factor making simulated ADCs comparable  data 
  Double_t   par_PplaneChargeFraction; /// divide charge between P/R plane

  Int_t      par_badSetup;/// general flag disabling fgt-slow-simu upon single error for the rest of the job
  Int_t      switch_addPeds; /// default 0=off, can be  re-set in bfc.C

  TRandom3* mRnd;

  /// stores the total charge of given pair as 2D gaussion in the target high resolution 2D histogram
  void   addHit(TVector3 rLocal, Double_t  ampl=1.) ;
  /// generates realistic distribution of primary ionization 
  void   responseMipModel(TVector3 Rloc, TVector3 Dloc);// detailed, w/ fluct
  /// accepted track segements
  vector<fgt_g2t_auxil> mG2tHitList[kFgtNumDiscs][kFgtNumQuads];

  void InitHisto1();
  void InitHisto2();
  void CloseHisto();

  /// converts hardcoded geant volumes to FGT official indexing
  void unpack_g2t_hits(St_g2t_fgt_hit *);
  ///  projects of 2D charge in to two 1D plaines of strips
  void projectQuad2strips( Int_t idisc, Int_t iquad);
  /// Storing of fired strips in to StFgtStrip-collection 
  void exportStripPlane2StEvent(TH1F *hInp, Int_t stripIdOffset, StFgtStripCollection *out);

 
  /* model transverse response in 1D
     Frank: However, from test beam data I calculate the RMS of each cluster, 
     and there I get something like 0.5 - 0.6 strips, which is around 350 um.
     all clusters (both in my diploma thesis and in my email) are 1D. A
     cluster is defined as something on ONE projection of the detector. So
     the FWHM and the sigma is all 1D. So you should not divide by Sqrt(2) to
     get the 1 D value.
  */

  Double_t  amplFunc(Double_t del_cm) {
    Double_t relD=del_cm/par_XYamplSigma;
    return exp(-relD*relD/2.);
  }


 public:
  StFgtSlowSimuMaker(const char *name="FgtSlowSimu");
  virtual       ~StFgtSlowSimuMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Finish(); 
  virtual Int_t  Make();
  virtual void  Clear(Option_t *option="");
  /// needed for exporting histograms
  void  setHList(TObjArray * x){HList=x;}
  /// needed to access DB tables
  void  setFgtDb(StFgtDb *x) { fgtDb=x;}
  ///  optional, allows adding of real pedestals to simulated data
  void  enableAddPeds(){switch_addPeds=1;}
  ///  optional, enforces use of specific seed for repetetive test simu
  void  setRndSeed(int x) { mRnd->SetSeed(x); } // force reproducible rnd sequence
  void  saveHisto(TString fname);

 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtSlowSimuMaker.h,v 1.3 2014/08/06 11:43:13 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
 private:
  
  ClassDef(StFgtSlowSimuMaker,0)   
};
    
#endif



// $Log: StFgtSlowSimuMaker.h,v $
// Revision 1.3  2014/08/06 11:43:13  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2012/06/20 18:32:40  avossen
// setting elec ids for strips now, implemented pulse shape over 7 timebins
//
// Revision 1.1  2012/06/06 20:35:09  jeromel
// Code  review closed (requested Anselm/Jan; reviewed Jonathan/Jason)
//

 
