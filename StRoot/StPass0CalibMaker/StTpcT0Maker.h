/*!
 * \class StTpcT0Maker 
 * \author David Hardtke
 * \version $Id: StTpcT0Maker.h,v 1.1 2002/01/26 18:55:33 jeromel Exp $
 *
 * StTpcT0Maker virtual base class for Maker  
 *
 */

// $Log: StTpcT0Maker.h,v $
// Revision 1.1  2002/01/26 18:55:33  jeromel
// StTpcT0Maker moved from directory of the same name. First version
// of StVertexSeedMaker.
//
// Revision 1.8  2001/09/25 20:10:30  hardtke
// Add ad hoc correction factor to take care of bias suggested by TRS
//
// Revision 1.7  2001/04/17 23:54:51  hardtke
// add z Vertex contraint -- default to +-40cm
//
// Revision 1.6  2001/03/15 19:49:02  hardtke
// Add diagnostic ntuple t0hist file
//
// Revision 1.5  2001/03/09 22:44:43  hardtke
// Add vertex diagnostic histograms, create root file with these histograms 
// by default
//
// Revision 1.4  2000/09/11 17:48:51  hardtke
// save values of trig offset, dvel, and tpc length for use in Finish()
//
// Revision 1.3  2000/08/28 19:53:46  hardtke
// change default name to tpc_t0
//
// Revision 1.2  2000/08/28 17:42:29  hardtke
// Add new histogram
//
// Revision 1.1  2000/08/24 23:51:27  hardtke
// New package for drift velocity calibrations
//
#ifndef STAR_StTpcT0Maker
#define STAR_StTpcT0Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TNtuple.h"
class StTpcDb;
class St_tpcDriftVelocity;


class StTpcT0Maker : public StMaker {
 public: 
                  StTpcT0Maker(const char *name="TpcT0");
   virtual       ~StTpcT0Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void PrintInfo();
   virtual void Clear(Option_t *option);
   virtual Int_t Finish();
   float   AverageT0();
   int GetValidityDate();
   int GetValidityTime();
   St_tpcDriftVelocity* driftTable();
   void WriteTableToFile();     //Write drift velocity table (assumes correct trigger offset)
   void SetMinEntries(int entries);  //minimum number of valid events for t0
   void SetDesiredEntries(int entries);  //desired number of valid events for t0
   void SetMaxRMS(float RMS);  //maximum allowed RMS for t0 histogram 
   void WriteHistFile();       // Write out t0hist.root file with results
   void HistFileByDefault();   // Write out file on Finish
   void SetVertexZmax(float zmax);  //Set max z vertex for t0 calculation
   void SetVertexZmin(float zmin);  //Set min z vertex for t0 calculation
   void SetCorrectionFactors(float constant, float linear, float quadratic); //del z correction factors from trs
   float GetCorrection(float z);  // Get Correction Factor for this Z
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcT0Maker.h,v 1.1 2002/01/26 18:55:33 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:

 private:
  TH1F* t0result;
  TH1F* t0guessError;
  TH1F* zVertexDiff;
  TH1F* yVertexDiff;
  TH1F* xVertexDiff;
  TNtuple* resNtuple;
  float t0guess;
  float t0current;
  float dvel_assumed;
  float trigger_assumed;
  float length_assumed;
  float zVertexWest;
  float zVertexEast;
  float yVertexWest;
  float yVertexEast;
  float xVertexWest;
  float xVertexEast;
  float multEast;
  float multWest;
  float eventNumber;
  float T0HIST_MIN;
  float T0HIST_MAX;
  float zVertexMax; //maximum allowed z vertex for t0 calculation
  float zVertexMin; //minimum allowed z vertex for t0 calculation
  float CorrFac[3];
  StTpcDb* theDb; //!
  int    date;
  int    time;
  int    minEntries;
  int    desiredEntries;
  float    maxRMS;
  Bool_t   mHistOut;

   ClassDef(StTpcT0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif





