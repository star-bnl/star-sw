// $Id: StTpcT0Maker.h,v 1.6 2001/03/15 19:49:02 hardtke Exp $
// $Log: StTpcT0Maker.h,v $
// Revision 1.6  2001/03/15 19:49:02  hardtke
// Add diagnostic ntuple t0hist file
//
// Revision 1.5  2001/03/09 22:44:43  hardtke
// Add vertex diagnostic histograms, create root file with these histograms by default
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

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcT0Maker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
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

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StTpcT0Maker.h,v 1.6 2001/03/15 19:49:02 hardtke Exp $ built "__DATE__" "__TIME__ ; return cvs;}

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
