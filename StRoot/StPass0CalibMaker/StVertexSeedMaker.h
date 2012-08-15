/*!
 * \class StVertexSeedMaker 
 * \author G. Van Buren, BNL
 * \version $Id: StVertexSeedMaker.h,v 1.16 2012/08/15 22:11:06 genevb Exp $
 * \brief BeamLine Constraint calibration base class
 *
 * StVertexSeedMaker calculates mean primary vertex positions from
 * suitable events to use as seeds in finding better
 * primary vertex positions (helpful for low
 * multiplicity events like pp collisions).
 *
 * More information on the BeamLine Constraint calibration can be found here:
 * http://drupal.star.bnl.gov/STAR/comp/calib/BeamLine
 *
 * If sufficient statistics exist, a file called vertexSeed.DDDDDDD.TTTTTT.C
 * will be written out (preferentially to a StarDb/Calibrations/rhic subdirectory,
 * otherwise the current directory) containing the calibration results, where
 * DDDDDDDD.TTTTTT is a date/timestamp associated with the first event processed.
 * When aggregating (see the macro AggregateVtxSeed.C),
 * the date/timestamp representing the start of a fill will be used.
 *
 * Relevant information for vertices used in the calibration
 * is stored in an ntuple in a file called vertexseedhist.DDDDDDDD.TTTTTT.ROOT,
 * location and date/timestamp as noted for the .C file, and the suffix
 * is capitalized to avoid being read in automatically by running jobs. The
 * ntuple file will be generated even if the .C file is not, and also contains
 * some basic histograms of the data.
 *
 * The ntuple is called resNtuple and includes the following quantities
 * for each vertex:
 * - event  : serial ID number of the event in which the vertex was found
 * - run    : run in which the event was acquired
 * - fill   : collider fill in which the run was acquired
 * - zdc    : ZDC coincidence rate at the time the event was acquired
 * - trig   : an offline trigger ID which the event satisfies
 *   (though an event may satisfy more than one, only one is stored)
 * - x,y,z  : coordinates of the vertex position
 * - ex,ey  : uncertainties on x,y
 * - rank   : ranking assigned to the vertex by the vertex-finder used
 * - mult   : number of daughter tracks for the vertex
 * - i/otpc : bitmaps of inner/outer TPC sectors where daughter tracks have hits
 *   (bits 0-23 represent sectors 1-24)
 * - detmap : packed information on daughter tracks matched in detectors
 *   (the number of matched tracks is always a subset of mult)...
 *   <table>
 *   <tr><th> bits  </th><th> store the number of </th><th> capped at </th></tr>
 *   <tr><td> 0,1,2 </td><td> BEMC matches        </td><td>     7     </td></tr>
 *   <tr><td> 3,4,5 </td><td> EEMC matches        </td><td>     7     </td></tr>
 *   <tr><td> 6,7,8 </td><td> BTOF matches        </td><td>     7     </td></tr>
 *   <tr><td>  9,10 </td><td> TPC CM crossers     </td><td>     3     </td></tr>
 *   </table>
 *   ...where a cap at N means values larger than N are recorded as N.<br><br>
 *   Using TTree::Draw() methods allows the bit-shifting operator in cuts:<br>
 *   <code> resNtuple.Draw("x","((detmap>>6)&7)==7"); </code><br>
 *   ...but reserves <code> >> </code> for histogram direction in the selection.
 *   Alternatively, one can see the BTOF matches via:<br>
 *   <code> resNtuple.Draw("(detmap&(7*8*8))/(8*8)"); </code>
 *
 */


#ifndef STAR_StVertexSeedMaker
#define STAR_StVertexSeedMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TString.h"
class TNtuple;
class St_vertexSeed;
class St_vertexSeedTriggers;


class StVertexSeedMaker : public StMaker {
 public: 
                  StVertexSeedMaker(const char *name="VtxSeedMkr",
                    const char* defaultDir="./StarDb/Calibrations/rhic/");
   virtual       ~StVertexSeedMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual void PrintInfo();
   virtual void Clear(Option_t *option);
   virtual Int_t Finish();
   virtual Int_t Aggregate(Char_t* dir=0, const Char_t* cuts="");

   virtual void FitData();
   virtual void FindResult(Bool_t checkDb=kTRUE);
   virtual int GetValidityDate();
   virtual int GetValidityTime();
   virtual void UseEventDateTime();
   virtual void UseFillDateTime();
   virtual void UseAllTriggers();
   virtual St_vertexSeed* VertexSeedTable();
   virtual void WriteTableToFile();     //Write drift velocity table (assumes correct trigger offset)
   virtual void SetMinEntries(int entries);  //minimum number of valid events for seed
   virtual void SetMaxX0Err(float err);  //maximum allowed error for x0 
   virtual void SetMaxY0Err(float err);  //maximum allowed error for y0 
   virtual void WriteHistFile();       // Write out vertexseedhist.root file w/results
   virtual void HistFileByDefault();   // Write out file on Finish
   virtual void SetVertexZmax(float zmax);  //Set max z vertex for seed calculation
   virtual void SetVertexZmin(float zmin);  //Set min z vertex for seed calculation
   virtual void SetVertexR2max(float r2max);  //Set max r^2 vertex for seed calculation
   virtual void SetDefDir(const char* dir) {defDir = dir;}
   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StVertexSeedMaker.h,v 1.16 2012/08/15 22:11:06 genevb Exp $ built "__DATE__" "__TIME__ ;
     return cvs;
   }

 protected:
   virtual void Reset();
   virtual Int_t FillAssumed();
   virtual Int_t GetVertexSeedTriggers();
   virtual void FillDateTime();
   virtual void GetFillDateTime();
   virtual Bool_t BetterErrors();
   virtual Bool_t ChangedValues();
   virtual Bool_t CheckTriggers() { return kTRUE; }
   virtual Bool_t ValidTrigger(unsigned int);
   virtual Int_t GetEventData() { return kStErr; }

  TH1F* xdist;
  TH1F* ydist;
  TH1F* xerr;
  TH1F* yerr;
  TNtuple* resNtuple;
  float xguess;
  float yguess;
  float zvertex;
  float yvertex;
  float xvertex;
  float eyvertex;
  float exvertex;
  float mult;
  float trig;
  float eventNumber;
  float HIST_MIN;
  float HIST_MAX;
  float zVertexMax; //maximum allowed z vertex for mean calculation
  float zVertexMin; //minimum allowed z vertex for mean calculation
  float r2VertexMax; //minimum allowed radius^2 vertex for mean calculation
  int    fill;
  int    date;
  int    time;
  int    run;
  float  zdc; // ZDC coincidence rate
  float  sumzdc; // running sum of zdc
  // The following integer maps can only be stored to 24 bits
  // because of the conversion to float
  int    itpc; // inner tpc track map
  int    otpc; // inner tpc track map
  int    detmap; // map any other detectors
  float  rank;
  int    minEntries;
  float    maxX0Err;
  float    maxY0Err;
  Bool_t   mHistOut;
  TFile*   mTempOut;
  Bool_t   useEventDateTime;
  Bool_t   useAllTriggers;
  double p[4];  // calculated params
  double ep[4]; // calculated errs
  double a[4];  // database params
  double ea[4]; // database errs
  double chi;
  TString defDir;
  St_vertexSeedTriggers* dbTriggersTable;

  ClassDef(StVertexSeedMaker,0)
};


inline void StVertexSeedMaker::UseEventDateTime() {useEventDateTime = kTRUE;}
inline void StVertexSeedMaker::UseFillDateTime() {useEventDateTime = kFALSE;}
inline void StVertexSeedMaker::UseAllTriggers() {useAllTriggers = kTRUE;}
inline void StVertexSeedMaker::SetMinEntries(int entries){minEntries = entries; }
inline void StVertexSeedMaker::SetMaxX0Err(float err){maxX0Err = err;}
inline void StVertexSeedMaker::SetMaxY0Err(float err){maxY0Err = err;}
inline int  StVertexSeedMaker::GetValidityDate(){return date;}
inline int  StVertexSeedMaker::GetValidityTime(){return time;}
inline void StVertexSeedMaker::HistFileByDefault(){mHistOut = kTRUE;} 
inline void StVertexSeedMaker::SetVertexZmax(float zmax){zVertexMax = zmax;}
inline void StVertexSeedMaker::SetVertexZmin(float zmin){zVertexMin = zmin;}
inline void StVertexSeedMaker::SetVertexR2max(float r2max){r2VertexMax = r2max;}

#endif

// $Id: StVertexSeedMaker.h,v 1.16 2012/08/15 22:11:06 genevb Exp $
// $Log: StVertexSeedMaker.h,v $
// Revision 1.16  2012/08/15 22:11:06  genevb
// Improved doxygen-ready documentation
//
// Revision 1.15  2012/08/14 23:56:06  genevb
// detmap now includes BEMC+EEMC+BTOF+CM, added mean zdc to log output
//
// Revision 1.14  2010/07/02 22:36:10  genevb
// Option for using all triggers
//
// Revision 1.13  2009/11/23 21:38:56  genevb
// Fix problems with memory-resident TNtuple by using a temporary disk file
//
// Revision 1.12  2009/11/16 22:31:11  genevb
// phase out usage of old tables
//
// Revision 1.11  2009/05/22 23:50:50  genevb
// Code mods for BEMC matches, BeamWidth
//
// Revision 1.10  2008/05/21 17:48:39  genevb
// Use vertex errors for weighting
//
// Revision 1.9  2008/04/29 23:30:34  genevb
// Added cuts capability to Aggregate
//
// Revision 1.8  2007/11/27 23:42:48  genevb
// Move valid triggers from code to DB
//
// Revision 1.7  2006/09/01 22:27:16  genevb
// More detailed info in ntuple
//
// Revision 1.6  2005/07/01 21:46:01  genevb
// Specify output directory
//
// Revision 1.5  2005/06/14 18:51:31  genevb
// Updates to allow for pp2005 triggers, and inheritance
//
// Revision 1.4  2002/03/20 00:40:43  genevb
// Addition of Aggregate feature, minor updates
//
//
//
