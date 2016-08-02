/*!
 * \class StVertexSeedMaker 
 * \author G. Van Buren, BNL
 * \version $Id: StVertexSeedMaker.h,v 1.25 2016/08/02 21:17:17 genevb Exp $
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
 * - run    : run in which the event was acquired (excludes Run year digits)
 * - fill   : collider fill in which the run was acquired
 * - zdc    : ZDC coincidence rate at the time the event was acquired
 * - trig   : an offline trigger ID which the event satisfies
 *   (though an event may satisfy more than one, only one is stored)
 * - x,y,z  : coordinates of the vertex position
 * - ex,ey  : uncertainties on x,y
 * - rank   : ranking assigned to the vertex by the vertex-finder used
 * - index  : ranking order among vertices in an event (0 = highest)
 * - mult   : number of daughter tracks for the vertex
 * - vpdz   : z coordinate of the highest ranked VPD vertex
 * - i/otpc : bitmaps of inner/outer TPC sectors where daughter tracks have hits
 *   (bits 0-23 represent sectors 1-24)
 * - bmatch : uncapped number of daughter tracks matched to BEMC
 * - ematch : uncapped number of daughter tracks matched to EEMC
 * - tmatch : uncapped number of daughter tracks matched to BTOF
 * - cmatch : uncapped number of daughter tracks matched across the TPC CM
 * - hmatch : uncapped number of daughter tracks matched to HFT
 * - pmatch : uncapped number of daughter tracks with TPC prompt hits
 * - pct    : uncapped number of daughter post-crossing tracks
 * - tDay   : time of event after start of the day (GMT) [seconds]
 * - tFill  : time of event after start of the fill [seconds]
 * - detmap : packed information on daughter tracks matched in detectors
 *   (the number of matched tracks is always a subset of mult)...
 *   <table cellspacing=0 cellpadding=3>
 *   <tr><th>    bits  </th><th> store the number of </th><th> capped at </th></tr>
 *   <tr><td>    0,1,2 </td><td> BEMC matches        </td><td>     7     </td></tr>
 *   <tr><td>    3,4,5 </td><td> EEMC matches        </td><td>     7     </td></tr>
 *   <tr><td>    6,7,8 </td><td> BTOF matches        </td><td>     7     </td></tr>
 *   <tr><td>     9,10 </td><td> TPC CM crossers     </td><td>     3     </td></tr>
 *   <tr><td> 11,12,13 </td><td> HFT  matches        </td><td>     7     </td></tr>
 *   <tr><td>    14,15 </td><td> TPC prompt hits     </td><td>     3     </td></tr>
 *   <tr><td> 16,17,18 </td><td> post-crossing tracks</td><td>     7     </td></tr>
 *   </table>
 *   ...where a cap at N means values larger than N are recorded as N.<br><br>
 *   Using TTree::Draw() methods allows the bit-shifting operator in selection cuts:<br>
 *   <code> resNtuple.Draw("x","((detmap>>6)&7)==7"); </code><br>
 *   While <code> >> </code> is reserved for histogram (re)direction in the
 *   drawn variables expression, one can alternatively draw the BTOF matches via:<br>
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
class TNtupleD;
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
   virtual int Aggregate(char* dir=0, const char* cuts="", const int offset=0);

   virtual void FitData();
   virtual void FindResult(bool checkDb=kTRUE);
   virtual int GetValidityDate();
   virtual int GetValidityTime();
   virtual void UseEventDateTime();
   virtual void UseFillDateTime();
   virtual void UseAllTriggers();
   virtual St_vertexSeed* VertexSeedTable();
   virtual void WriteTableToFile();     // Write vertex seed table
   virtual void SetOffset(int offset=0) {foffset = offset;} // time offset in writing table name
   virtual void SetNoClobber(bool noclob=kTRUE) {noclobber = noclob;} // clobber existing files?
   virtual void SetMinEntries(int entries);  //minimum number of valid events for seed
   virtual void SetMaxX0Err(float err);  //maximum allowed error for x0 
   virtual void SetMaxY0Err(float err);  //maximum allowed error for y0 
   virtual void WriteHistFile(bool writeFit);   // Write out vertexseedhist.root file w/results
   virtual void HistFileByDefault();   // Write out file on Finish
   virtual void SetVertexZmax(float zmax);  //Set max z vertex for seed calculation
   virtual void SetVertexZmin(float zmin);  //Set min z vertex for seed calculation
   virtual void SetVertexR2max(float r2max);  //Set max r^2 vertex for seed calculation
   virtual void SetDefDir(const char* dir) {defDir = dir;}
   virtual const char *GetCVS() const {
     static const char cvs[]="Tag $Name:  $ $Id: StVertexSeedMaker.h,v 1.25 2016/08/02 21:17:17 genevb Exp $ built " __DATE__ " " __TIME__ ;
     return cvs;
   }

 protected:
   virtual void Reset();
   virtual int FillAssumed();
   virtual int GetVertexSeedTriggers();
   virtual void GetADateTime();
   virtual void GetFillDateTime();
   virtual bool BetterErrors();
   virtual bool ChangedValues();
   virtual bool CheckTriggers() { return kTRUE; }
   virtual bool ValidTrigger(unsigned int);
   virtual int GetEventData() { return kStErr; }
   virtual void AddResults(TNtupleD* ntup);
   virtual TString NameFile(const char* type, const char* prefix, const char* suffix);
   virtual TNtupleD* newBLpars();
   virtual void Packer(int firstbit, int nbits, int& var, unsigned short val);

  TH1F* xdist;
  TH1F* ydist;
  TH1F* xerr;
  TH1F* yerr;
  TNtuple* resNtuple;
  TNtupleD* parsNtuple;
  float xguess;
  float yguess;
  float zvertex;
  float yvertex;
  float xvertex;
  float eyvertex;
  float exvertex;
  float vpd_zvertex;
  float mult;
  float trig;
  float HIST_MIN;
  float HIST_MAX;
  float zVertexMax; //maximum allowed z vertex for mean calculation
  float zVertexMin; //minimum allowed z vertex for mean calculation
  float r2VertexMax; //minimum allowed radius^2 vertex for mean calculation
  int   fill;
  int   date;
  int   time;
  int   foffset;
  bool  noclobber;
  int   run; // excludes first 2 digits (Run year) for 24 bit precision limit
  float zdc; // ZDC coincidence rate
  float sumzdc; // running sum of zdc
  int   timeEvent; // unix time of event
  int   timeFill; // unix time of fill
  // The following integer maps can only be stored to 24 bits
  // because of the conversion to float
  int   itpc; // inner tpc track map
  int   otpc; // inner tpc track map
  int   detmap; // map any other detectors
  int   bmatch; // matches with BEMC
  int   ematch; // matches with EEMC
  int   tmatch; // matches with BTOF
  int   cmatch; // matches across TPC Central Membrane
  int   hmatch; // matches with HFT
  int   pmatch; // matches with TPC prompt hits
  int   pct; // post-crossing tracks
  float rank;
  unsigned int pvn; // primery vertex index number
  int   minEntries;
  float maxX0Err;
  float maxY0Err;
  bool  mHistOut;
  TFile* mTempOut;
  bool  useEventDateTime;
  bool  useAllTriggers;
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

// $Id: StVertexSeedMaker.h,v 1.25 2016/08/02 21:17:17 genevb Exp $
// $Log: StVertexSeedMaker.h,v $
// Revision 1.25  2016/08/02 21:17:17  genevb
// Added tDay,tFill to resNtuple, and improved C++11 compliance
//
// Revision 1.24  2015/05/18 21:25:47  genevb
// Use HFT hits
//
// Revision 1.23  2015/05/15 05:38:21  genevb
// Include prompt hits and post-crossing tracks, simplify detmap packing, update doxygen documentation
//
// Revision 1.22  2015/05/14 20:29:25  genevb
// Add z of VPD vertex
//
// Revision 1.21  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.20  2013/08/14 21:42:48  genevb
// Introduce time offsets, noclobber toggle, more matched-tracks controls
//
// Revision 1.19  2012/08/22 04:52:35  genevb
// Add BeamLine parameter ntuples to output
//
// Revision 1.18  2012/08/17 22:57:33  genevb
// Add index of vertex within event to ntuple
//
// Revision 1.17  2012/08/15 22:16:53  genevb
// Re-worded documentation
//
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
