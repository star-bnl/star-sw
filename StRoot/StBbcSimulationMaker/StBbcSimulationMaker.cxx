/*!
Created by <a href="http://www.star.bnl.gov/~kopytin> 
Mikhail Kopytine </a> on Aug 20, 2002.

Here are
<A HREF="http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/St_TLA_Maker/README"> Victor Perevoztchikov's instructions on how to write an St***Maker </A>
 */
#ifdef __APPLE__
#include <sys/types.h>
#endif
#include "StBbcSimulationMaker.h"
#include "g2t/St_g2t_bbc_Module.h"
#include "TDataSetIter.h"
#include "TRandom.h"
#include <vector>
#include "StEvent.h"
#include "StTriggerDetectorCollection.h"
#include "StBbcTriggerDetector.h"
#include "StMessMgr.h"

TRandom BbcRndm = TRandom(0);

ClassImp(StBbcSimulationMaker)

  const float BbcTimingRMS = 900.E-12; /*! BBC timing resolution in seconds
for a single MIP according to the
<a href="http://connery.star.bnl.gov/protected/highpt/jacobs/BBC_proposal3.ps">
Proposal </a>.					*/



const u_short NPMTsmall1 = 16;
const u_short NPMTlarge1 = 8;
const u_short NPMT1 = NPMTsmall1+NPMTlarge1;//# of PMTs on one side (East/West)
const u_short NPMT2 = 2*NPMT1; // ditto on both sides
const float dE1MIPper_gcm2 = 1.95E-3;  // in GeV/(g/cm**2), for polystyrene
const float PolystereneDensity = 1.032;   // in g/cm**3
const float TyleThickness = 1.; // in cm
const float dE_1MIP = dE1MIPper_gcm2*PolystereneDensity*TyleThickness;
const float NPhotoelectrons_1MIP = 15.; 
const float pC_per_Photoelectron = 0.3;
const short NADCbins = 256; /* bins are numbered from 0 to 255 */
const short NTDCbins = 256; /* ditto */
const float pC_perADCbin = 0.25; /* Central trigger barrel 
				   Digitizer Boards (CDB) have such 
				   conversion gain (see STAR NIM) */
const float ADC0 = 0.;  /* beginning of ADC range */
const float s_perTDCbin = .1E-9; /* so that 25.6 ns is full range (guess) */
const float TDC0 = 0.;  /* beginning of TDC range */
const float OuterFactor = 0.8; /* larger scintillators of the outer ring
				  have less light output by this factor, 
				  compared with the inner ring, per
				  equal ionization */
const float SinglePhotoElectronResolution = 0.3; // according to Les Bland
/* Numbering: the real PMT numbering (used in the map) starts from 1.
ALL OTHERS start from 0 !
 */
//____________________________________________________________________________

bool IsSmall(short iPMT)
{
  /// true for inner annulus (small tiles)
  if ( 0<=iPMT && iPMT<NPMTsmall1) return 1;
  if ( NPMT1<=iPMT && iPMT<NPMT1+NPMTsmall1) return 1;
  return 0;
}

/*------------------------------------------------------------------------*/
class BbcTOF 
{
  /*! This class is designed to be used exclusively by StBbcSimulationMaker
(hence no header). It keeps and increments all the information required and
in the end generates response based on that + noise.
*/
private:
  vector<float> Times;

public: 
  BbcTOF():Times(vector<float>(NPMT2)){};  // NPMT1 !
  ~BbcTOF(){};
  void AddTOF(u_short ipmt, float time)
  {
    /*! for the TOF, take the smallest one among the PMT's tiles; 
      add resolution error 
     */
    if (Times[ipmt]==0 || Times[ipmt]>time) {Times[ipmt]=time;}
  }
  float GetTOF(u_short ipmt)  
  {
    /// returns TOF in s

    float tof = 0;
    if (Times[ipmt]!=0.){ tof = Times[ipmt]+BbcRndm.Gaus(0.,BbcTimingRMS); }
    return tof;
  }
  short GetTDC(u_short ipmt)
  { 
    /// returns digitized (TAC) TOF
    float T = this->GetTOF(ipmt);

    if (T<TDC0) {return 0;}
    short N = (short)((T-TDC0)/s_perTDCbin);
    if (N>=NTDCbins) {return NTDCbins-1;}
    return N;
  }

};
/*------------------------------------------------------------------------*/
class BbcDE
{
  /*! This class is designed to be used exclusively by StBbcSimulationMaker
(hence no header). It allows me to construct the energy measurement of a PMT 
by incrementing values of a vector; when that is finished, it returns 
the response based on that + noise.
   */
private:
  vector<float> dE;

public:
  BbcDE():dE(vector<float>(NPMT2)){};
  ~BbcDE(){};
  void AddDE(u_short ipmt, float de)
  {
    if (!IsSmall(ipmt)) {de *= OuterFactor;}
    dE[ipmt] += de;
  }
  float GetDE(u_short ipmt)
  {
    /// returns DE in pC of PMT signal

    float PoissonMean = dE[ipmt]/dE_1MIP*NPhotoelectrons_1MIP;
    short NPhotoelectrons = BbcRndm.Poisson(PoissonMean);
    float Q = pC_per_Photoelectron*
      (1+BbcRndm.Gaus(0.,SinglePhotoElectronResolution))*
       NPhotoelectrons;
    return Q;
  }
  short GetADC(u_short ipmt)
  {
    /// returns digitized (ADC) amplitude
    float A = this->GetDE(ipmt);
    if (A<ADC0) {return 0;}
    short N = (short)((A-ADC0)/pC_perADCbin);
    if (N>=NADCbins) {return NADCbins-1;}
    return N;
  }
};
//_____________________________________________________________________________
/// BbcSimulation constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
StBbcSimulationMaker::StBbcSimulationMaker(const char *name):StMaker(name)
{
  /*! See StBbcSimulationMaker.h and README for description of this class.
   */
}

//_____________________________________________________________________________
/// This is BbcSimulation destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StBbcSimulationMaker::~StBbcSimulationMaker()
{
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StBbcSimulationMaker::Init()
{
  /*!
Associate volume ID with channel ID -- use map
MLK: GEANT numbering (set up by Yiqun, see bbcmgeo.g) is as follows:
* West is 1, East is 2;
* inner annulus (small tiles) is 1, outer is 2;
* triplet numbering is counterclockwise azimuthally, looking at the West 
detector along the z axis (i.e. from inside !!!) (East is rotated 180 deg.
arong the vertical axis.) 
(<a href="http://wwwinfo.cern.ch/asdoc/geant_html3/node83.html#GDRAWC">GDRAWC
</a> -- direction of view is along the axis.)
* withing a triplet, tile numbering is counterclockwise, looking at the West
detector along the z axis, and starting with the rightmost tile.

Les Bland: BBC scintillator tiles numbers are specified 
<a href="http://www.star.bnl.gov/STAR/html/bbc_l/geom/front_view.html">
here.
</a>
This shows a schematic of the BBC scintillator array from a vantage point 
that is outside (!!! MLK) of the STAR magnet. The same numbering scheme 
applies from appropriate vantage points for both the east and west sides of 
STAR.
  */
  //   BBC tile           BBC PMT number      Comments/description
  // --------           --------------      --------------------
  for (short iew=1; iew<=2; iew++)
    {
      short EW = iew*1000; // for VID
      //      short EWshift = (iew-1)*NPMT1; if Akio numbered from West
      short EWshift = (2-iew)*NPMT1; // for PMT
      /*!
PMT numbering: Akio Ogawa numbers channels in the data structure starting
 from East.
VID numbering: Yiqun followed STAR standard in the GSTAR code:
first West, then East.
       */
  //      1                    1              inner small hex tile, 12:00 pos'n 90 degrees
  Geant2PMT[EW+113] = EWshift+1;
  //      2                    2              inner small hex tile,  2:00 pos'n 30 degrees
  Geant2PMT[EW+123] = EWshift+2;
  //      3                    3              inner small hex tile,  4:00 pos'n -30 degrees
  Geant2PMT[EW+133] = EWshift+3;
  //      4                    4              inner small hex tile,  6:00 pos'n -90 degrees
  Geant2PMT[EW+143] = EWshift+4;
  //      5                    5              inner small hex tile,  8:00 pos'n -150 degrees
  Geant2PMT[EW+153] = EWshift+5;
  //      6                    6              inner small hex tile, 10:00 pos'n 150 degrees
  Geant2PMT[EW+163] = EWshift+6;
  //      7                    7              outer small hex tile, 11:00 pos'n 120 degrees
  Geant2PMT[EW+111] = EWshift+7;
  //      8                    8              outer small hex tile, 12:00 pos'n 90 degrees
  Geant2PMT[EW+112] = EWshift+8;
  //      9                    7              outer small hex tile,  1:00 pos'n 60 degrees
  Geant2PMT[EW+121] = EWshift+7;
  //     10                    9              outer small hex tile,  2:00 pos'n 30 degrees
  Geant2PMT[EW+122] = EWshift+9;
  //     11                   10              outer small hex tile,  3:00 pos'n 0 degrees
  Geant2PMT[EW+131] = EWshift+10;
  //     12                   11              outer small hex tile,  4:00 pos'n -30 degrees
  Geant2PMT[EW+132] = EWshift+11;
  //     13                   12              outer small hex tile,  5:00 pos'n -60 degrees
  Geant2PMT[EW+141] = EWshift+12;
  //     14                   13              outer small hex tile,  6:00 pos'n -90 degrees
  Geant2PMT[EW+142] = EWshift+13;
  //     15                   12              outer small hex tile,  7:00 pos'n -120 degrees
  Geant2PMT[EW+151] = EWshift+12;
  //     16                   14              outer small hex tile,  8:00 pos'n -150 degrees
  Geant2PMT[EW+152] = EWshift+14;
  //     17                   15              outer small hex tile,  9:00 pos'n 180 degrees
  Geant2PMT[EW+161] = EWshift+15;
  //     18                   16              outer small hex tile, 10:00 pos'n 150 degrees
  Geant2PMT[EW+162] = EWshift+16;

  //     19                   17              inner large hex tile, 12:00 pos'n 90 degrees
  Geant2PMT[EW+213] = EWshift+17;
  //     20                   18              inner large hex tile,  2:00 pos'n 30 degrees
  Geant2PMT[EW+223] = EWshift+18;
  //     21                   18              inner large hex tile,  4:00 pos'n -30 degrees
  Geant2PMT[EW+233] = EWshift+18;
  //     22                   19              inner large hex tile,  6:00 pos'n -90 degrees
  Geant2PMT[EW+243] = EWshift+19;
  //     23                   20              inner large hex tile,  8:00 pos'n -150 degrees
  Geant2PMT[EW+253] = EWshift+20;
  //     24                   20              inner large hex tile, 10:00 pos'n 150 degrees
  Geant2PMT[EW+263] = EWshift+20;
  //     25                   21              outer large hex tile, 11:00 pos'n 120 degrees
  Geant2PMT[EW+211] = EWshift+21;
  //     26                   21              outer large hex tile, 12:00 pos'n 90 degrees
  Geant2PMT[EW+212] = EWshift+21;
  //     27                   21              outer large hex tile,  1:00 pos'n 60 degrees
  Geant2PMT[EW+221] = EWshift+21;
  //     28                   22              outer large hex tile,  2:00 pos'n 30 degrees
  Geant2PMT[EW+222] = EWshift+22;
  //     29                   22              outer large hex tile,  3:00 pos'n 0 degrees
  Geant2PMT[EW+231] = EWshift+22;
  //     30                   22              outer large hex tile,  4:00 pos'n -30 degrees
  Geant2PMT[EW+232] = EWshift+22;
  //     31                   23              outer large hex tile,  5:00 pos'n -60 degrees
  Geant2PMT[EW+241] = EWshift+23;
  //     32                   23              outer large hex tile,  6:00 pos'n -90 degrees
  Geant2PMT[EW+242] = EWshift+23;
  //     33                   23              outer large hex tile,  7:00 pos'n -120 degrees
  Geant2PMT[EW+251] = EWshift+23;
  //     34                   24              outer large hex tile,  8:00 pos'n -150 degrees
  Geant2PMT[EW+252] = EWshift+24;
  //     35                   24              outer large hex tile,  9:00 pos'n 180 degrees
  Geant2PMT[EW+261] = EWshift+24;
  //     36                   24              outer large hex tile, 10:00 pos'n 150 degrees
  Geant2PMT[EW+262] = EWshift+24;
    }

  // Create Histograms    
#ifdef BbcSimQa
  QaFile = new TFile("StBbcSimQa.root","recreate");
  QaBbcPmtdE = new TH1F("QaBbcPmtdE","BBC PMT",NPMT2,-0.5,NPMT2-0.5);
  QaBbcPmtTime = new TH1F("QaBbcPmtTime","BBC PMT",NPMT2,-0.5,NPMT2-0.5);
  QaBbcEastVid = 
    new TH1F("QaBbcEastVid","BBC PMT with East VID",256,-0.5,255.5);
  QaBbcWestVid = 
    new TH1F("QaBbcWestVid","BBC PMT with West VID",256,-0.5,255.5);
  QaBbcEastPmt =
    new TH1F("QaBbcEastPmt","BBC PMT with East #", 256,-0.5,255.5);
  QaBbcWestPmt =
    new TH1F("QaBbcWestPmt","BBC PMT with West #", 256,-0.5,255.5);
  // create a test map, reverse w.r.t. Geant2PMT 
  typedef map<short,short>::const_iterator CI;

  for (CI I=Geant2PMT.begin(); I!=Geant2PMT.end(); ++I)
    {
      PMT2Geant[(*I).second] = (*I).first;
    }
#endif  

   return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StBbcSimulationMaker::Make()
{
/// Make - this method is called in loop for each event

 TDataSet* ds= GetInputDS("geant");
 assert(ds);
 StEvent* event = (StEvent*)GetInputDS("StEvent");
 assert(event);
 St_g2t_ctf_hit* g2t_bbc_hit = (St_g2t_ctf_hit*)ds->Find("g2t_bbc_hit");

 if (g2t_bbc_hit) 
   {
     short nBBChits = g2t_bbc_hit->GetNRows();
     BbcTOF TOFdata;
     BbcDE DEdata;
     g2t_ctf_hit_st *bbc_hit = g2t_bbc_hit->GetTable();
     for (short iBBChit=0; iBBChit<nBBChits; iBBChit++)
     {
       float De = bbc_hit[iBBChit].de;
       float TOF = bbc_hit[iBBChit].tof;
       short Vid =  bbc_hit[iBBChit].volume_id;

       short PMTid = Geant2PMT[Vid];
       if (PMTid == 0) {
	 LOG_ERROR << "Cannot find a  PMTid in Geant2PMT for Vid = " << Vid << endm;
	 continue;
       }
       PMTid -= 1;
       DEdata.AddDE(PMTid,De);
       TOFdata.AddTOF(PMTid,TOF);
     }

     StTriggerDetectorCollection* myTrig =event->triggerDetectorCollection();
     if (!myTrig) {
       Warning("Make"," NoStTriggerDetectorCollection, Make the new one\n");
       myTrig = new StTriggerDetectorCollection;
       event->setTriggerDetectorCollection(myTrig);
     }
     StBbcTriggerDetector& myBbc = myTrig->bbc();


     for (u_short iPMT = 0; iPMT<NPMT2; iPMT++)
       {	  
	  short ADC = DEdata.GetADC(iPMT);
#ifdef BbcSimQa
	  //	  QaBbcPmtAdc->Fill(iPMT,ADC);
	  short Vid = PMT2Geant[iPMT+1];

	  if (Vid<2000) {QaBbcWestVid->Fill(ADC);}
	  if (Vid>2000) {QaBbcEastVid->Fill(ADC);}
	  if (iPMT<NPMT1) {QaBbcEastPmt->Fill(ADC);}
	  if (NPMT1<=iPMT && iPMT<NPMT2) {QaBbcWestPmt->Fill(ADC);}
	  QaBbcPmtTime->Fill(iPMT,TOFdata.GetTOF(iPMT));
	  QaBbcPmtdE->Fill(iPMT,DEdata.GetDE(iPMT));
#endif
	  myBbc.setAdc(iPMT, ADC);
	  if (IsSmall(iPMT))
	    {
	      short TDC = TOFdata.GetTDC(iPMT);
	      myBbc.setTdc(iPMT, TDC);
	    }
	}
   }
else
  {
    gMessMgr->Info
      ("MLK StBbcSimulationMaker::Make() could not inst g2t_bbc_hit\n");
  }

 return kStOK;
}
///////////////////////////////////////////////////////////////////////
Int_t StBbcSimulationMaker::Finish()
{
  /// end of run
#ifdef BbcSimQa
  QaFile->Write();
  QaFile->Close();
#endif
  return StMaker::Finish();
}









