// $Id: StFtpcSlowSimReadout.hh,v 1.4 2002/10/16 12:31:56 fsimon Exp $
// $Log: StFtpcSlowSimReadout.hh,v $
// Revision 1.4  2002/10/16 12:31:56  fsimon
// gain factors and time offset included, Hardware <-> DAQ mapping taken into
// account for Db access
//
// Revision 1.3  2001/04/20 12:50:26  jcs
// cleanup comments
//
// Revision 1.2  2001/03/06 23:36:19  jcs
// use database instead of params
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//  Author: W.G.Gong
//  Email: gong@mppmu.mpg.de
//  Date:  Oct 25, 1996
//
//  Modifications:
//         Janet Seyboth    jcs@mppmu.mpg.de
//         February 18, 1998
//         Remove all references to point file
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcSlowSimReadout
#define STAR_StFtpcSlowSimReadout

class StFtpcSlowSimCluster;
class StFtpcSlowSimField;
class StFtpcParamReader;
class StFtpcDbReader;
extern  int Locate(const int npt, const float* x, const float xx);

//
//  class readout will handle avalanche, pad/shaper response 
//  and digitization
//
class StFtpcSlowSimReadout
{
public:
  StFtpcSlowSimReadout(StFtpcParamReader *paramReader,
                       StFtpcDbReader *dbReader,
		       float *adcIn, const StFtpcSlowSimField *field);
  ~StFtpcSlowSimReadout();
  void Avalanche(const StFtpcSlowSimCluster *cl);      
                                // electron multiplification
  void PadResponse(const StFtpcSlowSimCluster *cl);    
                                // width including pad response in [um]
  void ShaperResponse(const StFtpcSlowSimCluster *cl); 
                                // width including shaper response in [nsec]
  void Digitize(const StFtpcSlowSimCluster *cl, const int irow);
                                // add noise and fill the pixel array
                                //     mADCArray[pad,time]
  void OutputADC();
  void Print() const ;   // print readout parameters
  
  float GetPadLength() const        {return pad_length; }
  
  int WhichSlice(const float time);    // which time slice
  int WhichPad(const float phi, int &isec);
  // which pad is phi and which sector
  float TimeOfSlice(const int slice);// time for slice in nsec
  float PhiOfPad(const int pad, const int deg_or_rad);
  // phi for pad
  // deg_or_rad = 1: in deg
  //              0: in rad
  float InteGauss(const float x_1, const float x_2,
                  const float x_0, const float sig);
  float ranmar();
  void rmarin(int ij, int kl);
  
  int GetHardPad(const int daqsec, const int daqpad, const int irow);
  int GetHardSec(const int daqsec, const int irow);


private:
  struct raset1 {
    float u[97];
    float c;
  };
  
  struct raset1 uc;
  StFtpcParamReader *mParam;
  StFtpcDbReader *mDb;

  int mMaxAdc;            // maximum ADC value from parameters
  int mGaussIntSteps;     // steps for gauss calculation from par's
  float mGasGain;         // gas gain from parameters
  float mOuterRadius;     // outer radius of chamber sensitive volume
  float mInverseFinalVelocity;
  int mRandomNumberGenerator; // which random number generator
  float* mADCArray;              // pixel array (float)
  
  float mFinalElectrons;   // number of electrons after avalanche
  float sigma_pad;        // azimuthal sigma after pad response in um
  float pad_off;          // azimuthal width from crossing angle in cm 
  float sigma_tim;        // radial sigma after shaper response  in nsec
  float time_off;         // radial width from dip angle in nsec
  
  int number_plane;     // number of readout planes
  float pad_pitch;        // pad pitch in cm
  float pad_length;       // pad length in cm 
  float sigma_prf;        // pad-response-function sigma in um 
  float shaper_time;      // FWHM of shaper time in nsec
  float slice;            // SCA time slice in nsec
  
  float* pcum;             // cumulative Polya dist
  int gnch;             // No. of bins
  float glow;             // lower bound
  float ghigh;            // upper bound
  float gdelta;           // bin size
  float phiMin, phiMax;   // angle range for base sector
  void polya(const int nch, const float low, 
	     const float high, const float delta);
                                // get pcum[]
  int sample_polya(const float gain);
                                // sample gas gain for the averaged gain
  
};

#endif

