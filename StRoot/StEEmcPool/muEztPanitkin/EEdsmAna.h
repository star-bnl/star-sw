#ifndef EEdsmAna_h
#define EEdsmAna_h
/**************************************************************
 * $Id: EEdsmAna.h,v 1.6 2009/04/02 14:30:06 pibero Exp $
 **************************************************************/
#include <TObject.h>
#include <TString.h>


class EEdsm0;
class EEdsm1;
class EEdsm2;
class EEdsm3;

struct DSM;

class TH1F;
class TH2F;
class TFile;


class EEdsmAna  {
public:
    EEdsmAna(const Char_t *name = 0, int year = 2009);
    virtual ~EEdsmAna();

    void usePed(const Char_t *filename = "dsm0inp.ped");

    void initHisto(TObjArray *HList = 0);
    void clear();
    void sort(const unsigned char *dsm0inp, 
	    const unsigned short int *dsm1inp,
	    const unsigned short int *dsm2inp,   
	    const unsigned short int *dsm3inp);
    void saveHisto(TFile *f) const;
    void resetHisto();

    int getNtot() const {return nTot;}

    void printAllEndcap(int k = 0) const;
    void printAllBarrel(int k = 0) const;
    void printDsm0map() const;

private:
#define  EEnJetPatch 6 // numbers of 1x1 Jet patches in EEMC
#define  EEnTPinJP 15 // numbers trigger patches in JetPatch
#define  EEnHalfJetPatch 12 // numbers .3 and .6 Jet patches
#define  EEnHalf 2 // 3x1 3-JP
#define  EEnThresh 4 //number of thresholds, below thr0, exceeds th0 th1 and th2

    int mJPthr[EEnThresh];
    //2005--: 54--> ped=45+9=>2.2 GeV

    const Char_t *myName;
    int nTot;

    //ENDCAP
    int Nee0; // # of Level-0 boards
    EEdsm0 * ee0; //  Level-0 boards, nine
    //BARREL-not implemented
    
    int **ped0; // pedestals for  Level-0 inputs 
    int Nee0out;  //# of Level-0 emuated outputs
    int *ee0outTPadc; // Level-0 emulated values of TP sums
    int *ee0outHTadc; // Level-0 emulated values of HT adc
    int mYear; // unpacking changed in 2006

    //ENDCAP:
    int Nee1; // # of Level-1 boards
    EEdsm1 * ee1; // Level-1 boards , two
    int *ee1out3JPadc; // Level-1 emulated values of 3x1 JP energy sums
    //BARREL-not implemented

    int ee1outJPadc[EEnJetPatch]; // Level-1 emulated values of JP energy
    int ee1outHT[EEnJetPatch]; // Level-1 emulated values of HT 2bit thresholds
    int ee1outTPthrMax  [EEnHalf]; // Level-1 emulated max TPthres (2bits)
    int ee1outHTTPthrMax[EEnHalf]; // Level-1 emulated max HTTPthres (2bits)
    int AdjJPsum[EEnJetPatch]; //Adjacent jet patch sums

    enum BrdCnt{Nbe2=3};
    //ENDCAP:
    EEdsm2 * ee2; // Level-2 board, just one
    int ee2outHT; // Level-2 emulated values of HT 2bit threshold
    //BARREL
    EEdsm2 *be2; // Level-2 boards

    EEdsm3 *ee3; // Trigger DSM input 

    void readDsm0(const unsigned char *);
    void readDsm1(const unsigned short *);
    void readDsm2(const unsigned short *);
    void readDsm3(const unsigned short *);
    void emulDsm0();
    void emulDsm1();
    void emulDsm2();
    void histoDsm0();
    void histoDsm1();
    void histoDsm2();
    void histoDsm3();

/*
  a) Layer-0 DSM input spectra, i.e., 6 pairs of 2D histograms (one per
  jet patch) showing ADC value vs. trigger patch # (1-15) for
  high tower (64 channels) and for trigger patch sum (256 channels).
  These will allow us to see, e.g., sharpness of the summed pedestal,
  malfunctioning FEE or DSM channels, sharp turnon of high tower
  threshold at the appropriate channel.
*/

    TH2F * H0inHT[EEnJetPatch]; // high tower 
    TH2F * H0inTP[EEnJetPatch]; //  trigger patch sum 
    TH2F * H0inHTall; // high tower, 90 chan
    TH2F * H0inTPall; //  trigger patch sum, 90 chan 

/*
  a 2D    histogram for each layer 1 DSM input channel
  (i.e., 12 2D histos in all) showing layer 1
  DSM input (0-3) vs. emulated (from layer 0 DSM
  inputs) HT input to that layer 1 DSM channel.
  This will allow us to see easily if the transitions
  among different DSM output values (0 to 1, 1 to 2, 2
  to 3) are occurring at the anticipated thresholds.
  We could basically debug all DSM's at a glance with
  such histograms.
*/

    TH2F *H1inHTvEmu[EEnHalfJetPatch]; //  high tower level-1 input vs. emulation
    TH2F *H1inTPvEmu[EEnHalfJetPatch]; //  TP sum level-1 input vs. emulation


    TH2F *H2inHTTP[EEnHalf]; // matrix of HT & HTTP
    TH1F *H1inEtot[EEnHalf]; // Etot for each half, input to level2
    
    TH2F *H3inHTTP;

/*
  Histos for debugging the jet patch trigger.  The first array of 6 are the summed jet
  patch ADC spectra emulated from the input to layer 0 DSMs.  This should allow us to 
  evaluate the sharpness of the pedestal and compare the 6 to each other.  They should
  be similar.  The index matches Steve's convention with sector 1 starting around 1 o'clock.
  The next triplet of histograms is the frequency that each patch passes th0
  th1 and th2.  These should be relatively uniform across the 6 jet patches. The following
  array of 6 are spectra of adjacent jet patch sums when both pass th0.  These should look
  somewhat like the individual patch spectra above the threshold.   The next array
  of 6 are 2D correlations between adjacent patch ADCs when they both exceed th0. The last
  is a frequency plot of adjacent patches passing th0.
*/

    TH1F *H4jpSums[EEnJetPatch]; // jet patch sums from DSM 0 inputs
    TH1F *H4jpFreq[EEnThresh];   // frequency of passing thresholds
    TH1F *H4adjpSums[EEnJetPatch]; //adjacent jet patch sums from DSM 0 inputs if both exceed th0
    TH2F *H4adjPcor[EEnJetPatch];  //adjacent patch adc correlations
    TH1F *H4adjpFreq;           // frequency of passing threshold 0 for adjacent jet patches

    // added in 2005 to help shift crew assess ETOW preformance

    TH2F *H5jpPed; // zoom in on pedestal for 1x1 JP
    TH1F *H5jpFreq; //  
    TH1F *H5jpHot; // 

    //Total energy histos
    enum {mxEtotBit=2};
    TH1F *HEetot[mxEtotBit]; //label: EEMC ETOT BIT
    TH1F *HBetot[mxEtotBit]; //label: BEMC ETOT BIT
    TH1F *HBEetot[mxEtotBit]; //label: B+EEMC ETOT BIT

    // 2009 trigger data
    DSM& EE101;
    DSM& EE102;

    ClassDef(EEdsmAna, 1) 
};
#endif


/*
 * $Log: EEdsmAna.h,v $
 * Revision 1.6  2009/04/02 14:30:06  pibero
 * Updates to include jet patch plots for Run 9
 *
 * Revision 1.5  2009/02/25 20:48:42  fisyak
 * Add forward declaration of TFile for ROOT >= 5.22
 *
 * Revision 1.4  2009/02/24 04:07:45  ogrebeny
 * Fixed part of the trigger histograms
 *
 * Revision 1.3  2009/01/23 00:14:50  ogrebeny
 * Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
 *
 * Revision 1.2  2006/04/05 18:33:02  balewski
 * new DSM bits allocation in 2006, possibly lost backward compatibility
 * use tagged 2005 version if needed
 *
 * Revision 1.1  2005/04/28 20:54:46  balewski
 * start
 *
 * Revision 1.4  2004/08/20 20:15:10  balewski
 * another printing format for DSM to compare w/ muDst
 *
 * Revision 1.3  2004/04/19 15:17:11  balewski
 * jime's JP code added
 *
 * new dsm plots for jet patch trigger sowinski
 *
 * Revision 1.2  2004/03/13 22:03:13  balewski
 * new plots from Hal added
 *
 * Revision 1.1  2004/02/17 03:09:17  balewski
 * *** empty log message ***
 *
 * Revision 1.3  2004/01/18 06:15:59  balewski
 * DSM plots a'la Hank added
 *
 * Revision 1.2  2003/12/30 04:01:10  balewski
 * start
 *
 * Revision 1.1  2003/12/29 02:18:39  balewski
 * star
 *
 * Revision 1.2  2003/05/26 02:10:51  balewski
 * added plenty of Histos for DSM analysis
 *
 * Revision 1.1  2003/05/22 19:39:01  balewski
 * analysis of DSM data
 *
 * 
 *
 processing of  EE-DSM 
 *
 **************************************************************/
