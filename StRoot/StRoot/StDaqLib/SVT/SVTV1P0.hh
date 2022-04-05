/***************************************************************************
 *      
 * $Id: SVTV1P0.hh,v 1.2 2001/04/18 19:47:25 ward Exp $
 *      
 * Author: Jeff Landgraf, M.J. LeVine, Marcelo Munhoz, J. Schambach
 *      
 ***************************************************************************
 *      
 * Description: declarations For SVT
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0.hh,v $
 * Revision 1.2  2001/04/18 19:47:25  ward
 * StDaqLib/SVT stuff from Jo Schambach.
 *
 * Revision 1.1  2000/06/06 18:03:10  jml
 * Initial version of SVT Reader code author (marcello munholz, helen caines)
 *
 *
 **************************************************************************/

#ifndef SVTV1P0_HH
#define SVTV1P0_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/Error.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "SVTV1P0.Banks.hh"
//#include "MemMan.hh"
#include "SVTV1P0_Reader.hh"

class SVTV1P0_Reader;
class SVTV1P0_ANODK_SR;

// The sector reader virtual classes

// Zero-suppressed reader
class SVTV1P0_ZS_SR : public ZeroSuppressedReader
{
public:
  int getPadList(int hybrid, unsigned char **anodeList);
  int getSequences(int hybrid, int Anode, int *nSeq, Sequence **SeqData);
  int getSpacePts(int hybrid, int *nSpacePts, SpacePt **SpacePts);

  int MemUsed();

  SVTV1P0_ZS_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_ZS_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  int getFeeSequences(int Fee, int Pin, int *nSeq, Sequence **SeqData);
  
  ~SVTV1P0_ZS_SR();


private:
  classname(Bank_SVTADCD) *adcd_p;
  classname(Bank_SVTADCX) *adcx_p;
  classname(Bank_SVTSEQD) *seqd_p;
  classname(Bank_SVTMZCLD) *cld_p;
  
  int barrel, ladder, wafer;
  int hyperSector, rcb, mezz, transitionBoard;
  int nspthybrid[SVT_HYBRIDS]; // num space pts each hybrid
  SpacePt *HybridSpacePts[SVT_HYBRIDS]; // pointer to SpacePt array for each padrow 
  struct Pad Anode_array[SVT_HYBRIDS][SVT_ANODES];
  struct PadRow Hybrid_array[SVT_HYBRIDS];
  u_char anodelist[SVT_HYBRIDS][SVT_ANODES];

  SVTV1P0_Reader *detector;
  SVTV1P0_ANODK_SR *anodkr;

};


// Reads Raw ADC values
class SVTV1P0_ADCR_SR : public ADCRawReader
{
public:
  int getPadList(int hybrid, unsigned char **anodeList);
  int getSequences(int hybrid, int Anode, int *nArray, u_char **Array);
  int MemUsed();
  
  SVTV1P0_ADCR_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_ADCR_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_ADCR_SR();

private:
  int barrel, ladder, wafer;
  SVTV1P0_Reader *detector;
  SVTV1P0_ANODK_SR *anodkr;
  u_char anodelist[SVT_HYBRIDS][SVT_ANODES];

  classname(Bank_SVTADCR) *banks;
};

// Reads the Pedestal values
class SVTV1P0_PEDR_SR : public PedestalReader
{
private:
  int barrel, ladder, wafer;
  SVTV1P0_Reader *detector;
  SVTV1P0_ANODK_SR *anodkr;
  classname(Bank_SVTPEDR) *banks;
  u_char anodelist[SVT_HYBRIDS][SVT_ANODES];
  int numEvents;

public:
  int getPadList(int hybrid, unsigned char **anodeList);
  int getSequences(int hybrid, int Anode, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  SVTV1P0_PEDR_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_PEDR_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_PEDR_SR();
};

// The RMS pedestal values
class SVTV1P0_PRMS_SR : public PedestalRMSReader
{
private:
  int barrel, ladder, wafer;
  SVTV1P0_Reader *detector;
  SVTV1P0_ANODK_SR *anodkr;
  classname(Bank_SVTRMSR) *banks;
  u_char anodelist[SVT_HYBRIDS][SVT_ANODES];
  int numEvents;

public:
  int getPadList(int hybrid, unsigned char **anodeList);
  int getSequences(int hybrid, int Anode, int *nArray, u_char **Array);
  int getNumberOfEvents();

  int MemUsed();

  SVTV1P0_PRMS_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_PRMS_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_PRMS_SR();
};

// The gain reader
class SVTV1P0_G_SR : public GainReader
{
private:
  int barrel, ladder, wafer;
  SVTV1P0_Reader *detector;

public:
  int getGain(int hybrid, int Anode, struct Gain **gain);
  int getMeanGain();
  int getNumberOfEvents();

  int MemUsed();

  SVTV1P0_G_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_G_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_G_SR();
};

// Reads Cluster Pointer Pairs from the ASIC
class SVTV1P0_CPP_SR : public CPPReader
{
public:
  int getClusters(int hybrid, int Anode, int *nClusters, 
			  struct ASIC_Cluster **clusters);
  int getAsicParams(ASIC_params *);
  int MemUsed();
  
  SVTV1P0_CPP_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_CPP_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_CPP_SR();

private:
  int barrel, ladder, wafer;
  SVTV1P0_Reader *detector;
  SVTV1P0_ANODK_SR *anodkr;
  classname(Bank_SVTCPPR) *banks[SVT_HYBRIDS];
};

// Reads the bad channels
class SVTV1P0_BC_SR : public BadChannelReader
{
private:
  int barrel, ladder, wafer;

public:
  int IsBad(int hybrid, int Anode);
	
  int MemUsed();

  SVTV1P0_BC_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_BC_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_BC_SR();
};

// Read the front end electronics configuration
class SVTV1P0_CR_SR : public ConfigReader
{
private:
  int barrel, ladder, wafer;

public:
  int FEE_id(int hybrid, int Anode) = 0;
	// returns FEE_id

  int MemUsed();
  
  SVTV1P0_CR_SR(int wafer, SVTV1P0_Reader *);
  SVTV1P0_CR_SR(int barrel, int ladder, int wafer, SVTV1P0_Reader *);
  int initialize();
  ~SVTV1P0_CR_SR();
};

struct ANODK_entry
{
  short offset;
  short mz;        // mz and rb are counted up from 1
  short rb;
  short hypersector;
};

class SVTV1P0_ANODK_SR
{
public:
  SVTV1P0_ANODK_SR(SVTV1P0_Reader *);
  int initialize(int maxSector);

  void place(short barrel, short ladder, short wafer, short hybrid, ANODK_entry *);
  void get(short barrel, short ladder, short wafer, short hybrid, ANODK_entry *);
  void get(short waferIndex, short hybrid, ANODK_entry *);
  short getWaferIndex(short barrel, short ladder, short wafer); 
  
  int getADCBytes() { return 128; };
  int getCPPBytes() { return 128; };
  int getPEDBytes() { return 128; };
  int getRMSBytes() { return 128; };
  int getCFGBytes() { return 1; };
  int getGAINBytes() { return 4; };

private:
  SVTV1P0_Reader *detector;

  short packed_address[SVT_WAFERS][SVT_HYBRIDS];  
  
  short pack(short hypersector, short rcb, short mz, short offset);
  void unpack(ANODK_entry *, short anodedress);
}; 

#endif





