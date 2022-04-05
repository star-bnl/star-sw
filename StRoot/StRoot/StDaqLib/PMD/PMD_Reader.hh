/****************************************************************************
 * Author : Susanta and Subhasis
 *
 * Description : Reads PMD Raw and Pedestals Data and fills PMD Specific Bank.
 *               PMD has been taken as Extended Detector as defined by Tonko.
 *               Designed based on Event Pool Reader of PMD ( Ref: Tonko )
 *
 * **************************************************************************/
#ifndef PMD_READER_HH
#define PMD_READER_HH

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"

#define	PMD_SECTOR	 2
#define	PMD_CRAMS_MAX	 12
#define	PMD_CRAMS_BLOCK	 2
#define	PMD_CRAMS_CH_MAX 1728
//#define	PMD_CRAMS_CH_MAX 2016
 
// bank seqeuence in the SECP bank
#define PMD_ADCD_N      0
#define PMD_PEDR_N      1
#define PMD_RMSR_N      2
#define PMD_THRR_N      3

#define	SM	15
#define	ROW	15
#define	COL	15
struct Bank_PMDP: public Bank
{
  struct Pointer sec[PMD_SECTOR];
} ;

struct Bank_PMDSECP: public Bank
{
  struct Pointer type[4];
};

struct Bank_PMDADCD: public Bank
{
  unsigned long data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)];
};

struct Bank_PMDPEDR: public Bank
{
  unsigned int data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)];
};

struct Bank_PMDRMSR: public Bank
{
  unsigned int data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)];
};

struct Bank_PMDTHRR: public Bank
{
  unsigned int data[PMD_CRAMS_MAX*2*(1+PMD_CRAMS_CH_MAX)];
};


struct data{
        int mode ;      // 0 normal, 1 ped
        int channels ;
        int max_channels ;      // 2*10*2*2016
        int no_of_channels_in_cram_blk[2][PMD_CRAMS_MAX][PMD_CRAMS_CH_MAX];	 
        // 2 sectors, 10 CRAMS, 2 CRAM channels, 2016 values max
	unsigned short adc[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	         
	unsigned short ped[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	unsigned short rms[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	unsigned short thr[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	                                 
} ;

struct Bank_DATA: public data{};


class PMD_Reader{
	int ProcessEvent(const Bank_PMDP *PmdPTR); /// Process PMD+CPV events
public:
	PMD_Reader(EventReader *er, Bank_PMDP *pPMDP); /// PMD_Reader constructor
	~PMD_Reader() {}; /// PMD_Reader destructor
        int adcReader(int sec, Bank_PMDADCD *adcd);
        int pedReader(int sec, int type, Bank_PMDPEDR *pedr);
	int NPMDHits();    // Not Implemented yet
	int NCPVHits();    // Not Implemented yet
	Bank_DATA getPMD_ADC();

protected:
	// copy of EventReader pointer
	 EventReader       *ercpy;
	 Bank_PMDP         *pBankPMDP;

	 // Data Banks
	 Bank_PMDADCD	mThePmdAdcD;
         Bank_PMDPEDR	mThePmdPedR;
         Bank_PMDRMSR	mThePmdRmsR;
         Bank_PMDTHRR	mThePmdThrR;
	 Bank_DATA	mThePmd;

	 bool	mPmdPresent;
	 bool	mCpvPresent;
};

PMD_Reader *getPMDReader(EventReader *er);

	 
#endif
