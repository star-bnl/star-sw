/*
 ***************************************************************************
 * Author: Herb Ward
 ***************************************************************************
 * Description: definitions for TRG
 *      
 *
 *   change log
 *
 *************************************************************************** 
*/

#ifndef TRG_Reader_hh
#define TRG_Reader_hh

//////////////////////////////////////////////  includes  //////////////////////
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
/////////////////////////////////////////////  classes and structures  /////////
class Bank_TRGP: public Bank {
public:
  Pointer theData; // unlike the TPC, we don't need an array here.
};
struct trgEventDescriptor {
  INT32 bunchCrossingNumber1;  // Warning: These should be interpreted as unsigned with sth like
  INT32 bunchCrossingNumber2;  // unsigned int hh2 = *(unsigned int*)(&bunchCrossingNumber2).
                               // I declare them as INT32 (int) so that I can feed them to the
                               // generic byte-swapping function.
  INT16 token;
  INT16 actionWord;
  INT16 lastDsmOutput;
  INT16 dsmRawDataAddress;
                 unsigned char busyBits;
                 unsigned char trgFlags;
  INT16 triggerWord;
  INT16 nPre;
  INT16 nPost;
};
struct trgSummary {
  INT16 lengthTriggerSummary;
  INT16 trgSummaryFormat;
                 unsigned char trgL0[2];
  INT16 L0Length;
  INT32 L0Data[32];
                 unsigned char trgL1[2];
  INT16 L1Length;
  INT32 L1Data[64];
                 unsigned char trgL2[2];
  INT16 L2Length;
  INT32 L2Data[64];
};
struct trgRawData {
  INT16 lengthTriggerRaw;
  INT16 trgRawFormat;
                 unsigned char trgCTB[2];
  INT32 CTBdata[64];
                 unsigned char trgMWZ[2];
  INT32 MWZdata[36];  // MWC+ZDC
                 unsigned char trgEMC[2];
  INT32 EMCdata[120];
};
class Bank_TRGD: public Bank {
public:
  trgEventDescriptor trgED;
  trgSummary         trgSum;
  trgRawData         trgRaw;
// Override default swap because of complicated structure of trg data bank.
  int swap();  
  int swapHerb2bytes(unsigned short *data,int number);
  int swapHerb4bytes(unsigned int   *data,int number);
  int swapHerb4bytes(unsigned long  *data,int number);

  int swapHerb2bytes(short *data, int number)
     {return swapHerb2bytes((unsigned short*)data,number);}
  int swapHerb4bytes(int   *data, int number)
     {return swapHerb4bytes((unsigned int  *)data,number);}
  int swapHerb4bytes(long  *data, int number)
     {return swapHerb4bytes((unsigned long *)data,number);}
  int HerbSwap();           //!
  int HerbSwap2000();       //!
  int HerbSwap2003(char*);  //!
  int HerbSwap2004(char*);  //!
  int HerbSwap2005(char*);  //! for 2007 and later see TRG_Reader::UnpackTrg below
  // void PrintAllTheData(FILE *ff);
  // void PrintDataCompact(FILE *ff);
  char *PrintHelp(char*,int);
};

class TRG_Reader {
  friend class EventReader;
public:
  TRG_Reader(EventReader *er, Bank_TRGP *pTRGP);
  ~TRG_Reader(){ 
    if (pBankUnp) delete[] pBankUnp;
    if (pTRGD)    delete   pTRGD;
  }; 
  Bank_TRGD *pBankTRGD;     // Making this public saves 2 large layers of accessor functions.
  int YearOfData(char *);   //!
  int S_mode;               //!
  int GetErr(){return mErr;}

protected:
  EventReader *ercpy;       // copy of EventReader pointer
  Bank_TRGP *pBankTRGP;     // Bank Pointers
  int mErr;                 //!
  char *pBankUnp;           //! Pointer to keep workspace for unpacked trigger struct
  unsigned int sizeUnp;     //! Size of unpacked bank space
  Bank_TRGD *pTRGD;         //! Dummy to use byte swap code

private:
  void dumpWordsToScreenInHexAndExit(int); //!
  void SanityCheck(int);                   //!
  void SanityCheck2000(int);               //!
  void SanityCheck2003(char*, int);        //!
  void SanityCheck2004(char*, int);        //!
  void SanityCheck2005(char*, int);        //!
  void SanityCheck2007(char*, int);        //!
  void SanityCheck2008(char*, int);        //!
  int  UnpackTrg2007(Bank_TRGP*);          //!
  int  Swap2007_DescSum(char*);            //!
  int  Swap2007_Raw(char*);                //!
  int  UnpackTrg2008(Bank_TRGP*);          //!
  int  Swap2008_TrgTowerTrnfer(char*);     //!
  int  Swap2008_DescSum(char*);            //!
  int  Swap2008_Raw(char*);                //!
};

TRG_Reader *getTRGReader(EventReader *er);

#endif
