// $Id: StGenericL2Emulator.h,v 1.2 2007/10/23 02:47:11 balewski Exp $

/* \class  StGenericL2Emulator
\author Jan Balewski

Interfaces L2 algos to the STAR ofl software

 */


#ifndef STAR_StGenericL2Emulator
#define STAR_StGenericL2Emulator


class  StEEmcDbMaker;
class  StEmcGeom;
class  StEmcDecoder;
class  L2VirtualAlgo;
class  L2EmcDb;


class StGenericL2Emulator  {
 private:
  int  mTotInpEve;
  TString  mOutPath;

  void doBanksFromStRawData();
  void doBanksFromMuDst();

  //replicas of oryginal daq data containers
  unsigned short *mBTOW_BANK;
  unsigned short *mETOW_BANK;
  int  mBTOW_in, mETOW_in;


  // needed to regenerate raw data banks
  StEEmcDbMaker *mDbE;
  StEmcGeom     *mGeomB;
  StEmcDecoder  *mMappB; 

 protected:
  int   mMCflag; // set mcFlag=0 for real data
  int   mYear;
  void *mTrigData; // I do not want to deal with this content here
  bool  mUseMuDst;
  
  // holds all instantiated L2algos
  L2VirtualAlgo **mL2algo; // actual algos
  int mL2algoN;  //# of existing algos (time-stamp dependent)
  L2EmcDb   *mL2EmcDb;
  int  mYearMonthDay;
  TString  mSetupPath;

  enum {mxPar=10}; // for any algo, separate ints & floats
  int intsPar[mxPar]; // params passed from run control gui
  float floatsPar[mxPar]; 

 public: 
  StGenericL2Emulator();
  virtual       ~StGenericL2Emulator();
  void printBEtowers();///<  hits in StEvent
  void printBEblocks();///<  regenerated banks

  void init();
  void make();
  void initRun();
  void  finish();
  void  clear();
  void    useStEvent() {mUseMuDst=false;}
  void    setMC(int x) {mMCflag=x;}
  void setSetupPath(char *x) { mSetupPath=x;}
  void setOutPath(char *x)   { mOutPath=x;}

  vector <int> mTriggerList;
  bool    isTrigger(int trigId);   
  ClassDef(StGenericL2Emulator,0) 
};

#endif

// $Log: StGenericL2Emulator.h,v $
// Revision 1.2  2007/10/23 02:47:11  balewski
// cleanup
//
// Revision 1.1  2007/10/22 23:09:58  balewski
// split L2 to generic and year specific, not finished
//
