// $Id: StL2EmulatorMaker.h,v 1.3 2007/10/12 20:11:50 balewski Exp $

/* \class  StL2EmulatorMaker
\author Jan Balewski

Interfaces L2 algos to the STAR ofl software

 */


#ifndef STAR_StEmcTrigSimuMaker
#define STAR_StEmcTrigSimuMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

class  StEEmcDbMaker;
class  StEmcGeom;
class  StEmcDecoder;
class  L2VirtualAlgo;
class  L2EmcDb;
// try ...
class  L2pedAlgo;
class  L2jetAlgo;


class StL2EmulatorMaker : public StMaker {
 private:
  int  mTotInpEve;
  bool mUseMuDst;
  int  mYear;
  int  mMCflag; // set mcFlag=0 for real data
  TString  mSetupPath;
  TString  mOutPath;

  void doBanksFromStRawData();
  void doBanksFromMuDst();
  bool getTriggerData();

  //replicas of oryginal daq data containers
  unsigned short *mBTOW_BANK;
  unsigned short *mETOW_BANK;
  void           *mTrigData; // I do not want to deal with this content here
  int  mBTOW_in, mETOW_in;


  // needed to regenerate raw data banks
  StEEmcDbMaker *mDbE;
  StEmcGeom     *mGeomB;
  StEmcDecoder  *mMappB; 

  // holds all instantiated L2algos
  L2VirtualAlgo **mL2algo; // actual algos
  int mL2algoN;  //# of existing algos (time-stamp dependent)
  L2EmcDb   *mL2EmcDb;
  L2pedAlgo *mL2pedAlgo;
  L2jetAlgo *mL2jetAlgo;

  void setupL2Algos2006(int yyyymmdd, int runNo); // year dependent setup
  void  addTriggerList();
  
 public: 
  StL2EmulatorMaker(const char *name="StarL2Emul");
  virtual       ~StL2EmulatorMaker();
  void printBEtowers();///<  hits in StEvent
  void printBEblocks();///<  regenerated banks

  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun(int runumber);
  Int_t   Finish();
  virtual void Clear(const Option_t* = "");
  void    useStEvent() {mUseMuDst=false;}
  void    setMC(int x) {mMCflag=x;}
  void setSetupPath(char *x) { mSetupPath=x;}
  void setOutPath(char *x)   { mOutPath=x;}

  vector <int> mTriggerList;
  bool    isTrigger(int trigId);   
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StL2EmulatorMaker.h,v 1.3 2007/10/12 20:11:50 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StL2EmulatorMaker,0)
};

#endif

// $Log: StL2EmulatorMaker.h,v $
// Revision 1.3  2007/10/12 20:11:50  balewski
// cleanu setup , output path
//
// Revision 1.2  2007/10/11 21:22:57  balewski
// added L2-->L0 interface class
//
// Revision 1.1  2007/10/11 00:33:09  balewski
// L2algo added
//
