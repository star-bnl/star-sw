/////////////////////Barrel Reader/////////////////////////

#ifndef EMC_SMDREADER_HH
#define EMC_SMDREADER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "EMC_Reader.hh"

class EMC_SmdReader {

public:
  //  move the constructor guts {...} to a .cxx file
                EMC_SmdReader(EventReader*er,Bank_EMCP *pEMCP);
  void          Initialize();
  int           ProcessBarrelSmd(const Bank_EMCP*);
  Bank_EMCSECP* getBarrelSection(const Bank_EMCP*,int);
  Bank_EMCRBP*  getBarrelSmdFiber(Bank_EMCSECP*,int);
  Bank_SMDADCR* getSmdADC(Bank_EMCRBP*);
  int           FillBarrelSmd(Bank_SMDADCR*,int);
  void          PrintSmdArray();

//methods used for conversion after Jo's aug'01 mail
  int           getSmdBin(const int,const int,int &,int &,int&);
  int           get_RDOch(const int,const int,int&,int&,int&);
  int           GetModuleFromConnector(int&,int&);
  int           checkdummy(int&);
  int           getsmdfiber(int&,int&,int&,int&);
  int           get_smdphistrip(int&,int&,int&);

//additional methods used for conversion before Jo's aug'01 mail
  int           getmodule(const int,int&,int&);
  int           get_fiberno(const int,int&);

  // int checkTowerId(const int);
  Bank_BSMDADCR& getBSMDADCR();


  ~EMC_SmdReader(){}; 
protected:
  Bank_EMCP*    pBankEMCP;
  EventReader*  ercpy;

  Bank_BSMDADCR mTheSmdAdcR;
  Bank_BSMDADCD mTheSmdAdcD;
  Bank_BSMDPEDR mTheSmdPedR;
  Bank_BSMDRMSR mTheSmdRMSR;

};
#endif
