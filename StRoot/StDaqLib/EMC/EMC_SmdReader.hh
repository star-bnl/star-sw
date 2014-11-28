#ifndef EMC_SMDREADER_HH
#define EMC_SMDREADER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "EMC_Reader.hh"
#include "StEmcDecoder.h"
/*!\class EMC_SmdReader
\author Subhasis, Herbert Ward and Alexandre A. P. Suaide

EMC Barrel SMD reader.
*/ 

class EMC_SmdReader 
{
  public:
    //  move the constructor guts {...} to a .cxx file
                  EMC_SmdReader(EventReader*er,Bank_EMCP *pEMCP);///<EMC_SmdReader constructor
                  ~EMC_SmdReader();///<EMC_SmdReader destructor
    void          Initialize();///<Initialization of arrays
    int           ProcessBarrelSmd(const Bank_EMCP*);
    int           FillBarrelSmd(Bank_SMDADCR*,int);
    Bank_EMCSECP* getBarrelSmdSection(const Bank_EMCP*,int);
    Bank_EMCRBP*  getBarrelSmdFiber(Bank_EMCSECP*,int);
    Bank_SMDADCR* getSmdADC(Bank_EMCRBP*);
    void          PrintSmdArray();

    Bank_BSMDADCR& getBSMDADCR();
                   
  protected:
    Bank_EMCP*    pBankEMCP;
    EventReader*  ercpy;
    StEmcDecoder* decoder;

    Bank_BSMDADCR mTheSmdAdcR;
    Bank_BSMDADCD mTheSmdAdcD;
    Bank_BSMDPEDR mTheSmdPedR;
    Bank_BSMDRMSR mTheSmdRMSR;

    int            mNSMD;
};
#endif
