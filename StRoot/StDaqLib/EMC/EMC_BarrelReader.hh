/////////////////////Barrel Reader/////////////////////////

#ifndef EMC_BARRELREADER_HH
#define EMC_BARRELREADER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
#include "EMC_Reader.hh"

//class EMC_BarrelReader:public EMC_Reader {
class EMC_BarrelReader
{

    public:
                       EMC_BarrelReader(EventReader *er,Bank_EMCP *pEMCP);
      void             Initialize();
      int              ProcessBarrelTower(const Bank_EMCP*);
      Bank_EMCSECP*    getBarrelSection(const Bank_EMCP*,int);
      Bank_EMCRBP*     getBarrelTowerFiber(Bank_EMCSECP*,int);
      Bank_TOWERADCR*  getBarrelADC(Bank_EMCRBP*);
      int              FillBarrelTower(Bank_TOWERADCR*);
      void             PrintTowerArray();
      int              GetCrate(int daq_tower,int& crate_seq);
      int              Getjose_tower(int start,int& crate_seq);   
      int              get_index_jose(int daq_tower,int& index_jose);
      int              getTowerBin(const int,int &,int &,int &);
      int              checkTowerId(const int);
      Bank_BTOWERADCR& getBTOWERADCR();
                       ~EMC_BarrelReader(){}; 

    protected:
      Bank_EMCP*       pBankEMCP;
      EventReader*     ercpy;

      Bank_BTOWERADCR  mTheTowerAdcR;
      Bank_BTOWERADCD  mTheTowerAdcD;
      Bank_BTOWERPEDR  mTheTowerPedR;
      Bank_BTOWERRMSR  mTheTowerRMSR;
};
#endif
