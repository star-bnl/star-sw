//*-- Author : Alexandre Suaide
// 
/*    
      for Towers
      0 <= RDO (daq_id)            <= 4799
      1 <= Crate                   <= 30
      0 <= Crate_sequency          <= 159
      1 <= crate board             <= 5
      0 <= position on board       <= 31
      0 <= TDC                     <= 29
      0 <= TDC channels            <= 29
      0 <= tdc sequency on channel <= 159
      1 <= Tower (jose) id         <= 4800 
      
      for SMD
      0 <= RDO             <= 7
      0 <= index           <= 4799
*/
#include <iostream.h>
#include <iostream.h>
#include <fstream.h>

#ifndef StEmcDecoder_HH
#define StEmcDecoder_HH
class StEmcDecoder
{
  protected:
    
    int       Init_Crate[30];
    int       TDC_Crate[30];
    int       Crate_TDC[30];
    int       ReverseOrder[4800];
    
    int       SmdModules[8][15];
    int       FEE1[4],FEE2[4],FEE3[4];
    int       connector1[20],connector2[20],connector3[20];
    int       SmdeRDO[120][150],SmdpRDO[120][10][15];
    int       SmdeIndex[120][150],SmdpIndex[120][10][15];
    
    int       Getjose_towerWest(int,int);
    int       Getjose_towerEast(int,int);
    int       getSmdModule(int,int,int&);
    int       checkDummy(int);
    int       getSmdPin(int,int,int,int&);
    int       getSmdpStrip(int,int&,int&);
    
    void      Init(unsigned int,unsigned int);
        
  public:
              StEmcDecoder(unsigned int date=20010101,unsigned int time=000000);
    virtual   ~StEmcDecoder();

    int       GetTowerIdFromDaqId(int,int&);
    int       GetDaqIdFromTowerId(int,int&);
    int       GetTowerIdFromCrate(int,int,int&);
    int       GetTowerIdFromTDC(int,int,int&);
    int       GetTowerCrateFromDaqId(int,int&,int&);
    int       GetTowerCrateFromTDC(int,int&);
    int       GetTowerTDCFromCrate(int,int&);
    int       GetTowerTDCFromDaqId(int,int&);
    int       GetTowerBin(int,int&,int&,int&);
    
    int       GetSmdCoord(int,int,int&,int&,int&,int&);
    int       GetSmdRDO(int,int,int,int,int&,int&);
    
    void      PrintTowerMap(ofstream *);
    void      PrintSmdMap(ofstream *);
    
};
#endif
