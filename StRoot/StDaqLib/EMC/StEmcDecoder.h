/*!\class StEmcDecoder
\author Alexandre A. P. Suaide

This class makes the decodification from EMC daq
and electronics scheme to software scheme. This class
has methods to decode the numbers in both directions and
it works for SMD and towers.<br><br>

The current id's definitions are:<br>
      - for Towers
            - 0 <= RDO (daq_id)            <= 4799
            - 1 <= Crate                   <= 30
            - 0 <= Crate_sequency          <= 159
            - 1 <= crate board             <= 5
            - 0 <= position on board       <= 31
            - 0 <= TDC                     <= 29
            - 0 <= TDC channels            <= 29
            - 0 <= tdc sequency on channel <= 159
            - 1 <= Tower (jose) id         <= 4800       
      - for SMD
            - 0 <= RDO             <= 7
            - 0 <= index           <= 4799
            
*/ 
#include <Stiostream.h>
#include <Stiostream.h>
#include "Stiostream.h"

#ifndef StEmcDecoder_HH
#define StEmcDecoder_HH
class StEmcDecoder
{
  protected:
    
    int       Init_Crate[30];
    int       TDC_Crate[30];
    int       Crate_TDC[30];
    int       ReverseOrder[4800];
		int       PMT_Box[60];
		int       ReversePMT_Box[4800][2];
    
    int       SmdModules[8][15];
    int       FEE1[4],FEE2[4],FEE3[4];
    int       connector1[20],connector2[20],connector3[20];
    int       SmdeRDO[120][150],SmdpRDO[120][10][15];
    int       SmdeIndex[120][150],SmdpIndex[120][10][15];
    int       PsdModules[4][15];
    int       PsdRDO[4800];
    int       PsdIndex[4800];
    
    int       Getjose_towerWest(int,int);///<Get Software Id for West size for towers
    int       Getjose_towerEast(int,int);///<Get Software Id for East side for towers
    int       getSmdModule(int,int,int&);///<Get SMD module in a given position in RDO
    int       checkDummy(int);///<Check dummy positions on SMD crate
    int       getSmdPin(int,int,int,int&);///<Get SMD pin number
    int       getSmdpStrip(int,int&,int&);///<Get SMDP strip
    
    void      Init(unsigned int,unsigned int);///< Init method
        
  public:
              StEmcDecoder(unsigned int date=20010101,unsigned int time=000000);///< StEmcDecoder constructor
    virtual   ~StEmcDecoder();///< StEmcDecoder destructor

    int       GetTowerIdFromDaqId(int,int&);///<Get Sofwtare Id from Daq Id for towers
    int       GetDaqIdFromTowerId(int,int&);///< Get Daq Id from Software Id for towers
    int       GetTowerIdFromCrate(int,int,int&);///<Get Software Id from Crate number and position in crate for towers
    int       GetTowerIdFromTDC(int,int,int&);///<Get Software Id from TDC channel number and position in TDC for towers
    int       GetTowerIdFromPMTBox(int,int,int&);///<Get Software Id from PMT Box number and position in the box for towers
    int       GetTowerCrateFromDaqId(int,int&,int&);///< Get crate number from Daq Id for towers
    int       GetTowerCrateFromTDC(int,int&);///<Get crate number from TDC channel for towers
    int       GetTowerTDCFromCrate(int,int&);///< Get TDC channel from crate number for towers
    int       GetTowerTDCFromDaqId(int,int&);///< Get TDC channel from Daq Id for towers
		int       GetPMTBoxFromTowerId(int,int&,int&); ///<Get PMT box and position from tower Id
    int       GetTowerBin(int,int&,int&,int&);///<Transition from environment rid to m,e,s for towers
    
    int       GetSmdCoord(int,int,int&,int&,int&,int&,bool=false);///<Get SMD detector (3==SMDE, 4==SMDP), m, e, s from RDO and position for SMD
    int       GetSmdRDO(int,int,int,int,int&,int&);///<Get SMD fiber and position from detector number (3==SMDE, 4==SMDP), m, e, s

    int       GetPsdId(int,int,int&,bool=false);///<Get PSD id
    int       GetPsdRDO(int,int&,int&);///<Get PSD fiber and position from id
    
    void      PrintTowerMap(ofstream *);///<Print Tower MAP
    void      PrintSmdMap(ofstream *);///<Print SMD MAP
    
};
#endif
