/*!\class StMuTowerData
\author Marco van Leeuwen

This class holds two byte arrays with the raw tower info for BEEMC and EEMC,
for storage on MuDst.

The StMuEmcCollection uses this.
*/
#ifndef StMuEmcTowerData__h
#define StMuEmcTowerData__h
 
#include "TObject.h"
#include "Stiostream.h"
#include "StEnumerations.h"

enum {bemc=1, bprs=2, bsmde=3, bsmdp=4, eemc=5, eprs=6, esmdu=7, esmdv=8};

class StMuEmcTowerData: public TObject
{
  public:
                      StMuEmcTowerData();
                      StMuEmcTowerData(const StMuEmcTowerData&);
    virtual           ~StMuEmcTowerData();
    int               towerADC(int id, int detector = bemc) const;    
    StEmcCrateStatus  crateStatus(int crate, int detector = bemc) const;
    void              clearBemc();
    void              clearEemc();

    // EEMC utility methods
    int   getNEndcapTowerADC() const { return nEndcapTowers;}
    void  getEndcapTowerADC(int ihit, int &adc, int &sec, int &sub, int & eta) const;

    void              setTowerADC(int,int, int detector = bemc);
    void              setCrateStatus(StEmcCrateStatus status, int crate, int detector = bemc);

    enum { nBTowCrates=30, nBSmdCrates=8, nBPrsCrates=4, nETowCrates=6, 
	   nESmdCrates=36, nEPrsCrates=12};

  protected:
    enum {nEmcTowers=4800, nEndcapTowers=720};
    unsigned short    mTowerADC[nEmcTowers];
    unsigned short    mEndcapTowerADC[nEndcapTowers];
    unsigned char     mBTowCrateFlags[nBTowCrates];
    unsigned char     mBSmdCrateFlags[nBSmdCrates];
    unsigned char     mBPrsCrateFlags[nBPrsCrates];
    unsigned char     mETowCrateFlags[nETowCrates];
    unsigned char     mESmdCrateFlags[nESmdCrates];
    unsigned char     mEPrsCrateFlags[nEPrsCrates];
    ClassDef(StMuEmcTowerData,1)
};



#endif  
    
