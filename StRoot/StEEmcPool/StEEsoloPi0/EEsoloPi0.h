// \class  EEsoloPi0
// \author Jan Balewski
#ifndef EEsoloPi0_h
#define EEsoloPi0_h
/*********************************************************************
 * $Id: EEsoloPi0.h,v 1.4 2004/05/07 21:38:38 balewski Exp $
 *********************************************************************
 * Descripion:
 *  finds pi0 based on EEMC tower response
 *********************************************************************/

#include "TObject.h"

class EEmcGeomSimple;
class TVector3;
class TObjArray;
class TH1F ;
class EEfeeRawEvent;
class EEstarTrig;
class EEmcEventHeader;
class EEmcDbItem;

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

#ifdef NO_ROOT4STAR
  class EEmcDb;
  typedef EEmcDb MYDB;
#else
  class StEEmcDbMaker;
  typedef StEEmcDbMaker MYDB;
#endif


class EEsoloPi0 :public TObject{
 protected:
  enum {MxTwEta=12, MxTwPhi=60, MxTw=12*60};

 private:
  EEmcGeomSimple *geom;
 protected:
  float seedEnergy; // lower limit in search for seeds
  float shapeLimit; // cut on eHTower/eCluster 
  float mLo, mHi; // define mass of meson

 
  int timeSec;// time interval from the first processed event
  int TotN2g; // total # of gam-gam pairs 
  int totPi0; // total # of real pi0
  int totXPi0; // total # of  pi0 from mixed events
  
  int nClust; // counts clusters
  struct EEsoloMipA{int key,id; float e;};
  struct Cluster {int k1; float eH,eC,fphi,feta; } clust[MxTw] ,oldClust;

  float scaleFactor; // converts energy from eeTree --> GeV, old

  TH1F *hA[32], *hR[64], *hM[64]; // all, real , mixed eve
  EEsoloMipA soloMip[MxTw]; // stores all towers

  const  EEmcDbItem  *soloMipDb[MxTw]; // stores DB pointers for all towers

  void clear();
  void tagCluster(int k0,int d=1);
  void sumTwClusterEnergy(int ic,int d=1);
  float sumPatchEnergy(int k0,int d,EEsoloMipA *soloMipX, float *maxVal=0);
  int findInvM(Cluster *, Cluster *, TH1F **);
  MYDB *db;
 public:
  
  EEsoloPi0();
  virtual ~EEsoloPi0();
  void print();
  void finish();
#ifdef NO_ROOT4STAR
  int getTowerAdc(EEfeeRawEvent  *feeEve, EEstarTrig *eTrig=0,EEmcEventHeader *eHead=0, int n1=0, int n2=240);
#endif
  int findTowerClust();
  void findTowerPi0();
  void init( MYDB *, TObjArray * L=0);
  void set(float a, float b,  float d, float m1=0.11, float m2=0.16 )
    {scaleFactor =a; seedEnergy=b;  shapeLimit=d;  mLo=m1; mHi=m2; }

 
  ClassDef(EEsoloPi0,1) 
};
#endif

/* fix in St-code
1) clear soloMipDb[MxTw] in InitRun

*/
 


/*****************************************************************
 * $Log: EEsoloPi0.h,v $
 * Revision 1.4  2004/05/07 21:38:38  balewski
 * gamma finder with SMD
 *
 * Revision 1.3  2004/05/05 04:39:45  balewski
 * works with muDst & ezTree
 *
 * Revision 1.2  2004/04/14 19:34:01  balewski
 * access to trigger data
 *
 * Revision 1.1  2004/04/14 17:09:09  balewski
 * new copy of pi0finder with towers only, should work on ezTree as well (after small cleanup)
 *
 * Revision 1.3  2004/03/01 15:23:17  balewski
 * inv M ves eta added
 *
 * Revision 1.2  2004/02/26 04:24:47  balewski
 * pi0
 *
 * Revision 1.1  2004/02/17 03:08:58  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2003/06/24 04:22:01  balewski
 * pi0-reco with towers only
 *
 *
 ********************************************************************/

