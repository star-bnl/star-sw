// \class  EEsoloPi0
// \author Jan Balewski
#ifndef EEsoloPi0_h
#define EEsoloPi0_h
/******************************************************
 * $Id: EEsoloPi0.h,v 1.7 2009/02/04 20:33:21 ogrebeny Exp $
 ******************************************************
 * Descripion:
 *  finds pi0 based on EEMC tower response
 ******************************************************/

class EEmcGeomSimple;
class TVector3;
class TObjArray;
class TH1F ;
class EEfeeRawEvent;
class EEstarTrig;
class EEmcEventHeader;
class EEmcDbItem;

#include "StEEmcUtil/EEfeeRaw/EEdims.h"


/// the trick to switch between two DB readers
#ifdef StRootFREE
  class EEmcDb;
  typedef EEmcDb EEDB;
#else
  class StEEmcDb;
  typedef StEEmcDb EEDB;
#endif


class EEsoloPi0 {
 protected:
  enum {MxTwEta=12, MxTwPhi=60, MxTw=12*60};
  enum {mxTile=4,kT=0,kP=1, kQ=2,kR=3, kU=0, kV=1}; // 0=tower, 1=pres1, 2=pres2, 3=post
 
 private:
  EEmcGeomSimple *geom;
 protected:
  int  nInpEve;
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

  int dbMapped;

  void clear();
  void tagCluster(int k0,int d=1);
  void sumTwClusterEnergy(int ic,int d=1);
  float sumPatchEnergy(int k0,int d,EEsoloMipA *soloMipX, float *maxVal=0);
  int findInvM(Cluster *, Cluster *, TH1F **);

  EEDB *eeDb; /// DB access point
  TObjArray  *HList; /// output histo access point

 public:
  
  EEsoloPi0();
  virtual ~EEsoloPi0();
  void print();
  void finish();
  int findTowerClust();
  void findTowerPi0();
  void init( );
  void initRun(int runID);// must be called after DB timestamp is known

  void set(float a, float b,  float d, float m1=0.11, float m2=0.16 )
    {scaleFactor =a; seedEnergy=b;  shapeLimit=d;  mLo=m1; mHi=m2; }
 
  ClassDef(EEsoloPi0,1) 
};
#endif

 


/*****************************************************************
 * $Log: EEsoloPi0.h,v $
 * Revision 1.7  2009/02/04 20:33:21  ogrebeny
 * Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
 *
 * Revision 1.6  2004/09/03 04:50:52  balewski
 * big clenup
 *
 * Revision 1.5  2004/08/26 04:39:40  balewski
 * towards pi0
 *
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

