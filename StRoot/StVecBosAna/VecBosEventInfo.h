#ifndef VecBosEventInfo_h
#define VecBosEventInfo_h

#include "Globals.h"


/**
 * A simple structure to keep track of the basic information about the barrel EMC detector signals.
 * This class has been inherited from the Run 9 analysis of the longitudinal asymmetry and contains
 * mostly transient data.
 */
class DetEventBemc
{
public:
   // Raw BTOW/BPRS hits
   int   tileIn[mxBTile];           //! 0 if no data
   float adcTile[mxBTile][mxBtow];  //!
   float eneTile[mxBTile][mxBtow];
   int   statTile[mxBTile][mxBtow]; //!
   float maxAdc;                    //!
   int   maxHtDsm;                  //!

   // Raw BSMD hits, both planes
   float adcBsmd[mxBSmd][mxBStrips];  //!
   int   statBsmd[mxBSmd][mxBStrips]; //!

   void clear();
   void print(int flag = 0);

   ClassDef(DetEventBemc, 2);
};


/**
 * A simple structure to keep track of the basic information about the endcap EMC detector signals.
 * This class has been inherited from the Run 9 analysis of the longitudinal asymmetry and contains
 * mostly transient data.
 */
class DetEventEtow
{
public:
   int   etowIn;
   float adc[mxEtowSec*mxEtowSub][mxEtowEta];  ///< the indices correspond to [phibin][etabin]
   float ene[mxEtowSec*mxEtowSub][mxEtowEta];
   int   stat[mxEtowSec*mxEtowSub][mxEtowEta];
   float maxAdc;
   int   maxSec, maxSub, maxEta;
   int   maxHtDsm;

   void clear() {
      memset(adc, 0, sizeof(adc));
      memset(ene, 0, sizeof(ene));
      memset(stat, -1, sizeof(stat)); // default all dead
      maxAdc = 0;
      maxSec = maxSub = maxEta = 0;
      maxHtDsm = -1;
   }

   ClassDef(DetEventEtow, 2);
};


/**
 * A simple structure to keep track of the basic information about the endcap preshower detector
 * signals. This class has been inherited from the Run 9 analysis of the longitudinal asymmetry and
 * contains mostly transient data.
 */
class DetEventEprs
{
public:
   int   eprsIn;
   float adc[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs];  ///< the indices correspond to [phibin][etabin][layer]
   float ene[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs];
   int   stat[mxEtowSec*mxEtowSub][mxEtowEta][mxPrs];

   void clear() {
      memset(adc, 0, sizeof(adc));
      memset(ene, 0, sizeof(ene));
      memset(stat, -1, sizeof(stat)); // default all dead
   }

   ClassDef(DetEventEprs, 1);
};


/**
 * A simple structure to keep track of the basic information about the endcap showermax detector
 * signals. This class has been inherited from the Run 9 analysis of the longitudinal asymmetry and
 * contains mostly transient data.
 */
class DetEventEsmd
{
public:
   int   esmdIn;
   float adc[mxEtowSec][mxEsmdPlane][mxEsmdStrip];  ///< the indices correspond to [phibin][etabin]
   float ene[mxEtowSec][mxEsmdPlane][mxEsmdStrip];
   int   stat[mxEtowSec][mxEsmdPlane][mxEsmdStrip];

   void clear() {
      memset(adc, 0, sizeof(adc));
      memset(ene, 0, sizeof(ene));
      memset(stat, -1, sizeof(stat)); // default all dead
   }

   ClassDef(DetEventEsmd, 1);
};

#endif
