#include <math.h>

#ifndef THLCONSTANTS
#define THLCONSTANTS
//
//    Values copied from the tpg_detector table
//
  static double vDrift         = 5.4716e+06;  // cms/s
  static double clockFrecuency = 1.3359e+07 ; // 1/s
//  static double triggerOffset  = 1.7e-06 ; // time diff between the event and theRDO trigger
  static double triggerOffset  = 0. ; // time diff between the event and theRDO trigger
//
  static double driftLength = 211.0 - vDrift * triggerOffset ;
  //static double driftLength = 0. ;
  static double timeScale   = driftLength / (512. - clockFrecuency * triggerOffset) ;
  //static double timeScale   = 1. ;
  static double offset      = 0. ;


/* statics
   padrow-offset (center pad) from detector-center in mm */
   static float padrowOffset [] =
   {
      60.0F, 64.8F, 69.6F, 74.4F, 79.2F, 84.0F, 88.8F, 93.60F, /*   7 * 4.80 cm spacing  */
      98.8F, 104.F, 109.20F, 114.4F, 119.6F,                   /*   5 * 5.20 cm spacing  */
     127.195F, 129.195F, 131.195F, 133.195F, 135.195F,        /*  32 * 2.00 cm spacing  */
     137.195F, 139.195F, 141.195F, 143.195F, 145.195F,
     147.195F, 149.195F, 151.195F, 153.195F, 155.195F,
     157.195F, 159.195F, 161.195F, 163.195F, 165.195F,
     167.195F, 169.195F, 171.195F, 173.195F, 175.195F,
     177.195F, 179.195F, 181.195F, 183.195F, 185.195F,
     187.195F, 189.195F
   };
/* cross-spacings between adjacent pads in cm */
   static float padSpacing [] =
   {
     .335F, .335F, .335F, .335F, .335F, /*  13 * .335 cm  */
     .335F, .335F, .335F, .335F, .335F,
     .335F, .335F, .335F,
     .670F, .670F, .670F, .670F, .670F, /*  32 * .670 cm  */
     .670F, .670F, .670F, .670F, .670F,
     .670F, .670F, .670F, .670F, .670F,
     .670F, .670F, .670F, .670F, .670F,
     .670F, .670F, .670F, .670F, .670F,
     .670F, .670F, .670F, .670F, .670F,     .670F, .670F
    };/* number of pads in padrow */
   static short nPadsInRow[45] =
   {
      88,96,104,112,118,126,134,142,150,158,166,174,182,
      98,100,102,104,106,106,108,110,112,112,114,116,
     118,120,122,122,124,126,128,128,130,132,134,136,
     138,138,140,142,144,144,144,144
   };
//
/* sector-rotation factors */
   static float SectorSinus [] =
   {
  /*  30 deg each segment */
      0.866025404F,  /*  60 deg */
      0.5F,          /*  30 deg */
      0.0F,          /*   0 deg */
     -0.5F,          /* 330 deg */
     -0.866025404F,  /* 300 deg */
     -1.0F,          /* 270 deg */
     -0.866025404F,  /* 240 deg */
     -0.5F,          /* 210 deg */
      0.F,           /* 180 deg */
      0.5,           /* 150 deg */
      0.866025404F,  /* 120 deg */
      1.0F           /*  90 deg */
   };
   static float SectorCosinus [] =
   {
      0.5F,          /*  60 */
      0.866025404F,  /*  30 */
      1.0F,          /*   0 */
      0.866025404F,  /* 330 */
      0.5F,          /* 300 */
      0.0F,          /* 270 */
     -0.5F,          /* 240 */
     -0.866025404F,  /* 210 */
     -1.0F,          /* 180 */
     -0.866025404F,  /* 150 */
     -0.5F,          /* 120 */
      0.0F           /*  90 */
   };
#endif
//
