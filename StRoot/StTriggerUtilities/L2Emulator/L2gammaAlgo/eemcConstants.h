/* EEMC geometry constants */
const unsigned short kEEmcNumEtas      = 12;     /* number of eta bins */
const unsigned short kEEmcNumPhis      = 60;     /* number of phi bins */
const unsigned short kEEmcNumSubs      = 5;      /* number of subsectors per sector */
const unsigned short kEEmcNumSecs      = kEEmcNumPhis / kEEmcNumSubs;

const unsigned short kEEmcNumTower     = 720;
const unsigned short kEEmcNumPatch     = kEEmcNumTower;
const unsigned short kEEmcNumRdo       = kEEmcNumTower;

const float kEEmcEtaBins[]=
  {
    2.0    , 1.9008 , 1.8065 , 1.7168 , 1.6317 , 1.5507 , 1.4738 ,
    1.4007 , 1.3312 , 1.2651 , 1.2023 , 1.1427 , 1.086  , 0.0000
  };

/* EEMC calibration constants */
const unsigned short kEEmcMaxADC       = 4095;
const float  kEEmcMaxET        = 60;
const float  kEEmcIdealGainT   = kEEmcMaxADC / kEEmcMaxET;


/* other control flags/settings for the algorithm */
const int    kEEmcHistoBase    = 1000;
const unsigned short kEEmcQAPrescale   = 10;  // reduce frequency of qa histograming
