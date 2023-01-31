/* BEMC geometry constants */
const unsigned short kBEmcNumEtas      = 40;     /* number of eta bins */
const unsigned short kBEmcNumPhis      = 120;    /* number of phi bins */
const unsigned short kBEmcNumSubs      = 10;     /* number of subsectors per sector */
const unsigned short kBEmcNumSecs      = kBEmcNumPhis / kBEmcNumSubs;

const unsigned short kBEmcNumTower     = 4800;
const unsigned short kBEmcNumPatch     = kBEmcNumTower;
const unsigned short kBEmcNumRdo       = kBEmcNumTower;

const float kBEmcEtaBins[]=
  {
    -1.000,  -0.950,  -0.900,  -0.850,  
    -0.800,  -0.750,  -0.700,  -0.650,  
    -0.600,  -0.550,  -0.500,  -0.450,  
    -0.400,  -0.350,  -0.300,  -0.250,  
    -0.200,  -0.150,  -0.100,  -0.050,  
    +0.000,  +0.050,  +0.100,  +0.150,  
    +0.200,  +0.250,  +0.300,  +0.350,  
    +0.400,  +0.450,  +0.500,  +0.550,  
    +0.600,  +0.650,  +0.700,  +0.750,  
    +0.800,  +0.850,  +0.900,  +0.950,  +1.000
  };

/* BEMC calibration constants */
const unsigned short kBEmcMaxADC       = 4095;
//const float  kBEmcMaxET        = 27.5; /* for 2005 data/testing */
const float  kBEmcMaxET        = 56.0; /* for 2006 online */
const float  kBEmcIdealGainT   = kBEmcMaxADC / kBEmcMaxET;


/* other control flags/settings for the algorithm */
const int    kBEmcHistoBase    = 1000;
const unsigned short kBEmcQAPrescale   = 10;  // reduce frequency of qa histograming
