//  
// l2 algorithms
//

#ifndef l2Algorithm_h
#define l2Algorithm_h

#define L2_EMC_CALIB_ALGO    22
#define L2_EMC_HIENE_ALGO    25
#define L2_EMC_PED_ALGO      15
#define L2_BTOW_GAMMA_ALGO   17
#define L2_ETOW_GAMMA_ALGO   19
#define L2_SPIN_SLOW         23
#define L2_SPIN_FAST         24
#define L2_EMC_JET_ALGO      12
#define L2_EMC_JET_HIGH_ALGO 13 
#define L2_EMC_UPSILON_ALGO  11
#define L2_BTOW_W_ALGO       16 
#define L2_EMULATE_HARDWARE  20

#define BTOW_CRATES 30
#define ETOW_CRATES 6

#if 0
//#include "emul_l0.h" //re-added for using old TCU in 500GeV
#include "L2EmcDb.h"
#include "L2EmcGeom.h"
#include "L2pedAlgo09.h"
#include "L2btowCalAlgo09.h"
#include "L2etowCalAlgo09.h"
#include "L2bemcGamma2009.h"
#include "L2eemcGamma2009.h"
#include "L2Upsilon2009.h"
#include "L2jetAlgo2009.h"
#include "L2hienAlgo09.h"
#include "L2wBemc2009.h"
#include "L2emulL0_2009.h"//added for using old TCU in 500GeV run.
#endif

//define offsets for writing to L2Result:
#define L2RESULTS_2009_OFFSET_EMC_CHECK   1
#define L2RESULTS_2009_OFFSET_EMC_PED     2
#define L2RESULTS_2009_OFFSET_BGAMMA      3
#define L2RESULTS_2009_OFFSET_EGAMMA      6
#define L2RESULTS_2009_OFFSET_DIJET       9
#define L2RESULTS_2009_OFFSET_UPSILON     17
#define L2RESULTS_2009_OFFSET_BEMCW       20
#define L2RESULTS_2009_OFFSET_DIJET_HIGH  25
//#define L2RESULTS_2009_OFFSET_BHIEN       42
#define L2RESULTS_2009_C2_OFFSET_BHIEN       0 //this writes to the start of the C2 array.
#define L2RESULTS_2009_OFFSET_EHIEN       0
#define L2RESULTS_2009_OFFSET_BTOW_CAL    0
#define L2RESULTS_2009_OFFSET_ETOW_CAL    0




//#define L2RESULTS_2008_OFFSET_EMC_CHECK   1


#endif

