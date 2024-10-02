/*
 * Some basic constants (enums) the GMT.  All GMT related constants
 * that are not in the database should be in this file, except those
 * which cannot be expressed as an int.  These remaining constants are
 * in the file StGmtGeom.h.
 *
 *
 */

#ifndef _ST_GMT_CONSTS_H_
#define _ST_GMT_CONSTS_H_

#include "StEvent/StEnumerations.h" // as many GMT consts are defined there
#include <string>

// The following are only used in StGmtGeom and StGmtDb, at present
const Int_t       kGmtError       = -999;
const Char_t      kGmtErrorChar   = -1;
const std::string kGmtErrorString = "XXXXXX";

// Constants related to clustering and pulses
const Int_t kGmtMaxClusterSize      = 11;
const Int_t kGmtNumAdditionalStrips = 5;

// For some reason, the MuDst directory fails during linking, so a seperate value is needed.
// For now, it is required that kMuGmtNumTimeBins == kGmtNumTimeBins
const Int_t kMuGmtNumTimeBins = 15;



// Jan's definitions for the final 400-800 micron pitch design 
// Note:
// using #define instead of const double to avoid requiring a .cpp
// file for the constants

// #define kGmtRout          38.25        //     cm ,
// #define kGmtRlast         38.1571      // location of last R strip before Rout
// #define kGmtRmid          19.125       //     cm, at Rout/2.
// #define kGmtRin           11.5         //     cm, 
// #define kGmtRfirst        11.5385      // location of first R strip after Rin
// #define kGmtPfirst        0.0324       // location of first Phi strip 
// #define kGmtPlast         1.5384       // location of last Phi strip
// #define kGmtRflat         35.85        //     cm, 
// #define kGmtPhiflat       (31.0/180.*3.1416) //  rad 
// #define kGmtRadPitch      0.09538     //     nominal '800 mu pitch'
// #define kGmtPhiPitch      0.08        //     800 mu, at outer radi or at Rmid
// #define kGmtPhiAnglePitch 0.002095 
// #define kGmtDeadQuadEdge  1.2         // (cm) effective dead area along quadrant edges
// #define kGmtMaxClusterSize 11        //maximum cluster size in strips that a cluster algo should return
// #define kGmtNumAdditionalStrips 5        //strips in addition to the cluster size that are passed up. Mainly for debugging.

// Constants for GMT (RW 3/15/2013)
#define kGmtXPitch      0.08     //     nominal '800 mu pitch'
#define kGmtYPitch      0.08        //     800 mu, at outer radi or at Rmid
#define kGmtSfirst      0.00     // location of first strip (local X) relative to readout plane origin
#define kGmtPfirst      0.00       // location of first pad (local Y) relative to readout plane origin
#define kGmtSlast       10.16        // location of last strip  [ (128 - 1 at origin) = 127 ]*pitch (in cm)
#define kGmtPlast       10.16       // location of last pad  [ (128 - 1 at origin) = 127 ]*pitch (in cm)

#endif
