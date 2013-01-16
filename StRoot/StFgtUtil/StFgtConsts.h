/*
 * Some basic constants (enums) the FGT.  All FGT related constants
 * that are not in the database should be in this file, except those
 * which cannot be expressed as an int.  These remaining constants are
 * in the file StFgtGeom.h.
 *
 *
 */

#ifndef _ST_FGT_CONSTS_H_
#define _ST_FGT_CONSTS_H_

#include "StEvent/StEnumerations.h" // as many FGT consts are defined there
#include <string>

// The following are only used in StFgtGeom and StFgtDb, at present
const Int_t       kFgtError       = -999;
const Char_t      kFgtErrorChar   = -1;
const std::string kFgtErrorString = "XXXXXX";

// Constants related to clustering and pulses
const Int_t kFgtMaxClusterSize      = 11;
const Int_t kFgtNumAdditionalStrips = 5;

// For some reason, the MuDst directory fails during linking, so a seperate value is needed.
// For now, it is required that kMuFgtNumTimeBins == kFgtNumTimeBins
const Int_t kMuFgtNumTimeBins = 15;



// Jan's definitions for the final 400-800 micron pitch design 
// Note:
// using #define instead of const double to avoid requiring a .cpp
// file for the constants

#define kFgtRout          38.25        //     cm ,
#define kFgtRlast         38.1571      // location of last R strip before Rout
#define kFgtRmid          19.125       //     cm, at Rout/2.
#define kFgtRin           11.5         //     cm, 
#define kFgtRfirst        11.5385      // location of first R strip after Rin
#define kFgtPfirst        0.0323748    // location of first Phi strip 
#define kFgtPlast         1.53841996   // location of last Phi strip
#define kFgtRflat         35.85        //     cm, 
#define kFgtPhiflat       (31.0/180.*3.1416) //  rad 
#define kFgtRadPitch      0.0954071684 //     nominal '800 mu pitch'
#define kFgtPhiPitch      0.08         //     800 mu, at outer radi or at Rmid
#define kFgtPhiAnglePitch 0.0020946386 
#define kFgtDeadQuadEdge  1.2          // (cm) effective dead area along quadrant edges
#define kFgtMaxClusterSize 11          //maximum cluster size in strips that a cluster algo should return
#define kFgtNumAdditionalStrips 5      //strips in addition to the cluster size that are passed up. Mainly for debugging.

#endif

/*
 * $Id: StFgtConsts.h,v 1.20 2013/01/16 19:31:53 avossen Exp $
 * $Log: StFgtConsts.h,v $
 * Revision 1.20  2013/01/16 19:31:53  avossen
 * updated number of tb
 *
 * Revision 1.19  2012/11/05 15:43:34  akio
 * FgtSlowSimu related fixes for r/phi consistency & speed up
 *
 * Revision 1.18  2012/04/13 18:56:56  sgliske
 * More adjustments based on the review:
 * - Lastest StEvents from Thomas U.
 * - StFgtA2CMaker can no longer remove strips other than bad status or bad ped
 * - other related updates
 *
 * Revision 1.17  2012/03/15 00:17:58  wwitzke
 * Added error constants to StFgtConsts.h
 *
 * Revision 1.16  2012/03/09 17:48:32  rfatemi
 * revert back to old version
 *
 * Revision 1.14  2012/03/07 17:05:58  sgliske
 * updated whitespace and comments
 *
 * Revision 1.13  2012/03/07 03:57:23  avossen
 * various updates
 *
 * Revision 1.12  2012/03/01 16:38:13  avossen
 * implemented tweaks to clustering
 *
 * Revision 1.11  2012/02/29 20:29:08  avossen
 * changes to seed and cluster algo
 *
 * Revision 1.10  2012/02/28 19:32:25  avossen
 * many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
 *
 * Revision 1.9  2012/02/15 19:18:38  balewski
 * more usefull constants added
 *
 * Revision 1.8  2012/02/09 16:16:17  wwitzke
 * Made minor fix to StFgtConsts.h to eliminate warnings.
 *
 * Revision 1.7  2012/02/07 06:14:45  balewski
 * *** empty log message ***
 *
 * Revision 1.6  2012/02/06 18:24:32  avossen
 *  changed max tb and default value for strip adcs
 *
 * Revision 1.5  2012/01/31 12:53:11  sgliske
 * Somehow kFgtNumQuads was lost--now restored
 *
 * Revision 1.4  2012/01/31 10:51:12  sgliske
 * Added number of octants (48)
 *
 * Revision 1.3  2012/01/28 10:44:50  sgliske
 * updated number of geoIds and elecIds to use products
 *
 * Revision 1.2  2012/01/28 10:29:47  sgliske
 * static const doubles moved from StFgtGeom to StFgtConsts
 * Also, geoName updated to more recent convention
 * disc in 1-6, quad in A-D, strip in 0-719
 *
 * Revision 1.1  2012/01/26 18:10:36  sgliske
 * creation
 *
 *
 */
