/*
 * Some basic constants (enums) the FGT.  All FGT related constants
 * that are not in the database should be in this file, except those
 * which cannot be expressed as an int.  These remaining constants are
 * in the file StFgtGeom.h.
 *
 *
 */

#ifndef _ST_FGT_ENUMS_H_
#define _ST_FGT_ENUMS_H_

#include <string>

// constants related to electric coordinates

const Int_t kFgtNumRdos = 2;           // rdo in {1,2}
const Int_t kFgtNumArms = 6;           // arm in 0-5, though 5 not used in run12.
const Int_t kFgtNumChannels = 128;     // channel in 0-127
const Int_t kFgtApvsPerAssembly = 12;  //
const Int_t kFgtMaxApvId=kFgtApvsPerAssembly*2; // covers 0-23 
const Int_t kFgtApvGap = 2;            // i.e. apvs 10 & 11
const Int_t kFgtApvsPerOct = 5;
const Int_t kFgtApvsPerQuad = 10;
const Int_t kFgtApvsPerArm = 20;
const Int_t kFgtNumElecIds = kFgtNumChannels * kFgtApvsPerArm * kFgtNumArms * kFgtNumRdos;  // elec id in 0 to kFgtNumElecIds-1

// constants related to physical coordinates

const Int_t kFgtNumDiscs = 6;
const Int_t kFgtNumQuads = 4;
const Int_t kFgtNumOctantsPerDisc = 8;
const Int_t kFgtNumOctants = kFgtNumOctantsPerDisc*kFgtNumDiscs;
const Int_t kFgtNumLayers = 2;
const Int_t kFgtNumStrips = 720;
const Int_t kFgtNumGeoIds = kFgtNumQuads * kFgtNumDiscs * kFgtNumLayers * kFgtNumStrips;   // geoId in 0 to kFgtNumGeoIds-1
const Int_t kFgtNumPstripsPerOctant = 360;
const Int_t kFgtNumRstripsPerOctant = 280;
const Int_t kFgtLowerStripOctant = 'L';    // i.e. a strip is in octant "kFgtLowerStripOctant" if
const Int_t kFgtHigherStripOctant = 'S';   // the strip index is below the number of strips per octant
                                  // for that layer
const Int_t kFgtNumStripsPerDisc = kFgtNumQuads  * kFgtNumLayers * kFgtNumStrips; // includes both planes, geoId for given disc will not exceed this range after common disc-offset is subtracted 

// unsorted constants
const Int_t kFgtNumTimeBins = 9;           // if using cosmic data, recompile with this value set to 7
const Int_t kFgtMaxAdc = 4096;

///cluster seed types
const Int_t   kFgtSeedTypeNo=0;
const Int_t   kFgtDeadStrip=1;
const Int_t   kFgtSeedType1=2;
const Int_t   kFgtSeedType2=3;
const Int_t   kFgtSeedType3=4;
const Int_t   kFgtClusterPart=5;
const Int_t   kFgtNextToDeadGuy=6;
const Int_t   kFgtClusterEndUp=7;
const Int_t   kFgtClusterEndDown=8;
const Int_t   kFgtStripShared=9;
const Int_t   kFgtClusterTooBig=10;
const Int_t   kFgtClusterSeedInSeaOfNoise=11;

//return errors on boundry checks
const Int_t kFgtError = -999;
const std::string kFgtErrorString = "XXXXXX";

// Jan's definitions for the final 400-800 micron pitch design 
// Note:
// using #define instead of const double to avoid requiring a .cpp
// file for the constants

const Double_t kFgtRout=          38.25;        //     cm ,
const Double_t kFgtRlast=         38.1571;      // location of last R strip before Rout
const Double_t kFgtRmid=          19.125;       //     cm, at Rout/2.
const Double_t kFgtRin=           11.5;         //     cm, 
const Double_t kFgtRfirst=        11.5385;      // location of first R strip after Rin
const Double_t kFgtPfirst=        0.0324;       // location of first Phi strip 
const Double_t kFgtPlast=         1.5384;       // location of last Phi strip
const Double_t kFgtRflat=         35.85;        //     cm, 
const Double_t kFgtPhiflat=       (31.0/180.*3.1416); //  rad 
const Double_t kFgtRadPitch=      0.09538;     //     nominal '800 mu pitch'
const Double_t kFgtPhiPitch=      0.08;        //     800 mu, at outer radi or at Rmid
const Double_t kFgtPhiAnglePitch= 0.002095; 
const Double_t kFgtDeadQuadEdge=  1.2;         // (cm) effective dead area along quadrant edges
const Int_t kFgtMaxClusterSize= 11;        //maximum cluster size in strips that a cluster algo should return
const Int_t kFgtNumAdditionalStrips=5;        //strips in addition to the cluster size that are passed up. Mainly for debugging.


#endif

/*
 * $Id: StFgtConsts.h,v 1.15 2012/03/09 12:31:53 rfatemi Exp $
 * $Log: StFgtConsts.h,v $
 * Revision 1.15  2012/03/09 12:31:53  rfatemi
 * Change entries from enums to consts
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
