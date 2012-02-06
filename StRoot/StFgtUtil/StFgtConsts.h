/*
 * Some basic constants (enums) the FGT.  All FGT related constants
 * that are not in the database should be in this file, except those
 * which cannot be expressed as an int.  These remaining constants are
 * in the file StFgtGeom.h.
 *
 * \author S. Gliske (sgliske@anl.gov) Jan 2012
 *
 */

#ifndef _ST_FGT_ENUMS_H_
#define _ST_FGT_ENUMS_H_

// constants related to electric coordinates
enum { 
   kFgtNumRdos = 2,           // rdo in {1,2}
   kFgtNumArms = 6,           // arm in 0-5, though 5 not used in run12.
   kFgtNumChannels = 128,     // channel in 0-127
   kFgtApvsPerAssembly = 12,  //
   kFgtApvGap = 2,            // i.e. apvs 10 & 11
   kFgtApvsPerOct = 5,
   kFgtApvsPerQuad = 10,
   kFgtApvsPerArm = 20,
   kFgtNumElecIds = kFgtNumChannels * kFgtApvsPerArm * kFgtNumArms * kFgtNumRdos  // elec id in 0 to kFgtNumElecIds-1
};

// constants related to physical coordinates
enum {
   kFgtNumDiscs = 6,
   kFgtNumQuads = 4,
   kFgtNumOctantsPerDisc = 8,
   kFgtNumOctants = kFgtNumOctantsPerDisc*kFgtNumDiscs,
   kFgtNumLayers = 2,
   kFgtNumStrips = 720,
   kFgtNumGeoIds = kFgtNumQuads * kFgtNumDiscs * kFgtNumLayers * kFgtNumStrips,   // geoId in 0 to kFgtNumGeoIds-1
   kFgtNumPstripsPerOctant = 360,
   kFgtNumRstripsPerOctant = 280,
   kFgtLowerStripOctant = 'L',    // i.e. a strip is in octant "kFgtLowerStripOctant" if
   kFgtHigherStripOctant = 'S',   // the strip index is below the number of strips per octant
                                  // for that layer
};

// unsorted constants
enum {
  kFgtNumTimeBins = 9,           // if using cosmic data, recompile with this value set to 7
   kFgtMaxAdc = 4096
};

// Jan's definitions for the final 400-800 micron pitch design 
// Note:
// using #define instead of const double to avoid requiring a .cpp
// file for the constants

#define kFgtRout          38.25        //     cm ,
#define kFgtRlast         38.1571      // location of last R strip before Rout
#define kFgtRmid          19.125       //     cm, at Rout/2.
#define kFgtRin           11.5         //     cm, 
#define kFgtRfirst        11.5385      // location of first R strip after Rin
#define kFgtPfirst        0.0324       // location of first Phi strip 
#define kFgtPlast         1.5384       // location of last Phi strip
#define kFgtRflat         35.85        //     cm, 
#define kFgtPhiflat       (31.0/180.*3.1416) //  rad 
#define kFgtRadPitch      0.09538     //     nominal '800 mu pitch'
#define kFgtPhiPitch      0.08        //     800 mu, at outer radi or at Rmid
#define kFgtPhiAnglePitch 0.002095 
#define kFgtDeadQuadEdge  1.2         // (cm) effective dead area along quadrant edges

#endif

/*
 * $Id: StFgtConsts.h,v 1.6 2012/02/06 18:24:32 avossen Exp $
 * $Log: StFgtConsts.h,v $
 * Revision 1.6  2012/02/06 18:24:32  avossen
 * changed max tb and default value for strip adcs
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
