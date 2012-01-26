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
   kFgtNumElecIds = 30720,    // elec id in 0-30719,
                              // note kFgtNumElecId = kFgtNumChan * kFgtApvsPerArm * kFgtNumArm * kFgtNumRdo
};

// constants related to physical coordinates
enum {
   kFgtNumDiscs = 6,
   kFgtNumQuads = 4,
   kFgtNumOctantsPerDisc = 8,
   kFgtNumLayers = 2,
   kFgtNumStrips = 720,
   kFgtNumGeoIds = 34560,         // geoId in 0-34559
                                  // kFgtNumGeoId = kFgtNumStrip
   kFgtNumPstripsPerOctant = 360,
   kFgtNumRstripsPerOctant = 280,
   kFgtLowerStripOctant = 'L',    // i.e. a strip is in octant "kFgtLowerStripOctant" if
   kFgtHigherStripOctant = 'S',   // the strip index is below the number of strips per octant
                                  // for that layer
};

// unsorted constants
enum {
   kFgtNumTimeBins = 5,           // if using cosmic data, recompile with this value set to 7
   kFgtMaxAdc = 4096
};

#endif

/*
 * $Id: StFgtConsts.h,v 1.1 2012/01/26 18:10:36 sgliske Exp $
 * $Log: StFgtConsts.h,v $
 * Revision 1.1  2012/01/26 18:10:36  sgliske
 * creation
 *
 *
 */
