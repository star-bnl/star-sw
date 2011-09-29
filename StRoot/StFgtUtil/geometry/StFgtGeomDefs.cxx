/*
 * Basic geometry definitions for the FGT
 *
 * \author S. Gliske (sgliske@anl.gov)
 *
 */

// header file
#include "StFgtGeomDefs.h"

// use values from DAQ, except that these apparently can't be ocmpiled in
//  properly using cons. Also, the information in daq_fgt.h wrt strips is
//  wrong.
//#include "StRoot/RTS/src/DAQ_FGT/daq_fgt.h"

/*
const Int_t kNumFgtDisks = FGT_DISK_COU;
const Int_t kNumFgtQuadrants = FGT_QUADRANT_COU;
const Int_t kNumFgtLayers = FGT_STRIP_TYPE_COU;
const Int_t kNumFgtStripsPerQuadrant = FGT_STRIP_PHI_COU;
*/

//  Number of discs can only be determined from the database.
//const Int_t kNumFgtDiscs = FGT_DISK_COU;
const Int_t kNumFgtQuadrants = 4;
const Int_t kNumFgtLayers = 2;
const Int_t kNumFgtStripsPerLayer = 720;


/*
 * $Id: StFgtGeomDefs.cxx,v 1.2 2011/09/29 18:34:53 sgliske Exp $
 * $Log: StFgtGeomDefs.cxx,v $
 * Revision 1.2  2011/09/29 18:34:53  sgliske
 * Fixed phiQuadXaxis, added asserts to getQuad,and added reverse lookup: elec. coord. from geoId
 *
 *
 */
