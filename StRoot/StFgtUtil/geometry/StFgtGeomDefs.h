/*
 * Basic geometry definitions for the FGT
 *
 * \author S. Gliske (sgliske@anl.gov)
 *
 */

#ifndef _ST_FGT_GEOM_DEFS_H_
#define _ST_FGT_GEOM_DEFS_H_

#include <Rtypes.h>

//  The number of discs can only be determined from the database. But, on the
//  other hand, it's not really necessary to know this for any of our current
//  code anyway.
//extern const Int_t kNumFgtDiscs;
extern const Int_t kNumFgtQuadrants;
extern const Int_t kNumFgtLayers;
//extern const Int_t kNumFgtStripsPerLayer;  --> moved to StFgtGeom

#endif

/*
 * $Id: StFgtGeomDefs.h,v 1.2 2011/09/29 18:34:53 sgliske Exp $
 * $Log: StFgtGeomDefs.h,v $
 * Revision 1.2  2011/09/29 18:34:53  sgliske
 * Fixed phiQuadXaxis, added asserts to getQuad,and added reverse lookup: elec. coord. from geoId
 *
 *
 */
