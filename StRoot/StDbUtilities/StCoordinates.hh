/***********************************************************************
 *
 * $Id: StCoordinates.hh,v 1.1 1999/11/19 19:01:07 calderon Exp $
 *
 * Author: brian May 19, 1998
 *
 ***********************************************************************
 *
 * Description: The Coordinates and transformation routines for the TPC
 *
 ***********************************************************************
 *
 * $Log: StCoordinates.hh,v $
 * Revision 1.1  1999/11/19 19:01:07  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.2  1999/01/28 02:48:59  lasiuk
 * include sector coordinates
 *
 * Revision 1.1  1998/11/10 17:12:04  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/05/25 17:11:06  lasiuk
 * Initial Revision
 *
 *
 ***********************************************************************/
#ifndef ST_TPC_COORDINATES_HH
#define ST_TPC_COORDINATES_HH

#include "StTpcPadCoordinate.hh"
#include "StTpcLocalSectorCoordinate.hh"
#include "StTpcLocalCoordinate.hh"
#include "StGlobalCoordinate.hh"
#include "StTpcCoordinateTransform.hh"

#include "StSvtLocalCoordinate.hh"
#include "StSvtWaferCoordinate.hh"

#include "StFtpcLocalCoordinate.hh"
#include "StFtpcSectorCoordinate.hh"
#endif
