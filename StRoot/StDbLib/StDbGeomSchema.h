/***************************************************************************
 *
 * $Id: StDbGeomSchema.h,v 1.3 1999/09/30 02:06:06 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Current schemaID for Geometry tables
 *
 ***************************************************************************
 *
 * $Log: StDbGeomSchema.h,v $
 * Revision 1.3  1999/09/30 02:06:06  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBGEOMSCHEMA_HH
#define STDBGEOMSCHEMA_HH

static int tpcWirePlanesID=1;
static int tpcElectronicsID=1;
static int tpcDimensionsID=1;
static int tpcPadPlanesID=1;
static int tpcSectorPositionID=1;

#endif
