/***************************************************************************
 *
 * $Id: StDbCalibSchema.h,v 1.3 1999/09/30 02:06:02 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Current schemaID for Calibration tables
 *
 ***************************************************************************
 *
 * $Log: StDbCalibSchema.h,v $
 * Revision 1.3  1999/09/30 02:06:02  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBCALIBSCHEMA_HH
#define STDBCALIBSCHEMA_HH

static int tpcDriftVelocityID=1;
static int tpcDedxPidAmpDbID=1;
static int tpcTimeOffsetsID=1;
static int tpcGainFactorsID=1;


#endif
