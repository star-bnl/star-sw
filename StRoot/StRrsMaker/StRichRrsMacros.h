/*********************************************************************
 * $Id: StRichRrsMacros.h,v 1.2 2000/02/08 16:30:23 lasiuk Exp $
 *  This file contains macros defined explicitly for RRS.
 *  The can be defined to limit the initialization/debug
 *  output of the code.
 *********************************************************************
 *
 * $Log: StRichRrsMacros.h,v $
 * Revision 1.2  2000/02/08 16:30:23  lasiuk
 * addition of bounds check and diagnostic rrs debug
 *
 * Revision 1.3  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
#define rrs if(RRS_DEBUG)cout
 *
 * Revision 1.2  2000/02/08 16:30:23  lasiuk
 *
 *********************************************************************/
#define uSE_MEMORY_INFO 1
#define RRS_DEBUG 0
#define rICH_COORDINATE_BOUNDS_CHECK 1
#define rICH_WITH_VIEWER 1
#define rICH_WITH_MONITOR 1
