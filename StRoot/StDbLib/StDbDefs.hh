/***************************************************************************
 *
 * $Id: StDbDefs.hh,v 1.5 1999/12/28 21:31:41 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  enum definitions for DataBase Type & Domain
 *
 ***************************************************************************
 *
 * $Log: StDbDefs.hh,v $
 * Revision 1.5  1999/12/28 21:31:41  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.4  1999/09/30 02:06:04  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBDEFS_HH
#define STDBDEFS_HH

enum StDbType {StarDb=0, DbServer, RunLog, Configurations, Conditions, Calibrations, Geometry, RunCatalog, RunParams, TestScheme };

enum StDbDomain {Unknown=0, Star, Tpc, Emc, Ftpc, Svt, Ctb, Trg, Daq, Scaler, Global, L3 };


#ifdef ST_NO_TEMPLATE_DEF_ARGS
#include <config.h>
#endif
//#ifdef SOLARIS
//# ifndef false
//typedef int bool;
//#define false 0
//#define true 1
//# endif
//#endif



#endif










