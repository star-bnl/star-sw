/***************************************************************************
 *
 * $Id: StDbDefs.hh,v 1.7 2000/01/10 20:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  enum definitions for DataBase Type & Domain
 *
 ***************************************************************************
 *
 * $Log: StDbDefs.hh,v $
 * Revision 1.7  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.6  1999/12/29 13:49:35  porter
 * fix for Solaris-CC4.2 within StRoot make (cons)...
 * replaced #include <config.h> with #include <ospace/config.h>
 *
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

enum StDbType { dbStDb=0, dbServer, dbRunLog, dbConfigurations, dbConditions, dbCalibrations, dbGeometry, dbRunCatalog, dbRunParams, dbTestScheme, dbUser};

enum StDbDomain {dbDomainUnknown=0, dbStar, dbTpc, dbEmc, dbFtpc, dbSvt, dbCtb, dbTrg, dbDaq, dbScaler, dbGlobal, dbL3 };



#ifdef ST_NO_TEMPLATE_DEF_ARGS
// to get bool from object-space
#include <ospace/config.h>
#endif

#endif










