/***************************************************************************
 *
 * $Id: StDbDefs.hh,v 1.32 2019/01/08 19:12:54 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  enum definitions for DataBase Type & Domain.
 *               This allows compile-time checks for standard DB accesses.
 *               The StDbManager _does_ support access by names (strings) for
 *               greater flexibility - though poorer compile-time safegaurds
 *
 ***************************************************************************
 *
 * $Log: StDbDefs.hh,v $
 * Revision 1.32  2019/01/08 19:12:54  dmitry
 * new subsystem: ETOF
 *
 * Revision 1.31  2018/11/30 20:04:01  dmitry
 * new detector added - RHICf
 *
 * Revision 1.30  2017/02/24 18:52:25  dmitry
 * new detector added - EPD
 *
 * Revision 1.29  2015/04/13 19:43:43  dmitry
 * added new db domain: SST
 *
 * Revision 1.28  2014/10/31 16:11:59  dmitry
 * adding FPS to the database domain list
 *
 * Revision 1.27  2013/09/11 17:40:06  dmitry
 * new database support: IST
 *
 * Revision 1.26  2013/05/28 18:07:43  dmitry
 * new db domain: MTD
 *
 * Revision 1.25  2013/04/01 14:42:51  dmitry
 * added new domain - PXL
 *
 * Revision 1.24  2011/06/16 14:44:00  dmitry
 * added new domain - FGT
 *
 * Revision 1.23  2009/10/12 15:06:11  dmitry
 * added new domain: pp2pp
 *
 * Revision 1.22  2009/09/11 13:11:49  dmitry
 * added FMS to domain list
 *
 * Revision 1.21  2004/08/11 02:09:35  deph
 * added zdc domain
 *
 * Revision 1.20  2004/02/02 00:41:24  deph
 * Added bbc and tracker domains
 *
 * Revision 1.19  2002/11/24 01:39:16  porter
 * added Pmd domain
 *
 * Revision 1.18  2002/11/14 17:43:00  porter
 * added dbEemc to enumerated domain list
 *
 * Revision 1.17  2002/10/29 21:40:39  porter
 * Added dbScalers as a dbType; dbScaler is a dbDomain from before but now
 * use as a dbType is more in line with how it will be handled
 *
 * Revision 1.16  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.15  2001/04/18 19:33:36  porter
 * added dbRts enumeration for run-time systems domain
 *
 * Revision 1.14  2001/01/22 18:37:53  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 * Revision 1.13  2000/08/15 22:51:51  porter
 * Added Root2DB class from Masashi Kaneta
 * + made code more robust against requesting data from non-existent databases
 *
 * Revision 1.12  2000/06/02 13:37:36  porter
 * built up list of minor changes:
 *  - made buffer more robust for certain null inputs
 *  - fixed small leak in StDbTables & restructure call to createMemory
 *  - added dbRhic as a database domain in StDbDefs
 *  - added setUser() in StDbManager
 *  - added more diagnostic printouts in mysqlAccessor.cc
 *
 * Revision 1.11  2000/05/04 15:13:11  porter
 * added dbOnl, dbRich, dbMwc domains as standards
 *
 * Revision 1.10  2000/04/25 18:26:02  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.9  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.8  2000/01/27 05:54:33  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
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

// enumerated standard set of database types
enum StDbType { dbStDb=0, dbServer, dbRunLog, dbConfigurations, dbConditions, dbCalibrations, dbGeometry, dbRunCatalog, dbRunParams, dbScalers, dbTestScheme, dbTUser1, dbTUser2, dbTUser3, dbTEnd};

// enumerated standard set of database domains
enum StDbDomain {dbDomainUnknown=0, dbStar, dbTpc, dbEmc, dbFtpc, dbSvt, dbCtb, dbTrg, dbDaq, dbScaler, dbGlobal, dbL3, dbOnl, dbRich, dbMwc, dbRhic, dbSsd, dbRts, dbTof, dbFpd, dbEemc, dbPmd, dbBbc, dbTracker, dbZdc, dbFms, dbpp2pp, dbFgt, dbPxl, dbMtd, dbIst, dbFps, dbSst, dbEpd, dbRhicf, dbEtof, dbDUser1, dbDUser2, dbDUser3, dbDEnd };

// enumerated sets of storage models (different sql content)
enum StDbStoreType { dbV00=0 };

enum dbFindServerMode { userHome=0, serverEnvVar, starDefault };

#endif










