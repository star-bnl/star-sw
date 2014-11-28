// *-- Author : Jan Balewski
// 
// $Id: StEEmcDbMaker.cxx,v 1.58 2009/10/18 22:46:01 perev Exp $
 

#include <time.h>
#include <string.h>

#include <TList.h>

//#include "StEventTypes.h"

#include "StEEmcDbMaker.h"

#include "StEEmcUtil/database/StEEmcDb.h"
//#include "StEEmcUtil/EEfeeRaw/EEname2Index.h" 

#include <StMessMgr.h>

ClassImp(StEEmcDbMaker)

//________________________________________________________
//________________________________________________________
StEEmcDbMaker::StEEmcDbMaker(const char *name)
    : StMaker(name), mEEmcDb(new StEEmcDb())
{
    if (mEEmcDb) mEEmcDb->setSectors(1, 12);
}


//________________________________________________________
//________________________________________________________
//_______________________________________________________
StEEmcDbMaker::~StEEmcDbMaker(){
    mEEmcDb = 0;
}

//________________________________________________________
//________________________________________________________
//________________________________________________________
Int_t StEEmcDbMaker::Init(){
    if (mEEmcDb) AddConst(mEEmcDb);
    return StMaker::Init();
}


//__________________________________________________
//__________________________________________________
//__________________________________________________

Int_t  StEEmcDbMaker::InitRun(int runNumber) {
    // Reloads database for each occurence of a new run number.
    StEEmcDb *db = (StEEmcDb*)this->GetDataSet("StEEmcDb");
    if (db) db->loadTables(this);
    return StMaker::InitRun(runNumber);
}  


// $Log: StEEmcDbMaker.cxx,v $
// Revision 1.58  2009/10/18 22:46:01  perev
// bfc .q crash fix
//
// Revision 1.57  2009/02/11 20:03:40  ogrebeny
// Initialize all 12 sectors
//
// Revision 1.56  2009/02/04 20:33:06  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.55  2007/04/28 17:56:00  perev
// Redundant StChain.h removed
//
// Revision 1.54  2007/03/11 01:25:47  balewski
// nicer printouts
//
// Revision 1.53  2007/01/26 20:45:58  balewski
// now we have pure new Logger, thanks Jason, Jan
//
// Revision 1.52  2007/01/26 00:47:58  balewski
// new logger only (almost)
//
// Revision 1.51  2006/12/12 20:29:09  balewski
// added hooks for Endcap embedding
//
// Revision 1.50  2005/12/15 16:05:11  balewski
// printouts with more details
//
// Revision 1.49  2005/08/17 22:08:45  balewski
// cleanup
//
// Revision 1.48  2005/08/17 22:00:48  balewski
// remove assert 
//
// Revision 1.47  2005/08/17 20:51:14  balewski
// allow to mask fibers based on event content
//
// Revision 1.46  2005/06/09 20:04:06  balewski
// upgrade for embedding
//
// Revision 1.45  2005/04/25 19:48:37  balewski
// overwrite of masks was not working properly
//
// Revision 1.44  2005/02/02 01:36:52  balewski
// few more access methods + sigPed visible in EEmcDbItem
//
// Revision 1.43  2005/01/24 05:08:26  balewski
// more get-methods
//
// Revision 1.42  2004/10/27 17:02:46  balewski
// move setKsig from Init() to constructor where it belongs
//
// Revision 1.41  2004/10/20 20:06:55  balewski
// only printouts
//
// Revision 1.40  2004/09/20 13:32:59  balewski
// to make Valgrind happy
//
// Revision 1.39  2004/08/09 20:17:12  balewski
// a bit more printout
//
// Revision 1.38  2004/08/07 02:46:51  perev
// WarnOff
//
// Revision 1.37  2004/07/27 22:00:19  balewski
// can overwrite gains & stat from DB
//
// Revision 1.36  2004/06/25 22:55:53  balewski
// now it survives missing fiberMap in DB , also gMessMgr is used
//
// Revision 1.35  2004/06/04 13:30:24  balewski
// use gMessMgr for most of output
//
// Revision 1.33  2004/05/20 16:40:14  balewski
// fix of strnlen --> strlen
//
// Revision 1.32  2004/05/14 20:55:34  balewski
// fix to process many runs, by Piotr
//
// Revision 1.31  2004/05/05 22:01:44  jwebb
// byStrip[] is now initialized when reading database from a file.
//
// Revision 1.30  2004/05/04 16:24:18  balewski
// ready for analysis of 62GeV AuAU production
//
// Revision 1.29  2004/04/28 20:38:10  jwebb
// Added StEEmcDbMaker::setAsciiDatabase().  Currently not working, since
// tube name missing for some towers, triggereing a "clear" of all EEmcDbItems.
//
// Revision 1.28  2004/04/12 16:19:51  balewski
// DB cleanup & update
//
// Revision 1.27  2004/04/09 18:38:10  balewski
// more access methods, not important for 63GeV production
//
// Revision 1.26  2004/04/08 16:28:06  balewski
// *** empty log message ***
//
// Revision 1.25  2004/04/04 06:10:37  balewski
// *** empty log message ***
//
// Revision 1.24  2004/03/30 04:44:57  balewski
// *** empty log message ***
//
// Revision 1.23  2004/03/28 04:09:08  balewski
// storage of EEMC raw data, not finished
//
// Revision 1.22  2004/03/19 21:31:53  balewski
// new EEMC data decoder
//
// Revision 1.21  2004/01/06 21:19:34  jwebb
// Added methods for accessing preshower, postshower and SMD info.
//
// Revision 1.20  2003/11/20 16:01:25  balewski
// towards run4
//
// Revision 1.19  2003/10/03 22:44:27  balewski
// fix '$' problem in db-entries name
//
// Revision 1.18  2003/09/11 05:49:17  perev
// ansi corrs
//
// Revision 1.17  2003/09/02 19:02:49  balewski
// fix for TMemeStat
//
// Revision 1.16  2003/08/27 03:26:45  balewski
// flavor option added:  myMk1->setPreferedFlavor("set-b","eemcPMTcal");
//
// Revision 1.15  2003/08/26 03:02:30  balewski
// fix of pix-stat and other
//
// Revision 1.14  2003/08/25 17:57:12  balewski
// use teplate to access DB-tables
//
// Revision 1.13  2003/08/22 20:52:20  balewski
// access to stat-table
//
// Revision 1.12  2003/08/02 01:02:17  perev
// change %d to %p int printf
//
// Revision 1.11  2003/07/18 18:31:46  perev
// test for nonexistance of XXXReader added
//
// Revision 1.10  2003/04/27 23:08:13  balewski
// clean up of daq-reader
//
// Revision 1.9  2003/04/25 14:42:00  jeromel
// Minor change in messaging
//
// Revision 1.8  2003/04/16 20:33:51  balewski
// small fixes in eemc daq reader
//
// Revision 1.7  2003/04/02 20:42:23  balewski
// tower-->tube mapping
//
// Revision 1.6  2003/03/26 21:28:02  balewski
// fix
//
// Revision 1.5  2003/03/26 15:26:23  balewski
// add print()
//
// Revision 1.4  2003/03/07 15:35:44  balewski
// towards EEMC daq reader
//
// Revision 1.3  2003/02/18 22:01:40  balewski
// fixes
//
// Revision 1.2  2003/02/18 19:55:53  balewski
// add pedestals
//
// Revision 1.1  2003/01/28 23:18:34  balewski
// start
//
// Revision 1.5  2003/01/06 17:09:21  balewski
// DB-fix
//
// Revision 1.4  2003/01/03 23:37:56  balewski
// move to robinson
//
// Revision 1.3  2002/12/05 14:22:24  balewski
// cleanup, time stamp fixed
//
// Revision 1.2  2002/12/04 13:39:04  balewski
// remove dependency on dbase/
//
// Revision 1.1  2002/11/30 20:01:26  balewski
// start DB interface for EEMC RELATED ROUTINES
//

