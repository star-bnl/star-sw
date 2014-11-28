// $Id: StEEmcDbMaker.h,v 1.33 2014/08/06 11:42:56 jeromel Exp $

/*! \class StEEmcDbMaker 
\author Jan Balewski

Interface to STAR-DB. Info from various DB tables is 
correlated for every ADC channel and stored in memory in
 mDbItem[].
mDbItem1[] has single index based on the channel name, 
calculated by EEname2index() utility.

<pre>
----------------------------------------
Usage:
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  dbMk->setTimeStampDay(20031215);  // format: yyyymmdd
  StEEmcDbMaker *myDbMk = new StEEmcDbMaker("EEmcDB");
  StEEfast2slowMaker *myMk= new StEEfast2slowMaker("EE-fast2slow");
  myMk->setDb(myDbMk);
</pre>
*/  

#ifndef STAR_SteemcDbMaker
#define STAR_SteemcDbMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEEmcDb;

class StEEmcDbMaker : public StMaker {
private:
    StEEmcDb *mEEmcDb; //!
public:
    StEEmcDbMaker(const char *name="EEmcDbMaker");
    virtual ~StEEmcDbMaker();
    virtual Int_t Init();
    virtual Int_t InitRun (int runumber); ///< to access STAR-DB
  
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StEEmcDbMaker.h,v 1.33 2014/08/06 11:42:56 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
        return cvs;
    }
  
    ClassDef(StEEmcDbMaker, 1)
};

#endif

// $Log: StEEmcDbMaker.h,v $
// Revision 1.33  2014/08/06 11:42:56  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.32  2009/02/04 20:33:06  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.31  2005/08/17 20:51:14  balewski
// allow to mask fibers based on event content
//
// Revision 1.30  2005/06/09 20:04:06  balewski
// upgrade for embedding
//
// Revision 1.29  2005/01/24 05:08:26  balewski
// more get-methods
//
// Revision 1.28  2004/09/01 04:16:39  balewski
// bug fix for getU(...), getV()  - order of argument was wrong in implementation
//
// Revision 1.27  2004/07/27 22:00:19  balewski
// can overwrite gains & stat from DB
//
// Revision 1.26  2004/06/25 22:55:53  balewski
// now it survives missing fiberMap in DB , also gMessMgr is used
//
// Revision 1.25  2004/06/04 13:30:24  balewski
// use gMessMgr for most of output
//
// Revision 1.24  2004/05/26 21:30:36  jwebb
// Fixed typo, added setPreferredFlavor method.  Kept setPreferedFlavor for
// backwards compatibility.
//
// Revision 1.23  2004/05/14 20:55:36  balewski
// fix to process many runs, by Piotr
//
// Revision 1.22  2004/04/28 20:38:11  jwebb
// Added StEEmcDbMaker::setAsciiDatabase().  Currently not working, since
// tube name missing for some towers, triggereing a "clear" of all EEmcDbItems.
//
// Revision 1.21  2004/04/12 16:19:52  balewski
// DB cleanup & update
//
// Revision 1.20  2004/04/09 18:38:11  balewski
// more access methods, not important for 63GeV production
//
// Revision 1.19  2004/04/08 16:28:06  balewski
// *** empty log message ***
//
// Revision 1.18  2004/04/04 06:10:37  balewski
// *** empty log message ***
//
// Revision 1.17  2004/03/30 04:44:57  balewski
// *** empty log message ***
//
// Revision 1.16  2004/03/19 21:31:53  balewski
// new EEMC data decoder
//
// Revision 1.15  2004/01/06 21:19:34  jwebb
// Added methods for accessing preshower, postshower and SMD info.
//
// Revision 1.14  2003/11/20 16:01:25  balewski
// towards run4
//
// Revision 1.13  2003/10/03 22:44:27  balewski
// fix '$' problem in db-entries name
//
// Revision 1.12  2003/09/10 19:47:08  perev
// ansi corrs
//
// Revision 1.11  2003/09/02 19:02:49  balewski
// fix for TMemeStat
//
// Revision 1.10  2003/08/27 03:26:46  balewski
// flavor option added:  myMk1->setPreferedFlavor("set-b","eemcPMTcal");
//
// Revision 1.9  2003/08/26 03:02:30  balewski
// fix of pix-stat and other
//
// Revision 1.8  2003/08/25 17:57:12  balewski
// use teplate to access DB-tables
//
