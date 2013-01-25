// $Id: eemcDb.C,v 1.1 2013/01/25 16:46:49 stevens4 Exp $
// Descripion: a kind of swiss army knife for EEMC databases
// Author: Piotr A. Zolnierczuk (IUCF)
// Contributions: Jan Balewski  (IUCF)
//
// Note: log info at the bottom now
//
#include "StDbManager.hh"
#include "StDbSql.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include "StDbElementIndex.hh"

#include <stdlib.h>
#include <stdio.h>

#include <string.h>
#include <getopt.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>


#include "EEmcDbIO.h"
#include "eemcDb.h"


const char  *cvsRevision="$Revision: 1.1 $";
const char  *keyToEverything = "jas";
const char  *dbTimeFormat[] = { 
  "%Y-%m-%d %H:%M:%S",
  "%Y-%m-%d %H:%M",
  "%Y-%m-%d",
  "%s",
  0
};


// global flags
enum  EEmcDbAction action = GetDB;
char  *argv0 = NULL; // program name
char  *dbPath       = NULL;
char  *dbTime       = "now";  
char  *dbExpTime    = NULL;
char  *dbFile       = NULL;
char  *dbFlavor     = "ofl";
char  *dbComment    = getenv("USER");
char  *dbName       = "Calibrations_eemc";
int    dataOnlyMode = false;
int    debugMode    = false;

static EEmcDbIOBase *
getDbIO(const char *preNode, const char *node, const int ndata)
{ 
  EEmcDbIOBase *dbIO;
  // ===================   CCIO ==========================
  if     (!strncmp(node,"eemcPMTconf" ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPMTconf>(EEMCDbMaxPmt);
  else if(!strncmp(node,"eemcADCconf" ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbADCconf>(EEMCDbMaxAdc);
  else if(!strncmp(node,"eemcBoxTconf",EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbBoxconf>(EEMCDbMaxBox);
  else if(!strncmp(node,"eemcPMTcal"  ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPMTcal> (EEMCDbMaxPmt);
  else if(!strncmp(node,"eemcPMTname"  ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPMTname> (EEMCDbMaxPmt);
  else if(!strncmp(node,"eemcPMTped"  ,EEmcDbMaxDbPathLen))  // 
    dbIO = new EEmcDbCCIO<eemcDbPMTped> (EEMCDbMaxAdc);
  else if(!strncmp(node,"eemcPMTstat" ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPMTstat>(EEMCDbMaxAdc);
  else if(!strncmp(node,"eemcPIXcal"  ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPIXcal> (EEMCDbMaxAdc);
  else if(!strncmp(node,"eemcPIXname"  ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbCCIO<eemcDbPIXname> (EEMCDbMaxAdc);

  else if(!strncmp(node,"eemcCrateConf" ,EEmcDbMaxDbPathLen)) 
    dbIO = new KretDbBlobSIO(ndata);

  else if(!strncmp(node,"eemcVaria" ,EEmcDbMaxDbPathLen)) 
    dbIO = new KretDbBlobSIO(ndata);

  // ===================   QAIO ==========================
  else if(!strncmp(node,"eemcPMTchar" ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbQAIO<eemcDbPMTchar>(ndata);
  else if(!strncmp(node,"eemcCWchar"  ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbQAIO<eemcDbCWchar> (ndata);
  // ===================   HVIO ==========================
  else if(!strncmp(node,"eemcHVsys"    ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbHVIO<eemcDbHVsys> (ndata);
  else if(!strncmp(node,"eemcHVtemp"   ,EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbHVIO<eemcDbHVtemp>(ndata);
  // ===================   RunConfig ======================
  else if(!strncmp(node,"eemcRunConfig",EEmcDbMaxDbPathLen)) 
    dbIO = new EEmcDbXML<eemcDbXMLdata>(ndata);
  // ===================   Kret ==========================
  else if(!strncmp(node,"cdev"  ,EEmcDbMaxDbPathLen) || 
	(!strncmp(preNode,"online",EEmcDbMaxDbPathLen)) && (
	     (!strncmp(node,"ETOW"  ,EEmcDbMaxDbPathLen)) || 
	     (!strncmp(node,"ESMD"  ,EEmcDbMaxDbPathLen)) || 
	     (!strncmp(node,"HVsys"  ,EEmcDbMaxDbPathLen)) ||
	     (!strncmp(node,"testBlobS" ,EEmcDbMaxDbPathLen))   ) ) 
    dbIO = new KretDbBlobSIO(ndata);
 // ===================  OTHER ==========================
  else {
    fprintf(stderr,"table %s/%s unknown to %s\n",preNode,node,argv0);
    return(NULL);
  }


  if(!dbIO) {
    fprintf(stderr,"table %s unknown to %s\n",node,argv0);
    return(NULL);
  }
  return(dbIO);
}


int 
printTree(FILE *out, StDbConfigNode *node , int level=0)
{
  static int items=0; // to count if we have found anything
  if(node==NULL) return items;
  for(int i=0;i<level;i++) fprintf(out,"\t");
  // level 0 == database name, so skip it
  if(level>0) fprintf(out,"%s/\n",node->printName());  
  if( node->hasData() ) {
    StDbTableIter *ti = node->getStDbTableIter();
    StDbTable     *t  = NULL;
    while( ( t = ti->next() ) ) { 
      for(int i=0;i<=level;i++) fprintf(out,"\t");
      fprintf(out,"%s",t->printName());
      fprintf(out,":%s",t->getCstructName()); 
      fprintf(out,"\n");
      items++;
    }
  }
  (void) printTree(out,node->getFirstChildNode(),level+1);
  (void) printTree(out,node->getNextNode()      ,level  );
  return items;
}



void 
printHistory(FILE *out, StDbManager *mgr, StDbTable *dbTab , EEmcDbIOBase *io)
{
  FILE *null = fopen("/dev/null","w");
  int nRec=0;
  time_t tUnix=getTimeStamp(dbTimeFormat,dbTime);
  time_t tOld =-1;
  fprintf(out,"\nRec BeginDate (BNL)      UnixTime      ValidPeriod             Comment\n");
  while( tUnix>=0) {
    mgr->setRequestTime(tUnix);
    int ret = mgr->fetchDbTable(dbTab);
    if(ret>0) {
      nRec++;
      //printf("%d %d (%d)\n",dbTab->getTableSize(),io->getBytes(),ret);
      io->setData(dbTab->GetTable());
      io->write(null);
      //      time_t tdiff=(tOld>0) ? dbTab->getBeginTime()-tOld : 0;
      time_t tdiff= dbTab->getEndTime()-  dbTab->getBeginTime();
      int sec    = tdiff %  60; tdiff /= 60;
      int min    = tdiff %  60; tdiff /= 60; 
      int hour   = tdiff %  24; tdiff /= 24; 
      int day    = tdiff ;
      char *tstr = new char[32];
      time_t bt  = dbTab->getBeginTime();
      if(bt==tOld) break; 
      strftime(tstr,32,dbTimeFormat[0],localtime(&bt));
      fprintf(out,"%-3d %.20s  %d    %6dday(s) %02d:%02d:%02d   %s\n",
	      nRec, tstr, dbTab->getBeginTime(),
	      day,hour,min,sec, io->getComment());
      io->resetData();
      tOld = dbTab->getBeginTime();
    }
    fflush(out);
    tUnix= dbTab->getEndTime();
    if(tUnix>EEmcDbMaxUnixTime) break;
  }

}


void 
printConfig(FILE *out, StDbManager *mgr)
{
  const char *eTime="entryTime";
  const char *vKey ="versionKey";
  char **var;
  int    len = 0;

  StDbSql*    db   = (StDbSql*)mgr->findDb(dbName);
  MysqlDb&    Db   = db->Db;
  StDbBuffer& buff = db->buff;
  
  Db<<"select "<< vKey << "," << eTime  << " from Nodes where nodeType='Config'"<<endsql;

  while(Db.Output(&buff)){  // loop over rows
    buff.ReadArray(var,len,vKey);
    fprintf(out,"%s : ",*var);
    buff.ReadArray(var,len,eTime);
    fprintf(out,"%s   ",fmtSqlTime(*var));
    fprintf(out,"\n");
  }
  buff.Raz();
  Db.Release();
}

static char *
printVersion() 
{
  const  int   vLen = strlen(cvsRevision);
  static char *vStr = new char[vLen];
  strncpy(vStr,cvsRevision+1,vLen-1);
  vStr[vLen-2]=0x00;
  return(vStr);
	
}


static void
usage(const char *message=NULL)
{
  if (message) fprintf(stderr,"%s: %s\n",argv0,message);
  fprintf(stderr,"usage: %s --path <path> [EXTRA_OPTIONS]\n",argv0);
  fprintf(stderr," -D|--database <name> : data base to use (default %s)\n",dbName);
  fprintf(stderr," -p|--path <path>     : full path to the table\n");
  fprintf(stderr," -t|--time <time>     : sets query time            (default: now)\n");
  fprintf(stderr," -x|--expire <time>   : sets query expiration time (default: forever)\n");
  fprintf(stderr," -F|--flavor          : set database flavor (default ofl)\n");
  fprintf(stderr," -f|--file <file>     : set file name for I/O  (default: stdin/stdout)\n");
  fprintf(stderr," -g|-r|--get|--read   : get (read)  data from database (default)\n");
  fprintf(stderr," -s|-w|--set|--write  : set (write) data to database\n");
  fprintf(stderr," -c|--comment <txt>   : set db comment   (default user id)\n");
  fprintf(stderr," -T|--tree            : print config tree\n");
  fprintf(stderr," -H|--history         : print time line for node\n");
  fprintf(stderr," -C|--config          : print available config versions\n");
  fprintf(stderr," -d|--dataonly        : don't write #node/table line, just the data\n");
  fprintf(stderr," -v|--verbose         : set verbose mode on\n");
  fprintf(stderr," -q|--quiet           : set quiet mode on\n");
  //fprintf(stderr," -w|--noWrite         : don't write #node/table line to the output\n");
  fprintf(stderr," -h|--help            : this short help\n");
  fprintf(stderr," supported time formats (cf. man date):\n");
  for(int i=0; dbTimeFormat[i]!=NULL ; i++ ) {
    const int MLEN=128;
    char tmpstr[MLEN];
    time_t t = time(0);
    strftime(tmpstr,MLEN,dbTimeFormat[i],gmtime(&t));
    fprintf(stderr,"  %-20s  e.g.: %s\n",dbTimeFormat[i],tmpstr);
  }
  fprintf(stderr,"%s\n",printVersion());
  if(message) exit(-1);
  return;
}

//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
int 
main(int argc, char *argv[]) 
{
  extern char *optarg;
  int    optInd   = 0;
  char   optChar  = 0;  

  static struct option optLong[] = {
    { "database"   , 1 , 0           , 'D' },
    { "path"       , 1 , 0           , 'p' },
    { "time"       , 1 , 0           , 't' },
    { "expiration" , 1 , 0           , 'x' },
    { "flavor"     , 1 , 0           , 'F' },
    { "file"       , 1 , 0           , 'f' },
    { "get"        , 0 , (int *)&action ,  GetDB  },
    { "read"       , 0 , (int *)&action ,  GetDB  },
    { "set"        , 0 , (int *)&action ,  SetDB  },
    { "write"      , 0 , (int *)&action ,  SetDB  },
    { "tree"       , 0 , (int *)&action ,  PrintTree },
    { "history"    , 0 , (int *)&action ,  PrintHistory },
    { "config"     , 0 , (int *)&action ,  PrintConfig },
    { "comment"    , 1 , 0              , 'c' },
    { "verbose"    , 0 , &verboseMode   , true},
    { "quiet"      , 0 , &verboseMode   , false},
    { "dataonly"   , 0 , &dataOnlyMode  , true},
    { "debug"      , 0 , &debugMode     , true},
    { "help"       , 0 , 0              , 'h' },
    { 0, 0, 0, 0}
  };


  //   cout << indexFromString("05TB01" )  << endl;
  //   cout << indexFromString("P05TB01")  << endl;
  //   cout << indexFromString("05P1" ,kEEmcMaxSect*kEEmcMaxBox)  << endl;
  //   cout << indexFromString("E05TB",kEEmcMaxSect*kEEmcMaxBox)  << endl;
  //   cout << indexFromString("12TB13" )  << endl;
  //   cout << indexFromString("V12TB13")  << endl;
  //   cout << indexFromString("13TG"  ,kEEmcMaxSect*kEEmcMaxBox) << endl;
  //   cout << indexFromString("A13TG" ,kEEmcMaxSect*kEEmcMaxBox) << endl;
  //   exit(0);

  // arguments and init
  argv0 = strrchr(argv[0],'/');
  argv0 = ( argv0 == NULL )  ? argv[0] : ++argv0 ;

  while((optChar = getopt_long(argc,argv,"D:p:t:x:F:f:c:grswTHCdvqh",optLong,&optInd)) != EOF) {
    switch(optChar) {
    case  0  : break;
    case 'D' : dbName      = optarg;       break;
    case 'p' : dbPath      = optarg;       break;
    case 't' : dbTime      = optarg;       break;
    case 'x' : dbExpTime   = optarg;       break;
    case 'F' : dbFlavor    = optarg;       break; 
    case 'f' : dbFile      = optarg;       break; 
    case 'r' : // same as g
    case 'g' : action      = GetDB;        break;
    case 'w' : // same as s
    case 's' : action      = SetDB;        break;
    case 'T' : action      = PrintTree;    break;
    case 'H' : action      = PrintHistory; break;
    case 'C' : action      = PrintConfig;  break;
    case 'c' : dbComment   = optarg;       break;  
    case 'd' : dataOnlyMode= true;         break;
    case 'v' : verboseMode = true;         break;
    case 'q' : quietMode   = true;         break;
    case 'h' : usage(); return(0);         break;
    default  : usage("unknown option");    break;
    };
  }
  if(!dbPath  && action!=PrintConfig ) usage("database path missing");


  // 
  StDbManager    *mgr    = StDbManager::Instance();
  StDbConfigNode *dbNode = NULL;
  StDbTable      *dbTab  = NULL;
  char           *node   = NULL;
  FILE           *file   = NULL;

  mgr->setVerbose(verboseMode);
  mgr->setQuiet(quietMode);

  // sanity check 
  if( ! mgr->findDefaultServer() ) {
    fprintf(stderr,"cannot find the default DB server\n");
    return(-1);
  }

  if(dbFile) { 
    file = fopen(dbFile,(action==SetDB) ? "r" : "w");
    if(!file) { 
      fprintf(stderr,"%s: fopen '%s' failed, %s\n",argv0,dbFile,strerror(errno)); 
      return(-1); 
    }
  } else {
    file = (action==SetDB) ? stdin : stdout;
  }

  switch(action) {
  case PrintTree:
    node   = node=strsep(&dbPath,"/");
    if(node) { 
      dbNode = mgr->initConfig(dbName,node);
      if(dbNode) { 
	if(verboseMode || !quietMode) 
	  fprintf  (file,"DATABASE TREE:\n");
	fprintf  (file,"%s/\n",node);
	if(printTree(file,dbNode)<=0) {
	  fprintf  (stderr,"config %s is empty\n",node);
	}
      }
    }
    return 0;
    break;
  case PrintConfig:
    if(verboseMode || !quietMode) 
     fprintf(file,"DATABASE VERSIONS:\n");
    printConfig(file,mgr);
    return 0;
  default:
    break;
  }


  // parse database path 
  char *preNode="";
  while( (node=strsep(&dbPath,"/")) != NULL ) {
    StDbConfigNode *tn = (dbNode==NULL) ? 
      mgr->initConfig(dbName,node) : dbNode->findConfigNode(node);
    if(!tn) break; // assume the last token is a db table
    dbNode = tn;
    printf("found database node: %s\n",node);
    preNode=node;
  }

  if(!node) usage("invalid path");

  dbTab = dbNode->findTable(node);
  if(!dbTab) { fprintf(stderr,"%s: table %s not found\n",argv0,node); return (-1); }

  dprintf("found table: %s\n",node);

  int nrows=dbTab->GetNRows();
  int ndata=0;
  
  EEmcDbIOBase *dbIO = getDbIO(preNode,node,nrows);
  if(dbIO==NULL) return(-1);

  char keyLine[EEmcDbMaxDbPathLen];
  //char keyFile[EEmcDbMaxKeyLength];
  char keyBase[EEmcDbMaxKeyLength];
  time_t tUnix    =                     getTimeStamp(dbTimeFormat,dbTime);
  time_t tExpUnix = (dbExpTime!=NULL) ? getTimeStamp(dbTimeFormat,dbExpTime) : 0;

  dbTab->setFlavor(dbFlavor);
  switch(action) {
  case PrintHistory:
    printHistory(file,mgr,dbTab,dbIO); 
    return 0; 
    break;
  case SetDB:
    fgets (keyLine,EEmcDbMaxDbPathLen-1,file);
    sprintf(keyBase,EEmcDbKeyFormat,dbNode->printName(),node);
    printf("AAA=%s=\n",keyBase);
    if(strstr(keyLine,keyBase)==0 &&  strcmp(keyLine,keyToEverything) ) {
      fprintf(stderr,"signature mismatch: data file key '%s', required '%s'\n",
	      keyLine,keyBase);
      return (-1);
    }

    dbIO->setComment(dbComment);

    ndata = dbIO->read(file);
    if(ndata<=0) {
      fprintf(stderr,"%s: reading file %s failed\n",argv0,dbFile);
      return(-1);
    }
    if(nrows>1) { 
      dbTab->SetTable(dbIO->getData(),ndata, dbIO->getIndices());
    } else {
      dbTab->SetTable(dbIO->getData(),ndata);
    }
    if(tExpUnix>0) { 
      dbTab->setEndStoreTime(tExpUnix);
      fprintf(stderr,"%s: setting end time %ld\n",argv0,tExpUnix);
    }

    //mgr->setStoreTime(getTimeStamp(dbTimeFormat,dbTime));
    mgr->setStoreTime(tUnix);
    if(! mgr->storeDbTable(dbTab) ) {
      fprintf(stderr,"%s: storing table %s failed\n",argv0,node);
      return(-1);
    }
  
    break;
  case GetDB:
    mgr  ->setRequestTime(tUnix);
    if( ! mgr->fetchDbTable(dbTab) ) {
      fprintf(stderr,"%s: problem with fetch for time stamp %ld / %s",argv0,tUnix,ctime(&tUnix));
      fprintf(stderr," There is no record for this table over the time period :\n");
      fprintf(stderr," BeginDate     = %s\t",dbTab->getBeginDateTime());
      fprintf(stderr,"   EndDate     = %s\n",dbTab->getEndDateTime());
      fprintf(stderr," BeginTimeUnix = %d\t",dbTab->getBeginTime());
      fprintf(stderr,"   EndTimeUnix = %d\n",dbTab->getEndTime());
      return(-1);
    }
    if(! dbTab->GetTable() ) {
      fprintf(stderr,"no data found in table %s\n",node);
      return(-1);
    } 
    dbIO->setData(dbTab->GetTable());

    if(verboseMode || !quietMode)
    fprintf(stderr,"BNL Time Stamp Range  from: %s   to:    %s \n",
	    fmtSqlTime(dbTab->getBeginDateTime()),fmtSqlTime(dbTab->getEndDateTime()));
    if(!dataOnlyMode) {
      fprintf(file,EEmcDbKeyFormat,dbNode->printName(),node);
      fprintf(file,"\n");
    }
    ndata     = dbIO->write(file);
    dbComment = dbIO->getComment();
    if(verboseMode || !quietMode)
      fprintf(stderr,"COMMENT: %s\n",dbComment);
    break;
  default:
    break;
  } 

  if(ndata>0) {  
    dprintf("database access successful %d\n",ndata);
  } else { 
    fprintf(stderr,"%s: table %s access failed\n",argv0,node); 
  }
    

  fclose(file);
  return 0; 
} 


// $Log: eemcDb.C,v $
// Revision 1.1  2013/01/25 16:46:49  stevens4
// Scripts used to upload EEMC tables to the DB
//
// Revision 1.17  2004/03/23 15:01:47  balewski
// access to eemcVaria table
//
// Revision 1.16  2003/11/30 04:18:52  zolnie
// pix db names fixed
//
// Revision 1.15  2003/11/30 04:16:17  zolnie
// PIX dbases in eemcdb
//
// Revision 1.14  2003/10/28 21:18:49  zolnie
// updates for Run2004
//
// Revision 1.13  2003/09/12 16:44:35  zolnie
// further gcc3.2 updates
//
// Revision 1.12  2003/08/29 20:21:02  zolnie
// flavor additons to the Tcl/Tk code, work on history with flavors
//
// Revision 1.11  2003/08/29 17:08:17  zolnie
// small fix in tExpTime
//
// Revision 1.10  2003/08/29 17:07:22  zolnie
// added: flavor and expiration time flag
//
// Revision 1.9  2003/08/19 18:56:32  zolnie
// added PMTstat table
//
// Revision 1.8  2003/08/07 16:33:23  zolnie
// replaced confusing --noWrite/-w option with a clearer one: --dataonly/-d
//
// Revision 1.7  2003/06/03 06:29:15  zolnie
// fixed time zone problem
//
// Revision 1.6  2003/04/11 18:27:57  balewski
// add -w option to prohibit writing of the '#node/table' string to the output
//
// Revision 1.5  2003/04/11 18:04:47  balewski
// add I/O for PMTname
//
// Revision 1.4  2003/04/10 21:44:25  zolnie
// *** empty log message ***
//
// Revision 1.3  2003/02/14 19:52:29  zolnie
// fixed history bug (when update was +1s)
//
// Revision 1.2  2003/02/04 18:10:08  zolnie
// added eemcHVtemp online database
//
// Revision 1.1  2003/01/28 23:22:18  balewski
// start
//
// Revision 1.43  2003/01/28 22:35:00  zolnie
// make sure quiet is quiet (once more)
//
// Revision 1.42  2003/01/25 21:15:15  balewski
// allow the 'keyBase' to be embeded in any string in the firts input line,
//  e.g. <xml target="#online/cdev/" >,
// to simplyfy remote execution of eemcDB with string Blob input
//
// Revision 1.41  2003/01/25 20:09:19  balewski
// add BlobS, remove old kret*
//
// Revision 1.40  2003/01/24 20:54:32  zolnie
// merger with Jan + updates for "HVindex" stuff
//
// Revision 1.39  2003/01/24 16:44:48  balewski
// added WCM+someRing online info
//
// Revision 1.38  2003/01/22 02:31:57  balewski
// small fix
//
// Revision 1.37  2003/01/21 23:47:40  balewski
// HVsys table added
//
// Revision 1.36  2003/01/13 18:20:49  zolnie
// modified history time to be local rather than GMT
//
// Revision 1.35  2003/01/10 20:51:02  zolnie
// small bug when creating a table from scratch
//
// Revision 1.34  2003/01/10 20:43:34  zolnie
// *** empty log message ***
//
// Revision 1.33  2003/01/10 18:48:34  zolnie
// submision version
//
// Revision 1.32  2003/01/10 05:57:07  zolnie
// little things at 1am
//
// Revision 1.31  2003/01/10 05:48:57  zolnie
// *** empty log message ***
//
// Revision 1.30  2003/01/10 05:21:56  zolnie
// version number introduced
//
// Revision 1.29  2003/01/10 05:15:36  zolnie
// make it really quiet when -q issued
//
// Revision 1.28  2003/01/10 04:52:04  zolnie
// updates to Tcl/Tk interface (czyli Zadana Pana Jana)
//
// Revision 1.27  2003/01/09 17:03:54  balewski
// reformating of the otput strings
//
// Revision 1.26  2003/01/09 15:26:46  zolnie
// *** empty log message ***
//
// Revision 1.25  2003/01/08 21:58:26  zolnie
// history clean-up
// tk interface updates
//
// Revision 1.24  2003/01/07 22:55:47  balewski
// -H flag added
//
// Revision 1.23  2003/01/03 23:46:40  zolnie
// *** empty log message ***
//
// Revision 1.22  2003/01/03 23:10:00  balewski
// switch to robinson
//
// Revision 1.21  2003/01/03 21:14:49  zolnie
// fixed string packing in EEmcDbCCIO<T>::read(FILE *f)
// added resetString
// first version of tkEEmcDb
//
// Revision 1.20  2003/01/03 16:40:13  zolnie
// added comment field
//
// Revision 1.19  2002/12/03 17:58:43  balewski
// added PMTped , VerC
//
// Revision 1.17  2002/11/14 19:22:26  zolnie
// working version of mkHVDB
//
// Revision 1.16  2002/11/13 20:12:34  zolnie
// towards (semi)(stable?) final solution
//
// Revision 1.15  2002/11/11 22:54:02  zolnie
// fixed a number of bugs in perl scripts (mainly in digestPmtConf.pl)
// patchy version of mkHVDB
// updated *.hh files
//
// Revision 1.14  2002/11/05 17:51:03  zolnie
// "characterization" updates
//
// Revision 1.13  2002/11/01 23:29:33  zolnie
// *** empty log message ***
//
// Revision 1.12  2002/11/01 20:47:39  zolnie
// added: file parameter added
// help updated
//
// Revision 1.11  2002/11/01 19:47:45  zolnie
// fixed bug: logical conditions in read/write_db were reversed
//
// Revision 1.10  2002/11/01 19:23:18  zolnie
// use templates in eemcDbOp
//
// Revision 1.9  2002/11/01 14:49:48  balewski
//
// ADCconf works
//
// Revision 1.8  2002/11/01 14:34:06  zolnie
// shorten EEMCDbMaxName from 32 to 16
//
// Revision 1.7  2002/11/01 14:18:12  balewski
// walka z ADC conf
//
// Revision 1.6  2002/10/31 22:13:24  zolnie
// bug fixed in eemcConstDB.hh (algebra problem)
//
// Revision 1.5  2002/10/31 20:44:30  zolnie
// for Jas
//
// Revision 1.4  2002/10/31 15:58:27  zolnie
// structure field naming bug fixed in eemcDbPMTcal
//
// Revision 1.3  2002/10/30 22:48:32  zolnie
// new dbase manipulation updates
//
// Revision 1.2  2002/10/30 15:56:29  zolnie
// updated help info
//
