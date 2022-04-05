// $Id: spinDbAPI.C,v 1.1 2005/09/30 23:47:48 balewski Exp $
// Descripion: a kind of swiss army knife for spin databases
// Author:  Jan Balewski  (IUCF)
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


#include "SpinDbIO.h"
#include "spinDbAPI.h"


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
enum  SpinDbAction action = GetDB;
char  *argv0 = NULL; // program name
char  *dbPath       = NULL;
char  *dbTime       = "now";  
char  *dbExpTime    = NULL;
char  *dbFile       = NULL;
char  *dbFlavor     = "ofl";
char  *dbComment    = getenv("USER");
char  *dbName       = "Calibrations_rhic";
int    dataOnlyMode = false;
int    debugMode    = false;

static SpinDbIOBase *
getDbIO(const char *node, const int ndata) { 

  SpinDbIOBase *dbIO=0;

  // dbIO = new SpinDbBXmaskIO(ndata);return (dbIO);//tmp
  // ===================   SPIN RELATED =================
  if (!strncmp(node,"spinV124"  ,SpinDbMaxDbPathLen)) 
    dbIO = new SpinDbV124IO(ndata);
  else if (!strncmp(node,"spinStar"  ,SpinDbMaxDbPathLen)) 
    dbIO = new SpinDbStarIO(ndata);
  else if (!strncmp(node,"spinBXmask"  ,SpinDbMaxDbPathLen))
    dbIO = new SpinDbBXmaskIO(ndata);
 // ===================  OTHER ==========================
  else {
    fprintf(stderr,"table %s unknown to %s\n",node,argv0);
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
printHistory(FILE *out, StDbManager *mgr, StDbTable *dbTab , SpinDbIOBase *io)
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
    if(tUnix>SpinDbMaxUnixTime) break;
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
  while( (node=strsep(&dbPath,"/")) != NULL ) {
    StDbConfigNode *tn = (dbNode==NULL) ? 
      mgr->initConfig(dbName,node) : dbNode->findConfigNode(node);
    if(!tn) break; // assume the last token is a db table
    dbNode = tn;
    dprintf(" database node: %p\n",node);
  }

  if(!node) usage("invalid path");

  dbTab = dbNode->findTable(node);
  if(!dbTab) { fprintf(stderr,"%s: table %s not found\n",argv0,node); return (-1); }

  dprintf("found table: %s\n",node);

  int nrows=dbTab->GetNRows();
  int ndata=0;
  
  SpinDbIOBase *dbIO = getDbIO(node,nrows);
  if(dbIO==NULL) return(-1);

  char keyLine[SpinDbMaxDbPathLen];
  char keyBase[SpinDbMaxKeyLength];
  time_t tUnix    =                     getTimeStamp(dbTimeFormat,dbTime);
  time_t tExpUnix = (dbExpTime!=NULL) ? getTimeStamp(dbTimeFormat,dbExpTime) : 0;

  dbTab->setFlavor(dbFlavor);
  switch(action) {
  case PrintHistory:
    printHistory(file,mgr,dbTab,dbIO); 
    return 0; 
    break;
  case SetDB:
    fgets (keyLine,SpinDbMaxDbPathLen-1,file);
    sprintf(keyBase,SpinDbKeyFormat,dbNode->printName(),node);
    // printf("AAA=%s=\n",keyBase);
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
    // fprintf(stderr,"TEST, terminated  by hand DB-write, JB\n"); exit(1);
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
      fprintf(file,SpinDbKeyFormat,dbNode->printName(),node);
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


// $Log: spinDbAPI.C,v $
// Revision 1.1  2005/09/30 23:47:48  balewski
// start
//
