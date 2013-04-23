#include <unistd.h>
#include <stdio.h>
#include <mysql.h>
#include <rtsLog.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>

#define DB_HOST "onldb.daq.bnl.local"
#define DB_PORT 3501
#define DB_NAME "RunLog"
#define DB_TABLE "qaFiles"

static MYSQL mysql;

void initMysql()
{
  if(!mysql_real_connect(&mysql,
			 DB_HOST,
			 "jml",
			 NULL,
			 DB_NAME,
			 DB_PORT,
			 NULL,
			 0)) {
    LOG(CRIT, "Can't connect to RunLog");
    exit(0);
  }
}


int writeToDB(int runNumber, char *pdfname, char *flavor)
{
  static int init=0;
  int ret;

  rtsLogOutput(RTS_LOG_NET);
  rtsLogAddDest((char *)"172.16.0.1",8004);
  rtsLogLevel((char *)WARN);

  if(!init) {
    initMysql();
    init = 1;
  }

  int fd = open(pdfname, O_RDONLY);
  if(fd < 0) {
    LOG(ERR, "Error opening pdf file %s (%s)", pdfname, strerror(errno));
    return -1;
  }

  struct stat statbuf;
  ret = fstat(fd, &statbuf);
  if(ret < 0) {
    LOG(ERR, "Error getting size of pdf file %s (%s)", pdfname, strerror(errno));
    close(fd);
    return -1;
  }

  int sz = statbuf.st_size;
  
  char *buff = (char *)malloc(sz);
  if(!buff) {
    LOG(ERR, "Couldn't allocate %d bytes (%s)", sz, strerror(errno));
    close(fd);
    return -1;
  }
  char *escaped_buff = (char *)malloc(sz*2+1);
  if(!escaped_buff) {
    LOG(ERR, "Couldn't allocate %d bytes (%s)", sz, strerror(errno));
    close(fd);
    return -1;
  }
  char *sql_buff = (char *)malloc(sz*2+512);
  if(!escaped_buff) {
    LOG(ERR, "Couldn't allocate %d bytes (%s)", sz, strerror(errno));
    close(fd);
    return -1;
  }

  int bytesread=0;
  while(bytesread < sz) {
    int ret = read(fd, buff+bytesread, sz-bytesread);
    if(ret < 0) {
      LOG(ERR, "Error reading from %s (%s)",pdfname, strerror(errno));
      close(fd);
      free(buff);
      free(escaped_buff);
      free(sql_buff);
      return -1;
    }

    bytesread += ret;
  }

  close(fd);


  mysql_real_escape_string(&mysql, escaped_buff, buff, sz);
  
  sprintf(sql_buff, "replace into " DB_TABLE " (runNumber, baseName, fileblob, extension, flavor) values (%d,'%d','%s','pdf', '%s')",
	  runNumber,
	  runNumber,
	  escaped_buff,
	  flavor);
  
  if(mysql_real_query(&mysql, sql_buff, strlen(sql_buff))) {
    LOG(ERR, "Error inserting pdf (%d:%s) record to %s (%s)",runNumber,flavor,DB_TABLE,mysql_error(&mysql));
    free(buff);
    free(escaped_buff);
    free(sql_buff);
    return -1;
  }

  free(buff);
  free(escaped_buff);
  free(sql_buff);
  return 0;
}

int main(int argc, char *argv[])
{
  if(argc != 4) {
    printf("%s <run> <pdffile> <flavor>\n", argv[0]);
    return -1;
  }
  
  int run=atoi(argv[1]);
  char *pdf = argv[2];
  char *flavor = argv[3];

  writeToDB(run, pdf, flavor);

}
