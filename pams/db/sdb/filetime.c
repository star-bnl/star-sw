#include <string.h>
#include <stdlib.h>

#ifndef WIN32
# include <dirent.h>
#endif /* WIN32 */

#include        <stdio.h>
#ifdef irix
#include        <sys/time.h>
#else
#include        <time.h>
#endif

/* Set of routines for manipulation with file names                 */
/* Used by SDBM and XDF2MEM packages, developed for STAR experiment */
/* Author: Vladimir Tikhomirov                                      */
/*         Uses some codes, developed by Akio Ogawa                 */
/* Created: June 1998                                               */


/* Convert time from structure tm format to character DATE.TIME format */
char *tm2datetime(struct tm *t){
  char ctime[17];
  int summer;
  if( 0 == strftime(ctime, 16, "%Y%m%d.%H%M%S", t)){
    puts("***get_datetime: failed to convert date to char.");
    return 0;
  }
  summer=check_summertime(t);
  sprintf(&ctime[15], "%1d", summer); 
  ctime[16]='\0';
  return ctime;
}


/* Convert time from character DATE.TIME format to structure tm format */
int datetime2tm(char* cdate, struct tm *t){
  int year,month,day,hour,min,sec,summer;
  if(strlen(cdate) != 16){
    return 1;
  }
  sscanf(cdate,"%4d%2d%2d.%2d%2d%2d%1d",&year,&month,&day,&hour,&min,&sec,&summer);
  (*t).tm_year = year - 1900;
  (*t).tm_mon  = month - 1;
  (*t).tm_mday = day;
  (*t).tm_hour = hour;
  (*t).tm_min  = min;
  (*t).tm_sec  = sec;
  (*t).tm_isdst= summer;
  if(year<-1900 || year>9999) return 2;
  if(month<0 || month>12) return 3;
  if(day<0   || day>31) return 4;
  if(hour<0  || hour>24) return 5;
  if(min<0   || min>60) return 6;
  if(sec<0   || sec>60) return 7;
  check_summertime(t);
  return 0;
}


int check_summertime(struct tm *t){
  if((*t).tm_isdst > 1){
    (*t).tm_isdst=1;
    return 1;
  } else if ((*t).tm_isdst < 0){
    puts("***check_summertime: Is it summer or winter time?");
    (*t).tm_isdst=2;
    return 2;
  } else {
    return 0;
  }
}


/* Get character string, corresponding to UNIX environment */
char* get_star_cal(char* calenv){
  char *env, starcal[256];
 
  if((env = getenv(calenv)) != NULL){
    strcpy(starcal, env);
    return starcal;
  }else{
    return NULL;
  }
}


/* Return full path and file name for next file from directory */
char* find_cal_file(char *domain, DIR **dirsave){
  char dir[256], fullpath[256];
  DIR *dirp;
  struct dirent *direntp;

  dirp=*dirsave;
  /* dirp=0 means it's the first call for current direcory - open dir first. */
  if(dirp == NULL) {
    strcpy(dir,domain);
    if( (dirp = opendir(dir)) == NULL){
      return NULL;
    }
    *dirsave=dirp;
    return NULL;
  }

  if((direntp = readdir(dirp)) != NULL ){
    strcpy(fullpath, direntp->d_name);
    return fullpath;
  }else{
    *dirsave=NULL;
    closedir(dirp);
    return NULL;
  }
}


/* Fortran interfaces to C code */
/* Author: Vladimir Tikhomirov  */

void open_dir_f_(char *domain, DIR **dsave, int c1){
  find_cal_file(domain, dsave);
}


void get_next_file_f_(char *domain, DIR **dsave, char* file, int c1, int c2){
  char *fullpath;
  fullpath = find_cal_file(domain, dsave);
  if(fullpath != NULL){
    strcpy(file, fullpath);
  }else{
    strcpy(file,"");
  }
}

void get_cal_dir_f_(char *calenv, char *starcal, int c1, int c2){
  char *fullpath;
  fullpath = get_star_cal(calenv);
  if(fullpath != NULL){
    c2=strlen(fullpath);
    strcpy(starcal,fullpath);
  }else{
    c2=0;
    strcpy(starcal,"");
  }
}


void char2time_f_(char *datetime, time_t *time, int c1){
  struct tm ftime;
  if(datetime2tm(datetime,&ftime)==0){
    *time=mktime(&ftime);
  }else{
    *time=-1;
  }
}

void time2char_f_(time_t *time, char *datetime, int c1){
  strcpy(datetime,tm2datetime(localtime(time)));
}
 
 








