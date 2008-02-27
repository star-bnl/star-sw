#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>
#include <SUNRT/clock.h>
#include "daqr_index.h"
#include "sfs_index.h"
#include "get_line.h"
#include "fsBankReader.h"
#include <rtsLog.h>

fs_index *idx;
int idx_fd;

char *striptodir(char *str)
{
  char *ostr = str;
  char *lslash = str-1;
  
  while(*str != '\0') {
    if(*str == '/') {
      // If already a dir, roll back to previous dir
      if(*(str+1) != '\0') lslash = str;
    }
    str++;
  }
  
  lslash++;
  *lslash = '\0';
  return ostr;
}


void write_env(char *var, char *value)
{
  //printf("Hereasf\n");
  char *user = getenv("USER");
  if(!user) return;
  char fn[256];
  sprintf(fn, "/tmp/%s_FS_%s",user,var);
  unlink(fn);
  int fd = open(fn, O_WRONLY | O_CREAT, 0666);
  write(fd, value, strlen(value));
  close(fd);
}

char *read_env(char *var)
{
  char *user = getenv("USER");
  if(!user) return NULL;

  char fn[256];
  sprintf(fn, "/tmp/%s_FS_%s",user,var);
  int fd = open(fn, O_RDONLY);
  
  if(fd < 0) return NULL;

  static char res[256];
  memset(res,0,sizeof(res));
  read(fd, res, 256);

  close(fd);
  return res;
}

int fs_cd(int argc, char *argv[])
{
  if(argc !=2) return -1;

  if(idx->cd(argv[1]) < 0) {
    printf("%s is invalid directory\n",argv[1]);
  }
  
  write_env("PWD", idx->cwd);
  printf("pwd set to %s\n", idx->cwd);
  
  return 0;
}

char *fs_getpwd()
{
  char cd[256];
  char mount[256];
  static char pwd[256];
  char *tmp;

  tmp = read_env("PWD");
  if(!tmp) tmp = "NONE";
  strcpy(cd,tmp);

  tmp = read_env("MOUNT");
  if(!tmp) tmp = "NONE";
  strcpy(mount,tmp);

  sprintf(pwd, "%s:%s",mount,cd);
  return pwd;
}


int fs_pwd()
{
  printf("%s\n", idx->cwd);
  return 0;
}


int fs_mount(int argc, char *argv[])
{
  if(idx) {
    idx->umount();
    delete idx;
  }

  double t = record_time();
  if(argc < 2) {
    printf("mount <daqfilename>\n");
    return -1;
  }

  char *pwd=getenv("PWD");
  char fn[255];
  
  if(argv[1][0] == '/') {
    strcpy(fn,argv[1]);
  }
  else {
    if(pwd == NULL) pwd = "/";
    sprintf(fn,"%s/%s",pwd,argv[1]);
  }

  struct stat filestat;
  if(stat(fn,&filestat) < 0) {
    printf("Error reading file %s: %s\n",fn,strerror(errno));
    return 1;
  }
  
  idx_fd = open(fn, O_RDONLY);
  if(idx_fd < 0) {
    printf("Error reading file %s\n",fn);
    write_env("MOUNT", "NONE");
    write_env("PWD", "NONE");
    return -1;
  }

//   char pp[4];
//   read(idx_fd,pp,4);

//   if(memcmp(pp,"SFS",3) == 0) {
//     printf("Mounting sfs file: %s\n",fn);
//     idx = new sfs_index;
//   }
//   if(memcmp(pp,"FIL",3) == 0) {
//     printf("Mounting sfs file  %s\n",fn);
//     idx = new sfs_index;
//   }
//   else {
//     printf("Mounting daq file: %s\n",fn);
//     idx = new sfs_index;
//   }
  
//   close(idx_fd);

  idx = new sfs_index;

  idx->mount(fn, O_RDONLY);
  t = record_time();
  printf("Mounted file %s: %d bytes in %5.2f sec\n",fn,(int)filestat.st_size,t);
  write_env("MOUNT", fn);
  write_env("PWD", "/");

  // printf("MNTDI\n");
  return 0;
}

int ls_dir(char *tabs, int recurse, fs_dir *dir)
{
  //  char ntabs[40];
  // sprintf(ntabs, "%s\t", tabs);

  fs_dirent *entry;
  while((entry = idx->readdir(dir))) {
    printf("%s [%7d bytes] %s%c\n",
	   tabs,entry->sz,entry->full_name,entry->has_child?'/':' ');
     
    if(recurse & entry->has_child) {
      fs_dir *ndir = idx->opendir(entry->full_name);
      ls_dir(tabs, recurse, ndir);
      idx->closedir(ndir);
    }
  }
  return 0;
}

int fs_ls(int argc, char *argv[])
{
  int recurse = 0;

  if(argc > 1) {
    if(strcmp(argv[1], "-r") == 0) {
      recurse = 1;
    }
  }

  //printf("start.n\n");
  fs_dir *dir = idx->opendir(idx->cwd);
  if(!dir) {
    printf("Error opening directory %s\n",idx->cwd);
    return -1;
  }
  
  ls_dir("", recurse, dir);

  idx->closedir(dir);

  return 0;
}

int fs_cat(int argc, char *argv[])
{
  if(argc < 2) {
    printf("cat <file> <optional output file>\n");
    return -1;
  }

  int type;
  if(strcmp(argv[0], "cat") == 0) type = 0;
  else if (strcmp(argv[0], "od") == 0) type = 8;
  else type = 1;

  fs_dirent *entry = idx->readdirent(argv[1]);
  if(!entry) {
    printf("Can't find file %s\n",argv[1]);
    return -1;
  }

  if(entry->sz == 0) {
    printf("%s has no data...\n",argv[1]);
    return 0;
  }

  char *buff = (char *)malloc(entry->sz);
  if(!buff) {
    printf("Error allocating %d bytes\n",entry->sz);
    return -1;
  }

  int ret = lseek(idx_fd, entry->offset, SEEK_SET);
  if(ret != entry->offset) {
    printf("Invalid seek %d vs %d  (%s)\n",ret,entry->offset,strerror(errno));
    return -1;
  }

  ret = read(idx_fd, buff, entry->sz);
  if(ret != entry->sz) {
    printf("Error reading file %d vs %d\n", ret, entry->sz);
    return -1;
  }

  if(strcmp(argv[0], "save") == 0) {
    if(argc < 3) {
      printf("Need a filename\n");
      free(buff);
      return -1;
    }
    int fd = open(argv[2], O_WRONLY | O_CREAT,0777);
    if(fd < 0) {
      free(buff);
      printf("error opening file %s\n",argv[2]);
      return -1;
    }
    int ret = write(fd,buff,entry->sz);
    if(ret < entry->sz) {
      printf("Error writing to file %s (only wrote %d of %d bytes\n",
	     argv[2], ret, entry->sz);
      free(buff);
      return -1;
    }
    close(fd);
    free(buff);
    return 0;
  }

  if(type == 0) {
    write(STDOUT_FILENO, buff, entry->sz);
    write(STDOUT_FILENO, "\n", sizeof("\n"));
   
  }
  else if (type == 8) {    // hex dump
    //printf("Header:\n");
    //fsBankReader::headerdump(buff);
    //printf("Data:\n");
    fsBankReader::hexdump(buff, entry->sz);
    printf("\n");
  }
  else {   // strings...
    int instr = 0;
    for(int i=0;i<entry->sz;i++) {
      if(isprint(buff[i])) {
	instr = 1;
	putchar(buff[i]);
      }
      else {
	if(instr == 1) {
	  printf("\n");
	  instr = 0;
	}
      }
    }
    printf("\n");
  }
 
  free(buff);
  return 0;
}



int help()
{
  printf("fs <cmd> <args>\n");
    
  printf("\tcd <dir>\n");
  printf("\tdump\n");
  printf("\tpwd\n");
  printf("\tmount <file>\n");
  printf("\tls\n");

  printf("\tstrings <fn>\n");
  printf("\tod <fn>\n");
  printf("\tsave <fn> <ofn>\n");

  printf("\n\n");
  return 1;
}


int docmd(int argc, char *argv[]) 
{  

  struct stat sstat;

  int ret = stat(argv[0], &sstat);
  if(ret == 0) {
    char *nargv[10];
    int nargc=argc+1;
    nargv[0] = "mount";
    for(int i=0;i<argc;i++) {
      nargv[i+1] = argv[i];
    }
    fs_mount(nargc, nargv);
    return 1;
  }

  if((strcmp(argv[0], "mount") == 0)) {
    fs_mount(argc, argv);
    return 0;
  }
  
  if(!idx) {
    printf("No mounted file:  mount a file first\n");
    help();
    return 0;
  }
  else if(strcmp(argv[0], "pwd") == 0) {
    fs_pwd();
    return 0;
  }
  else if(strcmp(argv[0], "cd") == 0) {
    fs_cd(argc, argv);
    return 0;
  }
  else if(strcmp(argv[0], "ls") == 0) {
    fs_ls(argc, argv);
    return 0;
  }
  else if(strcmp(argv[0], "dump") == 0) {
    idx->dump(idx_fd);
    return 0;
  }
  else if(strcmp(argv[0], "od") == 0) {
    fs_cat(argc, argv);
    return 0;
  }
  else if(strcmp(argv[0], "save") == 0) {
    fs_cat(argc, argv);
    return 0;
  }
  else if(strcmp(argv[0], "strings") == 0) {
    fs_cat(argc, argv);
    return 0;
  }
  else {
    printf("\nInvalid command: %s\n\n",argv[0]);
    help();
    return 0;
  }
}


int main(int argc, char *argv[])
{
  // Parse cmds...
  char *av[10];
  char ac = 0;
  char fn[256];

  rtsLogOutput(2);
  // rtsLogLevel(DBG);

  // Mount...
  char *mnt = read_env("MOUNT");
  if(!mnt) mnt = "NONE";
  strcpy(fn,mnt);
  av[1] = fn;
  ac = 2;
  fs_mount(ac,av);

  char *pwd = read_env("PWD");
  if(!pwd) pwd = "NONE";

  if(idx)
    idx->cd(pwd);

  if(argc > 1) {
    if(docmd(argc-1, &argv[1]) == 0) return 0;
    
    if(argc > 2) {
      docmd(argc-2, &argv[2]);
      return 0;
    } 
  }
    
  for(;;) {
    char buff[256];

    printf("%s > ", fs_getpwd());
    fflush(stdout);

    get_line(buff);

    av[0] = strtok(buff, " ");
    if(av[0] == NULL) continue;

    for(ac = 1; ac < 10; ac++) {
      av[ac] = strtok(NULL, " ");
      if(av[ac] == NULL) break;
    }

    if(strcmp(av[0], "quit") == 0) break;

    docmd(ac, av);

  }
    
  return 0;
}



