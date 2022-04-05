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
#include "sfs_index.h"
#include "get_line.h"
#include <rtsLog.h>

char g_filename[100];
sfs_index *idx;
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



int fs_cd(int argc, char *argv[])
{
  if(argc !=2) return -1;

  if(idx->cd(argv[1]) < 0) {
    printf("%s is invalid directory\n",argv[1]);
  }
  
  //write_env("PWD", idx->cwd);
  printf("pwd set to %s\n", idx->cwd);
  
  return 0;
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

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  struct stat64 filestat;
  if(stat64(fn,&filestat) < 0) 
#else
  struct stat filestat;
  if(stat(fn,&filestat) < 0) 
#endif
  {
    printf("Error reading file %s: %s\n",fn,strerror(errno));
    return 1;
  }
  
#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  idx_fd = open64(fn, O_RDONLY);
#else
  idx_fd = open(fn, O_RDONLY);
#endif
 
  if(idx_fd < 0) {
    printf("Error reading file %s\n",fn);
    strcpy(g_filename, "NONE");
    
    //write_env("MOUNT", "NONE");
    //write_env("PWD", "NONE");
    return -1;
  }

  idx = new sfs_index;

  idx->mount(fn, O_RDONLY);
  t = record_time();

#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  printf("Mounted file %s: %lld bytes in %5.2f sec\n",fn,(long long int)filestat.st_size,t);
#else
  printf("Mounted file %s: %d bytes in %5.2f sec\n",fn,filestat.st_size,t);
#endif

  //write_env("MOUNT", fn);
  //write_env("PWD", "/");

  // printf("MNTDI\n");
  strcpy(g_filename, fn);
  return 0;
}

int getDirSize(fs_dir *dir) {
    int sz = 0;

    fs_dirent *entry;
    fs_dirent storage;
    while((entry = idx->readdir(dir, &storage))) {

	sz += entry->sz;
	
	if(entry->has_child) {
	    fs_dir *ndir = idx->opendir(entry->full_name);
	    sz += getDirSize(ndir);
	    idx->closedir(ndir);
	}
    }
    
    return sz;
}

int getEntrySize(fs_dirent *entry)
{
    int sz = entry->sz;

    if(entry->has_child) {
	fs_dir *ndir = idx->opendir(entry->full_name);
	sz += getDirSize(ndir);
	idx->closedir(ndir);
    }
    
    return sz;
}

int ls_dir(char *tabs, int recurse, fs_dir *dir)
{
  //  char ntabs[40];
  // sprintf(ntabs, "%s\t", tabs);
    if(recurse > 0) recurse--;
 
  fs_dirent *entry;
  fs_dirent storage;
  while((entry = idx->readdir(dir, &storage))) {

      int sz = getEntrySize(entry);
    printf("%s [%7d bytes] %s%c\n",
	   tabs, sz, entry->full_name,entry->has_child?'/':' ');
     
    if(recurse && entry->has_child) {
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
      if(memcmp(argv[1], "-r", 2) == 0) {
	  recurse = 999;
	  if(strlen(argv[1]) > 2) {
	      recurse = atoi(&argv[1][2]);
	  }
      
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
  
  //printf("[%s: %s %s]\n",idx->pwd(),argv[0], argv[1]);
  int type;
  if(strcmp(argv[0], "cat") == 0) type = 0;
  else if (strcmp(argv[0], "od") == 0) type = 8;
  else type = 1;

  int sz = idx->fileSize(argv[1]);
  if(sz < 0) return -1;

  char *buff = (char *)malloc(sz);
  if(!buff) {
    printf("Error allocating %d bytes\n",sz);
    return -1;
  }

  int ret = idx->read(argv[1], buff, sz);
  if(ret != sz) {
    printf("Error reading %d bytes\n",sz);
  }

  if(strcmp(argv[0], "save") == 0) {
    if(argc < 3) {
      printf("Need a filename\n");
      free(buff);
      return -1;
    }
#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    int fd = open64(argv[2], O_WRONLY | O_CREAT,0777);
#else
    int fd = open(argv[2], O_WRONLY | O_CREAT, 0777);
#endif

    if(fd < 0) {
      free(buff);
      printf("error opening file %s\n",argv[2]);
      return -1;
    }
    int ret = write(fd,buff,sz);
    if(ret < sz) {
      printf("Error writing to file %s (only wrote %d of %d bytes\n",
	     argv[2], ret, sz);
      free(buff);
      return -1;
    }
    close(fd);
    free(buff);
    return 0;
  }

  if(type == 0) {
    write(STDOUT_FILENO, buff, sz);
    write(STDOUT_FILENO, "\n", sizeof("\n"));
   
  }
  else if (type == 8) {    // hex dump
    //printf("Header:\n");
    //fsBankReader::headerdump(buff);
    //printf("Data:\n");
    fs_index::hexdump(buff, sz);
    printf("\n");
  }
  else {   // strings...
    int instr = 0;
    for(int i=0;i<sz;i++) {
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
  printf("\tsize <dir>\n");
  printf("\n\n");
  return 1;
}


int docmd(int argc, char *argv[]) 
{  
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
  else if(strcmp(argv[0], "size") == 0) {
    SfsDirsize x;
    ((sfs_index *)idx)->getDirSize(argv[1], &x);
    printf("x->size = %lld   x->dataSize = %lld\n",x.size, x.dataSize);
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
    int ac = 0;
  
    rtsLogOutput(2);
    rtsLogLevel(WARN);
    strcpy(g_filename, "none");

    idx = NULL;

    if(argc > 1) {
	// Try mounting...
#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
	struct stat64 sstat;    
	int ret = stat64(argv[1], &sstat);
#else
	struct stat sstat;    
	int ret = stat(argv[1], &sstat);
#endif

	if(ret != 0) {
	    printf("No file %s\n",argv[1]);
	    return -1;
	}

	if(argc == 2) {
	    av[0] = "mount";
	    av[1] = argv[1];
	    ac = 2;

	    docmd(ac, av);
	}
	else {
	    idx = new sfs_index();
	    int ret = idx->mountSingleDir(argv[1]);
	    if(ret < 0) {
		printf("Error reading %s\n", argv[1]);
		return -1;
	    }
	    do {

		if(strcmp(argv[2],"ls") != 0) {  // if not cd to event directory
		    fs_dir *d = idx->opendir("/");
		    if(d) {
			fs_dirent storage;
			fs_dirent *e = idx->readdir(d, &storage);
			if(e) idx->cd(e->full_name);
			idx->closedir(d);
		    }
		}
	
		docmd(argc-2, &argv[2]);
		ret = idx->mountNextDir();
	    } while(ret > 0);

      
	    return 0;
	} 
    }
    
    for(;;) {
	char *_strtok_static_;
	char buff[256];
	char pwd[100];

	if(!idx) {
	    strcpy(pwd, "none");
	}
	else {
	    strcpy(pwd, idx->pwd());
	}

	printf("%s:%s > ", g_filename, pwd);

	fflush(stdout);

	get_line(buff);

	av[0] = strtok_r(buff, " ", &_strtok_static_);
	if(av[0] == NULL) continue;

	for(ac = 1; ac < 10; ac++) {
	    av[ac] = strtok_r(NULL, " ", &_strtok_static_);
	    if(av[ac] == NULL) break;
	}

	if(strcmp(av[0], "quit") == 0) break;

	docmd(ac, av);

    }
    
    return 0;
}



