#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <rts.h>
#include "sfs_index.h"
#include <SUNRT/clock.h>


int main(int argc, char *argv[])
{
  if(argc != 2) {
    printf("fs_ex filename\n");
    return 0;
  }
  
  int fd = open(argv[1], O_RDONLY);
  if(fd < 0) {
    printf("Bad file...%s\n",argv[1]);
  }

  printf("Sizeof inode=%ld\n",sizeof(fs_inode));
  fs_index *idx;

  char ff[4];
  read(fd, ff, 4);
  lseek(fd, 0, SEEK_SET);
  
  //if(memcmp(ff, "SFS", 3) == 0) {
    // sfs file...
    printf("SFS format input file...\n");
    
    idx = new sfs_index;
    //}
    //else {
    // printf("DAQ format input file...\n");

    //idx = new daqr_index;
    //}

  close(fd);
  //idx->ls(fd);

  double t;
  printf("Building index for file %s...\n",argv[1]);
  record_time();
  idx->mount(argv[1],O_RDONLY);
  t = record_time();
  printf("time to build index of %d inodes = %5.2lf seconds\n",idx->n_inodes,t);


  // use opendir/readdir same as unix directory api
  fs_dir *dir = idx->opendir("/");
  if(!dir) {
    printf("Bad /\n");
    return 0;
  }

  fs_dirent *entry;

  printf("First 10 elements in /\n");
  int i=0;
  while((entry = idx->readdir(dir))) {
    
      printf("%lld %d (%s)(%s)%c\n",entry->offset, entry->sz, idx->cwd,entry->d_name,entry->has_child ? '/' : ' ');

    
    if(i++ > 10) break;
  }

  idx->closedir(dir);

  // now, trace first directory to a file and dump it...
  fs_dir *dirs[20];
  dirs[0] = idx->opendir("/");
  if(!dirs[0]) {
    printf("bad...\n");
    return 0;
  }

  i=0;
  while(i<20) {
    entry = idx->readdir(dirs[i]);
    if(entry->has_child) { // this is a directory!
      i++;
      printf("i=%d entry->sz=%d, name=%s\n",i,entry->sz,entry->full_name);
      dirs[i] = idx->opendir(entry->full_name);   
      if(!dirs[i]) {
	printf("bad...\n");
	return 0;
      }
      continue;
    }
    
    break;  // we have bottomed out at a file.
  }

  while(i>=0) {
    idx->closedir(dirs[i]);
    i--;
  }

  printf("%lld %d %s\n",entry->offset, entry->sz, entry->full_name);

  /*
  int ret = lseek(fd, entry->offset, SEEK_SET);
  if(ret != entry->offset) {
    printf("Seek failed...\n");
    return 0;
  }

  char *buff = (char *)malloc(entry->sz);
  if(!buff) {
    printf("malloc faild...\n");
    return 0;
  }

  ret = read(fd, buff, entry->sz);

  char *cc = buff;
  for(int i=0;i<entry->sz;i++,cc++) {
    if(i%80==0) putchar('\n');
    if(isprint(*cc)) putchar(*cc); 
    else putchar('.');
  }

  putchar('\n');
  free(buff);
  close(fd);

  return 0;
  */
}
