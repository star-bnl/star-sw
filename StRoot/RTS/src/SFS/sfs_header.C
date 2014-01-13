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
#include "sfs_index.h"


void printittr(SFS_ittr *ittr, int ctr) {
  // n, offset, type, attr, name, stickypath, ppath, fullpath, 
  char attr[5];
  attr[0] = (ittr->entry.attr == SFS_ATTR_CD) ? 'C' : '-';
  attr[1] = (ittr->entry.attr & SFS_ATTR_NOCD) ? 'N' : '-';
  attr[2] = (ittr->entry.attr & SFS_ATTR_STICKY_CD) ? 'S' : '-';
  attr[3] = (ittr->entry.attr & SFS_ATTR_POPSTICKY) ? 'P' : '-';
  attr[4] = '\0';
  char type[5];
  memcpy(type, ittr->entry.type, 4);
  type[4] = '\0';
  printf("#%05d@%12lld: %s %s %s [S:%s] [P:%s] [F:%s] (totsz=%d)\n",
	 ctr,ittr->fileoffset,
	 type,
	 attr,
	 ittr->entry.name,
	 ittr->stickypath,
	 ittr->ppath,
	 ittr->fullpath,
	 ittr->entry.sz + ittr->entry.head_sz);
}

int main(int argc, char *argv[])
{
  if(argc < 2) {
    printf("sfs_headers filename\n");
    return -1;
  }

  char *fn = argv[1];
  
  struct stat statbuf;
  stat(fn, &statbuf);
  long long int sz = statbuf.st_size;

  SFS_ittr ittr;
  wrapfile file;
  
  if(file.opendisk(fn, O_RDONLY) < 0) {
    printf("Error openeing file %s (%s)\n",fn,strerror(errno));
    return -1;
  }

  int ctr=1;

  ittr.get(&file);

  long long int pos = 0;
  while((ittr.next() >=0)  && (ittr.filepos >= 0)) {
    printittr(&ittr,ctr);
    ctr++;
    pos += ittr.entry.sz + ittr.entry.head_sz;
  }

  printf("Filesz = %lld   Position = %lld\n",sz,pos);
  if(sz != pos) {
      int fd = open(fn, O_RDONLY);
      lseek(fd, pos-200, SEEK_SET);
      char buff[25600];
      int ret = read(fd, buff, 25600);

      printf("Errr Buffer: \n\t");
      for(int i=0;i<ret;i++) {
	  char c;
	  c = buff[i];

	  if(!isprint(c)) c = '.';

	  printf("%c",c);
	  if((i+1) % 10 == 0) printf(" ");
	  if((i+1) % 100 == 0) printf("\n\t");
	  if((i+1) == 200) printf("\n\t-----------------------\n\n\t");
      }
      
  }

}
  
 
