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
  
  SFS_ittr ittr;
  wrapfile file;
  
  if(file.opendisk(fn, O_RDONLY) < 0) {
    printf("Error openeing file %s (%s)\n",fn,strerror(errno));
    return -1;
  }

  int ctr=1;

  ittr.get(&file);

  while((ittr.next() >=0)  && (ittr.filepos >= 0)) {
    printittr(&ittr,ctr);
    ctr++;
  }
}
  
 
