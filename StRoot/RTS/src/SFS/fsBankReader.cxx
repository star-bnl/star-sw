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


#include <daqFormats.h>
#include <rts.h>

#include "fs_index.h"
#include "fsBankReader.h"

int fsBankReader::open(int fd, fs_index *idx, char *name) 
{
  // don't want to be screwed when somebody else modifies working dir
  char fullname[256];
  idx->getFullPath(fullname,name); 

  strcpy(this->name,fullname);
  this->fd = fd;
  this->idx = idx;
  return 0;
}

void fsBankReader::close() 
{
  idx = NULL;
  return;
}

void fsBankReader::dump() 
{
  if(!idx) return;

  fs_dirent *ent = idx->readdirent(name);
  if(!ent) return;

  char *buff = (char *)malloc(ent->sz);
  if(!buff) return;

  int ret = lseek(fd,ent->offset,SEEK_SET);
  if(ret != ent->offset) {
    free(buff);
    return;
  }

  ret = read(fd,buff,ent->sz);
  if(ret != ent->sz) {
    free(buff);
    return;
  }

  decdump(buff, ent->sz);
  free(buff);
}

void fsBankReader::headerdump(char *buff)
{
  bankHeader *bh = (bankHeader *)buff;
  char type[10];
  memcpy(type, bh->bank_type, 8);
  type[9] = '\0';
  int swap = (bh->byte_order == 0x04030201) ? 0 : 1;
  
  printf("%s: %d words, bank_id=%d format_ver=0x%x byte_order=0x%x format_number=%d token=%d\n",
	 type,
	 qswap32(swap,bh->length),
	 qswap32(swap,bh->bank_id),
	 qswap32(swap,bh->format_ver),
	 bh->byte_order,
	 qswap32(swap,bh->format_number),
	 qswap32(swap,bh->token));
}
  
void fsBankReader::decdump(char *buff, int sz)
{
  int *ib = (int *)buff;
  for(int i=0;i<sz/4;i++) {
    printf("v[%d] = %d\n",i,ib[i]);
  }
}
  
void fsBankReader::hexdump(char *buff, int sz)
{
  for(int i=0;i<sz;i+=16) {
    int k= sz-i;
    if(k>16) k=16;

  
    printf("0x%08x:  ",i);

    for(int j=0;j<16;j++) {
      if(j==8) printf("    ");
      int x = buff[i+j] & 0x00ff;
      if(j<k)
	printf(" %02x",x);
      else printf("   ");
    }

    printf("     ");
    for(int j=0;j<16;j++) {
      
      if(j == 8) printf(" ");

      if(j<k) {
	if(isprint(buff[i+j]))
	  printf("%c",buff[i+j]);
	else
	  printf("."); 
      }
    }
    printf("\n");
  }      
}
  
