#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <math.h>

int main(int, char *[])
{
  struct stat statbuff;

  int ret;

  ret = stat("map.bin", &statbuff);
  if(ret == -1) {
    printf("Can't stat file map.bin (%s)\n", strerror(errno));
    return 0;
  }
  
  int sz = statbuff.st_size;
  uint *buff = (uint *)malloc(sz);
  float *fbuff = (float *)buff;

  if(!buff) {
    printf("Can't allocate %d bytes\n",sz);
    return 0;
  }

  int fd = open("map.bin", O_RDONLY);
  if(fd == -1) {
    printf("Can't open file map.bin (%s)\n", strerror(errno));
    return 0;
  }
  
  ret = read(fd, buff, sz);
  if(sz != ret) {
    printf("Only read %d of %d bytes\n",ret,sz);
    return 0;
  }

  close(fd);

  // Print out buff...
  int head_words = buff[1];
  
#ifdef PRINTHEAD
  for(int i=0;i<head_words;i++) {
    printf("Head[%d]  = %d (%f)\n",i,buff[i],fbuff[i]);
  }
#endif

#ifndef PRINTGRID
  float dpad = fbuff[3];
  float dtb = fbuff[4];
  float maxtb = fbuff[5];
  
  int npgrid = (int) ceil(182./dpad);
  int ntbgrid = (int) ceil(maxtb/dtb);

  typedef float  map_t[45][npgrid+1][ntbgrid+1][3];
  map_t *map = (map_t *)&buff[head_words+1];
  
  int sector = 3;
  int pr = 20;

  for(int pg=0;pg<npgrid;pg++) {
    for(int tg=0;tg<ntbgrid;tg++) {
      float x = map[sector][pr][pg][tg][0];
      float y = map[sector][pr][pg][tg][1];
      float z = map[sector][pr][pg][tg][2];
      //printf("pg=%d tg=%d %f %f\n",pg,tg,y,z);
      printf("%f %f\n",y,z);
    }
  }


#endif

  free(buff);
}
