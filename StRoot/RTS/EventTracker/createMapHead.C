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
  char *cbuff = (char *)buff;

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

  printf("#define MAPDOTBINFILE_SZ %d\n", sz);
  printf("u_int mapdotbinfile[%d] = {\n", sz/4);
  
  for(int i=0;i<sz/4;i++) {
    printf("0x%08x, ", buff[i]);
    if( ((i+1) % 10) == 0) printf("\n");
  }

  printf("};\n");

  free(buff);
}
