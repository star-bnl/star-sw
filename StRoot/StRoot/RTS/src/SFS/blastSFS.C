#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>

#include <SUNRT/clock.h>
#include "sfs_lib.h"

#define BUFFSZ 100*1024*1024


int main(int argc, char *argv[])
{
  record_time();

  if(argc  < 2) {
    printf("%s filename\n",argv[0]);
    return 0;
  }

  // 100 mb...
  printf("Buffer size %d bytes\n",BUFFSZ);

  char head[256];
  SFS_File *file = (SFS_File *)head;

  char *buff = (char *)malloc(BUFFSZ + 8*1024);
  if(!buff) {
    printf("Error in malloc\n");
  }
  
  unsigned int buffx = (unsigned int) buff;

  buffx /= 8*1024;
  buffx *= 8*1024;
  if(buffx != (unsigned int)buff) {
    buffx += 8*1024;
    printf("new 0x%x / old 0x%x\n",buffx,(unsigned int)buff);
    buff = (char *)buffx;
  }

  for(int i=0;i<BUFFSZ;i++) {
    buff[i] = i % 256;
  }
  
  double t = record_time();
  printf("setup: %10.8f secs\n", t);

  double ltime = 0;
  double etime = 0;
  double sent = 0;
  double eventsz = 0;
  char fn[255];
  int filen=0;

  char *fp;
  srand(time(NULL));

  int fd=-1;

  char mntname[256];
  for(int ii=0;ii>=0;ii++) {
    filen++;

    if(ii % 1000 == 0) {
      if(ii > 0) close(fd);

      sprintf(mntname, "%s_%d",argv[1], ii/1000);
      fd = open(mntname, O_WRONLY | O_CREAT, 0666);
      
      char seedbuff[1000];
      SFS_desc desc;
      sfs_create_mimage(&desc, seedbuff, 1000);
      
      write(fd, seedbuff, desc.sz);
    }

    //sprintf(fn, "/event_%d",filen);
    // mkdir(fn,0777);

    eventsz = 0;
    fp = buff;

    for(int detn=0;detn<10;detn++) {
      //sprintf(fn,"/event_%d/det_%d",filen,detn);
      //mkdir(fn,0777);

      for(int subn=0;subn<10;subn++) {
	if(subn == 0)
	  sprintf(fn, "/event_%d/det_%d/sub_%d",filen,detn,subn);
	else 
	  sprintf(fn, "sub_%d",subn);
	
	float rnd = ((float)rand()/(float)RAND_MAX) * 5000.0;
	int fsz = 5*1024/2;
	fsz = (int)rnd + fsz;
	fsz /= 4;
	fsz *= 4;    // round to integer size...

	//	fsz = 4;

	sfs_genfilerec(fn, fsz, file);


	if(fd <0) perror("Bad file?: ");
	
	int ret = write(fd, (char *)file, file->head_sz);
	if(ret != file->head_sz) perror("Bad write?: ");

	ret = write(fd, (char *)fp, seeksize(file->sz));
	if(ret != seeksize(file->sz)) perror("Bad write?: ");

	fp += fsz;
	
	eventsz += fsz;
      }
    }

    t = record_time();
    etime += t;
    sent += eventsz;
    
    if(etime - ltime > 10) {
      ltime = etime;
      
      double rate = (sent / (1e6*etime));
      double lrate = ((double)eventsz) / (1e6 * t);
      
      printf("%6.2f   %6.2f\n",lrate, rate);
    }
  } 

  printf("sent = %d\n",(int)sent);
}
