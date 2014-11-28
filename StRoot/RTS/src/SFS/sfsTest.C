#include <stdio.h>
#include <unistd.h>
#include "sfs_index.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>


int buffer[1000];

int main(int argc, char *argv[])
{
  char name[20];
  strcpy(name, "123456");

  if(argc > 1) {
    strcpy(name,argv[1]);
  }

  sfs_index idx;


  memset((char *)buffer, 0xffffffff, sizeof(buffer));

  printf("Before: \n");
  for(int i=0;i<20;i++) {
    printf("%d -- 0x%08x\n",i,buffer[i]);
  }


  idx.putfileheader((char *)buffer, name, 0,0);

  printf("sizeof(SFS_File)=%d\n",sizeof(SFS_File));

  printf("After: \n");
  for(int i=0;i<20;i++) {
    printf("%d -- 0x%08x\n",i,buffer[i]);
  }
	 
}


// char filebuff[100000];


// char *b1 = "This is file 1\n\ntoasty in here.";
// char *b2 = "File 2 is shorter xx";

// char *n1 = "dumbodropblahblahblah";
// char *n2 = "jeffdrop";
// char *b3 = "hello";
// char *b4 = "hello there dumbo";

// int main(int argc, char *argv[])
// {
//   int ret;
//   sfs_index idx;

//   printf("sizeof(SFS_File) = %d\n",sizeof(SFS_File) );

//   ret = idx.mount("write2.sfs", O_WRONLY | O_CREAT | O_TRUNC);

//   //ret = idx.mountmem(filebuff, 100000, O_WRONLY | O_CREAT);

//   if(ret < 0) {
//     printf("error mounting write.sfs\n");
//     return 0;
//   }

//   printf("mounted\n");
  
//   idx.write("/this/long/filename/a",b1,strlen(b1));
  
//   printf("write1\n");
	 
//   idx.cd("/jeff");

//   printf("cd\n");

//   idx.write("bbbbb",b2,strlen(b2));

//   idx.write("injeff",b2,strlen(b2));

//   printf("write2\n");

//   idx.write("/500/a",b2,strlen(b2));
//   idx.write("/50/a",b2,strlen(b2));
//   idx.write("/5/a",b2,strlen(b2));
//   idx.write("/100000/a",b2,strlen(b2));
//   idx.write("/1/a",b2,strlen(b2));
//   idx.write("/2/a",b2,strlen(b2));
//   idx.write("/3/a",b2,strlen(b2));
//   idx.write("/4/a",b2,strlen(b2));




//   fs_iovec myfiles[2] = { { n1, b3, strlen(b3) },
// 			  { n2, b4, strlen(b4) } };

//   printf("blah\n");
//   int n = idx.getwritevsz(myfiles, 2);
//   printf("n=%d\n",n);
//   ret = idx.writev(myfiles, 2);
//   printf("writevsize = %d (%d)\n",n,ret);

  
//   int sz = idx.mountsz();
  
//   if(sz == -1) exit(0);

//   idx.umount();

//   idx.mount("write2.sfs", O_RDONLY);
//   // ret = idx.mountmem(filebuff, sz, O_RDONLY);
//   // printf("mount rdonly = %d\n",ret);

//   char buff[1000];
//   memset(buff, 0, sizeof(buff));
//   ret = idx.fileSize("/this/long/filename/a");
//   printf("file a size = %d\n",ret);

//   ret = idx.read("/this/long/filename/a",buff, 1000);
//   printf("file a: (%d):\n%s\n\n",ret,buff);

//   memset(buff, 0, sizeof(buff));
//   ret = idx.read("/jeff/b",buff, 1000);
//   printf("file b: (%d):\n%s\n\n",ret,buff);

//   memset(buff, 0, sizeof(buff));
//   buff[0] = '\0';
//   ret = idx.read("/jeff/dumbodrop",buff, 1000);
//   printf("file invalid: (%d):\n%s\n\n",ret,buff);

//   idx.umount();

//   //int fd = open("xxx.sfs", O_WRONLY | O_CREAT,0666);
//   // write(fd, filebuff, sz);
//   //close(fd);
// }
