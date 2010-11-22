#include <sfs_index.h>
#include <stdio.h>
#include <rtsLog.h>

//extern int debug;

int main(int argc, char *argv[])
{
  rtsLogOutput(2);
  // rtsLogLevel(DBG);

  if(argc < 2) {
    printf("Need a filename and offset\n");
    exit(0);
  }

 
 
  sfs_index sfs;
  //debug = 0;

  long long int off = 0;
  if(argc>2) off = atoi(argv[2]);

  int sz = 0;
  int ret = sfs.mountSingleDir(argv[1], off);
  //int ret = sfs.mountSingleDir(argv[1], 2147451904);
  if(ret < 0) {
    printf("Error mounting %s",argv[1]);
    return -1;
  }

  do {
    int last_sz = sz;
    long long int last_off = off;
    sz= sfs.singleDirSize;
    off = sfs.singleDirOffset;


    LOG("JEFF", "Mounting new directory: (%s) offset=%lld (calc: %lld) size=%d",
	   sfs.singleDirIttr->fullpath,
	   off, 
	   (last_off + last_sz) - off,
	   sz); 
    
    // fs_dirent *ent;
    fs_dir *dir = sfs.opendir("/");
    if(dir) {
      fs_filelist list;
      sfs.mem_ls(&list, 1, dir);
      for(int i=0;i<list.n;i++) {
	if(strstr(list.filename[i], "TRGID") != NULL) {
	  int sz = sfs.fileSize(list.filename[i]);
	  LOG("JEFF", "%s: %d bytes",list.filename[i],sz);
	  char *buff = (char *)malloc(sz);
	  sfs.read(list.filename[i], buff, sz);
	  sfs.hexdump(buff, sz);
	  free(buff);
	}
      }

     sfs.closedir(dir);
    }
    else {
      printf("no rootdir?\n");
    }

    ret = sfs.mountNextDir();
  } while(ret > 0);
  

}



