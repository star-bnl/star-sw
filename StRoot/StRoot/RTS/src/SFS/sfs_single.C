#include <sfs_index.h>
#include <stdio.h>
#include <rtsLog.h>

//extern int debug;

int main(int argc, char *argv[])
{
  //rtsLogOutput(2);
  //rtsLogLevel(DBG);

  if(argc < 2) {
    printf("Need a filename\n");
    exit(0);
  }

 
  sfs_index sfs;
  //debug = 0;
  int ret = sfs.mountSingleDir(argv[1]);
  //int ret = sfs.mountSingleDir(argv[1], 2147451904);
  if(ret < 0) {
    printf("Error mounting %s",argv[1]);
    return -1;
  }

  do {
    printf("Mounting new directory:\n");

    // fs_dirent *ent;
    fs_dir *dir = sfs.opendir("/");
    if(dir) {
      fs_filelist list;
      sfs.mem_ls(&list, 1, dir);
      for(int i=0;i<list.n;i++) {
	printf(":::%s\n",list.filename[i]);
      }

      sfs.closedir(dir);
    }
    else {
      printf("no rootdir?\n");
    }

    ret = sfs.mountNextDir();
    printf("mountnextdir()  ret=%d\n",ret);
  } while(ret > 0);
  

}



