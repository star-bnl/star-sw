#include <sfs_index.h>
#include <stdio.h>

extern int debug;

int main(int argc, char *argv[])
{
  if(argc < 2) {
    printf("Need a filename\n");
    exit(0);
  }

 
  sfs_index sfs;
  debug = 0;

  int ret = sfs.mountSingleDir(argv[1]);
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

// 	if(strcmp(list.filename[i], "/#1439/tpx/sec16/rb06/adc") == 0) {
// 	  fs_dirent *de = sfs.readdirent("/#1439/tpx/sec16/rb06/adc");
// 	  int *mem = (int *)malloc(de->sz);
// 	  sfs.read("/#1439/tpx/sec16/rb06/adc",(char *)mem,de->sz);
// 	  for(int ii=0;ii<100;ii++) {
// 	    printf("blah[%d] = 0x%x\n",ii,mem[ii]);
// 	  }

// 	  free(mem);
// 	  return 0;
// 	}
      }
    }
    else {
      printf("no rootdir?\n");
    }

    ret = sfs.mountNextDir();
    printf("mountnextdir()  ret=%d\n",ret);
  } while(ret > 0);
  

}



