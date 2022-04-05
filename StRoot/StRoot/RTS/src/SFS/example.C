#include <stdio.h>
#include "sfs_index.h"

int main(int argc, char *argv[])
{
  if(argc>=1) {
    printf("Mounting %s\n",argv[1]);
  }
  else {
    return -1;
  }

  sfs_index sfs;

  int pos=0;

  for(;;) {
    int sz = sfs.getSingleDirSize(argv[1],pos);
    if(sz <= 0) return -1;

    printf("next pos=%d sz=%d\n",pos,sz);
    sfs.mountSingleDir(argv[1],pos);

    fs_dir *dir = sfs.opendir("/");
    if(dir) {
      fs_filelist list;
      sfs.mem_ls(&list, 1, dir);
      for(int i=0;i<list.n;i++) {
	printf(":::%s\n",list.filename[i]);
      }
    }

    pos += sz;

    sfs.umount();
  }
}
