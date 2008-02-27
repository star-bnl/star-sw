#ifndef _DAQR_INDEX_H_
#define _DAQR_INDEX_H_

#include <daqFormats.h>
#include "fs_index.h"

class daqr_index : public fs_index {
 public:
  UINT32 run;
  
  daqr_index();

  int _create();
  //void dump(char *path, daqr_inode *inode);
  // void close();
  // daqr_dir *opendir(char *dirname);
  // void closedir(daqr_dir *dir);
  // daqr_dirent *readdir(daqr_dir *dir);

  //fs_dir *opendir(char *dir);
  // fs_dirent *readdir(fs_dir *dir);
  //fs_dirent *readdirent(char *dir);      // jump directly to the entry of a known file
  //void closedir(fs_dir *dir);

 
  void dump(int fd);

  // stuff...
  //  int cd(char *);
  // void close();
  //void getFullPath(char *fullpath,char *name);
 private:
  // void addnode(bankHeader *head);
  fs_inode *add_inode(fs_inode *parent, char *name, int offset, int sz, bankHeader *bh);
  //  fs_inode *alloc_inode(char *name, int offset, int sz);
  //void free_inode(fs_inode *inode);
  fs_inode *find_child(fs_inode *parent, char *name);

  void build(int fd, fs_inode *root);
  void addNode_r(int fd, fs_inode *node, char *ppath, bankHeader *bh, UINT32 curr_offset);

  UINT32 getChildBank(bankHeader *bh, int i, int &sz);
  void getBankInfo(bankHeader *bh,  UINT32 &pointerOffset,  int &nidx, int &datasize);
  bankHeader *readBank(int fd, UINT32 offset);
  void ls_r(int fd, char *ppath, bankHeader *bh, UINT32 curr_offset);
  // void free_inode(fs_inode *inode);
  void getName(bankHeader *bh, char *name);

  // daqr_inode *find_child(daqr_inode *parent, char *name);

  // int writev(iovec *, fs_iovec *, int n) { return -1; };
  // int write(char *, char *, int) { return -1; };

  // int writeFsHeader() { return -1; };
};
  

#endif
