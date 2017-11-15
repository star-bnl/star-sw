#ifndef _FS_INDEX_H_
#define _FS_INDEX_H_

#include <stdlib.h>
//#include <sys/uio.h>

typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;

struct fs_inode {
  fs_inode *parent;
  fs_inode *next;
  fs_inode *prev;
  fs_inode *fchild;
  fs_inode *lchild;
  long long int offset;
  int sz;
  int overhead;
  char *name;               // local name only

  UINT32 swap;              
};

struct fs_dirent {
  char full_name[256];
  char d_name[256];
  int sz;
  long long int offset;
  int has_child;
  int swap;
};

struct fs_dir {
  fs_inode *inode;
  fs_inode *currchild;
  char full_name[256];
};

struct fs_iovec {
  char *filename;
  void *buff;
  int len;
};


#define WRAP_CLOSED 0
#define WRAP_MEM 1
#define WRAP_DISK 2
#define WRAP_SOCKET 3

class wrapfile {
 public:
  int type;      // 0 closed, 1 mem, 2 disk, 3 socket
  
  // If disk / socket
  int fd;
  
  // If mem
  char *wbuff;
  long long int wfpos;
  long long int wsize;
  

 
  wrapfile() { type = 0; wbuff = 0; wfpos = 0; wsize = 0; };
  int openmem(char *wrapbuff, int wrapmsize);
  int opendisk(char *fn, int flags, int perms=0666);
  int openfd(int fd);
  int read(void *buff, int sz);
  int write(void *buff, int sz);


#if  defined(__USE_LARGEFILE64) || defined(__LARGEFILE64_SOURCE_)
  long long int lseek(long long int offset, int whence);
  int fstat(struct stat64 *stat);
#else
  int lseek(int offset, int whence);
  int fstat(struct stat *stat);
#endif

  int close();
};


class fs_filelist {
 public:
  char filename[50][256];
  int n;
  int max;

  fs_filelist() {
    n = 0;
    max = 50;
  }
};

class fs_index {
 public:

  char cwd[256];

  fs_index();
  
  virtual ~fs_index();
  
  // flags and perms are open() flags and perms...
  //
  // For memory mounting several points:
  //    1. no append implemented
  //    2. size for write only mount is max size
  //    3. size for read only mount is file size
  //    4. to find size of mountfile after memory writing use mountsz()
  //
  int mountmem(char *buffer, int sz, int flags);
  int mount(char *filename, int flags, int perms=0666);  
  int mount(int fd);                              // mounts to a socket
  int mount(int ip, int port) ;			// opens and mounts a socket

  void umount();

#if  defined(__USE_LARGEFILE64) || defined(__LARGEFILE64_SOURCE_)
  long long int mountsz();
#else
  int mountsz();
#endif

  static void hexdump(char *buff, int sz);

  int cd(char *fn);
  char *pwd() { return cwd; };

  // directory functions
  fs_dir *opendir(char *dir);
  void closedir(fs_dir *dir);
  fs_dirent *readdir(fs_dir *dir, fs_dirent *storage=NULL);
  fs_dirent *readdirent(char *name, fs_dirent *storage=NULL);

  // meant for users that have memory mapped/memory sfs indexes
  // and want to use the buffers in place to avoid memcpy
  // access the data through the dirent's offset / size
  // 
  inline fs_dirent *opendirent(char *name) { return readdirent(name); };

  int mem_ls(fs_filelist *filelist, int recurse, fs_dir *dir);

  // File operations
  int read(char *fn, char *buff, int sz);
  virtual int write(char *fn, char *buff, int sz) { return -1; };
  virtual int getwritevsz(fs_iovec *fsiovec, int n) { return 0; };
  virtual int writev(fs_iovec *iovec, int n) { return -1; };
  virtual int writev_sticky(fs_iovec *iovec, int n, int *sticky) { return -1; };

  int fileSize(char *fn);
  
  int n_inodes;
  void getFullPath(char *fullname, char *name);
  virtual void dump(int fd)=0;
 
  wrapfile wfile;
 protected:
  int cdchanged;
  int oflags;

  fs_inode *find_child(fs_inode *parent, char *name);
  void free_inode(fs_inode *inode);
#if  defined(__USE_LARGEFILE64) || defined(__LARGEFILE64_SOURCE_)
  fs_inode *alloc_inode(const char *name, long long int off, int sz, int overhead);
#else
  fs_inode *alloc_inode(const char *name, int off, int sz, int overhead);
#endif

  virtual int _create()=0;    // create index for reading...
  int index_created;

  fs_inode *root;
  fs_inode *cw_inode;

  virtual int writeFsHeader() { return -1; };

  int initmount();

  char *writevbuff;
  int writevbuffsz;

 private:
  fs_dirent _readdirent_static_;

 protected:
  char *_strtok_static_;
};
  
  

#endif
