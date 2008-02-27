#ifndef _SFS_INDEX_H_
#define _SFS_INDEX_H_

#include <string.h>
#include "fs_index.h"
//#include <sys/uio.h>

typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;

// File Format
//
// ----------------
// | VolumeSpec
// | HEAD  
// | FILE / (data)  
// | FILE / (data)
// | FILE / (data) 
// | .... 
// | TAIL 
// ----------------
//
// FILE Attributes:
//
// SFS_ATTR_INVALID  : deleted file
//
//
// Filenames:
//          
//   /xxx      absolute path
//   xxx       relative to directory part of previous entry
//   0 length  used to reset "previous entry"
//   xxx/      directory
//   xxx       file
//   xxx/xxx/yyy   xxx/xxx/ is "directory part" 


#define SFS_ATTR_INVALID 0x01
#define SFS_ATTR_NOCD    0x08
#define SFS_ATTR_CD      0x00
#define SFS_ATTR_STICKY_CD 0x02
#define SFS_ATTR_POPSTICKY 0x04    // path relative to last "sticky_cd"

struct SFS_VolumeSpec {
  char fs[12];      // "SFS V00.01"
};

struct SFS_Header {
  char type[4];     // "HEAD"
  UINT32 byte_order;
  UINT32 time;
};

struct SFS_File {
  char type[4];     // "FILE"
  UINT32 byte_order;
  UINT32 sz;        // any number, but file will be padded to be x4
  UINT8 head_sz;    // must be x4
  UINT8 attr;
  UINT16 reserved;
  char name[4];     // get rid of padding confusions... by alligning
};

class SFS_ittr {
 public:
  union {
    SFS_File entry;
    char entryBuff[256];
  };
  char stickypath[256];
  char ppath[256];
  char fullpath[256];

  int fileoffset;  // from start of file.
  int filepos;  // 0 start of header, 1 end of header, 2 end of file record, -1 at end of file system

  SFS_ittr() {
    fileoffset = 0;
  };

  SFS_ittr(int offset) {
    fileoffset = offset;
  };

  int get(wrapfile *wrap);
  //  int get(char *buff, int sz);

  int next();

  wrapfile *wfile;
};


inline int seeksize(int filesize)
{
  return (filesize + 3) & 0xfffffffc;
}

inline int get_sfsFileSize(SFS_File *file)
{
  return seeksize(strlen(file->name)+1) + sizeof(SFS_File) - 4;
}

// sfs_lib prototypes
void write_env(char *var, char *value);
char *read_env(char *var);
void getFullPath(char *out, char *in);
int seeksize(int filesize);
void striptofirst(char *str);
char *striptofile(char *str);
void stripfile(char *str);
char *striptodir(char *str);

char *SFS_getpayload(char *buff);  // takes a pointer to a FILE and gets the payload...


class sfs_index : public fs_index {
 public:

  int singleDirMount;
  SFS_ittr *singleDirIttr;
  int mountSingleDir(char *fn, int offset=0);
  int mountNextDir();

  sfs_index();
  int _create();
  void dump(int) { dump("/",root); };
 
  int getwritevsz(fs_iovec *fsiovec, int n);
  int writev(fs_iovec *fsiovec, int n);
  int writev_sticky(fs_iovec *iovec, int n, char *sticky);
  int write(char *fn, char *buff, int sz);
  
  int getfileheadersz(char *fn);
  int putfileheader(char *ptr, char *fn, int filesz, int flags=SFS_ATTR_NOCD);

 private:

  void addnode(SFS_ittr *ittr);
 
  fs_inode *add_inode(fs_inode *parent, char *name, int offset, int sz);
  fs_inode *add_inode_from(fs_inode *neighbor, char *name, int offset, int sz);
  fs_inode *find_last_lesser_child(fs_inode *parent, char *name, int &first, int &eq);
  fs_inode *find_last_lesser_neighbor(fs_inode *neighbor, char *name, int &eq);
  void dump(char *path, fs_inode *inode);

  int writeFsHeader();
};


#endif
