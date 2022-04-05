#ifndef _SFS_BASE_H_
#define _SFS_BASE_H_

#include <string.h>

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

/* Tonko: I need this convenience function */
inline int sfs_calcfileheader(char *fn)
{
	int n = sizeof(SFS_File) - 4 ;
	n += strlen(fn) + 1 ;
	n = (n+3)&0xfffffffc;

	return n ;
}

inline int sfs_putfileheader(char *ptr, char *fn, int filesz, int flags)
{
  SFS_File *file = (SFS_File *)ptr;

  int n = sizeof(SFS_File) - 4;
  n += strlen(fn) + 1;
  n = (n+3)&0xfffffffc;

  memcpy(file->type, "FILE", 4);
  file->byte_order = 0x04030201;
  file->sz = filesz;
  file->head_sz = n;
  file->attr = flags;
  file->reserved = 0;

  // u_int nm = (u_int)file->name;
  // u_int fl = (u_int)file;
  // u_int ct = (u_int)(((u_int)file->name) + n - 4);
  // printf("offset of file: %d.   offset to write to: %d\n",nm-fl, n-4);

  memcpy(((char *)file) + n - 4, "\0\0\0\0", 4);
  strcpy(file->name, fn);
  return n;
}

#endif
