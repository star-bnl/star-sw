#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include "sfs_index.h"
#include <sys/uio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <rtsLog.h>

#include <rts.h>
//#include <byteswap.h>
//#define swap16(x) bswap_16(x)
//#define swap32(x) bswap_32(x)

//int debug = 0;

/********************************/
/*  Utilities                   */
/********************************/

#define MAX_SEND_IOVECS 100

int mstrcmp(char *s1, char *s2)
{
  if((s1[0] == '#') && (s2[0] == '#')) {
    int x1;
    int x2;

    x1 = atoi(&s1[1]);
    x2 = atoi(&s2[1]);
    if(x1 > x2) return 1;
    if(x1 < x2) return -1;
    return 0;
  }
  return strcmp(s1,s2);
}


//    /dir_1/dir_2/file  -->  /
//    file               -->  file
//    dir_1/file         -->  dir_1/
//    dir_1/             -->  dir_1/
//    ""                 -->  ""
//
void striptofirst(char *str) 
{
  while(*str != '\0') {
    if(*str == '/') *(str+1) = '\0';
    str++;
  }
}

// /dir_1/dir_2  --> /dir_1
// dir_1/dir_2/dir_3 --> /dir_1/dir_2

void striptofirstdir(char *str)
{
  int scount = 0;
  while(*str != '\0') {
    if(*str == '/') scount++;
    if(scount >= 2) {
      *str = '\0';
    }
    else str++;
  }
}

//    /dir_1/dir_2/file  --> file
//    file               --> file
//    dir_1/file         --> file
//    dir_1/             --> ""
//    ""                 --> ""
//
char *striptofile(char *str)
{
  char *ostr = str;

  while(*str != '\0') {
    if(*str == '/') ostr = str+1;
    str++;
  }

  return ostr;
}

//    /dir_1/dir_2/file  --> /dir_1/dir_2/
//    file               --> /
//    dir_1/file         --> dir_1
//    dir_1/             --> dir_1/
//    ""                 --> /
//
void stripfile(char *str)
{
  char *ostr = str;
  char *lslash = NULL;

  while(*str != '\0') {
    if(*str == '/') lslash = str;
    str++;
  }

  if(lslash) *(lslash+1) = '\0';
  else strcpy(ostr, "/");
}

int SFS_ittr::get(wrapfile *wrap)
{
  //  printf("ittr get file 0x%x %d\n",wrapbuff, fd);
  skipped_bytes = 0;

  char buff[12];
  memset(buff, 0, sizeof(buff));

  wfile = wrap;

  stickypath[0] = '\0';
  ppath[0] = '\0';
  fullpath[0]='\0';
  
  //printf("fileoffset %d\n",fileoffset);
  filepos = 0;
  strcpy(ppath,"/");

  return 0;
}


void SFS_ittr::swapEntry()
{
  //  int swap;
  if(entry.byte_order == 0x04030201) return;

  entry.byte_order = swap32(entry.byte_order);

  entry.byte_order = swap32(entry.byte_order);
  entry.sz = swap32(entry.sz);
  entry.reserved = swap16(entry.reserved);
}


// Return -1 on error
// 0 on ok.
int SFS_ittr::next() 
{
  //if(legacy) return legacy_next();
  skipped_bytes = 0;

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  LOG(DBG, "Calling next:  fileoffset=%lld filepos=%d",fileoffset,filepos);
  long long int ret;
#else
  LOG(DBG, "Calling next:  fileoffset=%d filepos=%d",fileoffset,filepos);
  int ret;
#endif

  //printf("ittr next\n");

  LOG(DBG, "filepos = %d, entry.sz=%d entry.head_sz=%d",filepos,entry.sz,entry.head_sz);
  if(filepos == 1) {  // need to jump to next...
    ret = wfile->lseek(seeksize(entry.sz), SEEK_CUR);
    if(ret < 0) {
      LOG(ERR,"Error seeking: %s\n",strerror(errno));
      return -1;
    }
    filepos = 2;

    LOG(DBG, "fileoffset=%d --> + %d + %d",
	entry.sz, entry.head_sz);
    fileoffset += seeksize(entry.sz) + entry.head_sz;
  }

  if(filepos == 2) {  
    // update ppath...
    LOG(DBG,"---DIR:  name=%s entry.attr = 0x%x headsz=%d",entry.name, entry.attr,entry.head_sz);
   

    if(entry.attr & SFS_ATTR_NOCD) {
      // ppath unchanged....
      //printf("no change %s\n",entry.name);
    }
    else {   // update ppath regularly...
      if(entry.name[0] == '/') {
	//printf("strcpy %s\n",entry.name);
	strcpy(ppath, entry.name);
      }
      else {
	//printf("ccat %s %s\n",ppath,entry.name);
	strcat(ppath, entry.name);
      }
      stripfile(ppath);
    }

    if(entry.attr & SFS_ATTR_STICKY_CD) {
      strcpy(stickypath,ppath);
      // printf("(set) ppath = %s,  stickypath %s\n",ppath,stickypath);
    }
      

    //printf("----------ppath is now %s  (%s)\n",ppath,entry.name);

    filepos = 0;
  }


  // expect a FILE record, LRHD or DATAP...  jump the rest...
  for(;;) {
    char buff[16];
    int ret = wfile->read(buff, 8);
    
    if(ret == 0) {      // done...
      filepos = -1;
      return 0;  
    }

    if(ret != 8) {
      LOG(ERR, "Error reading next file record...");
      return -1;
    }

    buff[5] = 0;

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    wfile->lseek(-((long long int)8), SEEK_CUR);
    long long int xxx = wfile->lseek(0,SEEK_CUR);
    LOG(DBG, "fileoffset=%lld xxx=%lld buff=%s",fileoffset,xxx,buff);
#else
    wfile->lseek(-8, SEEK_CUR);
    int xxx = wfile->lseek(0,SEEK_CUR);
    LOG(DBG, "fileoffset=%d xxx=%d buff=%s",fileoffset,xxx,buff);
#endif

    if(memcmp(buff, "SFS V", 5) == 0) {
      //if(debug) LOG(DBG,"Found SFS version");
      wfile->lseek(12, SEEK_CUR);
      fileoffset += 12;
      skipped_bytes += 12;
      continue;
    }

    if(memcmp(buff, "LRHD", 4) == 0) {
      //if(debug) LOG(DBG,"Found LRHD");

      if(nextLRHD() >= 0) {  // good data LRHD... got entry...
	LOG(DBG, "lrhd entry.sz = %d",entry.sz);
	return 0;
      }
      
      //LOG(DBG, "Seek: %d",wfile->lseek(0,SEEK_CUR));

      wfile->lseek(60, SEEK_CUR);

      //LOG(DBG, "Seek2: %d",wfile->lseek(0,SEEK_CUR));

      fileoffset += 60;
      skipped_bytes += 60;
      LOG(DBG, "Not a DATA LRHD  fileoffset=%d skipped_bytes=%d seek=%d",
	  fileoffset,skipped_bytes,wfile->lseek(0,SEEK_CUR));

      continue;
    }

    if(memcmp(buff, "DATAP", 5) == 0) {
      //if(debug) LOG(DBG,"Found DATAP");

      LOG(DBG, "Before datap: file=%d filepos=%d offset=%d entrysz=%d",
	  wfile->lseek(0,SEEK_CUR), filepos,fileoffset,entry.sz);

      if(nextDatap() >= 0) {  // DATAP and no following FILE.  got entry
	LOG(DBG, "After datap: file=%d filepos=%d offset=%d entrysz=%d",
	  wfile->lseek(0,SEEK_CUR), filepos,fileoffset,entry.sz);
	return 0;
      }

      wfile->lseek(204, SEEK_CUR);
      fileoffset += 204;
      skipped_bytes += 204;
      continue;
    }

    if(memcmp(buff, "HEAD", 4) == 0) {
      //if(debug) LOG(DBG,"Found HEAD");
      wfile->lseek(12, SEEK_CUR);
      fileoffset += 12;
      skipped_bytes += 12;
      continue;
    }

    if(memcmp(buff, "FILE", 4) == 0) {  // finally!
      break;
    }

    else {
      
      if(wfile->type == WRAP_MEM) {   // ugly hack... done!
	filepos = -1;
	return 0;
      }

      char ttmp[16] ;
      memcpy(ttmp,buff,16) ;
      ttmp[15] = 0 ;
      LOG(ERR,"Error: %s is not a valid specifier\n",ttmp);
      filepos = -1;
      return 0;
    }
  }
  // Read File Desc
  // clear entrybuff
  memset(entryBuff, 0, sizeof(entryBuff));
  
  ret = wfile->read(entryBuff,16);
  if(ret == 0) {
    filepos = -1;
    return 0;  
  }

  if(ret != 16) {
    LOG(ERR,"Error reading file entry: %s (%d)\n",strerror(errno),ret);
    return -1;
  }
  
  //printf("1- compare file: %s\n",entry.type);

  if(memcmp(entry.type,"FILE",4) != 0) {
    LOG(ERR,"Error reading file entry: {%c%c%c%c} Not a file record...\n",
	   entry.type[0],entry.type[1],entry.type[2],entry.type[3]);
    return -1;
  }

  swapEntry();

  // printf("reading... bytes 0x%x,  head_sz %d,  sz = %d\n",
  // entry.byte_order, entry.head_sz, entry.sz);

  ret = wfile->read(entryBuff + 16, entry.head_sz - 16);
  if(ret != entry.head_sz - 16) {
    LOG(ERR,"Error reading file entry: size mismatch ret=%d sz=%d\n",ret,entry.head_sz);

    return -1;
  }


  LOG(DBG,"---DIR:  name=%s entry.attr = 0x%x\n",entry.name, entry.attr);


  // hacks for 2007
  if(strstr(entry.name, "legacy")) {
    entry.attr |= SFS_ATTR_POPSTICKY;
  }

  if(strstr(entry.name, "pad")) {
    entry.attr |= SFS_ATTR_POPSTICKY;
  }

  if(entry.attr & SFS_ATTR_POPSTICKY) {
    // doesn't reset stickypath!
    //printf("---DIR:   (pop) ppath %s stickypath %s\n",ppath,stickypath);
    if(stickypath[0] != '\0') 
      strcpy(ppath, stickypath);
  }

  if(entry.name[0] == '/') {
    strcpy(fullpath, entry.name);
  }
  else {
    strcpy(fullpath, ppath);
    strcat(fullpath, entry.name);
  }

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  LOG(DBG,"fullpath %s, entry.name: %s, fileoffset %lld/%d, sz %d  head_sz %d",
      fullpath, entry.name, fileoffset, filepos, entry.sz, entry.head_sz);
#else
  LOG(DBG,"fullpath %s, entry.name: %s, fileoffset %d/%d, sz %d  head_sz %d",
      fullpath, entry.name, fileoffset, filepos, entry.sz, entry.head_sz);
#endif

  filepos = 1;
  return 0;
}

char *SFS_getpayload(char *buff)
{
  SFS_File *f = (SFS_File *)buff;
  return (buff + f->head_sz);
}

int sfs_index::getInodeSize(fs_inode *inode, SfsDirsize *sizes)
{
  //printf("name: %s   offset: %lld   overhead: %d sz: %d (sz: %lld)\n",
  //	 inode->name, inode->offset, inode->overhead, inode->sz, sizes->dataSize);
  
  sizes->dataSize += inode->sz;
  sizes->size += inode->sz + inode->overhead;

  if(inode->fchild) getInodeSize(inode->fchild, sizes);
  if(inode->next) getInodeSize(inode->next, sizes);
  return 0;
}

int sfs_index::getDirSize(char *dir, SfsDirsize *sizes)
{
  memset(sizes, 0, sizeof(SfsDirsize));
  fs_dir *d = opendir(dir);
  if(!d) return -1;
 
  sizes->dataSize += d->inode->sz;
  sizes->size += d->inode->sz + d->inode->overhead;

  int dirsz = 0;
  if(d->inode->fchild) {
      dirsz = getInodeSize(d->inode->fchild,sizes);
  }

  closedir(d);

  return dirsz;
}

int sfs_index::writeFsHeader()
{
    static char *volumeSpec = (char *)"SFS V00.01\0\0\0";
    int ret;
    int sz;
    char *bb;
    SFS_Header head;
  
    bb = volumeSpec;
    sz = 12;
    while(sz) {
	ret = wfile.write(volumeSpec, sz);
    
	if(ret < 0) {
	    return -1;
	}
	sz -= ret;
	bb += ret;
    }
  
    memcpy(head.type, "HEAD", 4);
    head.byte_order = 0x04030201;
    head.time = time(NULL);
  
    bb = (char *)&head;
    sz = sizeof(head);
    while(sz) {
	ret = wfile.write(bb, sz);
    
	if(ret < 0) {
	    return -1;
	}

	sz -= ret;
	bb += ret;
    }
  
    return 0;
}

int sfs_index::write(char *fn, char *buff, int size)
{
    int ret;
    char *bb;
    int sz;
    char path[256];
    int attr = SFS_ATTR_NOCD;

    char b[256];
    SFS_File *file = (SFS_File *)b;

    if(cdchanged) {
	// (a) if filename is absolute filename do nothing, leave cdchanged as is
	// (b) if filename has no subdirs, then use fullpath and unset NOCD
	// (c) if filename has subdirs, then write a directory entry separately
    
	//printf("cdchanged\n");

	if(fn[0] == '/') {
	    strcpy(path, fn);
	}
	else {
	    // any non-trailing slash?
	    cdchanged = 0;        // now we do a cd either way!

	    int subdirs = 0;
	    int len = strlen(fn);
	    for(int i=0;i<len-1;i++) {
		if(fn[i] == '/') {
		    subdirs = 1;
		    break;
		}
	    }

	    if(!subdirs) {
		getFullPath(path, fn);
		attr = 0;
	    }
      
	    if(subdirs) {
		memcpy(file->type, "FILE", 4);
		file->byte_order = 0x04030201;
		file->sz = 0;
		file->head_sz = seeksize(strlen(cwd)+1) + sizeof(SFS_File) - 4;
		file->attr = 0;
		strcpy(file->name, cwd);
	
		bb = (char *)file;
		sz = file->head_sz;
	
		while(sz) {
		    ret = wfile.write(bb, sz);
		    if(ret < 0) {
			return -1;
		    }
	  
		    sz -= ret;
		    bb += ret;
		}
	
		strcpy(path, fn);
	    }
	}
    }
    else {
	strcpy(path, fn);
    }

    //printf("cd changed = %d  path = %s\n",cdchanged,path);
  

  
    memcpy(file->type, "FILE", 4);
    file->byte_order = 0x04030201;
    file->sz = size;
    file->head_sz = seeksize(strlen(path)+1) + sizeof(SFS_File) - 4;
    file->attr = attr;

    strcpy(file->name, path);
  
    bb = (char *)file;
    sz = file->head_sz;
  
    while(sz) {
	ret = wfile.write(bb, sz);
	if(ret < 0) {
	    return -1;
	}

	sz -= ret;
	bb += ret;
    }

    sz = size;
    bb = buff;

    while(sz) {
	ret = wfile.write(bb, sz);
	if(ret < 0) {
	    return -1;
	}

	sz -= ret;
	bb += ret;
    }

    // pad to multiple of 4 bytes.
    char *zero = (char *)"\0\0\0\0";
    sz = seeksize(size) - size;
    bb = zero;
  
    while(sz) {
	ret = wfile.write(bb, sz); 
	if(ret < 0) {
	    return -1;
	}
    
	sz -= ret;
	bb += ret;
    }
  
    return size;
}

int sfs_index::getwritevsz(fs_iovec *fsiovec, int n)
{
  int sz = 0;
  for(int i=0;i<n;i++) {
    if(fsiovec[i].filename) {
      sz += seeksize(strlen(fsiovec[i].filename)+1);
      sz += sizeof(SFS_File) - 4;
    }

    sz += seeksize(fsiovec[i].len);
  }
  return sz;
}

int sfs_index::writev(fs_iovec *fsiovec, int n)
{
  return writev_sticky(fsiovec, n, NULL);
}


// Non-interruptable writev
int sfs_index::writev_call_retry(int fd, iovec *iovec, int vec)
{
  if(vec > MAX_SEND_IOVECS) {
    LOG(CRIT, "writev with too %d iovecs... max=%d",vec,MAX_SEND_IOVECS);
    return -1;
  }

  int len=0;
  for(int i=0;i<vec;i++) {
    len += iovec[i].iov_len;
  }

  int ret = 0;
  int retries = 0;
  if((ret == 0) && (retries<5)) {   // retry 1 time!
      ret = ::writev(fd, iovec, vec);
      retries++;
  }

  if((ret==len) && (retries > 1)) {
      LOG(U_JEFF, "successful writev took %d tries!");
  }

  if(ret != len) {
      for(int i=0;i<vec;i++) {
	  LOG(NOTE, "socket error (%d)(%s) vec[%d]:base=%p len=%d",
	      ret, strerror(errno), i, iovec[i].iov_base, iovec[i].iov_len);
      }
      LOG(ERR, "socket error: ret=%d of %d in %d tries  (%s)", ret, len, retries, strerror(errno));
      return -1;
  }

  return ret;
}


// Header is an optional, non-fs file header
// of course in stardaq, it is the iccp2k ethernet header...
// this is now threadsafe!


int sfs_index::writev_sticky(fs_iovec *fsiovec, int n, int *sticky)
{
    iovec iovec[MAX_SEND_IOVECS];
    char _buff[(sizeof(SFS_File) + 40)*50];
    char *b = _buff;

    int i;
    int vec=0;

    if(n > MAX_SEND_IOVECS) {
	LOG(CRIT, "sending %d iovecs > than max=%d",n,MAX_SEND_IOVECS);
    }

    //   if(!writevbuff) {
    //     writevbuff = (char *)malloc(64*100);
    //   }

    //   char *b = writevbuff;

    for(i=0;i<n;i++) {   // each file...

	LOG(DBG, "i=%d\n",i);

	if(fsiovec[i].filename) {
	    SFS_File *file = (SFS_File *)b;
      
	    memcpy(file->type, "FILE", 4);
	    file->byte_order = 0x04030201;
	    file->sz = fsiovec[i].len;
	    file->head_sz = seeksize(strlen(fsiovec[i].filename)+1) + sizeof(SFS_File) - 4;
	    file->attr = SFS_ATTR_NOCD;
	    if(sticky) {
		if(sticky[i]) {
		    file->attr |= SFS_ATTR_POPSTICKY;
		}
		if(sticky[i] < 0) {
		    LOG(DBG, "%d %d",sticky[i], file->sz);
		    file->sz = -(sticky[i]);
		    LOG(DBG, "Set file->sz=%d",file->sz);
		}
	    }
	    strcpy(file->name, fsiovec[i].filename);
      

	    iovec[vec].iov_base = b;
	    iovec[vec].iov_len = file->head_sz;
	    LOG(DBG, "fn:   iovec[%d] name=%s 0x%x: base=0x%x len=%d",
		vec,
		fsiovec[i].filename,
		fsiovec[i].filename,
		iovec[vec].iov_base,
	  	iovec[vec].iov_len);


	    vec++;
   
	    b += file->head_sz;
	}
     
	iovec[vec].iov_base = fsiovec[i].buff;
	iovec[vec].iov_len = seeksize(fsiovec[i].len);

	LOG(DBG, "iovec[%d] name=%s 0x%x: base=0x%x len=%d",
	    vec,
	    fsiovec[vec].filename,
	    fsiovec[vec].filename,
	    iovec[vec].iov_base,
	    iovec[vec].iov_len);

	vec++;

 
    }

    // we need this check when writing over ethernet!
    int all_size = 0 ;
    for(int i=0;i<vec;i++) {
	all_size += iovec[i].iov_len ;
    }

    // This can not be interrupted, except by hard error!
    int ret = writev_call_retry(wfile.fd, iovec, vec);

    return ret;
}

sfs_index::sfs_index() : fs_index() 
{
  singleDirMount = 0;
  singleDirIttr = NULL;
  root = NULL;
  cw_inode = NULL;

  return;
}

#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
int sfs_index::mountSingleDirMem(char *buffer, int size, long long int offset)
#else
int sfs_index::mountSingleDirMem(char *buffer, int size, int offset)
#endif
{
  wfile.close();   // just in case
  oflags = O_RDONLY;
  wfile.openmem(buffer, size);

  singleDirOffset = offset;
  singleDirSize = size;
  return mountSingleDir();
}

// returns -1 on error
// returns 0 on eof
// returns 1 on valid dir
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
int sfs_index::mountSingleDir(char *fn, long long int offset)
#else
int sfs_index::mountSingleDir(char *fn, int offset)
#endif
{
  LOG(DBG,"the spec is: " __DATE__ ":" __TIME__);

  // open file...
  umount();  // just in case!
  wfile.close();   // just in case!
  wfile.opendisk(fn, O_RDONLY);
  if(wfile.fd < 0) {
    LOG(ERR, "Bad error opening %s mounting single directory (%s)",fn,strerror(errno));
    return wfile.fd;
  }
  wfile.lseek(offset, SEEK_SET);

  int ret = mountSingleDir();
  //wfile.close();  
  return ret;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
int sfs_index::getSingleDirSize(char *fn, long long int offset)
#else
int sfs_index::getSingleDirSize(char *fn, int offset)
#endif
{
  char topdir[40];
  int topdirlen=0;
  topdir[0] = '\0';

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  LOG(DBG, "singledirsize file=%s, offset=%lld",fn,offset);
#else 
  LOG(DBG, "singledirsize file=%s, offset=%d",fn,offset);
#endif

  wfile.close();
  wfile.opendisk(fn, O_RDONLY);
  if(wfile.fd < 0) return wfile.fd;
  wfile.lseek(offset, SEEK_SET);

  int sz = 0;

  SFS_ittr *ittr = new SFS_ittr(offset);
  if(ittr->get(&wfile) < 0) {
    delete ittr;
    wfile.lseek(offset,SEEK_SET);
    return sz;
  }

  sz += ittr->skipped_bytes;

  LOG(DBG, "partial sz=%d, skipped=%d",sz,ittr->skipped_bytes);

  while(ittr->next() >= 0) {

    if(ittr->filepos == -1) break;   // EOF

    // set topdir, if not yet set...
    if(topdir[0] == '\0') {
      strncpy(topdir,ittr->fullpath,40);
      for(int i=0;i<40;i++) {
	if(topdir[i] == '\0') break;
	if((i>0) && (topdir[i] == '/')) {
	  topdir[i] = '\0';
	  topdirlen = strlen(topdir);
	  break;
	}
      }
    }

    LOG(DBG, "Topdir=%s sz(before)=%d",topdir,sz);

    if(memcmp(topdir, ittr->fullpath, topdirlen) != 0) {
      break;
    }

    sz += ittr->skipped_bytes;
    sz += ittr->entry.head_sz;
    int esz = (ittr->entry.sz + 3) / 4;
    esz *= 4;
    sz += esz;
  }

  delete ittr;
  wfile.close();

  LOG(DBG, "returning %d",sz);
  return sz;
}

int sfs_index::mountSingleDir()   // mounts from current position of wfile...
{
    if(singleDirIttr) delete singleDirIttr;
  
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    long long int offset = wfile.lseek(0,SEEK_CUR);
    LOG(DBG, "mountSingleDir()   offset=%lld 0x%x", offset, wfile.wbuff);
    if(!wfile.wbuff) {   // if a memory mount, already done...
	singleDirOffset = offset;
	nextSingleDirOffset = offset;
    }
#else
    int offset = wfile.lseek(0,SEEK_CUR);
    if(!wfile.wbuff) {
	singleDirOffset = offset;
	nextSingleDirOffset = offset;
    }
#endif

    singleDirMount = 1;
    singleDirIttr = new SFS_ittr(offset);
 
    if(singleDirIttr->get(&wfile) < 0) {
	delete singleDirIttr;
	singleDirIttr = NULL;
	singleDirMount = 0;
    
	root = alloc_inode("",0,0,0);
	strcpy(cwd, "/");
	index_created = 1;
	return -1;
    }

    if(singleDirIttr->next() < 0) {

	root = alloc_inode("",0,0,0);
	strcpy(cwd, "/");
	index_created = 1;
    
	LOG(NOTE, "Couldn't get any SFS dirs...");
	return -1;
    }

    return _mountNextDir();
}

// returns -1 on error
// returns 0 on eof (no directory...)
// returns 1 on valid dir

// entry point for user code
int sfs_index::mountNextDir()
{
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  singleDirOffset = nextSingleDirOffset;
  //long long int offset = wfile.lseek(singleDirOffset,SEEK_SET);
  wfile.lseek(singleDirOffset,SEEK_SET);
#else
  //int offset = wfile.lseek(0,SEEK_CUR);
  wfile.lseek(0,SEEK_CUR);
  //singleDirOffset = nextSingleDirOffset;
#endif

  return(_mountNextDir());
}

// if called from sfs_index, already have offsets set...
int sfs_index::_mountNextDir()
{
  int files_added=0;

  if(index_created) free_inode(root);
  index_created = 0;

  root = alloc_inode("",0,0,0);
  strcpy(cwd, "/");
  cw_inode = root;
  index_created = 1;

  // eof?
  if(singleDirIttr->filepos == -1) {
    return 0;
  }

  // get to a non-'/' ittr...
  while(strcmp(singleDirIttr->fullpath,"/") == 0) {
    if(singleDirIttr->next() < 0) {
      LOG(DBG, "End of file/error before first directory");
      return -1;
    }
    if(singleDirIttr->filepos == -1) {
      return 0;
    }
  }
  
  // Have the first file record in place...
  char basedir[256];
  char currdir[256];
  strcpy(basedir, singleDirIttr->fullpath);
  striptofirstdir(basedir);
 
  for(;;) {
    
    files_added++;
    addnode(singleDirIttr);

#if defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    long long int last_offset = singleDirIttr->fileoffset;
    //    long long int last_filepos = singleDirIttr->filepos;
#else
    int last_offset = singleDirIttr->fileoffset;
    int last_filepos = singleDirIttr->filepos;
#endif

    int last_head_sz = singleDirIttr->entry.head_sz;
    int last_file_sz = seeksize(singleDirIttr->entry.sz);
      
    if(singleDirIttr->next() < 0) {
      nextSingleDirOffset = wfile.lseek(0, SEEK_CUR);
      return -1;
    }

    if(singleDirIttr->filepos == -1) {  // end of file...
      // printf("next() ittr filepos = -1\n");
      if(!wfile.wbuff) {   // if a memory mount, already done...
	singleDirSize = last_offset + last_head_sz + last_file_sz - singleDirOffset;
      }
      nextSingleDirOffset = wfile.lseek(0, SEEK_CUR);
      return (files_added > 0) ?  1 : 0;
    }


    strcpy(currdir, singleDirIttr->fullpath);
    striptofirstdir(currdir);

    //if(debug) {
    //  LOG(DBG,"basedir=%s singleDirIttr->fullpath=%s currdir=%s\n",
    //     basedir, singleDirIttr->fullpath, currdir);
    //}

    if(strcmp(basedir, currdir) != 0) {
      if(!wfile.wbuff) {   // if a memory mount, already done...
	singleDirSize = last_offset + last_head_sz + last_file_sz - singleDirOffset;
      }
      nextSingleDirOffset = wfile.lseek(0, SEEK_CUR);
      return (files_added > 0) ? 1 : 0;
    }
  }
  
  nextSingleDirOffset = wfile.lseek(0, SEEK_CUR);
  return 0;
}

int sfs_index::_create()
{
  root = alloc_inode("",0,0,0);
  strcpy(cwd,"/");
  cw_inode = root;
 
  SFS_ittr ittr;

  


  if(ittr.get(&wfile) < 0) {
    return -1;
  }

  for(;;) {
   
    //printf("ittrnext\n");
    //fflush(stdout);

    if(ittr.next() < 0) {
      //printf("Error itterating...\n");
      //fflush(stdout);
      return -1;
    }

    //printf("ittrnexted\n");
    //fflush(stdout);

    if(ittr.filepos == -1) {  // end of file...

      return 0;
    }

    //printf("addnode\n");
    //fflush(stdout);
    addnode(&ittr);
    //printf("addnode 2\n");
    //fflush(stdout);
  }

  LOG(ERR,"never happens\n");
  return -1;
}

void sfs_index::addnode(SFS_ittr *ittr)
{
  char fullpath[256];
  char thispathonly[256];
  char *next[20];

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  LOG(DBG, "addnode: %s %lld %d",ittr->fullpath, ittr->fileoffset, ittr->filepos);
#else
  LOG(DBG, "addnode: %s %d %d",ittr->fullpath, ittr->fileoffset, ittr->filepos);
#endif

  if(ittr->entry.attr == SFS_ATTR_INVALID) return;
  
  strcpy(thispathonly, ittr->fullpath);
  stripfile(thispathonly);
  
  strcpy(fullpath, ittr->fullpath);

  next[0] = strtok_r(&fullpath[1], "/", &_strtok_static_);
  
  int nn=1;
  while((next[nn] = strtok_r(NULL, "/", &_strtok_static_))) nn++;
    
  fs_inode *inode = root;
    
  for(int i=0;i<nn-1;i++) {
    inode = add_inode(inode, next[i], 0, 0, 0);
  }
    
  inode = add_inode(inode, next[nn-1], ittr->fileoffset + ittr->entry.head_sz, ittr->entry.sz, ittr->entry.head_sz);
}



void sfs_index::dump(const char *path, fs_inode *inode) {
  if(inode->fchild == NULL) {
    LOG(DBG,"%s%s\n",path,inode->name);
    return;
  }
  
  char fp[256];
  sprintf(fp,"%s%s/",path,inode->name);
  
  fs_inode *curr = inode->fchild;
  
  while(curr) {
    dump(fp,curr);
    curr = curr->next;
  }
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
fs_inode *sfs_index::add_inode_from(fs_inode *prev, char *name, long long int offset, int sz, int overhead)
#else
  fs_inode *sfs_index::add_inode_from(fs_inode *prev, char *name, int offset, int sz, int overhead)
#endif
{
  int eq=0;

  fs_inode *parent = prev->parent;

  fs_inode *link = find_last_lesser_neighbor(prev, name, eq);
  if(eq == 0) return link;

  // if(newn < prev, have to go from start...
  if(!link) return add_inode(parent,name,offset,sz,overhead);
  
  fs_inode *newn = alloc_inode(name,offset,sz,overhead);
  if(!newn) return NULL;

  
  newn->parent = parent;
  newn->next = link->next;
  link->next = newn;
  return newn;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
fs_inode *sfs_index::add_inode(fs_inode *parent, char *name, long long int offset, int sz, int overhead)
#else
  fs_inode *sfs_index::add_inode(fs_inode *parent, char *name, int offset, int sz, int overhead)
#endif
{
  int eq=0;
  int first=0;
 
  fs_inode *link = find_last_lesser_child(parent, name, first, eq);

  if(eq == 0) return link;
 
  fs_inode *newn = alloc_inode(name,offset,sz,overhead);

  if(!newn) return NULL;
  newn->parent = parent;

  if(first) {
    newn->next = parent->fchild;
    parent->fchild = newn;
    
    return newn;
  }

  newn->next = link->next;
  link->next = newn;
  return newn;
}

fs_inode *sfs_index::find_last_lesser_neighbor(fs_inode *first, char *name, int &eq)
{
  eq = -1;
  fs_inode *curr = first;
  if(!curr) return NULL;

  //printf("lln1 %s %s\n",curr->name, name);
  eq = mstrcmp(curr->name, name);
  
  if(eq > 0) return NULL;
  if(eq == 0) return curr;

  fs_inode *next = curr->next;

  while(next) {
    //printf("lln2 %s %s\n",next->name, name);
    eq = mstrcmp(next->name, name);
    if(eq > 0) return curr;
    if(eq == 0) return next;
    curr = next;
    next = curr->next;
  }
  return curr;
}

fs_inode *sfs_index::find_last_lesser_child(fs_inode *parent, char *name, int &first, int &eq)
{
  first = 1;
  eq = -1;

  fs_inode *curr = parent->fchild;
  if(!curr) return NULL;

  //printf("llc1 %s %s\n",curr->name, name);
  eq = mstrcmp(curr->name, name);

  if(eq > 0) return NULL;
  if(eq == 0) {
    return curr;
  }

  first = 0;

  fs_inode *next = curr->next;
  
  while(next) {
    //printf("llc2 %s %s\n",next->name, name);
    eq = mstrcmp(next->name, name);
    if(eq > 0) return curr;
    if(eq == 0) return next;

    curr = next;
    next = curr->next;
  }

  return curr;
}

int sfs_index::getfileheadersz(char *fn)
{
  return (sizeof(SFS_File) - 4 + seeksize(strlen(fn)+1));
}


int sfs_index::putfileheader(char *ptr, char *fn, int filesz, int flags)
{
  return sfs_putfileheader(ptr,fn,filesz,flags);
}

