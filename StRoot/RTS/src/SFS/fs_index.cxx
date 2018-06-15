#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <rts.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <ctype.h>

#include <rts.h>
#include <rtsLog.h>

#include "fs_index.h"

//extern int debug;

int wrapfile::openmem(char *buff, int size) {
  wbuff = buff;
  wsize = size;
  wfpos = 0;
  type = WRAP_MEM;
  return 1;
}

int wrapfile::opendisk(char *fn, int flags, int perms)
{
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  fd = open64(fn, flags, perms);
#else
  fd = open(fn, flags, perms);
#endif
  if(fd > 0) {
    type = WRAP_DISK;
  }
  else {
    type = 0;
  }
  return fd;
}

int wrapfile::openfd(int fd)
{
  this->fd = fd;
  type = WRAP_DISK;
  return fd;
}

int wrapfile::read(void *buff, int sz)
{
  switch(type) {
  case WRAP_MEM:

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    LOG(DBG, "read: wfpos=%lld wsize=%lld sz=%d",wfpos,wsize,sz);
#else
    LOG(DBG, "read: wfpos=%d wsize=%d sz=%d",wfpos,wsize,sz);
#endif

    if((wfpos + sz) > wsize) {   // max read is filesize...
      sz = wsize - wfpos;
    }
    memcpy(buff, wbuff + wfpos, sz);
    wfpos += sz;
    return sz;

  case WRAP_DISK:
    return ::read(fd, buff, sz);

  case WRAP_SOCKET:
    return -1;
  }

  return -1;
}

int wrapfile::write(void *buff, int sz)
{
  switch(type) {
  case WRAP_MEM:
    
    if((wfpos + sz) > wsize) {
      sz = wsize - wfpos;
    }
    memcpy(this->wbuff + wfpos, buff, sz);
    wfpos += sz;
    return sz;
 
  case WRAP_DISK:
  case WRAP_SOCKET:
    return ::write(fd, buff, sz); 
  }

  return -1;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
long long int wrapfile::lseek(long long int offset, int whence)
#else
int wrapfile::lseek(int offset, int whence)
#endif
{
  switch(type) {
    
  case WRAP_MEM: 
    {
      switch(whence) {
      case SEEK_CUR:
	wfpos += offset;
	break;
      case SEEK_SET:
	wfpos = offset;
	break;
      }
      if(wfpos > wsize) {
	wfpos = wsize;
      }
      return wfpos;
    }
    
  case WRAP_DISK:
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    return ::lseek64(fd, offset, whence);  
#else
    return ::lseek(fd, offset,whence);
#endif
  }

  return -1;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
int wrapfile::fstat(struct stat64 *stat)
#else
int wrapfile::fstat(struct stat *stat)
#endif
{
  stat->st_size = 0;

  switch(type) {
  case WRAP_MEM:
  case WRAP_SOCKET:
    return -1;
  
  case WRAP_DISK:
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    return ::fstat64(fd, stat);
#else
    return ::fstat(fd, stat);
#endif
  }
  return 0;
}

int wrapfile::close()
{
  switch(type) {
  case WRAP_DISK:
  case WRAP_SOCKET:
    {
      type = 0;
      return ::close(fd);
    }
  case WRAP_MEM:
    {
      type = 0;
      return 0;
    }
  }
  return 0;
}
      
        

fs_index::fs_index()
{
  strcpy(cwd, "/");
  root = NULL;
  n_inodes = 0;
  index_created=0;
  cdchanged = 0;
  writevbuff = NULL;

}

fs_index::~fs_index()
{   
  if(writevbuff) {
    free(writevbuff);
  }
}

void fs_index::hexdump(char *buff, int sz)
{
  for(int i=0;i<sz;i+=16) {
    int k= sz-i;
    if(k>16) k=16;

  
    printf("0x%08x:  ",i);

    for(int j=0;j<16;j++) {
      if(j==8) printf("    ");
      int x = buff[i+j] & 0x00ff;
      if(j<k)
	printf(" %02x",x);
      else printf("   ");
    }

    printf("     ");
    for(int j=0;j<16;j++) {
      
      if(j == 8) printf(" ");

      if(j<k) {
	if(isprint(buff[i+j]))
	  printf("%c",buff[i+j]);
	else
	  printf("."); 
      }
    }
    printf("\n");
  }      
}

int fs_index::mountmem(char *buffer, int sz, int flags)
{
  oflags = flags;
  wfile.openmem(buffer, sz);
  return initmount();    // build index/file header
}

// Mounts to a socket...
// always WRONLY!
// always append-like
// always prepends ethernet header to writes.
int fs_index::mount(int fd)
{
  oflags = O_WRONLY;
  wfile.openfd(fd);
  return wfile.fd;
}

// Flags currently allowed/required
//
// O_RDONLY,  O_WRONLY,  O_APPEND,  O_CREAT
//
int fs_index::mount(char *filename, int flags, int perms)
{
  oflags = flags;
  wfile.opendisk(filename,flags,perms);
 
  if(wfile.fd < 0) return wfile.fd;

  return initmount();  // build index/file header
}

int fs_index::mount(int ip, int port)
{
	int fd ;
	int ret;
	int optval ;
	sockaddr_in addr ;

	errno = 0 ;
	fd = socket(AF_INET, SOCK_STREAM, 0) ;
	if(fd < 0) {
	
		return -1 ;
	}

	optval = 1 ;
	setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, (char *)&optval, sizeof(optval)) ;
	setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;

	memset((char *)&addr,0,sizeof(addr)) ;
	addr.sin_family = AF_INET ;
	addr.sin_port = htons(port) ;
	addr.sin_addr.s_addr = htonl(ip) ;

	errno = 0 ;
	ret = connect(fd, (sockaddr *)&addr,sizeof(addr)) ;
	if(ret < 0) {

		return -1 ;
	}

	optval = 64*1024*1024 ;
	for(;;) {
		ret = setsockopt(fd, SOL_SOCKET, SO_SNDBUF, (char *)&optval, sizeof(optval)) ;
		if(ret==0) break ;
		optval /= 2 ;
	}

	// check
	int new_val ;
	socklen_t new_val_len  = sizeof(new_val) ;
	ret = getsockopt(fd, SOL_SOCKET, SO_SNDBUF, &new_val, &new_val_len) ;
	
	LOG(NOTE,"mount: IP 0x%08X:%d  claims %d buffer bytes, reports %d (%d)",ip,port,optval,new_val,ret) ;


	return mount(fd) ;

	
}

int fs_index::initmount()
{
  int ret;
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  struct stat64 stat;
#else
  struct stat stat;
#endif

  strcpy(cwd, "/");
  root = NULL;
  n_inodes = 0;
  index_created=0; 


  if(oflags & O_WRONLY) {   // filedescriptor
    wfile.fstat(&stat);
    
    LOG(TERR,"stat.st_size %d",stat.st_size) ;

    if(stat.st_size > 0) {
      if(!(oflags & O_APPEND)) {   // if not append do not allow...
	wfile.close();    
	LOG(ERR,"no APPEND") ;
	return -1;  
      }
    }
    else {    // writing file, but already exists...
      ret = writeFsHeader();
      if(ret < 0) {
	LOG(ERR,"writeFsHeader %d",ret) ;
	return ret;
      }
    }
  }
  else {  // O_RDONLY = 0
    ret = _create();

    
    if(ret < 0) {
	LOG(ERR,"_create() %d",ret) ;
	return ret;
    }
    index_created = 1;
  }
  return 0;
}

void fs_index::getFullPath(char *fullpath, char *name)
{
  if(name[0] == '/') {
    strcpy(fullpath,name);
  }
  else {
    strcpy(fullpath,cwd);
    int n = strlen(fullpath)-1;
    if(fullpath[n] != '/') strcat(fullpath, "/");
    strcat(fullpath,name);
  }
}

fs_inode *fs_index::find_child(fs_inode *parent, char *name) {
  fs_inode *next = parent->fchild;
  while(next) {
    //printf("finding child looking for %s in %s\n",name,next->name);
    if(strcmp(name, next->name) == 0) return next;
    next = next->next;
  } 
  return NULL;
}


int fs_index::cd(char *name)
{
  char fullpath[256];
  getFullPath(fullpath,name);
  cdchanged = 1;

  if(oflags & O_WRONLY) {  // writeonly, simply set name
    strcpy(cwd, fullpath);
    return 0;
  }

  // otherwise, make sure directory is valid & also set cw_inode.
  if(!index_created) return -1;
  
  char tokes[256];
  strcpy(tokes,fullpath);

  char *token = strtok_r(tokes,"/", &_strtok_static_);
  fs_inode *node = root;
  
  while(token) {
    fs_inode *newn = find_child(node, token);
    if(!newn) return -1;
    node = newn;
    token = strtok_r(NULL, "/", &_strtok_static_);
  }
  
  if(!node->fchild) return -1;
  
  cw_inode = node;
  strcpy(cwd,fullpath);
  return 0;
}


void fs_index::free_inode(fs_inode *inode)
{
    //printf("free\n");
  if(inode->fchild) free_inode(inode->fchild);

  if(inode->next) free_inode(inode->next);

  n_inodes--;
  free(inode->name);
  free(inode);
}

void fs_index::umount()
{
  if(index_created)
    free_inode(root);

  wfile.close();
  index_created = 0;
}

fs_dirent *fs_index::readdirent(char *dir, fs_dirent *ent)
{
  if(ent == NULL) {
      ent = &_readdirent_static_;
  }

  if(!index_created) return NULL;

  char fullname[256];
  getFullPath(fullname,dir);
  strcpy(ent->full_name, fullname);

  fs_inode *node = root;
  char *name = strtok_r(fullname, "/", &_strtok_static_);

  while(name) {   
    node = find_child(node, name);
    if(!node) return NULL;

    name = strtok_r(NULL, "/", &_strtok_static_);
  }


  strcpy(ent->d_name, node->name);

  ent->sz = node->sz;
  ent->offset = node->offset;
  if(node->fchild) ent->has_child = 1;
  else ent->has_child = 0;
  //  ent.token = node->token;
  //ent.format_ver = node->format_ver;
  //ent.format_number = node->format_number;
  ent->swap = node->swap;
  return ent;  
}


fs_dir *fs_index::opendir(char *dir)
{
  if(!index_created) return NULL;
 
  char fullpathc[256];
  char fullpath[256];

  getFullPath(fullpath, dir);
  if(fullpath[strlen(fullpath)-1] != '/') strcat(fullpath, "/");
  strcpy(fullpathc, fullpath);

  //if(debug) printf("opendir: path : (%s)\n",fullpathc);

  fs_inode *node = root;

  char *name = strtok_r(fullpath, "/", &_strtok_static_);
  while(name) {

    //if(debug) printf("opendir name = %s\n",name);
    //fflush(stdout);

    node = find_child(node,name);
    if(!node) return NULL;

    name = strtok_r(NULL, "/", &_strtok_static_);
  }
  
  fs_dir *ret = (fs_dir *)malloc(sizeof(fs_dir));
  if(!ret) return NULL;

  strcpy(ret->full_name, fullpathc);
  ret->inode = node;
  ret->currchild = NULL;

  LOG(DBG, "returning dir 0x%x",ret);
  return ret;
}

void fs_index::closedir(fs_dir *dir)
{
  if(!index_created) return;
  free(dir);
}


fs_dirent *fs_index::readdir(fs_dir *dir, fs_dirent *ent)
{
  if(!index_created) return NULL;
  
  fs_inode *node = dir->currchild;

  if(!node) 
    node = dir->inode->fchild;
  else
    node = node->next;

  if(!node) return NULL;

  dir->currchild = node;

  if(ent == NULL) {
   ent = &_readdirent_static_;
  }

  strcpy(ent->d_name, node->name);

  strcpy(ent->full_name, dir->full_name);
  strcat(ent->full_name, node->name);

  ent->sz = node->sz;
  ent->offset = node->offset;
  if(node->fchild) ent->has_child = 1;
  else ent->has_child = 0;

  ent->swap = node->swap;

  return ent;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
fs_inode *fs_index::alloc_inode(const char *name, long long int off, int sz, int overhead)
#else
  fs_inode *fs_index::alloc_inode(const char *name, int off, int sz, int overhead)
#endif
{
    //printf("alloc\n");
  n_inodes++;
  fs_inode *n = (fs_inode *)malloc(sizeof(fs_inode));
  if(!n) return NULL ;

  n->parent = NULL;
  n->fchild = NULL;
  n->lchild = NULL;
  n->prev = NULL;
  n->next = NULL;
  n->offset = off;
  n->overhead = overhead;
  n->sz = sz;
  n->name = (char *)malloc(strlen(name)+1);
  if(!n->name) {
	free(n) ;
	return NULL;
  }

  strcpy(n->name,name);

  return n;
}

int fs_index::fileSize(char *fn)
{
  if(!index_created) return -1;

  fs_dirent *entry = readdirent(fn);
  if(!entry) {
    //printf("Can't find file %s\n",fn);
    return -1;
  }

  return entry->sz;
}

int fs_index::read(char *fn, char *buff, int maxsize)
{
  if(!index_created) return -1;

  fs_dirent *entry = readdirent(fn);
  if(!entry) {
    //printf("Can't find file %s\n",fn);
    return -1;
  }

  if(entry->sz == 0) {
    LOG(NOTE,"%s has no data...",fn);
    return 0;
  }

  if(entry->sz > maxsize) {
    return -1;
  }
  
  int ret = wfile.lseek(entry->offset, SEEK_SET);
  if(ret != entry->offset) {
    LOG(ERR,"Invalid seek %d vs %d\n",ret,entry->offset);
    return -1;
  }
  
  ret = wfile.read(buff, entry->sz);
  if(ret != entry->sz) {
    LOG(ERR,"Error reading file %d vs %d\n", ret, entry->sz);
    return -1;
  }
  return ret;
}

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
long long int fs_index::mountsz()
#else
int fs_index::mountsz()
#endif
{
  
  if(oflags & O_WRONLY) {
    if(wfile.type == WRAP_MEM) {
      return wfile.wfpos;
    }
  }

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  struct stat64 stat;
#else
  struct stat stat;
#endif

  wfile.fstat(&stat);
  return stat.st_size;
}

int fs_index::mem_ls(fs_filelist *filelist, int recurse, fs_dir *dir)
{
  int ret;

  fs_dirent *entry;
  while((entry = readdir(dir))) {
    if(filelist->n >= filelist->max) return 0;

    if(!entry->has_child) {   // only save files, not dirs
      strcpy(filelist->filename[filelist->n], entry->full_name);
      filelist->n++;
    }
    
    if(recurse & entry->has_child) {
      fs_dir *ndir = opendir(entry->full_name);
      ret = mem_ls(filelist, recurse, ndir);
      closedir(ndir);
      if(ret == 0) return 0;
    }
  }
  return 1;
}
