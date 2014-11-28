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
// #include <byteswap.h>
// #define swap16(x) bswap_16(x)
// #define swap32(x) bswap_32(x)

#include <daqFormats.h>

// Ugly Ugly hack.
// searches the future for an event number
int SFS_ittr::findEventNumber()
{
  int ret;
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  long long int orig_pos = wfile->lseek(0, SEEK_CUR);
  LOG(DBG, "findEventNumber: pos=%lld",orig_pos);
#else
  int orig_pos = wfile->lseek(0, SEEK_CUR);
  LOG(DBG, "findEventNumber: pos=%d",orig_pos);
#endif

  char buff[12];
  for(;;) {
    ret = wfile->read(buff, 8);
    if(ret == 0) { ret = 0; break; }
    if(ret != 8) { ret = -1; break; }

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
    wfile->lseek(-((long long int)8), SEEK_CUR);
#else
    wfile->lseek(-8, SEEK_CUR);
#endif
    
    if(memcmp(buff, "SFS V", 5) == 0) {
      wfile->lseek(12, SEEK_CUR);
      continue;
    }
    else if(memcmp(buff, "LRHD", 4) == 0) { 
      wfile->lseek(60, SEEK_CUR);
      continue;
    }
    else if(memcmp(buff, "HEAD", 4) == 0) {
      wfile->lseek(12, SEEK_CUR);
      continue;
    }
    else if(memcmp(buff, "DATAP", 5) == 0) {
      DATAP datap;
      ret = wfile->read(&datap, sizeof(datap));
      if(ret != sizeof(datap)) {
	LOG(ERR, "Error reading datap: %d",ret);
	ret = -1;
	break;
      }
      
      int seq = datap.seq;
      if(datap.bh.byte_order != 0x04030201) {
	seq = swap32(seq);
      }

      LOG(DBG, "Got event #%d from DATAP",seq);
      ret = seq;
      break;
    }
    else if(memcmp(buff, "FILE", 4) == 0) {  // finally!
      char fbuff[120];
      SFS_File *f = (SFS_File *)fbuff;
      ret = wfile->read(fbuff, sizeof(SFS_File));
      if(ret != sizeof(SFS_File)) {
	LOG(ERR, "Error reading FILE: %d",ret);
	ret = -1;
	break;
      }
      int sz = f->head_sz;
      if(f->byte_order != 0x04030201) {
	sz = swap32(sz);
      }
      
      
      if((unsigned int)sz > sizeof(SFS_File)) {
	int sztogo = sz - sizeof(SFS_File);
	char *btogo = fbuff;
	btogo += sizeof(SFS_File);

	ret = wfile->read(btogo, sztogo);
	if(ret != sztogo) {
	  LOG(ERR, "Error reading FILE: %d",ret);
	  ret = -1;
	  break;
	}
      }

      ret = atoi(&f->name[2]);   // skip "/#"
   
      LOG(DBG, "Set event number to #%d from next FILE rec",ret);
      break;
    }
    else {
      LOG(DBG, "Else?");
      break;
    }
  }

  wfile->lseek(orig_pos, SEEK_SET);
  LOG(DBG, "Returning %d",ret);
  return ret;
}

// VERY VERY ugly hack
// jumps back from datap to LRHD and gets total event length
// then subtracts LRHD length and returns....
int SFS_ittr::findFullLength()
{
  LOGREC lrhd;
  //  int orig_pos = wfile->lseek(0, SEEK_CUR);
  
#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  wfile->lseek(-((long long int)sizeof(lrhd)),SEEK_CUR);
#else 
  wfile->lseek(-(sizeof(lrhd)),SEEK_CUR);
#endif

  int ret = wfile->read(&lrhd, sizeof(lrhd));
  
  if(ret != sizeof(lrhd)) {
    LOG(ERR, "Error reading lrhd %d", ret);
    return -1;
  }

  int sz = lrhd.length;
  if(lrhd.lh.byte_order != 0x04030201) {
    sz = swap32(sz);
  }
  sz *= 4;
  sz -= sizeof(lrhd);

  return sz;
}

int SFS_ittr::nextLRHD()
{
  // First check if valid?
  LOGREC lrhd;
  int ret = wfile->read(&lrhd, sizeof(lrhd));
  if(ret != sizeof(lrhd)) {
    LOG(ERR, "Error reading lrhd %d", ret);
    return -1;
  }

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  long long int xxx = wfile->lseek(-((long long int)sizeof(lrhd)),SEEK_CUR);  // go back to start...
  LOG(DBG, "nextLRHD():  (%c%c%c%c) off=%lld",
      lrhd.lh.bank_type[0],lrhd.lh.bank_type[1],
      lrhd.lh.bank_type[2],lrhd.lh.bank_type[3], xxx);
#else 
  int xxx = wfile->lseek(-(sizeof(lrhd)),SEEK_CUR);  // go back to start...
  LOG(DBG, "nextLRHD():  (%c%c%c%c) off=%d",
      lrhd.lh.bank_type[0],lrhd.lh.bank_type[1],
      lrhd.lh.bank_type[2],lrhd.lh.bank_type[3], xxx);
#endif



  if(memcmp(lrhd.lh.bank_type, "LRHD", 4) != 0) {
    LOG(ERR, "nextLRHD() not LRHD %c%c%c%c",
	lrhd.lh.bank_type[0],lrhd.lh.bank_type[1],
	lrhd.lh.bank_type[2],lrhd.lh.bank_type[3]);
    //wfile->lseek(-sizeof(lrhd), SEEK_CUR);  // put it back...
    return -1;
  }

  if(memcmp(lrhd.record_type, "DATA", 4) != 0) {
    LOG(DBG, "nextLRHD() not DATA %c%c%c%c",
	lrhd.record_type[0],lrhd.record_type[1],
	lrhd.record_type[2],lrhd.record_type[3]);
    //wfile->lseek(-sizeof(lrhd), SEEK_CUR);  // put it back...
    return -1;
  }

  // Okay.  Now we know there is a LRHD record
  int n = findEventNumber();

 
  memcpy(entry.type,"FILE",4);
  entry.byte_order = 0x04030201;
  entry.sz = sizeof(LOGREC);
  LOG(DBG, "LRHD: entry.sz = %d",entry.sz);
  sprintf(entry.name, "/#%d/lrhd",n);
  entry.head_sz = 0;
  entry.attr = 0;  

  strcpy(fullpath, entry.name);
  strcpy(ppath, entry.name);
  striptofile(entry.name);
  stripfile(ppath);

  int ccc = wfile->lseek(0,SEEK_CUR);

  filepos = 1;

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  LOG(DBG,"fullpath %s, entry.name: %s, fileoffset %lld (%d)/%d, sz %d  head_sz %d",
      fullpath, entry.name, fileoffset, ccc, filepos, entry.sz, entry.head_sz);
#else  
  LOG(DBG,"fullpath %s, entry.name: %s, fileoffset %d (%d)/%d, sz %d  head_sz %d",
      fullpath, entry.name, fileoffset, ccc, filepos, entry.sz, entry.head_sz);
#endif

  return 0;
}

int SFS_ittr::nextDatap()
{
  int sz = findFullLength();
  
  DATAP datap;
  int ret = wfile->read(&datap, sizeof(datap));
  if(ret != sizeof(datap)) {
    LOG(ERR, "Bad read of datap %d",ret);
    return -1;
  }

  LOG(DBG, "Nextdatap: (%c%c%c%c)",
      datap.bh.bank_type[0],
      datap.bh.bank_type[1],
      datap.bh.bank_type[2],
      datap.bh.bank_type[3]);

  char buff[8];
  ret = wfile->read(buff, 8);
  if(ret != 8) {
    LOG(ERR, "Bad read of datap %d",ret);
    return -1;
  }

#if  defined(__USE_LARGEFILE64) || defined(_LARGEFILE64_SOURCE)
  wfile->lseek(-((long long int)8),SEEK_CUR);
  wfile->lseek(-((long long int)sizeof(DATAP)), SEEK_CUR);
#else
  wfile->lseek(-8,SEEK_CUR);
  wfile->lseek(-sizeof(DATAP), SEEK_CUR);
#endif
  
  if(memcmp(buff, "FILE", 4) == 0) {  // SFS follows, so no legacy...
    LOG(DBG, "No legacy because SFS follows");
    return -1;
  }
  else {
    LOG(DBG, "Making legacy entry");
  }
    
  int seq = datap.seq;
  if(datap.bh.byte_order != 0x04030201) seq = swap32(seq);
  
  memcpy(entry.type,"FILE",4);
  entry.byte_order = 0x04030201;
  LOG(DBG, "sz = %d");
  entry.sz = sz;
  sprintf(entry.name, "/#%d/legacy",seq);
  entry.head_sz = 0;
  entry.attr = 0;  

  strcpy(fullpath, entry.name);
  strcpy(ppath, entry.name);
  striptofile(entry.name);
  stripfile(ppath);

  int ccc = wfile->lseek(0,SEEK_CUR);

  LOG(DBG,"fullpath %s, entry.name: %s, fileoffset %d (%d)/%d, sz %d  head_sz %d",
      fullpath, entry.name, fileoffset, ccc, filepos, entry.sz, entry.head_sz);

  filepos = 1;
  return 0;
}
