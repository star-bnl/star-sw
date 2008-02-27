#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <rts.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <rts.h>


#include "daqr_index.h"


daqr_index::daqr_index() : fs_index()
{
  run = 0;
  return;
}

void daqr_index::getBankInfo(bankHeader *bh,  UINT32 &pointerOffset,  int &nidx, int &datasize)
{
  char type[12];
  
  memcpy(type,bh->bank_type,8);
  for(int j=0;j<12;j++) if(type[j] == ' ') type[j] = '\0';
  type[8] = '\0';

  // printf("getBankInfo:  type=%s\n",type);

  int swap = (bh->byte_order == 0x04030201) ? 0 : 1;

  datasize = qswap32(swap, bh->length)*4;
  pointerOffset = 0;
  nidx = 0;

  if(strcmp(type, "DATAP") == 0) {
    DATAP *datap = (DATAP *)bh;
    pointerOffset = (UINT32)datap->det - (UINT32)datap;
    nidx = 10;
    //datasize = sizeof(DATAP)-sizeof(bankHeader);
  }
  else if (strcmp(type, "DATAPX") == 0) {
    DATAPX *datapx = (DATAPX *)bh;
    pointerOffset = (UINT32)datapx->det - (UINT32)datapx;
    nidx=22;
  }
  else if (strcmp(type, "TRGP") == 0) {
    TRGP *trgp = (TRGP *)bh;
    pointerOffset = (UINT32)&trgp->trgData - (UINT32)trgp;
    nidx = 2;
  }
  else if (strcmp(type, "TPCRBCLP") == 0) {
    TPCRBCLP *tpcrbclp = (TPCRBCLP *)bh;
    pointerOffset = (UINT32)tpcrbclp->mz - (UINT32)tpcrbclp;
    nidx = RB_MZ_NUM;
    //datasize = sizeof(TPCRBCLP) - sizeof(bankHeader);
  }
  else if ((strcmp(type, "TPCRBP") == 0) || 
           (strcmp(type, "SVTRBP") == 0) ||
	   (strcmp(type, "FTPRBP") == 0)) {
    TPCRBP *tpcrbp = (TPCRBP *)bh;
    pointerOffset = (UINT32)tpcrbp->mz - (UINT32)tpcrbp;
    nidx = RB_MZ_NUM;
    //datasize = sizeof(TPCRBP) - sizeof(bankHeader);
  }
  else if (strcmp(type, "TPCSECP") == 0) {
    TPCSECP *tpcsecp = (TPCSECP *)bh;
    pointerOffset = (UINT32)tpcsecp->rb - (UINT32)bh;
    nidx = SB_RB_NUM+1;    // special hack for the tpcseclp banks...
  } 
  else if (type[strlen(type)-1] != 'P') {  // Not a pointer bank!
    //    printf("Not a pointer bank...%s\n",type);
    //datasize = qswap32(swap,bh->length) * 4 - sizeof(bankHeader);
    return;
  }
  else {  // generic pointer banks...
    pointerOffset = sizeof(bankHeader);
    nidx = (qswap32(swap, bh->length)*4 - sizeof(bankHeader))/sizeof(offlen);
    // printf("getbankinfo: nidx = %d\n",nidx);
  }
}

// Get offset of i'th child bank
// nidx = number of potential children
// datasize = amount of data stored in this bank....(0 if only pointers
//                                      but includes pointers otherwise)
//
UINT32 daqr_index::getChildBank(bankHeader *bh, int i, int &sz) 
{
  UINT32 off;
  int nidx;
  getBankInfo(bh, off, nidx, sz);

  if(i >= nidx) return 0;

  int swap = (bh->byte_order == 0x04030201) ? 0 : 1;

  if((i == nidx-1) && memcmp(bh,"TPCSECP ",8)==0) {  // special hack to get the TPCSECLP's
    //printf("Here... %d %d\n",qswap32(swap,bh->format_number),qswap32(swap,bh->w9));
    
    if(qswap32(swap, bh->format_number) != 2) {
      sz = 0;
      return 0;
    }
    
    //printf("there\n");
    off = qswap32(swap,bh->w9) * 4;
    sz = sizeof(TPCSECLP);
    return off;
  }

  //printf("offset = %d\n",off);

  if(off == 0) return 0;

  offlen *offsets = (offlen *)((UINT32)bh + off);

  off = qswap32(swap,offsets[i].off);
  off *= 4;
  sz = qswap32(swap,offsets[i].len);
  sz *= 4;
  
  if((off && !sz) || (sz && !off)) {
    printf("trouble: off=%d sz=%d\n",off,sz);
    off = 0;
    sz = 0;
  }
  return off;
}

// Reads bank if pointer bank.
// if not pointer bank only reads header.
bankHeader *daqr_index::readBank(int fd, UINT32 offset)
{
  UINT32 ret = lseek(fd, offset, SEEK_SET);
  if(ret != offset) {
    printf("Bad seek (offset=%d) %s\n",offset,strerror(errno));
    return NULL;
  }

  UINT32 max = sizeof(TPCP);   // datap is the largest bank...
  bankHeader *bh = (bankHeader *)malloc(max);
  if(!bh) {
    printf("Bad allocation %s\n",strerror(errno));
    return NULL;
  }

  char *buff = (char *)bh;
  ret = ::read(fd, buff, sizeof(bankHeader));
  if(ret == 0) return NULL;
  if(ret != sizeof(bankHeader)) {
    printf("Bad read: %s\n",strerror(errno));
    free(buff);
    return NULL;
  }

  UINT32 off=0;
  int n=0;
  int sz=0;
  getBankInfo(bh, off, n, sz);
  
  if(off > 0) {  // This is a point bank, finish...
    int swap = (bh->byte_order == 0x04030201) ? 0 : 1;
    UINT32 len = qswap32(swap,bh->length) * 4;
    if(len > max) {
      printf("len(%d) > max(%d)\n",len,max);
      free(buff);
      return NULL;
    }
    len -= sizeof(bankHeader);
    char *buff2 = buff + sizeof(bankHeader);
    ret = ::read(fd, buff2, len);
    if(ret != len) {
      printf("Bad read... %s\n",strerror(errno));
      free(buff);
      return NULL;
    }
  }
  return (bankHeader *)buff;
}

void daqr_index::getName(bankHeader *bh, char *name)
{
  int swap = bh->byte_order == 0x04030201 ? 0 : 1;
  char base[10];
  memcpy(base,bh,8);
  base[9] = ' ';
  for(int i=0;i<10;i++) {
    if(base[i] == ' ') { base[i] = '\0'; break; }
  }
  
  if(strcmp(base, "DATAP") == 0) {
    DATAP *datap = (DATAP *)bh;
    sprintf(name, "%u",qswap32(swap,datap->seq));
  }
  else if ((strcmp(base, "TPCSECLP") == 0) || 
	   (strcmp(base, "TPCADCD") == 0) ||
	   (strcmp(base, "TPCSEQD") == 0) ||
	   (strcmp(base, "TPCADCX") == 0) ||  
	   (strcmp(base, "SVTADCD") == 0) ||
	   (strcmp(base, "SVTSEQD") == 0) ||
	   (strcmp(base, "SVTADCX") == 0) ||
	   (strcmp(base, "FTPADCD") == 0) ||
	   (strcmp(base, "FTPSEQD") == 0) ||
	   (strcmp(base, "FTPADCX") == 0) ||
	   (strcmp(base, "EMCADCR") == 0) ) {
    strcpy(name, base);
  }
  else if ((strcmp(base,"EECSECP") == 0) ||
	   (strcmp(base,"EMCSECP") == 0)) {
    switch(qswap32(swap,bh->bank_id)) {
    case 1: 
      strcpy(name, "TOW");
      break;
    case 2:
      strcpy(name, "SMD");
      break;
    case 3:
      strcpy(name, "PRE");
      break;
    }
  }
  else if(strcmp(base, "EMCRBP") == 0) {   // for some reason these are numbered from zero?
    sprintf(name, "%s_%02d",base,qswap32(swap,bh->bank_id));
  }
  else if(bh->bank_id == 0) {
    strcpy(name,base);
  }
  else {
    sprintf(name, "%s_%02d",base,qswap32(swap,bh->bank_id));
  }
}

void daqr_index::ls_r(int fd, char *ppath, bankHeader *bh, UINT32 curr_offset)
{
  bankHeader *cbh;
  UINT32 tmp;
  int nidx;
  int datasz;
  char path[256];
  char name[256];
  int i;

  //printf("calling ls_r (%s) %d\n",ppath,strlen(ppath));

  getBankInfo(bh,  tmp,  nidx, datasz);

  strcpy(path, ppath);
  strcat(path, "/"); 
  getName(bh,name);
  strcat(path,name);
  
  //printf("abcd %s\n",path);
  //printf("new path will be... %s  (nidx=%d)\n",path,nidx);
  if(nidx <= 0) {
    printf("--->  (%6d) %s\n",datasz,path);
    
    return;
  }


  //printf("nidx=%d\n",nidx);

  for(i=0;i<nidx;i++) {
    int off = getChildBank(bh, i, datasz);
    // printf("%s    child[%d].offset = %d (0x%x) sz=%d (0x%x)  nidx=%d\n",path,i,off,off,datasz,datasz,nidx);
    if(off <= 0) continue;

    cbh = readBank(fd, curr_offset + off);
    if(!cbh) {
      printf("Error... No bank read at curr_offset=%d + off=%d\n",curr_offset,off);
      continue;
    }

    // printf("i=%d\n",i);
    ls_r(fd, path, cbh, curr_offset + off);
    free(cbh);
  }

  //printf("returning from ls_r=%s\n",path);
}

void daqr_index::dump(int fd)
{
  UINT32 curr_offset;
  UINT32 next_offset;
  bankHeader *bh;
  int swap;

  curr_offset = 0;
  next_offset = 0;
  for(;;) {
    bh = readBank(fd,curr_offset);
    if(!bh) {
      printf("End of file\n");
      return;
    }

    if(memcmp(bh->bank_type, "LRHD", 4) == 0) {
      swap = (bh->byte_order == 0x04030201) ? 0 : 1;
      //printf("/LRHD_%02d\n",qswap32(swap, bh->bank_id));

      LOGREC *logrec = (LOGREC *)bh;
      char *buff = (char *)bh;
      buff += sizeof(bankHeader);

      // Read the rest...
      ::read(fd, buff, qswap32(swap,bh->length) * 4 - sizeof(bankHeader));

      next_offset = curr_offset + qswap32(swap, logrec->length) * 4;

      // printf("[%s] 0x%x curr=%d next=%d len=%d\n",logrec->record_type,bh->byte_order,curr_offset,next_offset,qswap32(swap,logrec->length) *4);
      curr_offset += qswap32(swap, bh->length) * 4;
    }
    else {
      //printf("curr_offset = %u   next_offset = %u\n",curr_offset, next_offset);

      if(memcmp(bh->bank_type, "DATAP", 5) != 0) {
	//printf("bank type = %s\n",bh->bank_type);
	return;
      }
      ls_r(fd, "", bh, curr_offset);
      curr_offset = next_offset;
    }


    free(bh);
  }
}
  

// No sort, add at end...
fs_inode *daqr_index::add_inode(fs_inode *parent, char *name, int offset, int sz, bankHeader *bh)
{
  fs_inode *newn = alloc_inode(name,offset,sz);
  if(!newn) return NULL;

  newn->swap = (bh->byte_order == 0x04030201) ? 0 : 1;
  //  newn->format_ver = qswap32(newn->swap,bh->format_ver);
  // newn->format_number = qswap32(newn->swap,bh->format_number);
  //newn->token = qswap32(newn->swap,bh->token);

  newn->parent = parent;

  if(parent->fchild == NULL) {    
    parent->fchild = newn;
    parent->lchild = newn;
    return newn;
  }
  
  newn->prev = parent->lchild;
  newn->prev->next = newn;
  newn->next = NULL;
  parent->lchild = newn;
  
  return newn;
}

void daqr_index::addNode_r(int fd, fs_inode *node, char *ppath, bankHeader *bh, UINT32 curr_offset)
{
  bankHeader *cbh;
  UINT32 tmp;
  int nidx;
  int datasz;
  char path[256];
  char name[256];
  int i;

  //printf("calling ls_r (%s) %d\n",ppath,strlen(ppath));

  getBankInfo(bh,  tmp,  nidx, datasz);

  // strcpy(path, ppath);
  //strcat(path, "/");
  getName(bh,name);
  //strcat(path,name);

  fs_inode *newn = add_inode(node, name, curr_offset, datasz, bh);
  if(!newn) return;

  //printf("abcd %s\n",path);
  //printf("new path will be... %s  (nidx=%d)\n",path,nidx);
  if(nidx <= 0) {
    //printf("--->  (%6d) %s\n",datasz,path);
    
    return;
  }

  //printf("nidx=%d\n",nidx);

  for(i=0;i<nidx;i++) {
    int off = getChildBank(bh, i, datasz);
    // printf("%s    child[%d].offset = %d (0x%x) sz=%d (0x%x)  nidx=%d\n",path,i,off,off,datasz,datasz,nidx);
    if(off <= 0) continue;

    cbh = readBank(fd, curr_offset + off);
    if(!cbh) {
      printf("Error... No bank read at curr_offset=%d + off=%d\n",curr_offset,off);
      continue;
    }

    // printf("i=%d\n",i);
    addNode_r(fd, newn, path, cbh, curr_offset + off);
    free(cbh);
  }

  //printf("returning from ls_r=%s\n",path);
}

void daqr_index::build(int fd, fs_inode *root)
{
  UINT32 curr_offset;
  UINT32 next_offset;
  bankHeader *bh;
  int swap;

  curr_offset = 0;
  next_offset = 0;
  for(;;) {
    bh = readBank(fd,curr_offset);
    if(!bh) {
      printf("End of file\n");
      return;
    }

    if(memcmp(bh->bank_type, "LRHD", 4) == 0) {
      swap = (bh->byte_order == 0x04030201) ? 0 : 1;
      // printf("/LRHD_%02d\n",qswap32(swap, bh->bank_id));

      LOGREC *logrec = (LOGREC *)bh;
      char *buff = (char *)bh;
      buff += sizeof(bankHeader);

      // Read the rest...
      ::read(fd, buff, qswap32(swap,bh->length) * 4 - sizeof(bankHeader));

      next_offset = curr_offset + qswap32(swap, logrec->length) * 4;
      if(memcmp(logrec->record_type, "DATA", 4) == 0) {
	if(run == 0) run = qswap32(swap, logrec->lh.run);
      }

      // printf("[%s] 0x%x curr=%d next=%d len=%d\n",logrec->record_type,bh->byte_order,curr_offset,next_offset,qswap32(swap,logrec->length) *4);
      curr_offset += qswap32(swap, bh->length) * 4;
    }
    else {
      //printf("curr_offset = %u   next_offset = %u\n",curr_offset, next_offset);

      if(memcmp(bh->bank_type, "DATAP", 5) != 0) {
	//printf("bank type = %s\n",bh->bank_type);
	return;
      }
      addNode_r(fd, root, "", bh, curr_offset);
      curr_offset = next_offset;
    }

    free(bh);
  }
}

int daqr_index::_create()
{
  root = alloc_inode("/",0,0);

  strcpy(cwd, "/");
  build(wfile.fd,root);
  return 0;
}







  


  
  
