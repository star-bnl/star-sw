#include <string.h>
#include "asuAlloc.h"
#include "cfortran.h"

#include "hbook.h"

#include "hbkCWN.h"

/* A few extra def's not already in hbook.h */
/*#ifdef hpux *
#define HNTVDEF(A1,A2,A3,A4,A5) CCALLSFSUB5(HNTVDEF_,hntvdef_,INT,INT,PSTRING,PSTRING,PINT,A1,A2,A3,A4,A5);
#define HNTVAR(A1,A2,A3,A4,A5,A6,A7,A8) CCALLSFSUB8(HNTVAR_,hntvar_,INT,INT,PSTRING,PSTRING,PINT,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8);
#define HGTDIR(A1) CCALLSFSUB1(HGTDIR_,hgtdir_,PSTRING,A1);
#define HNSIZE(A1,A2,A3) CCALLSFSUB3(HNSIZE_,hnsize_,PINT,PINT,PINT,A1,A2,A3);
#else
*/
#define HNTVDEF(A1,A2,A3,A4,A5) CCALLSFSUB5(HNTVDEF,hntvdef,INT,INT,PSTRING,PSTRING,PINT,A1,A2,A3,A4,A5);
#define HNTVAR(A1,A2,A3,A4,A5,A6,A7,A8) CCALLSFSUB8(HNTVAR,hntvar,INT,INT,PSTRING,PSTRING,PINT,PINT,PINT,PINT,A1,A2,A3,A4,A5,A6,A7,A8);
#define HGTDIR(A1) CCALLSFSUB1(HGTDIR,hgtdir,PSTRING,A1);
#define HNSIZE(A1,A2,A3) CCALLSFSUB3(HNSIZE,hnsize,PINT,PINT,PINT,A1,A2,A3);
/*#endif */

/*--------------------------------------------------------------------*/
/* A simple wrapper around HBNT. */
STAFCV_T
hbkCWNbook(long hid, char *title) {
  HBNT((int)hid, title, "");
  
  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T 
hbkCWNscalarBlock(long id, char *blockName, int *dataPtr, char *chform) {
  HBNAME(id, blockName, dataPtr, chform);

  return STAFCV_OK;
}    

/*--------------------------------------------------------------------*/
/* Unfortunately, the macros in cfortran.h don't seem to be able to
   handle the situation we're facing here.  In order to pass a vector
   of strings, they need to know the size of each element - there's no
   provision for just passing the address of the beginning of the
   data.  */
STAFCV_T 
hbkCWNcharBlock(long id, char *blockName, char *dataPtr, char *chform) {
  /* This is a hardwired C/Fortran linkage. */
  /* #ifndef WIN32 */
#if 0
  hbnamc_(&id, blockName, dataPtr, chform, strlen(blockName), 4,
	  strlen(chform));
#else
 /*  hbnamc_(&id, blockName, dataPtr, chform, strlen(blockName), 4,
 	  strlen(chform));
  */
  HBNAMC(id, blockName, dataPtr, chform);

#endif
  /* The follow code looks like it ought to work, but it doesn't - see
     comments above.  
     
      */

  return STAFCV_OK;
}    

/*--------------------------------------------------------------------*/
size_t 
hbkCWNentryCount(long hid) {
  int noEnt;

  HNOENT(hid, noEnt);

  return (size_t)noEnt;
}

/*--------------------------------------------------------------------*/
size_t 
hbkCWNcolumnCount(long hid) {  
  size_t numEntries;
  int iCol, lCol, cCol;

  /* This is just a painless way to make sure that the proper ntuple 
     structures are being pointed to before the call to HNSIZE. */
  numEntries = hbkCWNentryCount(hid);

  /* Get the number of integer, real and character columns. */
  HNSIZE(iCol, lCol, cCol);
  
  return (size_t)(iCol + lCol + cCol);
}

/*--------------------------------------------------------------------*/
size_t
hbkCWNblockCount(long hid) {
  size_t numColumns, numBlocks;
  char oldBlock[9], newBlock[9], chtag[9]; /* maximum length 8 chars */
  int nsub, itype, isize, ielem;
  int i;

  /* Determine the number of columns */
  numColumns = hbkCWNcolumnCount(hid);
  numBlocks = 0;

  strcpy(oldBlock,"");
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,newBlock,nsub,itype,isize,ielem);
    if (strcmp(newBlock,oldBlock) != 0) {
      numBlocks++;
      strcpy(oldBlock,newBlock);
    }
  }

  return numBlocks;
}

/*--------------------------------------------------------------------*/
/* Return the name of the i'th block.  The C API will number blocks
   from 0, even though the underlying Fortran code numbers them from
   1. */
char * 
hbkCWNblockName(long hid, size_t iblock) {
  size_t numColumns, numBlocks, iBlock;
  char oldBlock[9], newBlock[9], chtag[9]; /* maximum length 8 chars */
  int nsub, itype, isize, ielem;
  int i;
  char *cp = 0;

  /* Be explicit about numbering from 0. */
  iBlock = iblock - 1;

  /* Determine the number of columns */
  numColumns = hbkCWNcolumnCount(hid);
  numBlocks = 0;
  
  strcpy(oldBlock,"");
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,newBlock,nsub,itype,isize,ielem);
    if (strcmp(newBlock,oldBlock) != 0) {
      strcpy(oldBlock,newBlock);
      numBlocks++;
      if (numBlocks == iBlock) {
	break;
      }
    }
  }

  /* Make sure we actually found the block we were looking for. */
  if (numBlocks == iBlock) {
    cp = strdup(newBlock);
  }

  return cp;
}

/*--------------------------------------------------------------------*/
/* Return the chform of the i'th block.  The C API will number blocks
   from 0, even though the underlying Fortran code numbers them from
   1. */
char *
hbkCWNblockChform(long hid, size_t iblock) {
  size_t numColumns, numBlocks, iBlock;
  char oldBlock[9], newBlock[9]; /* maximum length 8 chars */
  char chtag[33];
  int nsub, itype, isize, ielem;
  int i, j;
  char *cp = 0;

  /* Be explicit about numbering from 0. */
  iBlock = iblock - 1;

  /* Determine the number of columns */
  numColumns = hbkCWNcolumnCount(hid);
  numBlocks = 0;
  
  strcpy(oldBlock,"");
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,newBlock,nsub,itype,isize,ielem);
    if (strcmp(newBlock,oldBlock) != 0) {
      strcpy(oldBlock,newBlock);
      numBlocks++;
      if (numBlocks == iBlock) {
	break;
      }
    }
  }

  /* Make sure we actually found the block we were looking for. */
  if (numBlocks == iBlock) {
    /* The variable `i' still points to the first column of the block. */
    for (j = i; j <= numColumns; j++) {
      HNTVDEF(hid,j,chtag,newBlock,itype);
      if (strcmp(newBlock,oldBlock) != 0) {
	break; /* We've left the block of interest. */
      }
      if (cp) {
	strcat(cp,chtag);
      } else {
	cp = strdup(chtag);
      }
    }
  }

  return cp;
}

/*--------------------------------------------------------------------*/
size_t
hbkCWNcolumnSize(long hid, size_t icolumn) {
  size_t numColumns;
  char block[9];
  char chtag[33];
  int nsub, itype, isize, ielem;

  numColumns = hbkCWNcolumnCount(hid);
  if (icolumn < 0 || icolumn >= numColumns) {
    return 0;
  }
  HNTVAR(hid,icolumn,chtag,block,nsub,itype,isize,ielem);
  
  return isize;
}

/*--------------------------------------------------------------------*/
/* Return the size of the i'th block.  The C API will number blocks
   from 0, even though the underlying Fortran code numbers them from
   1. */
size_t 
hbkCWNblockSize(long hid, size_t iblock) {
  size_t numColumns, numBlocks;
  size_t blockSize = 0;
  char block[9];
  char chtag[33];
  int nsub, itype, isize, ielem;
  int i;
  char *cp;

  /* Make sure iblock points to a valid block. */
  numBlocks = hbkCWNblockCount(hid);
  if (iblock < 0 || iblock >= numBlocks) {
    return blockSize;
  }
  
  cp = hbkCWNblockName(hid,iblock);
  
  numColumns = hbkCWNcolumnCount(hid);
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,block,nsub,itype,isize,ielem);
    if (strcmp(block,cp) == 0) {
      blockSize += isize;
    }
  }
  
  return blockSize;
}

/*--------------------------------------------------------------------*/
size_t 
hbkCWNblockOffset(long hid, size_t iblock) {
  size_t numColumns, numBlocks;
  size_t blockOffset = 0;
  char block[9];
  char chtag[33];
  int nsub, itype, isize, ielem;
  int i;
  char *cp;

  /* Make sure iblock points to a valid block. */
  numBlocks = hbkCWNblockCount(hid);
  if (iblock < 0 || iblock >= numBlocks) {
    return blockOffset;
  }
  
  cp = hbkCWNblockName(hid,iblock);
  numColumns = hbkCWNcolumnCount(hid);
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,block,nsub,itype,isize,ielem);
    if (strcmp(block,cp) == 0) {
      break;
    }
  }
  FREE(cp);

  return blockOffset;
}

/*--------------------------------------------------------------------*/
unsigned char
hbkCWNblockIsChar(long hid, size_t iblock) {
  size_t numColumns, numBlocks;
  char block[9];
  char chtag[33];
  int nsub, itype, isize, ielem;
  int i;
  char *cp;

  /* Make sure iblock points to a valid block. */
  numBlocks = hbkCWNblockCount(hid);
  if (iblock < 0 || iblock >= numBlocks) {
    EML_ERROR(INVALID_BLOCK);
  }
  
  cp = hbkCWNblockName(hid,iblock);
  numColumns = hbkCWNcolumnCount(hid);
  for (i = 1; i <= numColumns; i++) {
    HNTVAR(hid,i,chtag,block,nsub,itype,isize,ielem);
    if (strcmp(block,cp) == 0) {
      break;
    }
  }
  FREE(cp);

  return (unsigned char)itype;
}

/*--------------------------------------------------------------------*/
STAFCV_T
hbkCWNclear(long hid) {
  HRESET(hid, "");
  /* Eventually, this should identify the directory, if any,
     associated with this ntuple and HSCR the ntuple buffers from
     there. */

  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
size_t 
hbkCWNcolumnOffset(long hid, size_t iColumn) {
  size_t i;
  size_t offset = 0;

  /* Should make sure this is a valid column first. */
  for (i = 0; i < iColumn; i++) {
    offset += hbkCWNcolumnSize(hid, i);
  }
  
  return offset;
}

/*--------------------------------------------------------------------*/
char *
hbkCWNcolumnTag(long hid, size_t icolumn) {
  char chtitl[32];
  int nvar;
  char chtag[100][8];
  float rmin[100], rmax[100];
  char *tag;

  HGIVEN(hid, chtitl, nvar, chtag, rmin[0], rmax[0]);
  tag = strdup(chtag[icolumn]);

  return tag;
}

/*--------------------------------------------------------------------*/
NT_TYPE_CODE_T
hbkCWNcolumnType(long hid, size_t icolumn) {
  NT_TYPE_CODE_T types[]={NT_TYPE_LONG,NT_TYPE_FLOAT,NT_TYPE_FLOAT,NT_TYPE_FLOAT,NT_TYPE_CHAR,NT_TYPE_FLOAT};
  return types[icolumn];
}

/*--------------------------------------------------------------------*/
size_t 
hbkCWNcolumnDimension(long hid, size_t icolumn) {
  size_t dims[]={1,1,1,1,16,3};
  return dims[icolumn];
}

/*--------------------------------------------------------------------*/
char * 
hbkCWNcolumnChform(long hid, size_t icolumn) {
  char chtag[32], block[8];
  int itype;
  char *chform;

  HNTVDEF(hid, icolumn, chtag, block, itype);
  chform = strdup(chtag);

  return chform;
}

/*--------------------------------------------------------------------*/
char * 
hbkCWNtitle(long hid) {
  char chtitl[32];
  int nvar;
  char chtag[100][8];
  float rmin[100], rmax[100];
  char *title;

  HGIVEN(hid, chtitl, nvar, chtag, rmin[0], rmax[0]);
  title = strdup(chtitl);

  return title;
}

/*--------------------------------------------------------------------*/
STAFCV_T
hbkCWNhprnt(long hid) {

  HPRNT(hid);

  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T 
hbkCWNsetDataBuffer(long hid, size_t iblock, char *buffer) {
  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T
hbkCWNputRowBlock(long hid, size_t irow, size_t iblock) {
  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T
hbkCWNputRow(long hid) {

  HFNT((int)hid);
  
  return STAFCV_OK;
}

/*--------------------------------------------------------------------*/
STAFCV_T
hbkCWNgetRowBlock(long hid, size_t irow, size_t iblock) {
  return STAFCV_OK;
}

