/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:		xdf2rzd.c
*:DESCRIPTION:	Converts a XDF file to a RZD file.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		18may95-v000a- Only works on flat datasets.
*:BUGS:		-- STILL IN DEVELOPMENT --
*:HISTORY:	18may95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

#include <stdio.h>
#include <string.h>
#include "dstype.h"
#include "dsxdr.h"
#include "dsuType.h"
#include "cfortran.h"
#include "hbook.h"

/*----------------------------------
*  globals
*/
char cdir[32]="//", *name=&cdir[2];

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	long xdf2rzd
*:DESCRIPTION:	Convert a DSL file to a CWN file.
*:ARGUMENTS:	int lunn	:RONLY:  logical unit number
*:ARGUMENTS:	char* inFile	:RW:IN:  DSL filename.
*:ARGUMENTS:			:RW:OUT: CWN filename.
*:RETURN VALUE:	TRUE or FALSE
*:<---------------------------------------------------------------------
*/
long xdf2rzd(int lunn, char* inFile)
{
   long status, event;
/*- Input xdf file. -*/
   FILE *inputFile = NULL;
   char *type="rb";
   DS_DATASET_T *pInputDS = NULL;
   XDR inputXdr;
/*- Output rzd file. -*/
   char outFile[128];
   int record_size=1024, istat=0, icycle=0;

/*- Calculate file names. -*/
      strcpy(name,inFile);
      strcpy(outFile,basename(name));
      strcat(outFile,".rzd");

/*- Open the OUTPUT file. -*/
   HROPEN(lunn,name,outFile,"N",record_size,istat);
   HBSET("BSIZE",4096,istat);

/*- Open the INPUT file. -*/
   if ((inputFile = fopen(inFile, type)) == NULL) {
      dsPerror("xdf2rzd.E0- file open failed ");
      return 0;
   }

/*- Create a dataSet pointer associated with the file. -*/
   xdrstdio_create(&inputXdr, inputFile, XDR_DECODE);

   event=0;
   while(TRUE) {
/*- Read dataset. -*/
      pInputDS = NULL;
      if ( !xdr_dataset(&inputXdr, &pInputDS) ) {
	 dsPerror("xdf2rzd.E1- error reading dataset");
	 break;
      }
      dsuPrintDataset(pInputDS);
      status = dsl2cwn(pInputDS,100*event+1);
      event++;
   }

/*- Close INPUT file, free dataset, and destroy xdr. -*/
   if(pInputDS != NULL) {
      dsFreeDataset(pInputDS);
      XDR_DESTROY(&inputXdr);
      fclose(inputFile);
#ifndef OLD_DSL
      dsAllocStats();                      /* show allocation stats */
#else   /*OLD_DSL*/
      dsDatasetAllocStats();               /* show allocation stats */
#endif  /*OLD_DSL*/

   }

/*- Close OUTPUT file, cleanup ZEBRA directory. -*/
/* HCDIR(cdir," "); */
   HROUT(0, icycle," ");
   HREND(name);

   return status;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	long dsl2cwn
*:DESCRIPTION:	Recursively create CWNs from DSL tables.
*:ARGUMENTS:	DS_DATASET_T *pDataset  - pointer to dataSet
*:ARGUMENTS:	long hid		- base HBOOK ID
*:RETURN VALUE:	Last HBOOK ID used.
*:<---------------------------------------------------------------------
*/
#define LAST_BTYPE(A) (A > 0 ? btype[A-1] : btype[0])
long dsl2cwn(DS_DATASET_T *pDataset,long hid)
{
   long status;
   long ie,ic,ir,ib;
   bool_t result;

   size_t entries;			/* dataset dimension */
   DS_DATASET_T *pTable = NULL;		/* table ptr */
   const char *name;			/* table name */
   char *pData;				/* ptr to table data */
   int  *pDataI;
   size_t nrows,ncols,maxrows;		/* table dimensions */
   const char *cname;				/* column name */
   DS_TYPE_CODE_T ctype;		/* column type code */
   size_t cdims, cdim[20];		/* column dimensions */
   char bspec[2048];			/* cwn spec */
   long nb;				/* cwn block number */
   long bcolm[20];			/* block column no.s */
   CWN_BLOCK_TYPE_T btype[20];		/* cwn block types */

/*- Recursively handle datasets. -*/
   if ( !dsIsDataset(&result, pDataset) ) {
      dsPerror("dsl2cwn.E0- bad dataset ");
      return 0;
   } else if ( result ) {
      if( !dsDatasetEntryCount(&entries,pDataset) ) {
	 dsPerror("dsl2cwn.E1- bad dataset ");
	 return 0;
      } else {
	 for(ie=0;ie<entries;ie++) {
	    if ( !dsDatasetEntry(&pTable, pDataset, ie) ) {
	       dsPerror("dsl2cwn.E2- bad entry ");
	       return hid;
	    }
	    hid++;
	    status = dsl2cwn(pTable,hid);
	 }
	 return status;
      }
   }

/*- Handle individual table. -*/
   pTable = pDataset;
   nb = 0;
   bcolm[nb] = 0;
   btype[nb] = UNDEFINED;
   strcpy(bspec,"");
   if(  !dsTableName(&name, pTable)
   ||   !dsTableColumnCount(&ncols, pTable)
   ||   !dsTableDataAddress(&pData, pTable) ) {
      dsPerror("dsl2cwn.E3- bad table ");
    return 0;
   } else {
/*    HCDIR(cdir," "); */
      HBNT(hid,((char*)name)," ");
      for(ic=0;ic<ncols;ic++) {
	 if( !dsColumnName(&cname, pTable, ic)
	 ||  !dsColumnTypeCode(&ctype, pTable, ic)
	 ||  !dsColumnDimCount(&cdims, pTable, ic)
	 ||  !dsColumnDimensions(cdim, pTable, ic) ) {
	    dsPerror("dsl2cwn.E5- bad column ");
	 } else {
	    if( LAST_BTYPE(nb) != block_type(ctype) ) {
	       if(LAST_BTYPE(nb) != UNDEFINED) {
		  bspec[strlen(bspec)-1]='\0';
		  switch (btype[nb-1]) {
		     case CHAR_BLOCK: /* should use HBNAMC */
		       HBNAMC(hid,block_name(name,nb-1),pData,bspec);
		       break;
		     case NUMB_BLOCK:
		       pDataI = (int *)pData;
		       HBNAME(hid,block_name(name,nb-1),pDataI,bspec);
		       break;
		     default:
			break;
		  }
	       }
	       bcolm[nb] = ic;
	       btype[nb] = block_type(ctype);
	       nb++;
	       strcpy(bspec,"");
	       if( !dsCellAddress(&pData,pTable,0,ic) ) {
		  dsPerror("dsl2cwn.E6- bad cell ");
	       } else
	       if( !dsTableDataAddress(&pData,pTable) ) {
		  dsPerror("dsl2cwn.E7- bad data ");
		  pData = NULL;
	       }
	    }
	    strcat(bspec,col2spec(cname,ctype,cdims,cdim));
	    strcat(bspec,",");
	 }
      }
      strcpy(&bspec[strlen(bspec)-1],"");
      switch (btype[nb-1]) {
	 case CHAR_BLOCK: /* should use HBNAMC */
	   HBNAMC(hid,(block_name(name,nb-1)),pData,bspec);
	   break;
	 case NUMB_BLOCK:
	   pDataI = (int *)pData;
	   HBNAME(hid,(block_name(name,nb-1)),pDataI,bspec);
	   break;
	 default:
	    break;
      }
      if(  !dsTableRowCount(&nrows, pTable) ) {
	 dsPerror("dsl2cwn.E3- bad row count ");
      } else {
/*	 dsPrintTableData(stdout, pTable); */
	 for(ir=0;ir<nrows;ir++) {
	    for(ib=0;ib<nb;ib++) {
	       if( !dsCellAddress(&pData, pTable, ir, bcolm[ib]) ) {
		  dsPerror("dsl2cwn.E8- bad cell ");
	       } else {
		  switch(btype[ib]) {
		     case CHAR_BLOCK: /* should use HBNAMC */
		       HBNAMC(hid,(block_name(name,ib)),pData,"$SET");
		       break;
		     case NUMB_BLOCK:
		       pDataI = (int *)pData;
		       HBNAME(hid,block_name(name,ib),pDataI,"$SET");
		       break;
		     default:
			break;
		  }
	       }
	    }
	    HFNT(hid);
	 }
      }
      HPRINT(hid); return hid;
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	CWN_BLOCK_TYPE_T block_type
*:DESCRIPTION:	Determines CWN block type from column type
*:ARGUMENTS:	(DS_TYPE_CODE_T type)
*:RETURN VALUE:	CWN block type.
*:<---------------------------------------------------------------------
*/
CWN_BLOCK_TYPE_T block_type(DS_TYPE_CODE_T type)
{
   switch (type) {
      case DS_TYPE_CHAR:
	 return CHAR_BLOCK;
	 break;
      case DS_TYPE_SHORT:
      case DS_TYPE_U_SHORT:
      case DS_TYPE_LONG:
      case DS_TYPE_U_LONG:
      case DS_TYPE_FLOAT:
      case DS_TYPE_DOUBLE:
	 return NUMB_BLOCK;
	 break;
      case DS_TYPE_OCTET:
      case DS_TYPE_STRUCT:
      default:
	 return UNKNOWN;
	 break;
   }
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	char* col2spec
*:DESCRIPTION:	Generates a CWN spec element for one column
*:ARGUMENTS:	(char *name,DS_TYPE_CODE_T type,size_t dims,size_t *dim)
*:RETURN VALUE:	CWN spec element
*:<---------------------------------------------------------------------
*/
char* col2spec(const char *name,DS_TYPE_CODE_T type,size_t dims,size_t *dim)
{
   static char spec[64];	/* storage of spec */
   char numb[16];
   long i;

   strcpy(spec,name);
   if( dims > 0
   &&  type != DS_TYPE_CHAR ) {
      strcat(spec,"(");
/*    sprintf(spec[strlen(spec)],"%d",dim[0]); */
      sprintf(numb,"%d",dim[0]);
      strcat(spec,numb);
      for(i=1;i<dims;i++){
	 strcat(spec,",");
	 sprintf(numb,"%d",dim[i]);
	 strcat(spec,numb);
      }
      strcat(spec,")");
   }
   strcat(spec,":");
   switch (type) {
      case DS_TYPE_CHAR:
	 strcat(spec,"C*");
	 sprintf(numb,"%d",dim[0]);
	 strcat(spec,numb);
	 break;
      case DS_TYPE_OCTET:
	 return "";
	 break;
      case DS_TYPE_SHORT:
	 strcat(spec,"I*2");
	 break;
      case DS_TYPE_U_SHORT:
	 strcat(spec,"U*2");
	 break;
      case DS_TYPE_LONG:
	 strcat(spec,"I*4");
	 break;
      case DS_TYPE_U_LONG:
	 strcat(spec,"U*4");
	 break;
      case DS_TYPE_FLOAT:
	 strcat(spec,"R*4");
	 break;
      case DS_TYPE_DOUBLE:
	 strcat(spec,"R*8");
	 break;
      case DS_TYPE_STRUCT:
	 return "";
	 break;
      default:
	 return "";
	 break;
   }
   return spec;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:	char* block_name
*:DESCRIPTION:	Determines CWN block name.
*:ARGUMENTS:	(char *name,long n)
*:RETURN VALUE:	CWN block name.
*:<---------------------------------------------------------------------
*/
char* block_name(const char *name,long n)
{
   static char bname[16];
   int lname,i,ii;
   lname = strlen(name); if (lname > 8) lname =8;
   memset(bname,' ',15); bname[15] = '\0';
   memcpy(bname,name,lname);
   
   if (n > 0 && n <100) {
     sprintf(bname+7,"%d",n);
     if (n < 10) bname[6] = ' ';
   }
   for (i=0,ii=0; bname[i]; i++) { if (bname[i] != ' ') bname[ii++] = bname[i];};
   memset(bname+ii,' ',8-ii); bname[8]= '\0';
   return bname;
}
#ifndef linux
/*
*:>---------------------------------------------------------------------
*:ROUTINE:	char* basename
*:DESCRIPTION:	Return basename of a filename.
*:ARGUMENTS:	(char* filename)
*:RETURN VALUE:	-- NONE --
*:<---------------------------------------------------------------------
*/
char* basename(char* filename)
{
   char *ss=filename;
   return strtok((((ss = strrchr(ss,'/')) != NULL) ? ss : filename)
	   , "/.");
}
#endif /* not linux*/
