/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:		dsuType.h
*:DESCRIPTION:	Include file for using DSU functions.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		-- NONE KNOWN --
*:HISTORY:	12jun95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

#ifndef NULL
#define NULL 0
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

/*----------------------------------
*  typedefs
*/
   typedef enum cwn_block_type_t {
      UNDEFINED=0,
      CHAR_BLOCK,
      NUMB_BLOCK,
      UNKNOWN
   }CWN_BLOCK_TYPE_T;

/*----------------------------------
*  prototypes
*/
CWN_BLOCK_TYPE_T block_type(DS_TYPE_CODE_T type);
char* block_name(const char *name,long n);
char* col2spec(const char *name,DS_TYPE_CODE_T type,size_t dims,size_t *dim);
long dsl2cwn(DS_DATASET_T *pDataset,long hid);
long dsuPrintDataset(DS_DATASET_T *pDataset); /* Was dspPrintDataset.
						 Had to be a typo. */
long xdf2rzd(int lunn, char* inFile);
int dsuDoCuts(size_t, char *, char *, DS_DATASET_T *);

/* Added because tbr uses it */
extern int dsuRowPassedCuts(char *, long);

#ifndef linux
char* basename(char* filename);
#endif
