/*:Copyright 1995, Lawrence Berkeley Laboratory
*:>---------------------------------------------------------------------
*:FILE:		dsu_showDS.c
*:DESCRIPTION:	Show DSL datasets and tables.
*:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:		-- STILL IN DEVELOPMENT --
*:HISTORY:	23dec96-v010a-cet- adapt to NEW_DSL
*:HISTORY:	12jun95-v000a-cet- extraction from other files
*:<---------------------------------------------------------------------
*/

#include <stdio.h>
#include "dstype.h"

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      long dsuPrintDataset
*:DESCRIPTION:  Recursively print dataSet and all its children.
*:ARGUMENTS:    DS_DATASET_T *pDataset  - pointer to dataSet
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
long 
dsuPrintDataset(DS_DATASET_T *pDataset)
{
   long i;
   size_t iclen;
   const char *typeSpec;

   if(pDataset->tid == 0) {
      printf(" ######################################## \n");
      printf("DATASET %s",pDataset->name);
      printf("[%d%%%d]\n",pDataset->elcount,pDataset->maxcount);
      for (i=0;i<pDataset->elcount;i++) {
         printf(" %4ld - ",i);
#ifndef OLD_DSL
         dsuPrintDataset(pDataset->p.link[i]);
#else	/* OLD_DSL */
         dsuPrintDataset(&pDataset->p.child[i]);
#endif	/* OLD_DSL */
      }
   }else{
      printf(" ---------------------------------------- \n");
      dsTypeSpecifier(&typeSpec,&iclen,pDataset->tid);
      printf("%s ",typeSpec);
      printf("%s",pDataset->name);
      printf("[%d%%%d];\n",pDataset->elcount,pDataset->maxcount);
   }

   return TRUE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      long dsuListDataset
*:DESCRIPTION:  Recursively List dataSet and all its children.
*:ARGUMENTS:    DS_DATASET_T *pDataset  - pointer to dataSet
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
long
dsuListDataset(DS_DATASET_T *pDataset)
{
   long i;

   if(pDataset->tid == 0) {
      printf(" ######################################## \n");
      printf("DATASET %s",pDataset->name);
      printf("[%d%%%d]\n",pDataset->elcount,pDataset->maxcount);
      for (i=0;i<pDataset->elcount;i++) {
         printf(" %4ld - ",i);
#ifndef OLD_DSL
         dsuListDataset(pDataset->p.link[i]);
#else	/* OLD_DSL */
         dsuListDataset(&pDataset->p.child[i]);
#endif	/* OLD_DSL */
      }
   }else{
      printf("%s",pDataset->name);
      printf("[%d%%%d];\n",pDataset->elcount,pDataset->maxcount);
   }

   return TRUE;
}

