/*CMZ :          14/03/98  22.05.30  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   28/11/97*/
/*****************************************************/
/*               S T A F   i n t e r f a c e         */
/*****************************************************/
#ifdef STAF
#include <stream.h>
#include <stdlib.h>
#include <string.h>
#include "asuLib.h"
#include "emlLib.h"
#include "socLib.h"
#include "spxLib.h"
#include "tdmLib.h"
#include "duiLib.h"
#include "dioLib.h"
#include "amiLib.h"
#include "tntLib.h"
#include "topLib.h"
extern "C" void staf_banner (FILE* stream);
#endif
 
extern "C" void staf_start_ () {
#ifdef STAF
   asu_init(); asu_start();
   eml_init(); eml_start();
   soc_init(); soc_start();
   spx_init(); spx_start();
   tdm_init(); tdm_start();
   dui_init(); dui_start();
   dio_init(); dio_start();
   ami_init(); ami_start();
   top_init(); top_start();
   tnt_init(); tnt_start();
   staf_banner(stdout);
#endif
}
 
extern "C" void staf_stop_ ()  {
#ifdef STAF
   tnt_stop();
   ami_stop();
   dio_stop();
   dui_stop();
   tdm_stop();
   spx_stop();
   soc_stop();
   eml_stop();
   asu_stop();
#endif
}
 
/*---------------------------------------------------------------------*/
 
extern "C" int staftab_(char* path, char* name, char* spec, long* l,
                        char* data, int lp, int ln, int ls)
{
#ifdef STAF
  tdmDataset*       tDs = NULL; // pointer to tdm class member function
  tdmTable*         aDs = NULL; // pointer to table finder function
  DS_DATASET_T*     pDs = NULL; // pointer to directory table
  DS_DATASET_T*     dDs = NULL; // pointer to dataset table
 
  char cpath[132];  cpath[0]=0;  strncat(cpath,path,lp);
  char cname[32];   cname[0]=0;  strncat(cname,name,ln);
  char cspec[2048]; cspec[0]=0;  strncat(cspec,spec,ls);
  long k=*l;
  //             this should work,  but it does not
  //  ier=dsNewDataset(&ds,cpath);  tdm->createTable(name, ds);
 
  if (!(tDs=tdm->findDataset(cpath))) dui->mkdir(cpath);
 
  if ( (tDs=tdm->findDataset(cpath))
       &&  (tDs->cvtDslPointer((DSL_PTR_T &)pDs)) && pDs)
     { for (int i=0;  i < pDs->elcount;  i++)
       { // printf (" dataset %d %d %d %s tested \n",i,pDs->p.link[i],
         //          (pDs->p.link[i])->flags, (pDs->p.link[i])->name);
         if ((dDs=pDs->p.link[i]) && dDs->flags && !strcmp(dDs->name,cname))
         {  if (k<0)
            { size_t rsize;  dsTableRowSize(&rsize,dDs);
              if (rsize)     k=(-k)/(rsize/sizeof(k));
              int d=k*rsize+(*l)*sizeof(k);
              if (!d) printf (" table %s rsize=%d k=%d %d\n",cname,rsize,*l,d);
            }
            dDs->maxcount = k;
            dDs->elcount  = k;
            dDs->p.data   = data;
            return          k;
            // return (DSL_PTR_T &) dDs;
       } }
       return dsAddTable(pDs,cname,cspec,k,&data);
     }
  printf (" staftab: directory %s not found \n",cpath);
#endif
  return 0;
}
 
 
