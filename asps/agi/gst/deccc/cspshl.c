/*
 * $Id: cspshl.c,v 1.1 1998/04/10 15:30:58 fisyak Exp $
 *
 * $Log: cspshl.c,v $
 * Revision 1.1  1998/04/10 15:30:58  fisyak
 * Move gstar into CVS
 *
 */
#include "pawlib/comis/comis/pilot.h"
#if (defined(CERNLIB_HPUX))&&(defined(CERNLIB_SHL))
/*CMZ :          18/05/97  14.03.45  by  Pavel Nevski*/
/*-- Author :*/
#include <string.h>
#include <stdlib.h>
#include <dl.h>
 
void perror();
int  cs_shl_load_(path, n)
   char *path;
   int n;
{
   shl_t  handle;
   char   lib_name[80];
/* int    flags=BIND_DEFERRED; */
   int    flags=BIND_IMMEDIATE | BIND_VERBOSE;
/* int    flags=BIND_IMMEDIATE | BIND_NONFATAL; */
   long   address=0L;
   extern int errno;
 
   strncpy(lib_name, path, n);  lib_name[n] = '\0';
   handle = shl_load(lib_name, flags, address);
   return (errno);
}
/****************************************************************************/
long cs_get_func_(sym,n)
   char *sym;
   int n;
{
   shl_t handle;
   short type;
   long  addr;
   char  func_name[80];
 
   strncpy(func_name, sym, n);  func_name[n] = '\0';
 
   handle = NULL;
   if (shl_findsym(&handle,func_name,TYPE_PROCEDURE,&addr) == 0) return(addr);
   else /* printf(" CS: function not found: %s\n",func_name); */ return (0L);
}
/****************************************************************************/
void cs_shl_unload_(path, n)
   char *path;
   int n;
{
   shl_t  handle;
   struct shl_descriptor *desc;
   char   lib_name[80];
   int    index;
 
   strncpy(lib_name, path, n);  lib_name[n] = '\0';
 
   /* find handle of shared library using its name */
   index  = 0;
   handle = NULL;
   while (shl_get(index++, &desc) == 0)
   { if (!strcmp(lib_name, desc->filename)) { handle = desc->handle; break; } }
 
   if (!handle)
   {  printf(" CS: Shared library not loaded: %s\n", lib_name);  return; }
 
   if (shl_unload(handle) == -1)
      printf(" CS: Could not unload shared library: %s\n", lib_name);
}
/****************************************************************************/
void cs_shl_symbols_(path, ns, symbol, n)
   char *path, *symbol;
   int  *ns;
   int   n;
{
   shl_t  handle;
   struct shl_descriptor *desc;
   char   lib_name[80];
   int    index, flags;
   short  type;
   static nsym;
   static struct shl_symbol *symbols;
 
   if (*ns == -1)
   {  strncpy(lib_name, path, n);   lib_name[n] = '\0';
 
      /* find handle of shared library using its name */
      index  = 0;
      handle = NULL;
      while (shl_get(index++, &desc) == 0)
      { if (!strcmp(lib_name,desc->filename)) { handle=desc->handle; break; } }
      if (!handle) { *ns = -2; return;  }
 
      nsym = shl_getsymbols(handle, TYPE_PROCEDURE,
                            EXPORT_SYMBOLS|NO_VALUES, malloc, &symbols);
      if (nsym == -1)
      {  printf(" CS: Could not get symbols from shared library: %s\n",
                lib_name);  *ns = -2; return;
      }
      *ns = 0;
   }
   else
   {  if (*ns >= nsym-1) { *ns = -2;  free(symbols);  return; }
      else                (*ns)++;
   }
   memset(symbol, ' ', 32);
   strncpy(symbol, symbols[*ns].name, strlen(symbols[*ns].name));
}
/****************************************************************************/
void cs_shl_get_(ns, symbol, n)
   char *symbol;
   int  *ns;
   int   n;
{  /* find name of ns [ns=0 for first] shared library  */
   struct shl_descriptor *desc;
 
   memset(symbol, ' ', n);
   if (shl_get(*ns, &desc) == 0)
      strncpy(symbol,desc->filename , strlen(desc->filename));
}
 
 
 
#endif
