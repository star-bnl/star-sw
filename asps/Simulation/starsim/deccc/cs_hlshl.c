/*
* $Id: cs_hlshl.c,v 1.3 2018/11/19 23:13:52 perev Exp $
* $Log: cs_hlshl.c,v $
* Revision 1.3  2018/11/19 23:13:52  perev
* 64bits function pointer ==> token
*
* Revision 1.2  2004/06/26 00:10:43  potekhin
* Removed an unused integer and added stdlib where appropriate.
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.7  2001/11/25 15:36:24  nevski
* dlfch is for hp only
*
* Revision 1.6  2001/11/25 15:27:16  nevski
* dlfch is for hp only
*
* Revision 1.5  2001/06/15 00:02:17  nevski
* merging differences between private cmz and this version
*
* Revision 1.2  2001/03/02 21:05:14  nevski
* cs loader for all platforms combined together
*
* Revision 1.1  2001/02/27 10:15:15  nevski
*  first working release
*/
#include "comis/pilot.h"
#define  MAXLENFL  256
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
int csToken(long);
void strNcpy (lib,path,n)
   char *lib,*path;
   int n;
{  int i;
   if (n>=MAXLENFL) printf(" CS_SHL error: name too long %d %256s \n",n,path);
   strncpy(lib, path, n);                lib[n] = ' ';
   for (i=n; i>0 && lib[i]==' '; i--)  { lib[i] = '\0'; }
}

#if defined(CERNLIB_HPUX)
#include <dl.h>
void perror();
int  cs_shl_load_(path, n)
   char *path;
   int n;
{
   shl_t  handle;
   char   lib_name[MAXLENFL];
/* int    flags=BIND_DEFERRED; */
   int    flags=BIND_IMMEDIATE | BIND_VERBOSE;
/* int    flags=BIND_IMMEDIATE | BIND_NONFATAL; */
   long   address=0L;
   extern int errno;

   strNcpy(lib_name, path, n);
   handle = shl_load(lib_name, flags, address);
   return (errno);
}
/****************************************************************************/
long cs_get_func_(sym,n)
   char *sym;
   int n;
{
   shl_t handle=NULL;
   short type;
   long  addr;
   char  func_name[MAXLENFL];

   strNcpy(func_name, sym, n);
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
   char   lib_name[MAXLENFL];
   int    index;

   strNcpy(lib_name, path, n);

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
   char   lib_name[MAXLENFL];
   int    index, flags;
   short  type;
   static nsym;
   static struct shl_symbol *symbols;

   if (*ns == -1)
   {
      strNcpy(lib_name, path, n);

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

#else    //#if !defined(CERNLIB_HPUX)

#if defined(CERNLIB_ALPHA_OSF)
# define  ALPHA_OSF
#endif

#include <dlfcn.h>

/* new strategy - if no GLOBAL defined, use LAZY */
#ifdef  RTLD_GLOBAL
# define RTLD_NOW_CONST (RTLD_NOW | RTLD_GLOBAL)
#else
# define RTLD_NOW_CONST  RTLD_LAZY
#endif

/****************************************************************************/

struct procedures
{  char procname[32];
   int  (*funcptr)();
   struct procedures *next;
};

struct files
{  char filename[MAXLENFL];
   void              *file_handle;
   struct procedures *first_proc;
   struct files      *next;
};

static struct files *first_file = NULL;
static int    debug_level = 0;
extern int    csdebug_,gcflag_;

/****************************************************************************/
struct files *searchfile(filename, f)
  char *filename;
  struct files *f;
{
  while (f != NULL)
  { if (strcmp(filename,f->filename) == 0) return(f);  else f = f->next; }
  return(f);
}
/****************************************************************************/
struct procedures *searchproc(procname, p)
  char *procname;
  struct procedures *p;
{
  while (p != NULL)
  { if (strcmp(procname,p->procname) == 0) return(p);  else p = p->next; }
  return(p);
}
/****************************************************************************/
void Delete_all(f)
struct files *f;
{
 struct procedures *p;

 while (f->first_proc != NULL)
 {  p = f->first_proc;  f->first_proc = p->next;  free(p);  }
}
/****************************************************************************/
int  cs_shl_load_(path, n)
   char *path;
   int n;
{
   struct files *f;
   void*  file_handle;
   char   lib_name[MAXLENFL];

   strNcpy(lib_name, path, n);

    if (strcmp(lib_name, "./0.sl") == 0)
    {  file_handle = dlopen( NULL,    RTLD_NOW_CONST); }
    else
    {  file_handle = dlopen(lib_name, RTLD_NOW_CONST); }

    if (!file_handle) { printf("  %s \n",dlerror() ); return 1; }

/*   Add new file to the files list */

   f = (struct files *) malloc(sizeof(struct files));
   strcpy(f->filename,lib_name);
   f->file_handle = file_handle;
   f->next        = first_file;
   f->first_proc  = NULL;
   first_file     = f;
   return 0;
}
/****************************************************************************/
void cs_shl_unload_(path, n)
   char *path;
   int n;
{
   struct files *f,  *before;
   char   lib_name[MAXLENFL];

   strNcpy(lib_name, path, n);

   /*       find file    */

   f = searchfile(lib_name,first_file);
   if (f == NULL)
   {  if (debug_level > 0) printf("File not found.\n");  return; }

   if (f != first_file)
   {  before = first_file;
      while (before->next != f) before = before->next;
      before->next = f->next;
   }
   else
   {  first_file = f->next; }
   Delete_all(f);

   if (dlclose(f->file_handle) != 0)
   {  if (debug_level > 0) printf("Error in dlclose()...\n");  return; }
   free(f);
   if (debug_level > 0) printf("Unlink %s file.\n",lib_name);
   return;
}
/****************************************************************************/
int cs_get_func_(sym,n)
   char *sym;
   int n;
{
   struct procedures *p;
   struct files      *f;
   void   *fill_procaddr;
   char   procname[MAXLENFL];
   strNcpy(procname, sym, n);

/* --   Search for all files -- */

   f = first_file;
   while (f != NULL)
   { p = searchproc(procname, f->first_proc);
     if (p != NULL) return csToken(p->funcptr);
     fill_procaddr =  dlsym(f->file_handle, procname);
     if (fill_procaddr != (void *) NULL)
     {   p = (struct procedures *) malloc(sizeof(struct procedures));
         strcpy(p->procname, procname);
         p->funcptr = (int (*) ()) fill_procaddr;
         p->next = f->first_proc;
         f->first_proc = p;
         return csToken(p->funcptr);
     }
     f = f->next;
   } /* end while */
  return 0;
}
/****************************************************************************/
void cs_shl_get_(ns, libname, n)
   char *libname;
   int  *ns;
   int   n;
{  /* find name of ns [ns=0 for first] shared library  */
   struct files *f;
   int i=0;

   f=first_file;
   while (f != NULL && i < *ns)  { f = f->next;  i++; }

   memset(libname, ' ', n);
   if (f != NULL) strncpy(libname, f->filename, strlen(f->filename));
}
/****************************************************************************/
void cs_shl_symbols_(path, ns, symbol, n, nsy)
   char *path, *symbol;
   int  *ns;
   int   n, nsy;
{  char   lib_name[MAXLENFL];
   struct files  *f;
   static struct procedures *p;

   if (*ns == -1)
   {
      strNcpy(lib_name, path, n);

      /* find shared library using its name */
      f = first_file;  *ns = -2;
      while (f != NULL)
      {  if (!strcmp(lib_name, f->filename))  { p = f->first_proc;  break; }
         else f = f->next;
      }
      if (f == NULL) return;
      if (p == NULL)
      { if(csdebug_) printf(" CS: no symbols in shared library %s\n",lib_name);
        return;
      }
     *ns = 0;
   }
   else {  if (p == NULL) { *ns = -2; return; }  }

   memset(symbol, ' ', 32);
   strncpy(symbol, p->procname, strlen(p->procname));
   p = p->next;  (*ns)++;
}
#endif
