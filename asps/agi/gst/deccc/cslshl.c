/*
 * $Id: cslshl.c,v 1.1 1998/04/10 15:30:57 fisyak Exp $
 *
 * $Log: cslshl.c,v $
 * Revision 1.1  1998/04/10 15:30:57  fisyak
 * Move gstar into CVS
 *
 */
#include "pawlib/comis/comis/pilot.h"
#if !defined(CERNLIB_HPUX)
/*CMZ :          05/11/97  15.53.29  by  Pavel Nevski*/
/*-- Author : Vladimir Berejnoi*/
#if defined(CERNLIB_ALPHA_OSF)
#define  ALPHA_OSF
#endif
 
#include "dlfcn.h"
#include <string.h>
 
#define MAXLENFL        60
#ifndef RTLD_NOW
#define RTLD_NOW	2	/* immediate function call binding */
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL	0x100	/* allow symbols to be global */
#endif
#define RTLD_NOW_CONST (RTLD_NOW || RTLD_GLOBAL)
 
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
   void   *file_handle;
   char   lib_name[MAXLENFL];
 
   strncpy(lib_name, path, n);  lib_name[n] = '\0';
 
    if (strcmp(lib_name, "./0.sl") == 0)
    {  file_handle = dlopen( NULL,    RTLD_NOW_CONST); }
    else
    {  file_handle = dlopen(lib_name, RTLD_NOW_CONST); }
    if (file_handle == NULL)
    {  printf("  %s \n",dlerror() );  return 1;        }
 
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
 
   strncpy(lib_name, path, n);  lib_name[n] = '\0';
 
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
void * cs_get_func_(sym,n)
   char *sym;
   int n;
{
   struct procedures *p;
   struct files      *f;
   void   *fill_procaddr;
   char   procname[80];
#ifdef ALPHA_OSF
         int jumpad_();
         unsigned long ptr = (unsigned long)jumpad_;
#endif
   strncpy(procname, sym, n);  procname[n] = '\0';
 
/* --   Search for all files -- */
 
   f = first_file;
   while (f != NULL)
   { p = searchproc(procname, f->first_proc);
     if (p != NULL) return (void *)(p->funcptr);
     fill_procaddr =  dlsym(f->file_handle, procname);
     if (fill_procaddr != (void *) NULL)
     {   p = (struct procedures *) malloc(sizeof(struct procedures));
         strcpy(p->procname, procname);
#ifdef ALPHA_OSF
         ptr = (unsigned long) fill_procaddr - ptr;
         p->funcptr = (int (*) ()) ptr;
#else
         p->funcptr = (int (*) ()) fill_procaddr;
#endif
         p->next = f->first_proc;
         f->first_proc = p;
         return (void *)(p->funcptr);
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
   {  strncpy(lib_name, path, n);  lib_name[n] = '\0';
 
      /* find shared library using its name */
      f = first_file;  *ns = -2;
      while (f != NULL)
      {  if (!strcmp(lib_name, f->filename))  { p = f->first_proc;  break; }
         else f = f->next;
      }
      if (f == NULL) return;
      if (p == NULL)
      { printf(" CS: no symbols in shared library: %s\n",lib_name); return;}
 
     *ns = 0;
   }
   else {  if (p == NULL) { *ns = -2; return; }  }
 
   memset(symbol, ' ', 32);
   strncpy(symbol, p->procname, strlen(p->procname));
   p = p->next;  (*ns)++;
}
 
 
 
#endif
