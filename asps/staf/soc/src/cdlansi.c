#include <stdio.h>
#include <string.h>
#include "asuAlloc.h"
#include "cdl.h"

static FuncPtr procList = NULL;
static FilesPtr fileList = NULL;
static int debug_level = 0;
struct cdldefs cdl_def;
 
void 
cdl_init()
{

/*
*  set defaults for path, language, compilers and loader options
*/
    strcpy(cdl_def.path, "/tmp/");
    strcpy(cdl_def.lang, "f77");
    cdl_def.ld[0] = '\0';
#if defined(__hpux)
    strcpy(cdl_def.f77, "f77 -c +z +ppu -K -O");
    strcpy(cdl_def.cc,  "cc -c +z -O");
    strcpy(cdl_def.CC,  "");
#endif

#ifdef AIX
    strcpy(cdl_def.f77, "xlf -qextname -qrndsngl -qcharlen=32767 -c");
    strcpy(cdl_def.cc,  "cc -c");
*    strcpy(cdl_def.CC,  "xlC -c");
    strcpy(cdl_def.CC,  "");
#endif

#if defined(SUN) && defined(SOLARIS)
/* increase some internal tables: up to 100 nested control statements,
   200 continuation lines, 10000 identifier names per module
*/
    strcpy(cdl_def.f77, "/opt/SUNWspro/bin/f77 -Nc100 -Nl200 -Nn10000 -c -pic");
    strcpy(cdl_def.cc,  "/opt/SUNWspro/bin/cc -c -K pic");
    strcpy(cdl_def.CC,  "");
#endif

#if defined(SUN) && (!defined(SOLARIS))
    strcpy(cdl_def.f77, "f77 -c -pic");
    strcpy(cdl_def.cc,  "cc -c -pic");
    strcpy(cdl_def.CC,  "");
#endif

#ifdef SGI
    strcpy(cdl_def.f77, "f77 -c");
    strcpy(cdl_def.cc,  "cc -cckr -c");
    strcpy(cdl_def.CC,  "");
#endif

#if defined(__alpha)
    strcpy(cdl_def.f77, "f77 -c");
    strcpy(cdl_def.cc,  "cc -c");
    strcpy(cdl_def.CC,  "");
#endif

/*
*    Try add libraries loaded implicitly at startup time to filelist.
*/
#if defined(__hpux)
    {
    FilesPtr  f, fp;
    struct shl_descriptor *desc;
    int       ns = 0;

/*  link libs in reverse order */
    fp = NULL;
    while (shl_get(ns, &desc) == 0) {
      f = (FilesPtr) MALLOC(sizeof(Files));
      f->filename = strdup( desc->filename);
      f->file_handle = desc->handle;
      if (!fileList) fileList = f;
      f->next  = NULL;
      if(fp) fp->next = f;
      fp = f;
      ns++;
    }
  }
#endif

#if defined(SUN) || defined(SGI) || defined(__alpha)
    {
    char       lib_name[82];
    FilesPtr   f;
    FileHandle file_handle;

    strcpy(lib_name, "./0.sl");
    file_handle = dlopen(NULL, 1);
    if (file_handle ==  NULL) return;

/*   Add new file to the files list
*/
   f = (FilesPtr) MALLOC(sizeof(Files));
   f->filename = strdup(lib_name);
   f->file_handle = file_handle;
   f->next  = fileList;
   fileList = f;
  }
#endif
}

/* to call from Fortran level, maybe will better use cfortran.h */

void 
cdl_init_()
{
 cdl_init();
}

void 
cdl_set_opt(char *text, char *chvar)
{
    if (*chvar == 'p') {
        strcpy(cdl_def.path, text);
        if (*cdl_def.path == '\0') {
            strcpy(cdl_def.path, "./");
	  }

        if (cdl_def.path[strlen(cdl_def.path) - 1] != '/') {
            strcat(cdl_def.path, "/");
	  }
      } else if (*chvar == 'f') {
        strcpy(cdl_def.f77, text);

      } else if (*chvar == 'c') {
        strcpy(cdl_def.cc, text);

      } else if (strncmp(chvar, "ld", 2) == 0) {
        strcpy(cdl_def.ld, " ");
        strcat(cdl_def.ld, text);

      } else if (strncmp(chvar, "la", 2) == 0) {
        strcpy(cdl_def.lang, text);

      } else if (*chvar == 'C') {
        strcpy(cdl_def.CC, text);

      } else if (*chvar == '\0') {

        printf(" PATH=%s\n", cdl_def.path);
        printf(" FORT=%s\n", cdl_def.f77);
        printf("   cc=%s\n", cdl_def.cc);
        printf("   CC=%s\n", cdl_def.CC);
        printf("   LD=%s\n", cdl_def.ld);
        printf(" LANG=%s\n", cdl_def.lang);

      } else {
        printf(" cdl_set_opt: unknown option:%s",chvar);
        printf(" possible options are: path, fort, cc, CC, ld, lang\n");

      }
}

#if defined(AIX)

void cdl_create_imp(char *import, char *prog);
void cdl_fnparse(char *command,char *path,char *file,char *ext);
void getarg_(int *iarg, char *command, int size);

void cdl_get_import(char *fname)
{
#define STRSIZE 255
char command[STRSIZE], path[STRSIZE],
     file[STRSIZE],    ext[STRSIZE];
static char import[STRSIZE];
static int limport=0;
int iarg=0, lcomm;
   
    if(!limport) {
      getarg_(&iarg, command, STRSIZE);
      for(lcomm=STRSIZE-1; command[lcomm]==' ';lcomm--);
      command[++lcomm]='\0';
      printf(" command=%s \n",command);
      cdl_fnparse(command,path,file,ext);
      strcpy(import,file);
      strcat(import,"_Import.o");
      limport=strlen(import);
      if(!strlen(path)) {
        strcpy(path,command);
        strcpy(command,"$olddir/");
        strcat(command,path);
      }
      cdl_create_imp(import,command);
    }
    strcpy(fname,import);
}

void 
cdl_create_imp(char *import, char *prog)
{

FILE *st;
int ierr;
    st = fopen("import.script", "w");
    if (st == NULL) {
        printf("*** cdl:could not open file: import.script ***\n");
        return;
    }

    fprintf(st, "#! /bin/sh\n");
    fprintf(st, "olddir=`pwd`\n");
    fprintf(st, "cd /tmp\n");
    fprintf(st, "echo \"#! %s \" > %s\n",prog,import);
    fprintf(st, "nm %s | egrep ' [BAD] '| cut -f1 -d' ' |sed -e 's/^#/ #/' | sort | uniq >> %s \n",prog,import);
    fprintf(st, "cd $olddir\n");
    fprintf(st, "exit \n");
    fclose(st);
    if(ierr=system("sh import.script")!=0)
       printf(" something wrong in the import.script: ierr=%d\n",ierr);

}
#endif


int 
cdl_script(char *fexec, char *path, char *name, char *fext)
{
    FILE *st;
#if defined (AIX)
    char import[19];
    cdl_get_import(import);
    import[19] = '\0'; 
#endif

    st = fopen(fexec, "w");
    if (st == NULL) {
        printf("*** cdl:could not open file: %s ***\n", fexec);
        return 1;
    }
/* ----------------------------------------------------------------- */
    if (*path == '\0')	strcpy(path, "$olddir/");

    fprintf(st,           "#! /bin/sh\n");
    fprintf(st,           "olddir=`pwd`\n");
    fprintf(st,           "cd %s\n",cdl_def.path);
    fprintf(st,           "/bin/rm -f %s.sl\n",name);

    if (*cdl_def.lang == 'c') {

        fprintf(st,"%s %s%s.c\n",cdl_def.cc,path,name);

    } else if (*cdl_def.lang == 'C') {

        fprintf(st,"%s %s%s.f\n",cdl_def.CC,path,name);

    } else if (*cdl_def.lang == 'f') {

        fprintf(st,"%s %s%s.f\n",cdl_def.f77,path,name);

    } else {

	printf(" unknown language: %s\n",cdl_def.lang);
	return 2;
    }

    fprintf(st,            "errno=$?\n");
    fprintf(st,            "if [ $errno != 0 ]\n");
    fprintf(st,            "then\n");
    fprintf(st,            "   exit $errno\n");
    fprintf(st,            "fi\n");
#if defined (__hpux)

    fprintf(st, "ld -b -o %s.sl %s.o %s\n",name,name,cdl_def.ld);

#endif
#if defined (AIX)
    fprintf(st, "nm %s.o | egrep ' [BAD] '| cut -f1 -d' '",name);
    fprintf(st, " | sed -e 's/^#/ #/' | sort | uniq > %s.exp\n", name);

    fprintf(st, 
  "ld -bE:%s.exp %s -o %s.sl %s.o  -bh:4 -berok -bM:SRE -bnoentry -T512 -H512 -lc",
               name,import,name,name);
/*  "ld -bE:%s.exp testcdl.o -o %s.sl  %s.o -bh:4 -T512 -H512 -lc",
               --------- MUST be changed  
               name,name,name);
*/

    if      (*cdl_def.lang == 'c') fprintf(st," -lc\n");
    else if (*cdl_def.lang == 'C') fprintf(st," -lc\n");
    else                           fprintf(st," -lxlf90\n");

#endif
#if defined(SUN) && defined(SOLARIS)

    fprintf(st, "/usr/ccs/bin/ld -G -o %s.sl %s.o\n", name, name);

#endif
#if defined(SUN) && (!defined(SOLARIS))

    fprintf(st, "ld -o %s.sl %s.o\n", name, name);

#endif
#if defined(SGI)

    fprintf(st, "ld -shared -o %s.sl %s.o\n", name, name);

#endif
#if defined(__alpha)

    fprintf(st, 
    "ld -shared -o %s.sl %s.o -lUfor -lfor -lFutil -lm -lots -lc\n",
                 name, name);
#endif
    fprintf(st,            "errno=$?\n");
    fprintf(st,            "if [ $errno != 0 ]\n");
    fprintf(st,            "then\n");
    fprintf(st,            "   exit $errno\n");
    fprintf(st,            "fi\n");

    fprintf(st,            "/bin/rm -f %s.o\n",name);
    fprintf(st,            "cd $olddir\n");
    fprintf(st,            "exit 0\n");

    fclose(st);
    return 0;

} /* cdl_script */

void cdl_fnparse( );

int cdl_create(char *filename)
{
    char name[72], path[72];
    int ierr;
    char fext[72];
    char fexec[72];
    char chline[72];

    cdl_fnparse(filename, path, name, fext);

    if (*name == '\0') {
        printf( " cdl:error in file_name **%s**\n",filename);
        return 1;
      }

    strcpy(fexec, "script.exec");

    if (cdl_script(fexec, path, name, fext) != 0) 
        return 1;

    strcpy(chline, "/bin/sh ");
    strcat(chline, fexec);

    if ((ierr  = system(chline)) != 0)
        return ierr;

    strcpy(chline, "/bin/rm -f ");
    strcat(chline, fexec);

/*tmp    ierr = system(chline); */
    return ierr;
}

int cdl_create_(char *filename, int n)
{
   char file_name[80];
   strncpy(file_name, filename, n); 
   file_name[n] = '\0';
   return cdl_create(file_name);
}

void cdl_fnparse(char *filename, char *path, char *name, char *fext)
{
    char *str, *str1;
    int  fl, fll, flll;

    fl = strlen(filename);
    str = strrchr(filename, '/');
    if (str == NULL) {
      *path = '\0';
      str1 = strchr(filename, '.');
      if (str1 == NULL) {
        *fext = '\0';
        strcpy(name, filename);
      }
      else {
        flll = strlen(str1);
        strncpy(name, filename, fl - flll); 
        name[ fl - flll]=0; /* hjw 19Feb98 */
        name[fl - flll] = '\0';
        strncpy(fext, str1 + 1, flll - 1); 
        fext[flll - 1] = '\0';
      }
    }
    else {
      fll = strlen(str);
      strncpy(path, filename, fl - fll + 1); 
      path[fl - fll + 1] = '\0';
      str1 = strchr(str, '.');
      if (str1 == NULL) {
        *fext = '\0';
        strcpy(name, str + 1);
      }
      else {
        strcpy(fext, str1 + 1);
        strncpy(name, str + 1, fll - strlen(str1) - 1); 
        name[fll - strlen(str1) - 1] = '\0';
      }
    }
}


FilesPtr cdl_find_file(char *filename)
{
   register FilesPtr fp = fileList;

   while (fp != NULL) {
     if (strcmp(filename,fp->filename) == 0)
        return(fp);
     else
        fp = fp->next;
 }
   return(fp);
}


void *cdl_get_func_lib();

FuncPtr cdl_find_proc(char *procname)
{
   register FuncPtr pp;
   FilesPtr         fp;
   void            *addr;

   for (pp = procList; pp; pp = pp->next)
     if (strcmp(procname,pp->name) == 0)
        return(pp);

   addr = (void *)cdl_get_func_lib(procname, &fp);
   if(addr != NULL) {
       pp = (FuncPtr)MALLOC(sizeof(FuncDesc));
       pp->name    = strdup(procname);
       pp->file    = fp;
       pp->funcptr = (GenFuncPtr)addr;
       pp->type    = UNDEF_T;
       pp->narg    = 0;
       pp->arglist = NULL;
       pp->next    = procList;
       procList    = pp;
   }
   return(pp);
}

void cdl_delete_all(FilesPtr f)
{
   register FuncPtr   fp = procList;
   char *file = f->filename;

  while (fp) {
    if(strcmp(fp->file->filename,file) == 0){
       fp->file = NULL;
       fp->funcptr = NULL;
    }
    fp = fp->next;
 }

}

int 
cdl_load(char *lib_name)
{
   FilesPtr   f;
   FileHandle file_handle;
   int flags;

#if defined(__hpux)
   long address = 0L;
   extern int errno;
   flags = BIND_DEFERRED | DYNAMIC_PATH;

   file_handle = shl_load(lib_name, flags, address);
   if (file_handle==0){
      perror("cdl_load: ");
      return (errno);
    }
#else

   /* The Alpha doesn't recognize the RTLD_GLOBAL flag; besides, 
      it declares shared library symbols globally by default. */

#if defined(__alpha)
    flags = RTLD_LAZY;
#elif !defined(WIN32)
    flags = RTLD_LAZY | RTLD_GLOBAL;
#else 
    flags = 0;
#endif

    file_handle = dlopen(lib_name, flags);
    if (file_handle ==  NULL) {
       printf("cdl_load:  %s\n",dlerror() );
       return 1;
     }
#endif

/*   Add new file to the files list
*/
   f = (FilesPtr) MALLOC(sizeof(Files));
   f->filename = strdup(lib_name);
   f->file_handle = file_handle;
   f->next  = fileList;
   fileList = f;
   return 0;
}

int cdl_load_(char *lib_name, int n)
{
    char name[82];
    strncpy(name, lib_name, n); 
    name[ n]=0; /* hjw 19Feb98 */
    name[n] = '\0';
    return cdl_load(name);
}

int cdl_unload(char *lib_name)
{
   FilesPtr  f, before;

   f = cdl_find_file(lib_name);
   if (f == NULL) {
      if (debug_level > 0) printf("File not found.\n");
      return 2;
   }
   if (f != fileList) {
      before = fileList;
      while (before->next != f) before = before->next;
      before->next = f->next;
   }
   else {
      fileList = f->next;
   }

#if defined(__hpux)
   if(shl_unload(f->file_handle) != 0) {
#else
   if (dlclose(f->file_handle) != 0) {
#endif
      if (debug_level > 0) printf("Error in dlclose()...\n");
         return 1;
   }

   cdl_delete_all(f);
   FREE(f->filename);
   FREE(f);
   if (debug_level > 0) printf("Unlink %s file.\n",lib_name);
   return 0;
}

int cdl_unload_(char *lib_name, int n)
{
    char name[82];
    strncpy(name, lib_name, n); 
    name[n] = '\0';
    return cdl_unload(name);
}

void *cdl_get_func_lib(char *func_name, FilesPtr *fp)
{
   void  *addr;
#if defined(__hpux)
   shl_t handle;
#endif
#ifdef SUN
/*VP*   int *addr;*/
#endif
#if defined(__alpha)
   int jumpad_();
   unsigned long ptr = (unsigned long)jumpad_;
#endif

/*   Search for all files  */
   *fp = fileList;
   while (*fp != NULL) {
#if defined(__hpux)
      handle = (*fp)->file_handle;
      if (shl_findsym(&handle, func_name, TYPE_PROCEDURE, &addr) == 0){
        return(addr);
      }
#else
     addr =  dlsym((*fp)->file_handle, func_name);
     if (addr !=  NULL) {
#if defined(__alpha)
         addr = (unsigned long) addr - ptr;
#endif
         return (void *) addr;
      }
#endif
     *fp = (*fp)->next;
   } 
/* end while */
  return NULL;
}

void *cdl_func_addr(char *func_name)
{
   FilesPtr  fp;

   return cdl_get_func_lib(func_name, &fp);
}  

void *cdl_func_addr_(char *func_name, int n)
{
   char name[82];
   strncpy(name, func_name, n); 
   name[n] = '\0';
   return cdl_func_addr(name);
}

char *cdl_get_lib(int *ns )
{
   /* find name of ns [ns=0 for first] shared library  */
  FilesPtr f = fileList;
  int i=0;

  while (f != NULL && i < *ns) {
        f = f->next;
        i++;
  }
   if (f != NULL)
      return(f->filename);
   return NULL;
}

void cdl_print_lib_()
{
     char *name;
     int  n;

     printf(" --- list of loaded libraries ----\n");
     n = 0;
     while ((name = cdl_get_lib(&n))){
       printf(" %d %s\n", n, name);
       n++;
     }
}

void cdl_symbols(char *lib_name, int *ns, char *symbol)
{
   static FuncPtr pp;
   FilesPtr       fp;

   if (*ns == -1) pp = procList;
   while (pp) {
      fp = pp->file;
      if (fp->filename && !strcmp(lib_name, fp->filename))  break;
      pp = pp->next;
   }
   if (pp == NULL) {
      *ns = -2;
      return;
   }
   strcpy(symbol, pp->name);
   pp = pp->next;
   (*ns)++;
}

void cdl_print_symbols_(char *lib_name, int n)
{
     char name[80];
     char symbol[80];
     int  ns;

     strncpy(name, lib_name, n); 
     name[n] = '\0';

     printf(" contents of library --%s--\n", name);
     for(ns = -1; ;){
       cdl_symbols(name, &ns, symbol);
       if( ns == -2)break;
       printf(" %d %s \n", ns, symbol);
     }
}

FuncPtr cdl_get_func(char *func_name)
{
  FilesPtr  fp;
  FuncPtr   pp;

    pp = cdl_find_proc(func_name);
    if(pp != NULL) {
      if(pp->funcptr == NULL) {
         pp->funcptr = (GenFuncPtr)cdl_get_func_lib(func_name, &fp);
         if (pp->funcptr != NULL) pp->file    = fp;
      }
      if(pp->funcptr == NULL) return NULL;
    }
   return pp;
}

FuncPtr cdl_get_func_(char *func_name, int n)
{
   char name[80];
   strncpy(name, func_name, n); 
   name[n] = '\0';
   return cdl_get_func(name);
}


#define MAXARGS 16

char *argname[ (int) LAST_T ] = { "undefined" , 
    "void", "void*", "char", "char*", "logical","logical*",
    "int",  "int*",  "float","float*","double", "double*",
    "complex", "complex*"
				};

int cdl_proto(char *func_proto)
{
/*
*  proto := type name(type,...,type) 
*  type:=(void|char|int|float|double|...) [*]
*/

  char     *ftype, *fname, *arg, *temp, *proto;
  ArgType  argsp[MAXARGS], *atp;
  ArgType  proc_type, type, at;
  int      narg, i;
  int      ispointer;

  FuncPtr   pp;

#define compress(ps)  \
  {char *sp, *spc;  \
   spc=sp=ps; \
   for(;*sp;sp++) if(*sp != ' '){*spc = *sp; spc++;} \
   *spc = 0; \
  }

    proto  = strdup(func_proto);

    arg = strchr(proto, '(' );
    temp  = strchr(proto, ')' );

    if(!arg || !temp)  goto err;
    if(strlen(temp+1)) goto err;

    fname = strtok(proto,"(");
    *temp = '\0';

    if ((temp = strchr(fname, '*'))){
      ispointer = 1;
      ftype = strtok(fname, "*");
      fname = strtok(0, " ");
    }
    else {
      ispointer = 0;
      ftype = strtok(fname, " ");
      fname = strtok(0, " ");
    }
    if(!strlen(fname)) goto err;
    if(!strlen(ftype)) goto err;
    compress(fname);
    compress(ftype);
/*
    printf("ftype(%s) fname(%s)",ftype,fname);
*/
    proc_type = UNDEF_T;
    for ( at=VOID_T; at < LAST_T; at++) {
       if(!strcmp(ftype, argname[at])){
          proc_type = at;
          break;
	}
    }
    if (proc_type == UNDEF_T) goto err;
    if(ispointer)proc_type++;

    arg = strtok(arg+1, ",");
    narg = 0;
    while(arg){
       compress(arg);
       type = UNDEF_T;
       for ( at=VOID_T; at < LAST_T; at++) {
          if(!strcmp(arg, argname[at])){
            type = at;
            break;
          }
       }
       if (type == UNDEF_T) goto err;
       argsp[narg] = type;       
       narg++;
/*       printf(" %d(%s)",narg,arg); */
       arg = strtok(0,",");
    }

    pp = cdl_find_proc(fname);
    if(pp == NULL) {
       pp = (FuncPtr)MALLOC(sizeof(FuncDesc));
       pp->name    = strdup(fname);
       pp->file    = NULL;
       pp->funcptr = NULL;
       pp->arglist = NULL;
       pp->next    = procList;
       procList    = pp;
    }
    if(pp->arglist != NULL) FREE(pp->arglist);
    pp->type    = proc_type;
    pp->narg    = narg;

    if (narg){
       pp->arglist = atp = (ArgType *)MALLOC(narg * sizeof(ArgType));
       for (i=0; i<narg; i++,atp++) *atp = argsp[i];
     } else 
       pp->arglist = NULL;
    free (proto); 
    return 0;
  err:
    free (proto); 
    /*    printf("---%s---\n");  What is %s supposed to refer to? */
    printf("cdl_proto: syntax error\n");
    return 1;
}

int cdl_proto_(char *func_proto, int n)
{
   char proto[80];
   strncpy( proto, func_proto, n); 
   proto[n] = '\0';
   return cdl_proto(proto);
}


#include <varargs.h>

void
cdl_call_(res, fd, va_alist)
void    *res;
FuncPtr  *fd;
va_dcl
{
	va_list ap;
	char   *arglist[MAXARGS];
	int     narg, i;
	ArgType ftype;
        FuncPtr fdp = *fd;

        extern int    cdl_calli_();
        extern float  cdl_callr_();	
        extern double cdl_calld_();

	ftype = fdp->type;
	narg  = fdp->narg;

	va_start(ap);
	for ( i=0; i<narg; i++)  arglist[i] = va_arg(ap, char *);
	va_end(ap);
	switch (ftype) {

	case  VOID_T:
		i = cdl_calli_(&fdp->funcptr, &narg, arglist);
		break;

	case  CHAR_T:
	case  LOGICAL_T:
	case  INTEGER_T:
		*(int *)res = cdl_calli_(&fdp->funcptr, &narg, arglist);
		break;

	case REAL_T:
		*(float *)res = cdl_callr_(&fdp->funcptr, &narg, arglist);
		break;

	case DOUBLE_T:
	case COMPLEX_T:
		*(double *)res = cdl_calld_(&fdp->funcptr, &narg, arglist);
		break;

	case  VOID_P:
	case  CHAR_P:
	case LOGICAL_P:
	case REAL_P:
	case DOUBLE_P:
	case COMPLEX_P:
		printf(" don't know yet\n");
		break;

	case  UNDEF_T:
	default:
		printf(" Undefined type for: %s\n",fdp->name);
		break;
	}
}

void cdl_callv_(void *res, FuncPtr *fd, char *arglist)
{
	int     narg, i;
	ArgType ftype;
        FuncPtr fdp = *fd;

        extern int    cdl_calli_();
        extern float  cdl_callr_();	
        extern double cdl_calld_();

	ftype = fdp->type;
	narg  = fdp->narg;

	switch (ftype) {

	case  VOID_T:
		i = cdl_calli_(&fdp->funcptr, &narg, arglist);
		break;

	case  CHAR_T:
	case  LOGICAL_T:
	case  INTEGER_T:
		*(int *)res = cdl_calli_(&fdp->funcptr, &narg, arglist);
		break;

	case REAL_T:
		*(float *)res = cdl_callr_(&fdp->funcptr, &narg, arglist);
		break;

	case DOUBLE_T:
	case COMPLEX_T:
		*(double *)res = cdl_calld_(&fdp->funcptr, &narg, arglist);
		break;

	case  VOID_P:
	case  CHAR_P:
	case LOGICAL_P:
	case REAL_P:
	case DOUBLE_P:
	case COMPLEX_P:
		printf(" don't know yet\n");
		break;

	case  UNDEF_T:
	default:
		printf(" Undefined type for: %s\n",fdp->name);
		break;
	}
}

