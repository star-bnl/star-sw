/*
* $Id: aix_dlfcn.h,v 1.1 1998/06/10 00:52:22 perev Exp $
*
* $Log: aix_dlfcn.h,v $
* Revision 1.1  1998/06/10 00:52:22  perev
* Change || to | in dlopen
*
* Revision 1.2  1998/06/05 20:55:15  perev
* AGI commit
*
* Revision 1.1  1998/04/16 17:01:59  fisyak
* 2nd pass with gstar
*
*
* ---------------------------------------------
*
* dlfcn.inc
*/
#if (defined(CERNLIB_AIX))&&(defined(CERNLIB_SHL))
/*CMZ :          20/11/97  22.10.22  by  Pavel Nevski*/
/*-- Author :    HELIOS Software GmbH*/
/*
 * @(#)dlfcn.h	1.4 revision of 95/04/25  09:36:52
 * This is an unpublished work copyright (c) 1992 HELIOS Software GmbH
 * 30159 Hannover, Germany
 */
 
#ifndef __dlfcn_h__
#define __dlfcn_h__
 
#ifdef __cplusplus
extern "C" {
#endif
 
/*
 * Mode flags for the dlopen routine.
 */
#define RTLD_LAZY	1	/* lazy function call binding */
#define RTLD_NOW	2	/* immediate function call binding */
#define RTLD_GLOBAL	0x100	/* allow symbols to be global */
 
/*
 * To be able to intialize, a library may provide a dl_info structure
 * that contains functions to be called to initialize and terminate.
 */
struct dl_info {
	void (*init)(void);
	void (*fini)(void);
};
 
#if __STDC__ || defined(_IBMR2)
 void *dlopen(const char *path, int mode);
 void *dlsym(void *handle, const char *symbol);
 char *dlerror(void);
 int dlclose(void *handle);
#else
 void *dlopen();
 void *dlsym();
 char *dlerror();
 int dlclose();
#endif
 
#ifdef __cplusplus
}
#endif
 
#endif /* __dlfcn_h__ */
 
 
 
#endif
