#ifndef _FIX_ALLOC_HH_
#define _FIX_ALLOC_HH_

#include <semLib.h>

#define FIX_ALLOC_TMOUT		1000	// 10 seconds...

class fixAlloc 
{
public:
	fixAlloc(char *wh, int bnum, int bytes) ;
	~fixAlloc() ;

	void resize(char *wh, int bnum, int bytes) ;

	char *alloc(int bytes, int block=1)  ;
	int free(void *wh)  ;
	void clear(void) ;	
	void shrink(void *wh, int real) ;
	int check(void *wh) ;
	int getblk(void *wh) ;

	int take_mutex(void) {	return semTake(semutex,FIX_ALLOC_TMOUT) ; } ; 
	int take_semfree(void)  {  return semTake(semfree,FIX_ALLOC_TMOUT) ; } ; 
	int give_mutex(void) {	return semGive(semutex) ; } ; 
	int give_semfree(void)  {  return semGive(semfree) ; } ; 
	int alloced ;	// for debugging

	int blbytes ;
	int blnum ;
private:
	char *where ;
	int myalloc ;
	int cleared ;
	int *desc ;	// storage for states

	int last_free ;
	int last_free_blks ;
	
	SEM_ID semutex ;
	SEM_ID semfree ;
	
} ;


#endif
