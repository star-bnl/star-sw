#define KUMAC_UNWIND -30041961 /* error status to quit macro execution */
 
#ifdef __cplusplus
extern "C" {
#endif
 
/*
 * C-interface functions
 */
 
extern char*  ku_getc(void);
extern char*  ku_gete(void);
extern char*  ku_getf(void);
extern int    ku_geti(void);
extern char*  ku_getl(void);
extern double ku_getr(void);
extern char*  ku_gets(void);
 
extern int    ku_npar(void);

extern void   ku_what( void(*styleG)() );
 
#ifdef __cplusplus
}
#endif
 
