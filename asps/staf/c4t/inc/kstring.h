/*CMZ :  2.05/09 12/07/94  14.55.03  by  Alfred Nathaniel*/
/*CMZ :  2.05/10 14/07/94  15.44.52  by  Alfred Nathaniel*/
/*-- Author :*/
 
#include <string.h>
 
#ifdef __cplusplus
extern "C" {
#endif
 
/*
 * quasi-standard functions missing in some C-libraries
 */
extern void* memmove( void* dst, const void* src, size_t n );
 
extern int   strcasecmp( const char* str1, const char* str2 );
extern int   strncasecmp( const char* str1, const char* str2, size_t n );
 
extern char* strrstr( const char* str1, const char* str2 );
 
#define strdup Strdup           /* prototype without const */
extern char* strdup( const char* str );
 
 
/*
 * convenience functions from kkern.c
 */
extern char* str0dup( const char* str );
extern char* str2dup( const char* str1, const char* str2 );
extern char* str3dup( const char* str1, const char* str2, const char* str3 );
extern char* str4dup( const char* str1, const char* str2, const char* str3,
                     const char* str4 );
extern char* str5dup( const char* str1, const char* str2, const char* str3,
                     const char* str4, const char* str5 );
extern char* strndup( const char* buf, size_t n );
extern char* stridup( int i );
 
extern char* mstrcat( char* ptr, const char* str );
extern char* mstr2cat( char* ptr, const char* str1, const char* str2 );
extern char* mstr3cat( char* ptr, const char* str1, const char* str2,
                      const char* str3);
extern char* mstr4cat( char* ptr, const char* str1, const char* str2,
                      const char* str3, const char* str4 );
extern char* mstrncat( char* ptr, const char* buf, size_t n );
extern char* mstrccat( char* ptr, char c, size_t n );
extern char* mstricat( char* ptr, int i );
 
extern char* strrpbrk( const char* str1, const char* str2 );
extern char* strqtok( char* str );
extern char* strlower( char* str );
extern char* strupper( char* str );
extern char* strtrim( char* str );
extern char* struntab( char* str );
extern char* strfromd( double d, size_t prec );
extern char* strfromi( int i, size_t prec );
 
extern int   shsystem( const char* shell, const char* cmd );
extern int   checksum( const char* path );
 
extern char* fexpand( const char* fname, const char* ftype );
extern char* fsearch( const char* fname, const char* ftype, const char* path );
extern char* fsymlink( const char* path );
 
extern char* fstrdup( const char* buf, size_t len );
extern char* fstr0dup( const char* buf, size_t len );
extern char* fstrtrim( const char* buf, size_t len );
extern char* fstr0trim( const char* buf, size_t len );
 
extern size_t fstrlen( const char* buf, size_t len );
extern size_t fstrset( char* buf, size_t len, const char* str );
extern double fstrtod( const char* str, char** tail );
extern int    fstrtoi( const char* str, char** tail );
extern char*  fstrvec( char** pstr, size_t n, size_t* len );
extern size_t mstrlen( char** pstr, size_t n );
 
#ifdef __cplusplus
}
#endif
 
 
/*-- Author :    Alfred Nathaniel   12/07/94*/
