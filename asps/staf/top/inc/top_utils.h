/* top_utils.h */
#ifndef boolean
typedef unsigned char top_bool;
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P 
#endif
extern CC_P top_bool isValidSelectSpec(const char * spec);
extern CC_P top_bool isValidWhereClause(const char * clause);
