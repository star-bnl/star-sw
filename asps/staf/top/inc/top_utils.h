/* top_utils.h */
#ifndef boolean
typedef unsigned char boolean;
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
extern CC_P boolean isValidSelectSpec(char * spec);
extern CC_P boolean isValidWhereClause(char * clause);
