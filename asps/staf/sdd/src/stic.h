#ifndef __STIC_H__
#define __STIC_H__

/* Prototype declarations for STIC utility functions */

#include <stdio.h>

#ifdef CC_P
#undef CC_P
#endif

#ifdef __cplusplus
#define CC_P "C"
#else
#define CC_P
#endif

extern CC_P void TemplateFBegin(char *fn,FILE *ff);
extern CC_P void TemplateFMiddle(FILE *ff);
extern CC_P void TemplateFEnd(FILE *ff);
extern CC_P void TemplateCBegin(FILE *ff);
extern CC_P void TemplateCMiddle(char *filename,FILE *ff);
extern CC_P void TemplateCEnd(FILE *ff);

#endif /* __STIC_H__ */
