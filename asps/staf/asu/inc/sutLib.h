#ifndef SUTLIB_H
#define SUTLIB_H

#ifdef __cplusplus
#define CC_P "C"
#endif

extern CC_P int sutMatchWild(char *pattern, char* string);
extern CC_P int sutMatchReg(char *pattern, char* string);
extern CC_P int sutMatchPrefix(char *prefix, char* string);
extern CC_P int sutStripWhitespace(char **outstring, char* string);
extern CC_P char* strntok(const char * str, const char * del
		, const int n);
extern CC_P int strsplit(const char * str, const char * del, char*** a);
extern CC_P int strbracket(const char *str, const char * od
		, const char * cd, char *** a);
extern CC_P int isInteger(char *c);
extern CC_P int sutFortran2Cindex(char ** index);

#endif /*SUTLIB_H*/

