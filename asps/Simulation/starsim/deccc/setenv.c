#include <stdlib.h>

void      setenv_(const char *name, const char *value, int ln, int lv)
{ int i = setenv (name,value, 1); }

void      unsetenv_(const char *name, int ln)
{         unsetenv (name);        }

