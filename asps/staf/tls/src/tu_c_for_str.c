/* C functions to convert character strings between C and FORTRAN. */
#include <strings.h>

/* Convert C string to FORTRAN string. */
char* c2f_string(char* fs, char* cs)
{
   int i,ilen;
   ilen = strlen(fs);
   strncpy(fs,cs,ilen);
   for(i=strlen(fs);i<ilen;i++) {
      strcat(fs," ");
   }
   return fs;
}

/* Convert FORTRAN string to C string. */
char* f2c_string(char* cs, char* fs)
{
   int i,ilen;
   ilen = strlen(cs);
   strncpy(cs,fs,ilen);
   i=strlen(cs)-1;
   while(strcmp(&cs[i]," ")==0) {
      strcpy(&cs[i],"");
      i--;
   }
   return cs;
}
