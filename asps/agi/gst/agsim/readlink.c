/*CMZ :  2.00/00 01/09/99  22.08.19  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   22/04/99*/
int readlink (char*, char*, int);
int readlink_(char* a, char *b, int La, int Lb)
{
   char path[256];
   path[0]=0;
   strncat (path,a,La);
   return readlink(path,b,Lb);
}
 
