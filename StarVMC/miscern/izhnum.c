//      SUBROUTINE IZHNUM (HOLL,INTV,NP)
void izhnum_(unsigned char *HOLL,int *INTV,int *NP)
{
  static unsigned char *tst = "1234567890abc";
  static int sz=0;
  static int jc=0;
  int i;
  if (!sz) { 
    sz=sizeof(int);
    if (((*(int*)tst)&255)=='1') {jc=0;} else {jc=sz-1;}
  }
  for (i=0;i<*NP;i++) {
    INTV[i] = HOLL[jc];
    HOLL +=sz;
  }

}
