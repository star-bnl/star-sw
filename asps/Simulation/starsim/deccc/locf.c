/*
 * $Id: locf.c,v 1.6.2.2 2020/08/07 02:49:55 perev Exp $
 *
 * $Log: locf.c,v $
 * Revision 1.6.2.2  2020/08/07 02:49:55  perev
 * Cleanup
 *
 * Revision 1.6.2.1  2020/07/09 19:48:44  perev
 * Comis64_0
 *
 * Revision 1.5  2020/06/04 23:22:40  perev
 * assert added
 *
 * Revision 1.4  2018/12/03 00:50:44  perev
 * locf & locb use csvptokn
 *
 * Revision 1.3  2018/11/26 22:55:06  perev
 * Remove too strong test
 *
 * Revision 1.2  2018/11/21 23:15:26  perev
 * Test for zero added
 *
 * Revision 1.1  2018/11/19 23:20:12  perev
 * 64bits new comis files added from /CERN
 *
 * Revision 1.4  2004/07/29 14:06:07  mclareni
 * Alice version for 64-bit pointer systems using the CERNLIB_QMLXIA64 cpp flag
 *
 * Revision 1.2  2002/12/02 16:37:45  brun
 * Changes from Federico Carminati and Peter Hristov who ported the system
 * on the Ithanium processors.It is tested on HP, Sun, and Alpha, everything
 * seems to work. The optimisation is switched off in case of gcc2.xx.yyy
 *
 * Revision 1.1.1.1  2002/07/24 15:56:28  rdm
 * initial import into CVS
 *
 * Revision 1.1.1.1  2002/06/16 15:18:46  hristov
 * Separate distribution  of Geant3
 *
 * Revision 1.1.1.1  1999/05/18 15:55:28  fca
 * AliRoot sources
 *
 * Revision 1.3  1997/09/02 14:26:38  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/02/04 17:34:35  mclareni
 * Merge Winnt and 97a versions
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:29:36  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/15 17:49:24  mclareni
 * Kernlib
 *
 */
/*>    ROUTINE LOCF
  CERN PROGLIB# N100    LOCF            .VERSION KERNFOR  4.36  930602
*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
int csToken(unsigned long fun);
unsigned long  csPoter( int token); 

unsigned long csvplong(         int  tokn);
         int  csvptokn(unsigned long addr);
         int  csvpyes (         int  tokn);        
static unsigned long gBase=0;
//______________________________________________________________________________
void setBase(unsigned long addr,int test)
{
  if (!gBase) {
    gBase = addr;
    assert(!(gBase&3));
  } else {
    assert(!(addr&3));
    if (!test) return;
    long dif = addr-gBase;
    if (dif <0) dif = -dif;
    assert(dif/sizeof(int) <= 0xEFFFFFFF);
    assert(dif             <= 0xFFFFFFFF);
  }
}
//______________________________________________________________________________
__UINT64_TYPE__ longf_(char *iadr )
{
  return ((unsigned long)iadr)/sizeof(int);
}
//______________________________________________________________________________
__UINT64_TYPE__ longb_(char *iadr )
{
  return ((unsigned long)iadr);
}

//______________________________________________________________________________
int  locf_(char *iadr )
{
  setBase((unsigned long)iadr,1);
  int myDif = (((unsigned long)iadr) - gBase);
  return myDif/(int)sizeof(int);
}
//______________________________________________________________________________
int  locb_(char *iadr )
{  
  setBase((unsigned long)iadr,0);
  long myDif = (((unsigned long)iadr) - gBase);
  if (myDif >= -0xFFFFFFFFL && myDif<= 0xFFFFFFFFL) {
    assert(!csvpyes((unsigned int)myDif));
    return myDif;
  }
  myDif = csvptokn (iadr);
  return myDif;
}

//______________________________________________________________________________
char *getPntB(int myDif)
{
  if (!(csvpyes(myDif))) return (char*)gBase+myDif;
  return (char*)csvplong(myDif);

}
//______________________________________________________________________________
int getbyteb_(int *myDif)
{
  return *getPntB(*myDif);
}
//______________________________________________________________________________
int getbytef_(int *myDif)
{
  return *getPntB(*myDif);
}
//______________________________________________________________________________
int getfun2b_(int *myFun)
{
  unsigned long fun = csPoter( *myFun);

  return locb_(&fun);
}
//______________________________________________________________________________
int getf2b_(int *myFun)
{
  unsigned long uk = csPoter(*myFun);
  return locb_(uk);
}

