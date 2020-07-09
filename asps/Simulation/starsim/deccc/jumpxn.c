/*
 * $Id: jumpxn.c,v 1.1.2.1 2020/07/09 19:48:44 perev Exp $
 *
 * $Log: jumpxn.c,v $
 * Revision 1.1.2.1  2020/07/09 19:48:44  perev
 * Comis64_0
 *
 * Revision 1.1  2018/11/19 23:20:12  perev
 * 64bits new comis files added from /CERN
 *
 * Revision 1.1.1.1  1996/02/15 17:49:32  mclareni
 * Kernlib
 *
 */
/*>    ROUTINE JUMPXN
  CERN PROGLIB# Z042    JUMPXN          .VERSION KERNVMI  1.08  930527
  ORIG. 21/04/88 JZ+FCA, adapted 11/05/93 AP+JZ
C
C-    To transfer to the user routine TARGET (say) with 2 parameters
C-    three steps are needed :

C- 1) EXTERNAL TARGET              to get the address of TARGET
C-    IADR = JUMPAD (TARGET)

C- 2) CALL JUMPST (IADR)           to set the tranfer address

C- 3) CALL JUMPX2 (par1,par2)      to transfer
*/
#include <assert.h>
int jumpad_(unsigned long *fun);
//static unsigned long uumpad = (unsigned long)jumpad_;
static unsigned long uumpad = (unsigned long)0;
int           csvpotokn(unsigned long addr);
unsigned long csvplong (         int  tokn);

#define kMASK 0x40000000
#define kMAZK 0xE0000000

/* ----   csToken   ---------------------------------------------  */
int csToken(unsigned long fun) 
{
#if 1
return csvptokn(fun);
#else

  int token = fun-uumpad;
  assert(uumpad+token == fun);
  return token;
#endif
} 
/* ----   csPoter   ---------------------------------------------  */
unsigned long  csPoter( int token) 
{
#if 1
  return csvplong(token);
#else
  return uumpad+token;
#endif
}

void (*tarsub)();
/* ----   jumpad   ---------------------------------------------  */
int  jumpad_(ifun)
    unsigned long *ifun;
{
    return csToken(ifun);
}

/* ----   jumpst   ---------------------------------------------  */
void jumpst_(iadr)
    int  *iadr;
{
    unsigned long true;

    true = csPoter(*iadr);
    tarsub = (void (*)())true;
}

/* ----   jumpxn   ---------------------------------------------  */
jumpx0_()
{
    (*tarsub)();
    return;
}

jumpx1_(ipara)
    char *ipara;
{
    (*tarsub)(ipara);
    return;
}

jumpx2_(ipara, iparb)
    char *ipara, *iparb;
{
    (*tarsub)(ipara, iparb);
    return;
}
jumpx3_(ipara, iparb, iparc)
    char *ipara, *iparb, *iparc;
{
    (*tarsub)(ipara, iparb, iparc);
    return;
}
jumpx4_(ipara, iparb, iparc, ipard)
    char *ipara, *iparb, *iparc, *ipard;
{
    (*tarsub)(ipara, iparb, iparc, ipard);
    return;
}
/*> END <----------------------------------------------------------*/
