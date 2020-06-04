/*
 * $Id: jumptn.c,v 1.2 2020/06/04 21:55:31 perev Exp $
 *
 * $Log: jumptn.c,v $
 * Revision 1.2  2020/06/04 21:55:31  perev
 * 64b
 *
 * Revision 1.1  2018/11/19 23:20:12  perev
 * 64bits new comis files added from /CERN
 *
 * Revision 1.1.1.1  1996/02/15 17:49:32  mclareni
 * Kernlib
 *
 */
/*>    ROUTINE JUMPTN
  CERN PROGLIB# Z043    JUMPTN          .VERSION KERNVMI  1.09  940531
  ORIG. 21/04/88 JZ+FCA
C
C-    To transfer to the user routine TARGET (say) with 2 parameters
C-    two steps are needed :

C- 1) EXTERNAL TARGET              to get the address of TARGET
C-    IADR = JUMPAD (TARGET)

C- 3) CALL JUMPT2 (IADR,par1,par2)      to transfer
*/
          int csToken(unsigned long fun);
unsigned long csPoter( int token); 


#define jumpt0 jumpt0_
#define jumpt1 jumpt1_
#define jumpt2 jumpt2_
#define jumpt3 jumpt3_
#define jumpt4 jumpt4_

void (*jumpto)();

void jumpt0(iadr)
     int *iadr;
{
    jumpto = (void(*)()) csPoter(*iadr);
    jumpto();
    return;
}

void jumpt1(iadr,ipara)
     int *iadr;
     char *ipara;
{
    jumpto = (void(*)()) csPoter(*iadr);
    jumpto (ipara);
    return;
}

void jumpt2(iadr, ipara, iparb)
     int *iadr;
     char *ipara, *iparb;
{
    jumpto = (void(*)()) csPoter(*iadr);
    jumpto (ipara, iparb);
    return;
}
void jumpt3(iadr, ipara, iparb, iparc)
     int *iadr;
     char *ipara, *iparb, *iparc;
{
    jumpto = (void(*)()) csPoter(*iadr);
    jumpto (ipara, iparb, iparc);
    return;
}
void jumpt4(iadr, ipara, iparb, iparc, ipard)
     int *iadr;
     char *ipara, *iparb, *iparc, *ipard;
{
    jumpto = (void(*)()) csPoter(*iadr);
    jumpto (ipara, iparb, iparc, ipard);
    return;
}
/*> END <----------------------------------------------------------*/
