*
* $Id: mdpool.inc,v 1.2 2020/06/04 23:05:02 perev Exp $
*
* $Log: mdpool.inc,v $
* Revision 1.2  2020/06/04 23:05:02  perev
* add csguard call
*
* Revision 1.1  2018/11/19 20:43:51  perev
* 64bits add files
*
* Revision 1.1.1.1  1996/02/26 17:16:34  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_MDPOOL_INC
#define CERNLIB_COMIS_MDPOOL_INC
*
*
* mdpool.inc
*
      integer GUARD
      COMMON/MDPOOL/IQ(LHP),GUARD(10)
      INTEGER ISEM(LHP),KD(LHP),IDA(LHP),KD1(99),KD2(99),KD3(99)
      EQUIVALENCE (IQ,ISEM),(IQ,KD),(IQ,IDA)
      EQUIVALENCE (KD1(1),KD(2)),(KD2(1),KD(3)),(KD3(1),KD(4))


#endif
