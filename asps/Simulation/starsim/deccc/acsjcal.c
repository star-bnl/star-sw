/* $Id: acsjcal.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $ */
/* $Log: acsjcal.c,v $
 * Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
 *
/* Revision 1.2  2003/10/03 17:18:09  nevski
/* better dynamic call
/* */

#define MDPOOL mdpool_;
extern struct  {int ia0, names[99];} mdpool_;

int acsjcal_(iadr,np,p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                     p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                     p21,p22,p23,p24,p25,p26,p27,p28,p29,p30)
     int   *iadr,*np;
     void  *p01,*p02,*p03,*p04,*p05,*p06,*p07,*p08,*p09,*p10,
           *p11,*p12,*p13,*p14,*p15,*p16,*p17,*p18,*p19,*p20,
           *p21,*p22,*p23,*p24,*p25,*p26,*p27,*p28,*p29,*p30;
{ 
  int (*name) () =  (int (*)()) mdpool_.names[*iadr];
  if (!*iadr || !*name) return -1;
  switch (*np)
  { case 0:  return (*name) ();
    case 1:  return (*name) (p01);
    case 2:  return (*name) (p01,p02);
    case 3:  return (*name) (p01,p02,p03);
    case 4:  return (*name) (p01,p02,p03,p04);
    case 5:  return (*name) (p01,p02,p03,p04,p05);
    case 6:  return (*name) (p01,p02,p03,p04,p05,p06);
    case 7:  return (*name) (p01,p02,p03,p04,p05,p06,p07);
    case 8:  return (*name) (p01,p02,p03,p04,p05,p06,p07,p08);
    case 9:  return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09);
    case 10: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10);
    case 11: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11);
    case 12: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12);
    case 13: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13);
    case 14: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14);
    case 15: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15);
    case 16: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16);
    case 17: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17);
    case 18: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18);
    case 19: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19);
    case 20: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20);
    case 21: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21);
    case 22: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22);
    case 23: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23);
    case 24: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24);
    case 25: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25);
    case 26: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25,p26);
    case 27: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25,p26,p27);
    case 28: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25,p26,p27,p28);
    case 29: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25,p26,p27,p28,p29);
    case 30: return (*name) (p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,
                             p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
                             p21,p22,p23,p24,p25,p26,p27,p28,p29,p30);
    default:
     printf(" acsjcal ERROR: This case not yet implemented, npar=%i \n",*np);
     return;
  }
}
