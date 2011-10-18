#ifndef L2PEDRESULTS2012_H
#define L2PEDRESULTS2012_H

/*********************************************************************
 * $Id: L2pedResults2012.h,v 1.1 2011/10/18 15:11:44 jml Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * Output container for pedestal algo in L2 
 * stored in the trigger data
 *********************************************************************
 */


struct L2pedOutInt0 { //  4 bytes
  unsigned char decision;   /* meaning of bits:  
			       0-2 :free
			       3,4 : seenBtow, seenEtow data block
			       5: free
			       6: pedSubtraction ON/OFF
			       7: free
			    */
  unsigned char free;
  unsigned short kTick; /*  CPU ticks used for this event in kTicks,
 			    2^32-1=overflow, */
};


struct L2pedResults2012 { // all output bits lump together
  enum{mySizeChar=4};
  struct L2pedOutInt0 int0; 
};

//---------------
#define GET_OB(x,n) ( (x & 1 << n)!=0) // get one bit
//---------------
inline void L2pedResults2012_print(struct L2pedResults2012  *p) {
  if(p==0) {printf("print L2pedResults2012() - NULL pointer ????\n"); return;}
  unsigned int x=p->int0.decision;
  printf("L2pedResults2012():  bemcIn=%d  eemcIn=%d ,  pedSubtr=%d  \n", GET_OB(x,3),  GET_OB(x,4),  GET_OB(x,6));
  printf("  used CPU kTicks=%d \n",  p->int0.kTick);

};
#undef GET_OB
#endif
/**********************************************************************
  $Log: L2pedResults2012.h,v $
  Revision 1.1  2011/10/18 15:11:44  jml
  adding 2012 algorithms

  Revision 1.1  2010/04/17 17:14:37  pibero
  *** empty log message ***

  Revision 1.2  2008/01/17 01:57:33  kocolosk
  inline printing functions

  Revision 1.1  2007/10/11 00:33:25  balewski
  L2algo added

  Revision 1.7  2006/03/28 19:46:51  balewski
  ver16b, in l2new

  Revision 1.6  2006/03/11 17:08:35  balewski
  now CVS comments should work

*/

