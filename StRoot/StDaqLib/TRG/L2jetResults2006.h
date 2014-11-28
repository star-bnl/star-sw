#ifndef L2JETRESULTS2006_H
#define L2JETRESULTS2006_H

/*********************************************************************
 * $Id: L2jetResults2006.h,v 1.3 2010/01/13 18:17:01 akio Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * Output container for di-jets reco in L2 
 * stored in the trigger data
 *********************************************************************
 */


#define  L2JET_RESULTS_VERSION 2

struct L2jetOutInt0 { //  4 bytes
  unsigned char version; // to tag format of the L2Result
  unsigned char decision;   /* meaning of bits:  
			       0-2 : useBtowEast, useBtowWest, useEtow,
			       3,4 : seenBtow, seenEtow data block
			       5: accepted at random
			       6: accepted by one-jet cuts
			       7: accept by di-jet cuts
			    */
  unsigned char cutTag; /* passed from input */
  unsigned char kTick; /*  CPU ticks used for this event in kTicks,
 			    255=overflow, */
};

struct L2jetOutInt1 { //  4 bytes
  unsigned short iTotEne;  // total transverse energy  Et/GeV=iEne/100.
  unsigned char  checkSum; // to monitor if I get what I wrote
  unsigned char  free;
};
struct L2jetOutInt2 { //  4 bytes
  unsigned short nBtowTw; // # of working towers above ADC thres
  unsigned short nEtowTw; //   --//--
};

struct L2jetOutJet { // first & second jet 
  unsigned char  jPhi; // phi index, [0:180], phi/deg=jPhi*2;
  unsigned char  jEta; // eta index, [0:149], etaValue=jEta/50.-1.
  unsigned short iEne; // transverse energy  Et/GeV=iEne/100.
};

struct L2jetResults2006{ // all output bits lump together
  struct L2jetOutInt0 int0;
  struct L2jetOutInt1 int1;
  struct L2jetOutInt2 int2;
  struct L2jetOutJet  jet1, jet2;
};


//====================================
//====================================
// get value of n-th bit
#define GET_OB(x,n) ( (x & 1 << n)!=0 ) 

inline void 
L2jetResults2006_print(L2jetResults2006 *p) {
  if(p==0) {printf("print L2jetResults2006 - NULL pointer ????\n"); return;}

  unsigned int x=p->int0.decision;
  printf("L2jetResults2006:\n    Accept:    rnd=%d  oneJet=%d  diJet=%d   cutTag=%d\n    useBtowEast=%d useBtowWest=%d  useEndcap=%d  bemcIn=%d eemcIn=%d\n",  GET_OB(x,5),  GET_OB(x,6),  GET_OB(x,7),p->int0.cutTag, GET_OB(x,0), GET_OB(x,1),  GET_OB(x,2), GET_OB(x,3),  GET_OB(x,4));
 
  printf("    Jet1: Et/GeV=%.2f  phi/deg=%3d  eta=%.2f\n",  p->jet1.iEne/100.,p->jet1.jPhi*2,p->jet1.jEta/50.-1. );
  printf("    Jet2: Et/GeV=%.2f  phi/deg=%3d  eta=%.2f\n",  p->jet2.iEne/100.,p->jet2.jPhi*2,p->jet2.jEta/50.-1. );
  
  printf("    totEne/GeV=%.2f  nBtowTw=%d nEtwTw=%d, \n    CPU kTicks=%d  format=%d  checkSum=%d\n",  p->int1.iTotEne/100., p->int2.nBtowTw, p->int2.nEtowTw, p->int0.kTick,p->int0.version, p->int1.checkSum);


};

#undef GET_OB

//====================================
//====================================
inline unsigned char
L2jetResults2006_doCheckSum(L2jetResults2006 *p) {
  if(p==0) {printf("print L2jetResults2006 - NULL pointer ????\n"); return 0xff;}
  unsigned char* buf=(unsigned char*) p;
  int len=sizeof( L2jetResults2006);
  unsigned char sum=0;
  int i;
  for(i=0;i<len;i++) sum+=buf[i];
  return sum;
}
#endif

/**********************************************************************
  $Log: L2jetResults2006.h,v $
  Revision 1.3  2010/01/13 18:17:01  akio
  clean up compilcation warning

  Revision 1.2  2006/05/23 17:44:56  akio
  inline functions in L2*.h

  Revision 1.1  2006/04/14 17:56:15  akio
  Adding L2 Result Structs

  Revision 1.6  2006/03/28 19:46:49  balewski
  ver16b, in l2new

  Revision 1.5  2006/03/11 17:08:34  balewski
  now CVS comments should work

*/

