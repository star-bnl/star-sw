* $Id: hiscom.inc,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: hiscom.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:04  nevski
*  first working release
*
      Implicit Character*30 (T)
      Parameter (LC=30,L=50,Idm=2000)
      Common    /histab/List(Idm),Tab(L,3),WW(L,3),
     +                  ie(3),Nen(3),nn(l),aa(l),bb(l)
*     - - - - - - - - - - - - - - - - - - - - - - - -
