* $Id: getcharge.F,v 1.1 1998/02/14 23:02:42 fisyak Exp $
* $Log: getcharge.F,v $
* Revision 1.1  1998/02/14 23:02:42  fisyak
* Split Fortran sources
*
      Subroutine GetCharge(Rnum,Charge)

******-****************************************************************-*******
*
*..Description: This program gets a randomly distributed Muon charge.
*                 Each Muon can have either a +1 or a -1 charge.
*                 Using the ratio of the two charges a simple selection
*                 method.  The ratio of + to - was taken from:
*                      Particle Properties Data Booklet  June 1992
*                      American Institute of Physics         P.129
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
******-****************************************************************-*******
      Implicit none
*..Input:
      Real*4 Rnum               !Random number
*..Output:
      Integer Charge            !Muon Charge
*..Local
      Real*4 Ratio              !Ratio of + to Total[(-) + (+)]
      Parameter (Ratio=5.0/(4.0+5.0))
*=====-================================================================-=======
      If (Rnum.le.Ratio) Charge=1
      If (Rnum.gt.Ratio) Charge=-1

 999  Return
      End
