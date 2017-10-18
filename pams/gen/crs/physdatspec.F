* $Id: physdatspec.F,v 1.1 1998/02/14 23:02:47 fisyak Exp $
* $Log: physdatspec.F,v $
* Revision 1.1  1998/02/14 23:02:47  fisyak
* Split Fortran sources
*
      Real*8 Function  PhysDatSpec(X)

******-****************************************************************-*******
*
*..Description: Differential Momentum Spectrum
*               This spectrum is a function that is used my the
*                 integration package to obtain an integrated spectrum.
*                 This integrated spectrum can then be used to find a
*                 properly distributed momentum value by choosing a random
*                 number between 0 and 1 and finding where it lies in the
*                 spectrum.
*
*               The function used in this subroutine was derived from
*                  the data in:
*                    Physics Data 25-1 'Cosmic Rays on Earth' (1984)
*                     (ISBN 0344-8401)   O.C. Allkofer and P.K.F. Grieder
*                     Table 2.2.3              page 241
*                  and is a curve fit based upon the data in the table.  The
*                  curve is valid between 0.5 and 1000 GeV/c.
*
*..Author     : Michael P Bringle
*..History    :
*
*
******-****************************************************************-*******
      Implicit None
*..Input:
        Real*8 X
*..Local:
      Real*8 a,b,c,d,e
*=====-================================================================-=======
      a=3.095047d-3
      b=2.187907d0
      c=0.205373d0
      d=0.462988d0
      e=0.17077d0

      PhysDatSpec=a*x**(-3.0d0 + (b/(c*x+d)**e))

      Return
      End
