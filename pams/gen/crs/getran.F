* $Id: getran.F,v 1.2 1998/02/14 23:02:45 fisyak Exp $
* $Log: getran.F,v $
* Revision 1.2  1998/02/14 23:02:45  fisyak
* Split Fortran sources
*
      Real*4 Function GetRan(I)

******-****************************************************************-******
*
*..Description: This function gets a random number and tests to be
*                 sure that it is not on the boundarys.  If it is
*                 then there is probably an error and we exit if it
*                 happens more than 10 times.
*
*               Ensures that random numbers are in the open interval (0, 1).
*
*..Author     : Michael P Bringle
*
*..History    : Bringle and Kral, 8/20/94 - First version.
*
*
******-****************************************************************-******
      Implicit none
*..Input:
      Integer I
*..Local:
      Integer L1
*..Funct:
      Real*4 Begran
      External Begran

*=====-================================================================-======
      Do L1=1, 10
        GetRan=BegRan(I+L1)
        If (GetRan.gt.0.0 .and. GetRan.lt.1.0) goto 999
      Enddo
      Write(*,*)'GetRan:'
      Write(*,*)'Error:  Program termination due to random number'
      Write(*,*)'  generation anomaly.  0 and or 1 was generated 10'
      Write(*,*)'  times in a row.'
      Write(*,*)'Terminating program...'
      Stop  !Stopped in GetRan

 999  Return
      End
