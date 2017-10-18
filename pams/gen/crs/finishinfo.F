* $Id: finishinfo.F,v 1.1 1998/02/14 23:02:41 fisyak Exp $
* $Log: finishinfo.F,v $
* Revision 1.1  1998/02/14 23:02:41  fisyak
* Split Fortran sources
*
      Subroutine FinishInfo(Trial,Runs,dT,Rate_PD)

******-****************************************************************-*******
*
*..Description: This program prints some data for the user.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*               Selected rate, August 22, 1994
*               Formatting Xrate, August 25, 1994
*
******-****************************************************************-*******
      Implicit none
*..Input:
      Integer Trial             ! Number of Particles hitting the detector
      Integer Runs              ! Number of particles hitting the hemisphere
      Real*4 dT                 ! Time step for each particle
      Real*4 Rate_PD            ! Rate of particle hitting cylinder
*..Local:
      Real*4 Xrate              ! Multiplication factor to get selected rate

*=====-================================================================-=======
 10   Format(A,F13.4,A)
 11   Format(A,I8)
 12   Format(A,F18.7,A)
 13   Format(A,G18.7,A)

      Call Shadow(3,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

      Rate_PD=(Real(Trial)/Real(Runs))/dT ! Rate on detector
      Xrate = 1.0/Real(Runs)/dT ! Multiplication factor to get selected rate

      Write(*,*)
      Write(*,*)'HemiCosm/FinishInfo Final Report:'
      Write(*,10)'   Effective Flux      : ',Rate_PD,' Hz'
      Write(*,10)'   Hit/total Ratio     : ',Real(Trial)/Real(Runs)
      Write(*,10)'   Time Span           : ',dT*Runs,' Seconds'
      Write(*,11)'   Total Particles     : ',Runs
      Write(*,11)'   Total Detector Hits : ',Trial
      Write(*,*)
      Write(*,*)
     +  'To find selected rate (where you define selection criteria),'
      Write(*,*)
     +  ' Selected Rate =  [N_Selected/Total Particles]*Rate(Predicted)'
      If (Xrate .ge. 0.10) then
        Write(*,12)
     +    ' In this run, Selected Rate = N_Selected * ',Xrate,' Hz'
      Else
        Write(*,13)
     +    ' In this run, Selected Rate = N_Selected * ',Xrate,' Hz'
      Endif                     ! (Xrate)
      Write(*,*)

 999  Return
      End
