* $Id: getp3ini.F,v 1.1 1998/02/14 23:02:44 fisyak Exp $
* $Log: getp3ini.F,v $
* Revision 1.1  1998/02/14 23:02:44  fisyak
* Split Fortran sources
*
      Subroutine GetP3Ini(Bins,P_Min,P_Max,BNum,ISpc)

******-****************************************************************-*******
*
*..Description: DarInv Momentum Spectrum
*                 This method uses data tabulated in bins of integrated
*                   momentum vs angle theta.  That data for this subroutine
*                   comes from a BASIC program(darinv3.exe) that was given
*                   to us by Prof. Henry W. Kendall at Massachusetts Institute
*                   of Technology.  It calculates the momentum spectrum vs
*                   zenith angle using Arnon Dar's data:
*                     Cosmic Ray Muons at Gound Level and Deep Underground
*                     By Arnon Dar               1984
*                     TECHNION-PHYS-84-41
*                     Department of Physics
*                     Technion _ Israel Institute of Technology
*                     Haifa, Israel
*                   The program (darinv3.exe) states that if the zenith angle
*                   is chossen with a Cos^2 this will yield the total momentum
*                   energy distribution for the given zenith angle.
*                   The data is read into arrays that are indexed by bin
*                   number and theta angle.  With the array have 1000 bins,
*                   which contain the integrated momentum spectrum, by 19
*                   theta numbers(in 5 degree steps.)
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History:
*    Bringle    8/5/94  - First Version.
*    Kral       8/8/94  - Changed file names from thetaxx.dat to cosmdarxx.dat.
*                       - Logical unit is 78 (for time being).
*    Bringle    8/18/94 - Changed file names from cosmdarxx.dat to cosdarxx.dat
*                         to allow for use on machines with 8 character
*                         filename limits(IBM PC's)
*
******-****************************************************************-*******
      Implicit None
*..Input:
      Integer Bins              !Number of bins
      Real*4  P_Min,P_Max       !Momentum Cutoff(GeV/c)
*..Output:
      Real*4 ISpc(Bins,0:18)    !Integrated Spectrum value for a given bin
                                !  number and theta index.
      Real*4 BNum(Bins,0:18)    !Bin number for random number indexing that
                                !  corresponds to ISpc(#1,#2)
*..Local:
      Integer Error             !Indicates Error on file read
      Integer L1,L2             !Do Loop variables
      Integer Iluncosm          !Logical unit

*..File Names (one for every 5 degrees in theta):
      Character*40 File_Name(0:18) /  'cosdar00.dat','cosdar05.dat',
     +  'cosdar10.dat','cosdar15.dat','cosdar20.dat','cosdar25.dat',
     +  'cosdar30.dat','cosdar35.dat','cosdar40.dat','cosdar45.dat',
     +  'cosdar50.dat','cosdar55.dat','cosdar60.dat','cosdar65.dat',
     +  'cosdar70.dat','cosdar75.dat','cosdar80.dat','cosdar85.dat',
     +  'cosdar90.dat'/
*=====-================================================================-=======
*Initialization Routine
*     Report Information to user
 10   Format(A,F13.4,A)
      Write(*,*)'Momentum Spectrum Data:'
      Write(*,*)'   The method uses look up tables to produce a'
      Write(*,*)'   momentum spectrum that has a zenith angle'
      Write(*,*)'   dependence.  The values are taken from DarInv,'
      Write(*,*)'   inverted integral flux from Arnon Dar,'
      Write(*,*)
     +  '   Cosmic Ray Muons at Ground Level and Deep Underground,'
      Write(*,*)'   TECHNION-PHYS-84-41, Haifa, Israel (1984).'
      Write(*,*)'   Flux is valid between 1 and 1000 GeV/c.'

      If (P_Max.gt.1000.0.or.P_Min.lt.1.0) then
        Write(*,*)
        Write(*,*)'GetP3Ini:  Error - program terminating'
        Write(*,*)'   *************** WARNING ******************'
        Write(*,*)'   The Momentum limits you have chosen are'
        Write(*,*)'   outside the allowed range(see above.)'
        Write(*,10)'     P_Min allowed:    1.0 GeV/c   Yours: ',
     +            P_Min,' GeV/c'
        Write(*,10)'     P_Max allowed: 1000.0 GeV/c   Yours: ',
     +            P_Max,' GeV/c'
        Write(*,*)'   Program terminating...'
        Write(*,*)
        Stop  !Stopped in GetP3Ini - momentum limits
      Endif

*     Read in Spectrum
      Iluncosm = 78             ! Hardwired logical unit number
      Do L1=0, 18
        Open(Iluncosm,File=File_Name(L1),STATUS='OLD',IOSTAT=Error,
     +    BLANK='NULL')

        If(Error.ne.0) then
          Write(*,*)
          Write(*,*)'GetP3Ini:  Error - program terminating'
          Write(*,*)' Fatal error in reading in DarInv files:'
          Write(*,*)' File Error in ',File_Name(L1)
          Write(*,*)' Does file or link exist in working directory?'
          Write(*,*)' Program terminating...'
          Write(*,*)
          Stop  !Stopped in GetP3Ini - file read error
        Endif
        Do L2=1, Bins
          BNum(L2,L1)=L2        !Random number Bin
          Read(Iluncosm,*) ISPC(L2,L1) !Momentum Value(Integrated Spectrum)
        Enddo

        Close (Iluncosm,Status='KEEP')

      Enddo

      Write(*,*)

      Return
      End
