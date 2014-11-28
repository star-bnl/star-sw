* $Id: startup.F,v 1.1 1998/02/14 23:02:48 fisyak Exp $
* $Log: startup.F,v $
* Revision 1.1  1998/02/14 23:02:48  fisyak
* Split Fortran sources
*
      Subroutine StartUp(Det_Len,Det_Rad,Det_Off,P_Min,P_Max,P_Mode,
     +                   dT,HemiRad,Version)

******-****************************************************************-******
*
*..Description: HemiCosm initialization Routine
*                 This subroutine first calculates the smallest radius
*                 hemisphere that will enclose the detector. Next it uses
*                 this radius to calculate the incident flux rate on the
*                 hemisphere.  From this it calculates the 'average' time
*                 step to be assigned to each particle impact(event).  The
*                 subroutine also reports some data which may be useful,
*                 and warning messages as needed.  Calls to other routines
*                 are used to initialize them with this data.
*
*                 NOTE: The flux is a function of momentum.  If the limits
*                   of P_Min and P_Max are changed in then the total
*                   percentage of flux between 0.3 and 1000 GeV/c that this
*                   corresponds to, must be recalculated and substituted into
*                   the FluxRatio parameter.  If the values of P_Min and
*                   P_Max differ, significantly, from the values used to
*                   calculate the current flux a warning message will be
*                   displayed stating that the flux rate will be incorrect.
*                   The current value for the flux percentage was calculated
*                   using the data taken from:
*                       Physics Data 25-1 'Cosmic Rays on Earth' (1984)
*                        (ISBN 0344-8401) by O.C. Allkofer and P.K.F. Grieder.
*                        Table 2.2.1                 page 240
*                   Using the intensity from around 0.3 GeV/c and 1.0
*                   GeV/c, I extrapolated how much was present at each
*                   point and then divided to find the ratio of flux
*                   at 1.0 GeV/c to 0.3 GeV/c.  This number is not
*                   very accurate(good to about 10%) so a P_Min +- 0.2GeV/c
*                   and a P_Max above 40 GeV/c will still give an 'accurate'
*                   flux rate.  If the Momentum cutoff fall outside this
*                   range a warning message will be displayed.  The value
*                   for the rate of incident flux was taken from:
*                      Particle Properties Data Booklet  June 1992
*                      American Institute of Physics         P.129
*                   The hard component(>0.3 GeV/c) crossing a unit
*                   horizontal area from above was used.  Because of the
*                   shadow method(discussed later) a factor of 5/4 was used
*                   to give the correct rate of the hemisphere.
*
*..Author     : Michael P Bringle
*               Lawrence Berkeley Lab
*
*..History    : First Version August 5, 1994
*               Second Version August 19, 1994
*
*
******-****************************************************************-*******
      Implicit none
*..Input:
      Integer P_Mode
      Real*4 Det_Len     ! Mother Cylinder Length(cm)
      Real*4 Det_Rad     ! Mother Cylinder Radius(cm)
      Real*4 Det_Off     ! Mother Cylinder Offset from ground(cm)
      Real*4 P_Min,P_Max ! Momentum Spectrum Cutoffs(GeV/c)
      Real*4 Version     ! Version of Program (filled in HemiCosm)
*..Output :
      Real*4 dT          ! Time Step for each hit(seconds)
      Real*4 HemiRad     ! Hemisphere radius (cm)
*..Local :
      Real*4 Flux        ! Incident flux rate (>0.3GeV/c) (Part/cm^2 sec)
      Real*4 Flux0       ! Flux rate greater than 1.0GeV/c (Part/cm^2 sec)
      Real*4 FluxRatio   ! Ratio of flux accepted to total(See Note)
      Real*4 Rate        ! Rate of particles hitting the hemisphere(Hz)
      Real*4 Geomfact           ! Geometry factor for hemi-sphere to base
      Parameter(Flux=0.013)
      Parameter(FluxRatio=0.7642)
      Parameter(Flux0=Flux*FluxRatio)
      Parameter (Geomfact = 5.0/4.0)

*..Constants
      Real*4 Pi             ! Constant
      Parameter (Pi=3.14159265358979323846264338328)
*=====-================================================================-=======
*Find minimum enclosing sphere radius
      HemiRad=Sqrt( (2.0*Det_Rad+Det_Off)**2 + (Det_Len/2.0)**2 )

*Get rate and time step for each particle hit
      Rate=(Flux0*Pi*HemiRad**2)*Geomfact !Rate=Flux times area(Hz)
      dT=1.0e0 / Rate                     !Time per particle(Seconds)

*Initialize Shadow Routine
      Call Shadow(1,Flux0,Rate,0.0,0.0,0.0,0.0,0.0)

*Report data to whoever is listening
 10   Format(A,F13.4,A)
 11   Format(A,I8)
 12   Format(A)
 13   Format(A,F3.1,A)
 14   Format(A,F13.2)

      Write(*,*)
      Write(*,12)' ***********************************'
      Write(*,12)' * HemiCosm - Cosmic Ray Generator *'
      Write(*,13)' *            Version ',Version,
     +                                    '          *'
      Write(*,12)' ***********************************'
      Write(*,*)
      Write(*,12)' HemiCosm/StartUp Data:'
      Write(*,10)'   Predicted rate of impacts: ',Rate,
     +                                          ' (Hz) on Hemisphere'
      Write(*,14)'   Hemisphere geomet. factor: ',Geomfact
      Write(*,10)'   Rate before geometry     : ',Rate/Geomfact,
     +                                          ' (Hz) on base'
      Write(*,10)'   Incident Flux(>0.3 GeV/c): ',Flux, ' (s^-1 cm^-2)'
      Write(*,10)'   Flux Ratio (1.0/0.3GeV/c): ',FluxRatio
      Write(*,10)'   Incident Flux(>1.0 GeV/c): ',Flux0,' (s^-1 cm^-2)'
      Write(*,10)'   Hemisphere radius        : ',HemiRad,' (cm)'
      Write(*,10)'   Cylinder Radius          : ',Det_Rad,' (cm)'
      Write(*,10)'   Cylinder Length          : ',Det_Len,' (cm)'
      Write(*,10)'   Cylinder Offset          : ',Det_Off,' (cm)'
      Write(*,10)'   Momentum Minimum         : ',P_Min,' (GeV/c)'
      Write(*,10)'   Momentum Maximum         : ',P_Max,' (GeV/c)'
      Write(*,11)'   Momentum Mode            : ',P_Mode

*Warning Message for Momentum out of range
*Note P_Mode=0 sets off this error(correctly) because P_Max is set equal to
*  P_Min upon start-up, and the two values will always fail the test
      If (P_Min.gt.1.2.or.P_Min.lt.0.8.or.P_Max.lt.40) then
        Write(*,*)'   The requested momentum spectrum is outside the'
        Write(*,*)'     limits of accuracy of the calculated rate.  '
        Write(*,*)'     The reported rate and elapsed time will not be'
        Write(*,*)'     correct.'
        Write(*,*)
      Endif

      Write(*,*)

 999  Return
      End
