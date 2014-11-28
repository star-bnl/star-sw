* $Id: begcosm.F,v 1.4 1999/10/09 22:38:03 nevski Exp $
* $Log: begcosm.F,v $
* Revision 1.4  1999/10/09 22:38:03  nevski
*  pgf-> g77 cleanup
*
* Revision 1.2  1998/02/14 23:02:38  fisyak
* Split Fortran sources
*
      Subroutine BegCosm(D_Mode,P_Mode,Det_Len,Det_Rad,Det_Off,
     +                   P_Min,P_Max,End_Hits,Rate_PD,
     +                               End_Nsph,Rate_Sph,Version)
******-****************************************************************-*******
*
*..Description: Interface to STDHEP library /HEPEVT/ for Beget generator.
*..Author        : Michael P Bringle    
*..Date          : August  5, 1994
*..Latest Update : August 22, 1994
*    
******-****************************************************************-*******
      Implicit none
*..Include:
*..Input:
      Integer D_Mode            ! Data mode (2 or 23=event, See hemicosm.F)
      Integer P_Mode            ! Momentum mode            (See hemicosm.F)
      Real*4 P_Min,P_Max        ! Momentum range           (See hemicosm.F)
      Real*4 Det_Len,Det_Rad,Det_Off ! Detector dimensions (See hemicosm.F)
*..Output:
      Integer End_Hits          !Number of Detector hits
      Integer End_Nsph          !Number of Total Particles Thrown on Sphere
      Real*4 Rate_PD            !Rate on Detector (Actual)
      Real*4 Rate_Sph           !Rate on Sphere (Predicted)
      Real*4 Version            ! Program version (filled in HemiCosm)
*..Local for Beg1part:
      Integer Charge            !Muon
      Real*4 P                  !Momentum (GeV/c)
      Real*4 Theta, Phi         !Angular Distribution(Radians)
      Real*4 X,Y,Z              !Position(cm)

      Character*4 C1p           ! Calling routine identifier string (optional)
      Integer*4 Imo             ! Mother particle (or background process id)
      Integer*4 Hepid           ! HEP library particle id (see manual)
      Real*4    Mass            ! Mass [GeV/c**2]
      Parameter (Mass=0.105658387)
      Real*4    Ptot            ! Momentum [GeV/c]
      Real*4    Cost            ! Cos(theta) [-1 to 1]
      Real*4    Phir            ! Phi [radians]
      Integer Ierr
*..Local for Beg1vert:
      Real*4 V(4)               !Position 4-Vector(X,Y,Z,t)

*=====-================================================================-=======
      Call HemiCosm(Det_Len,Det_Rad,Det_Off,P_Mode,P_Max,P_Min,D_Mode,
     +              Theta,Phi,P,X,Y,Z,Charge,
     +              End_Hits,Rate_PD,End_Nsph,Rate_Sph,Version)

      If (D_Mode.eq.2.or.D_Mode.eq.23) then
            
        C1p  ='COSM'            !Function routine indentafier
        Imo  =-97               !Process ID Number        
        Hepid=(-13)*Charge      !+13 for muons (mu-), -13 for Antimuons (mu+)
        Ptot =p                 !Total Momemtum
        Cost =Cos(Theta)        !Trajectory in Cosine Theta 
        Phir =Phi               !Trajectory in Phi(Radians)

*..  If you wish to just test the particle generator then use Beg1Cosm.
*     Beg1cosm sends all of the particles away from the detector so
*     that you can run much faster due to the fact that the particles
*     are not tracked through the detector.  Beg1Part sends the particles
*     corectly into the detector.

        Call Beg1Part(C1p, Imo, Hepid, Mass, Ptot, Cost, Phir, Ierr)
***        Call Beg1Cosm(C1p, Imo, Hepid, Mass, Ptot, Cost, Phir, Ierr)

        V(1)=X*10.0             !X Cordinate(mm)
        V(2)=Y*10.0             !Y Cordinate(mm)
        V(3)=Z*10.0             !Z Cordinate(mm)
        V(4)=0.0                !Time (seconds)

        Call Beg1vert(C1p, V, Ierr)

      endif                     ! (D_Mode=2)

 999  Return
      End
