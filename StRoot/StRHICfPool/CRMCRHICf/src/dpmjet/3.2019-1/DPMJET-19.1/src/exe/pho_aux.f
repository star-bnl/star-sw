C* This program demonstrates how to use auxiliary routines
C* from phojet. Here: ultra-peripheral collisions.
C* Usage:
C*    ./bin/pho_aux 10000 (for 10k event)
C*
C* Anatoli Fedynitch, U. Alberta, (2019)

      PROGRAM PHOUPC
C**********************************************************************
C
C   example program calling PHOJET photon flux routines without cards
C
C**********************************************************************
      IMPLICIT NONE

      DOUBLE PRECISION ee , p1 , p2 , pcm , PHO_PMASS, pm1 , pm2 , s , 
     &                 sigcur , sigmax , sqs , ZERO
      INTEGER IARGC , id , irej , itry , k , neve, IDUM

      EXTERNAL PHO_PMASS
      EXTERNAL PYDATA

      SAVE 
 
      PARAMETER (ZERO=0.D0)
 
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
      INCLUDE 'inc/poevt1'

C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'

C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DIMENSION p1(4) , p2(4)
      CHARACTER*72 title
      CHARACTER*32 arginp
 
C  *********** hp compiler settings ******************************
C     ON DOUBLE PRECISION UNDERFLOW IGNORE
C     ON REAL UNDERFLOW IGNORE
C  **************************************************************
 
C  number of events
      IF ( IARGC().GT.0 ) THEN
         CALL GETARG(1,arginp)
         READ (arginp,*) neve
      ELSE
         neve = 1
      END IF
C  general initialization of PHOJET data structures (mandatory)
C  (-2 means that no steering file is expected)
      irej = 20
      CALL PHO_INIT(-2,6,irej)
 

C Set PDFs to the "old" GRV98, since photon interactions are not
C tuned to the CT14 default in the "new" DPMJET/PHOJET

C  proton
      CALL PHO_SETPDF(2212,IDUM,5,6,0,0,-1)
      CALL PHO_SETPDF(-2212,IDUM,5,6,0,0,-1)
C  neutron
      CALL PHO_SETPDF(2112,IDUM,5,6,0,0,-1)
      CALL PHO_SETPDF(-2112,IDUM,5,6,0,0,-1)
C  photon
      CALL PHO_SETPDF(22,IDUM,5,3,0,0,-1)
C  pomeron
      CALL PHO_SETPDF(990,IDUM,4,0,0,0,-1)

C Call the function that generates spectrum and the collisions
C there are a number of other relevant functions that start with
C src/phojet/PHO_GG***

C You need to define some photon flux kinematics to make the routine
C work. You can also use some other inputs for the flux, but this needs
C other functions and more insight into what the routine actually does
C The patch below is from the GHHIAS docstring
C            from /LEPCUT/:
C                YMIN2   lower limit of Y
C                        (energy fraction taken by photon from hadron)
C                YMAX2   upper cutoff for Y, necessary to avoid
C                        underflows
C                Q2MIN2  minimum Q**2 of photons (should be set to 0)
C                Q2MAX2  maximum Q**2 of photons (if necessary,
C                        corrected according size of hadron)

      YMIN2 = 1.D-7
      YMAX2 = 1.D-2
      Q2MIN2 = 0.D0
C Set debug flag to print the photon flux table
      IDEb(30) = 10
C I assume 7 TeV for the proton and 5 TeV/nucleon for Lead
      CALL PHO_GHHIAS(neve, 7.D3, 5.D3, 208, 82)
 
C  some (optional) output of PHOJET-internal statistics
      CALL PHO_EVENT(-2,p1,p2,sigcur,irej)
 
      END PROGRAM
