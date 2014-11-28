       SUBROUTINE HIJEV 
C****************************************************************************
C
C
C
C       The following is an example program for calling HIJING. one should 
C       include all the common blocks and the data values which are listed 
C       below in his own program.
C***************************************************************************
CCC#include "headpss.inc"
      COMMON/HEADPSS/PSSHEP(5),VSSHEP(4),IFIRST,IRUN
      REAL PSSHEP,VSSHEP
      INTEGER IFIRST, IRUN
      SAVE /HEADPSS/
*--
      CHARACTER FRAME*8,PROJ*8,TARG*8, line*80
      DATA lin, lout / 40, 50 /
*--
      COMMON/HIMAIN1/NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
      SAVE  /HIMAIN1/
      COMMON/HIMAIN2/KATT(130000,6),PATT(130000,5)
      SAVE  /HIMAIN2/
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
      SAVE  /HIPARNT/
      COMMON/HIJJET1/NPJ(300),KFPJ(300,500),PJPX(300,500),
     &                PJPY(300,500),PJPZ(300,500),PJPE(300,500),
     &                PJPM(300,500),NTJ(300),KFTJ(300,500),
     &                PJTX(300,500),PJTY(300,500),PJTZ(300,500),
     &                PJTE(300,500),PJTM(300,500)
      SAVE  /HIJJET1/
      INTEGER ISTAT
      character input_file*100
      DATA IFIRST /0/
       IF(IFIRST.EQ.0) THEN
       IRUN=0
       CALL HEPRUN(IRUN)
*--
*      character output_dir*80, output_file*80, input_file*100
*--
      input_file = 'hijev.inp'
      call getopt('-inp',input_file)
      open ( lin, file=input_file, status='old', err = 913)
 10   continue
         read ( lin, '(a80)', end=20 ) line
       write ( 6, * ) line
         goto 10
 20   continue
      rewind ( lin )
*--      output_dir = ' '
*-- 
      Read ( lin, * ) line, line, line
      read ( lin, * ) line, VSSHEP(4)
      read ( lin, * ) line, frame, efrm
      read ( lin, * ) line, proj, iap, izp
      read ( lin, * ) line, targ, iat, izt
      read ( lin, * ) line, bmin, bmax     
      read ( lin, * ) line, iquench
      read ( lin, * ) line, ihard, ptjet
      read ( lin, * ) line, ihard_switch
      read ( lin, * ) line, ih11, ih12
      read ( lin, * ) line, ih21, ih18
      read ( lin, * ) line, hipr7 
      read ( lin, * ) line, ISTAT 
*           
*      read ( lin, * ) line, nevt_file, output_dir 
*
*       open ( lout, file=output_file, status='unknown'

      !@@
      !@@ ==================================================
      !@@ Optional flags.  Once the EOF is reached, the
      !@@ program will branch to the setup below
      !@@
      read ( lin, *, end=30 ) line               ! comment line
      read ( lin, *, end=30 ) line, ihpr2(6)     ! shadowing
      !@@
 30   continue 
      !@@
      !@@ Setup hijing common blocks and prepare for run
      !@@ ==================================================
      !@@

       PSSHEP(1) = float(iap)
       PSSHEP(2) = float(izp)
       PSSHEP(3) = float(iat)
       PSSHEP(4) = float(izt)
       PSSHEP(5) = efrm

*
       ihpr2(4) = iquench          ! Switch quenching
*
       ihpr2(11) = ih11            ! Set baryon production  
       ihpr2(12) = ih12            ! turn on/off decay of particles
       ihpr2(21) = ih21            ! turn on/off mothers-daughters info 
*
       ihpr2(3) = ihard            ! Switch hard scattering
*
       ihpr2(18) = ih18            ! Set B production                          
       hipr1(7) = hipr7
*
       ihpr2(8) = ihard_switch     ! Switch jet production
*
       hipr1(10) = ptjet           ! Pt jet
*   
*****  Initialize HIJING 
          call hijset(efrm,frame,proj,targ,iap,izp,iat,izt)
          IFIRST = 1
          IF(ISTAT.GT.1) IFIRST=2
        endif
*****  Generate event  
           natt = 0
           do while ( natt .le. 0 ) 
             call hijing ( frame, bmin, bmax ) 
         enddo
         VSSHEP(1) = HINT1(19)
         VSSHEP(2) = HINT1(20) 
         return
C           
 913  write(6,*) '*** can not open file:',input_file
      call abort()
      end

      REAL FUNCTION RAN(IDUMMY)

 100  RAN = RNDM(IDUMMY)
      if(RAN.LE.0. .OR. RAN.GE.1.) GOTO 100
      
      RETURN
      END

  
! $Log: hijev.F,v $
! Revision 1.2  2011/01/05 21:58:18  jwebb
! Added additional (but optional) line to configuration file to
! specify whether shadowing should be used or not.  Is backwards
! compatible with previous version.
! 














