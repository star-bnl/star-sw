
      SUBROUTINE PHO_SDECAY(Npos,Isp,Ilev)
C**********************************************************************
C
C     decay of single resonance of /POEVT1/:
C       decay in helicity frame according to polarization, isotropic
C       decay and decay with limited transverse phase space possible
C
C     ATTENTION:
C     reference to particle number of CPC has to exist
C
C     input:   NPOS    position in /POEVT1/
C              ISP     0  decay according to phase space
C                      1  decay according to transversal polarization
C                      2  decay according to longitudinal polarization
C                      3  decay with limited phase space
C              ILEV    decay mode to use
C                      1 strong only
C                      2 strong and ew of tau, charm, and bottom
C                      3 strong and electro-weak decays
C                      negative: remove mother resonance after decay
C
C     output:  /POEVT1/,/POEVT2/ final particles according to decay mode
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION amist , amsum , cx , cxs , cy , cys , cz , czs , 
     &                 DEPS , DT_RNDM , ee , EPS , gam , gbet , 
     &                 PHO_PMASS , ptot , wghd , wgsum , xi , xx
      DOUBLE PRECISION yy , zz
      INTEGER i , id , idabs , idcpc , ik , Ilev , imo1 , imo2 , imode , 
     &        iph1 , iph2 , IPHO_ANTI , ipos , Isp , k , kch , l , Npos
      SAVE 
 
      PARAMETER (EPS=1.D-15,DEPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  general particle data
      INCLUDE 'inc/popar2'
C  particle decay data
      INCLUDE 'inc/popar3'
C  auxiliary data for three particle decay
      INCLUDE 'inc/po3dcy'
 
      DIMENSION wghd(20) , kch(20) , id(3)
 
      imode = ABS(Ilev)
      IF ( LPRi.GT.4 .AND. IDEb(36).GE.15 ) WRITE (LO,'(1X,A,3I5)')
     &      'PHO_SDECAY: NPOS,ISP,ILEV' , Npos , Isp , Ilev
 
C  comment entry
      IF ( ISThep(Npos).GT.11 ) RETURN
 
C  particle stable?
      idcpc = IMPart(Npos)
      IF ( idcpc.EQ.0 ) RETURN
      idabs = ABS(idcpc)
      IF ( IDEc_list(1,idabs).EQ.0 ) RETURN
 
C  different decay modi (times)
      IF ( imode.EQ.1 ) THEN
         IF ( IDEc_list(1,idabs).NE.1 ) RETURN
      ELSE IF ( imode.EQ.2 ) THEN
         IF ( IDEc_list(1,idabs).GT.2 ) RETURN
      ELSE IF ( imode.EQ.3 ) THEN
         IF ( IDEc_list(1,idabs).GT.3 ) RETURN
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &         'PHO_SDECAY: invalid mode (ILEV)' , Ilev
         CALL PHO_ABORT
      END IF
 
C  decay products, check for mass limitations
      k = 0
      wgsum = 0.D0
      amist = PHEp(5,Npos)
      DO i = IDEc_list(2,idabs) , IDEc_list(3,idabs)
         amsum = 0.D0
         DO l = 1 , 3
            id(l) = ISEc_list(l,i)
            IF ( id(l).NE.0 ) amsum = amsum + PHO_PMASS(id(l),0)
         END DO
         IF ( amsum.LT.amist ) THEN
            k = k + 1
            wghd(k) = WG_sec_list(i)
            kch(k) = i
         END IF
      END DO
      IF ( k.EQ.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6,3E12.4)')
     &         'PHO_SDECAY: particle mass too small (NPOS,MA,DCYM)' , 
     &        Npos , amist , amsum
         CALL PHO_PREVNT(0)
         RETURN
      END IF
 
C  sample new decay channel
      xi = (DT_RNDM(amsum)-EPS)*wgsum
      k = 0
      wgsum = 0.D0
 100  k = k + 1
      wgsum = wgsum + wghd(k)
      IF ( xi.GT.wgsum ) GOTO 100
      ik = kch(k)
      id(1) = ISEc_list(1,ik)
      id(2) = ISEc_list(2,ik)
      id(3) = ISEc_list(3,ik)
      IF ( idcpc.LT.0 ) THEN
         id(1) = IPHO_ANTI(id(1))
         id(2) = IPHO_ANTI(id(2))
         IF ( id(3).NE.0 ) id(3) = IPHO_ANTI(id(3))
      END IF
 
C  rotation
      ptot = SQRT(PHEp(1,Npos)**2+PHEp(2,Npos)**2+PHEp(3,Npos)**2)
      cxs = PHEp(1,Npos)/ptot
      cys = PHEp(2,Npos)/ptot
      czs = PHEp(3,Npos)/ptot
C  boost
      gbet = ptot/amist
      gam = PHEp(4,Npos)/amist
 
      IF ( id(3).EQ.0 ) THEN
C  two particle decay
         CALL PHO_SDECY2(amist,PHO_PMASS(id(1),0),PHO_PMASS(id(2),0),
     &                   Isp)
      ELSE
C  three particle decay
         CALL PHO_SDECY3(amist,PHO_PMASS(id(1),0),PHO_PMASS(id(2),0),
     &                   PHO_PMASS(id(3),0),Isp)
      END IF
 
      IF ( Ilev.LT.0 ) THEN
         IF ( NHEp.NE.Npos ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2I5)')
     &            'PHO_SDECAY:ERROR: ' , 
     &           'cannot remove resonance (NPOS,NHEP)' , Npos , NHEp
            CALL PHO_ABORT
         END IF
         imo1 = JMOhep(1,Npos)
         imo2 = JMOhep(2,Npos)
         NHEp = NHEp - 1
      ELSE
         imo1 = Npos
         imo2 = 0
      END IF
      iph1 = IPHist(1,Npos)
      iph2 = IPHist(2,Npos)
 
C  back transformation and registration
      DO i = 1 , 3
         IF ( id(i).NE.0 ) THEN
            CALL PHO_LTRANS(gam,gbet,cxs,cys,czs,COD(i),COF(i),SIF(i),
     &                      PCM(i),ECM(i),ptot,cx,cy,cz,ee)
            xx = ptot*cx
            yy = ptot*cy
            zz = ptot*cz
            CALL PHO_REGPAR(1,0,id(i),imo1,imo2,xx,yy,zz,ee,iph1,iph2,0,
     &                      0,ipos,1)
         END IF
      END DO
 
C400  CONTINUE
C  debug output
      IF ( IDEb(36).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &        'PHO_SDECAY: /POEVT1/' , '--------------------'
         CALL PHO_PREVNT(0)
      END IF
 
      END SUBROUTINE
