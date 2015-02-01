!

! Code converted using TO_F90 by Alan Miller
! Date: 2012-03-03  Time: 17:00:22

!> \file
!! Test program.

!> Simple tests with GBL.
!!
!! Runs with gcc4 (for RANDOM_NUMBER) or gcc3 (simple uniform distribution).
!!
!! Simple track model:
!!      - initial direction (COSL,0.,SINL)
!!      - solenoidal magnectic field B=(0,0,Bz)
!!      - simplified jacobian (parabolic in arclength)
!!      - distortions in curvilinear system
!!      - multiple scattering

PROGRAM test
    use gbltraj, only: gblini, gbladp, gbladm, gblads, gbladl, gbladg, gbladx, &
                       gblfit, gblres, gblmp2

    PARAMETER (nloc=2) ! number of 'local' parameters
    PARAMETER (mp=5+nloc)
    PARAMETER (mp2=(mp+1)*mp/2)
    DOUBLE PRECISION :: dpar(mp), dcov(mp2), dpseed(mp2), djac(5,5),daux(15),  &
    ajacp,ajacn,pm2l,pl2m,det

    DIMENSION clpar(5),dirt(3),dirm(3,2),tmp(5),sarc(100),  &
    dif(2),sig(2),prec(2),ajacp(5,5),ajacn(5,5),  &
    dirz(3),diru(3),dirv(3),pm2l(2,2),pl2m(2,2),beta0(2),  &
    clerr(5),lder(nloc),derlc(2,nloc),thsig(2),thprec(2)

    DATA dirz / 0.0, 0.0, 1.0 / ! Z direction


    ! (type of) local system

    !     ITYPE=0 ! curvilinear track parameter (q/p,lambda,phi,x_t,y_t)
    itype=1 ! curvilinear local system    (q/p,v',w',v,w), (v,w)=(x_t,y_t)
    !     ITYPE=2 ! 'magnetic'  local system    (q/p,v',w',v,w), (v,w)=(x_t,z)
    !               (perpendicular, parallel to B)
    PRINT *, ' GBLTST ITYPE: ', itype

    bz=1.0 ! magnetic field
    bfac=bz*0.2998
    !     initial track direction (from prefit)
    sinl=0.3               ! sin(lambda)
    cosl=SQRT(1.0-sinl**2) ! cos(lambla)
    phi=0.                 ! move in X direction
    !     preset to unit matrix
    DO i=1,5
        DO j=1,5
            ajacp(i,j)=0.0D0
            ajacn(i,j)=0.0D0
        END DO
        ajacp(i,i)=1.0D0
        ajacn(i,i)=1.0D0
    END DO
    beta0(1)=0.0
    beta0(2)=0.0
    !     distortions to track parameters
    clerr(1)= 0.001
    clerr(2)=-0.1
    clerr(3)= 0.2
    clerr(4)=-0.15
    clerr(5)= 0.25

    ip=0
    DO i=1,mp
        DO j=1,i
            dpseed(ip+j)=0.0D0
        END DO
        ip=ip+i
        IF (i <= 5) dpseed(ip)=1.0D0/DBLE(clerr(i)**2)
    END DO
    !      print *, ' DPSEED '
    !      CALL DBPRV(6,DPSEED,MP)

    ntr=10000 ! number of tracks
    schi2=0.0
    DO itr=1,ntr
        n=10
        s=0.0            ! at vertex
        s0=10.           ! first measurement
        step1=1.0/cosl   ! constant steps
        step2=2.0/cosl   !    in RPhi
        dert0=1.0
        !       curvilinear distortions d(q/p,theta,phi,xt,yt)
        DO k=1,5
            !          CLPAR(K)=0.0
            !          CLPAR(K)=CLERR(K)
            clpar(k)=clerr(k)*unrm()
        END DO
  
        ipnt1=0        ! first measurement, distortions defined here
  
        theta0=1.0E-3  ! 1 mrad multiple scatt.
        IF (itype == 1) THEN
            thsig(1)=theta0         ! MS error in v' (dx_t/dz_t)
            thsig(2)=theta0         ! MS error in w' (dy_t/dz_t)
        ELSE IF (itype == 2) THEN
            thsig(1)=theta0/cosl    ! MS error in v' (dx_t/ds_2D)
            thsig(2)=theta0/cosl**2 ! MS error in w' (dz/ds_2D)
        ELSE
            thsig(1)=theta0         ! MS error in lambda
            thsig(2)=theta0/cosl    ! MS error in phi
        END IF
        thprec(1)=1.0/thsig(1)**2 ! diagonal of inverse
        thprec(2)=1.0/thsig(2)**2 ! covariance matrix
  
        sig(1)=1.0E-3         ! 10 mu resolution
        sig(2)=1.0E-3         ! 10 mu resolution
        prec(1)=1.0/sig(1)**2 ! diagonal of inverse
        prec(2)=1.0/sig(2)**2 ! covariance matrix
        CALL gblini(1)
        !        CALL GBLINP(1,1)
        !        point at vertex
        !        CALL gbladp(ajacn,ipnt)
        !        sarc(ipnt)=s
        !        CALL gbltjc(itype,s0,bfac,cosl,ajacn)

        ! create track
        s=s0
  
        DO i=1,n
            !         print *, ' measurement ', I, S, OFF, SLP
            ! track direction
            cphi=COS(phi)
            sphi=SIN(phi)
            dirt(1)= cosl*cphi
            dirt(2)= cosl*sphi
            dirt(3)= sinl
            ! U = Z x T / |Z x T|
            diru(1)=-sphi
            diru(2)= cphi
            diru(3)= 0.
            ! V = T x U
            dirv(1)=-sinl*cphi
            dirv(2)=-sinl*sphi
            dirv(3)= cosl
            ! V = Z
            IF (itype == 2) THEN
                dirv(1)=0.
                dirv(2)=0.
                dirv(3)=1.
            END IF
            ! RPhi meas. ( = Y as track moves in X direction)
            eps=0.
            IF (MOD(i,2) == 0) eps=0.5
            dirm(1,1)=eps ! direction of measurement
            dirm(2,1)=SQRT(1.0-eps**2)
            dirm(3,1)=0.0
            ! Z meas.
            eps=0.
            !          IF (MOD(I,2).EQ.0) EPS=0.3
            dirm(1,2)=0.0 ! direction of measurement
            dirm(2,2)=eps
            dirm(3,2)=SQRT(1.0-eps**2)
            ! projection (du/dm: measurement to local (curvilinear))
            DO l=1,2
                pm2l(l,1)= DBLE(dirm(1,l)*diru(1)+dirm(2,l)*diru(2)+dirm(3,l)*diru(3))
                pm2l(l,2)= DBLE(dirm(1,l)*dirv(1)+dirm(2,l)*dirv(2)+dirm(3,l)*dirv(3))
            END DO
            ! projection (dm/du: local to measurement)
            det=pm2l(1,1)*pm2l(2,2)-pm2l(1,2)*pm2l(2,1)
            pl2m(1,1)= pm2l(2,2)/det
            pl2m(1,2)=-pm2l(1,2)/det
            pl2m(2,1)=-pm2l(2,1)/det
            pl2m(2,2)= pm2l(1,1)/det
            ! measurement - prediction in measurement system with error
            DO m=1,2
                dif(m)=clpar(4)*SNGL(pl2m(m,1)) +clpar(5)*SNGL(pl2m(m,2))+sig(m)*unrm()
            END DO
            ! distort with local parameters
            !          DIF(1)=DIF(1)+DERT0*0.0075
            !          DIF(2)=DIF(2)-DERT0*0.0025
    
            CALL gbladp(ajacn,ipnt)
            sarc(ipnt)=s
            CALL gbladm(pl2m,dif,prec)
            IF (ipnt1 == 0) ipnt1=ipnt
            ! local or global parameters
            lder(1)=4711
            lder(2)=4712
            derlc(1,1)= dert0
            derlc(2,1)=0.0
            derlc(1,2)=0.0
            derlc(2,2)=-dert0
            !          CALL GBLADL(2,DERLC,IRET)
            !          CALL GBLADG(2,LDER,DERLC,IRET)
            dert0=-dert0
            ! propagate by STEP1
            ds=step1
            CALL gbltjc(itype,step1,bfac,cosl,ajacn)
    
            DO k=1,5
                tmp(k)=clpar(k)
            END DO
            DO k=1,5
                clpar(k)=0.0
                DO l=1,5
                    clpar(k)=clpar(k)+SNGL(ajacn(k,l))*tmp(l)
                END DO
            END DO
            s=s+step1
            IF (i < n) THEN
                CALL gbladp(ajacn,ipnt)
                sarc(ipnt)=s
                CALL gblads(beta0,thprec)
            END IF
            ! scatter a little
            clpar(2)=clpar(2)+thsig(1)*unrm()
            clpar(3)=clpar(3)+thsig(2)*unrm()
            ! propagate by STEP1
            CALL gbltjc(itype,step2,bfac,cosl,ajacn)

            DO k=1,5
                tmp(k)=clpar(k)
            END DO
            DO k=1,5
                clpar(k)=0.0
                DO l=1,5
                    clpar(k)=clpar(k)+SNGL(ajacn(k,l))*tmp(l)
                END DO
            END DO
            s=s+step2
        END DO
  
        npnt=ipnt
               CALL GBLADX(IPNT1,DPSEED)
  
        !        CALL GBLDMP
        CALL gblfit('',mrank,np,ndf,chi2,wls)
        !        print *, ' GBLFIT ', MRANK, NP, NDF, CHI2, WLS
        schi2=schi2+chi2/FLOAT(ndf)
        !        CALL GBLFIT('HT',MRANK,NDF,CHI2,WLS)
        IF (itr <= 1) THEN
            PRINT *, ' GBLFIT ', mrank, np, ndf, chi2, wls
            CALL gblres(ipnt1,dpar,dcov)
            PRINT *, ' DPAR ', (dpar(k),k=1,np)
            PRINT *, ' DCOV(loc) '
            CALL dbprv(6,dcov,np)
            CALL gbll2c(itype,cosl,djac)
            CALL dbavat(dcov,djac,daux,5,5)
            PRINT *, ' DCOV(cvl) '
            CALL dbprv(6,daux,5)
        END IF
  
    !        CALL GBLMP2(IRET) ! prepare input for Millepede-II
  
    END DO

    schi2=schi2/FLOAT(ntr)
    PRINT *, ' === <Chi2/ndf> === ', ntr, schi2

END PROGRAM test

!> Simplified jacobian (quadratic in DS)

SUBROUTINE gbltjc(itype,ds,bfac,cosl,ajac)


    INTEGER, INTENT(IN)                      :: itype
    REAL, INTENT(IN)                         :: ds
    REAL, INTENT(IN)                         :: bfac
    REAL, INTENT(IN OUT)                     :: cosl
    DOUBLE PRECISION, INTENT(OUT)            :: ajac(5,5)


    IF (itype == 1) THEN
        !       curvilinear local system (q/p,v',w',v,w), (v,w)=(x_t,y_t)
        ajac(1,1)= 1.0D0
        ajac(2,1)=-DBLE(bfac*ds*cosl)
        ajac(2,2)= 1.0D0
        ajac(3,3)= 1.0D0
        ajac(4,1)=-DBLE(0.5*bfac*ds*ds*cosl)
        ajac(4,2)= DBLE(ds)
        ajac(4,4)= 1.0D0
        ajac(5,3)= DBLE(ds)
        ajac(5,5)= 1.0D0
    ELSE IF (itype == 2) THEN
        !       local system (q/p,v',w',v,w), (v,w)=(x_t,z)
        ajac(1,1)= 1.0D0
        ajac(2,1)=-DBLE(bfac*ds)
        ajac(2,2)= 1.0D0
        ajac(3,3)= 1.0D0
        ajac(4,1)=-DBLE(0.5*bfac*ds*ds*cosl)
        ajac(4,2)= DBLE(ds*cosl)
        ajac(4,4)= 1.0D0
        ajac(5,3)= DBLE(ds*cosl)
        ajac(5,5)= 1.0D0
    ELSE
        !       curvilinear track parameter (q/p,lambda,phi,x_t,y_t)
        ajac(1,1)= 1.0D0
        ajac(2,2)= 1.0D0
        ajac(3,1)=-DBLE(bfac*ds)
        ajac(3,3)= 1.0D0
        ajac(4,1)=-DBLE(0.5*bfac*ds*ds*cosl)
        ajac(4,3)= DBLE(ds*cosl)
        ajac(4,4)= 1.0D0
        ajac(5,2)= DBLE(ds)
        ajac(5,5)= 1.0D0
    END IF

    RETURN
END SUBROUTINE gbltjc

!> Simplified transformation local to curvilinear

SUBROUTINE gbll2c(itype,cosl,djac)


    INTEGER, INTENT(IN)                      :: itype
    REAL, INTENT(IN OUT)                     :: cosl
    DOUBLE PRECISION, INTENT(OUT)            :: djac(5,5)

    !     preset with unit matrix
    DO k=1,5
        DO l=1,5
            djac(k,l)=0.0D0
        END DO
        djac(k,k)=1.0D0
    END DO

    IF (itype == 1) THEN
        !       curvilinear local system (q/p,v',w',v,w), (v,w)=(x_t,y_t)
        djac(2,2)=0.0D0
        djac(2,3)=1.0D0/DBLE(cosl)
        djac(3,2)=1.0D0
        djac(3,3)=0.0D0
    ELSE IF (itype == 2) THEN
        !       local system (q/p,v',w',v,w), (v,w)=(x_t,z)
        djac(2,2)=0.0D0
        djac(2,3)=1.0D0
        djac(3,2)=DBLE(cosl)**2
        djac(3,3)=0.0D0
        djac(5,5)=DBLE(cosl)
    END IF

    RETURN
END SUBROUTINE gbll2c

!> Simple unit normal
FUNCTION unrm()
    DIMENSION r(12)
    !      DATA IX / 4711 /      ! use for gcc3
    CALL random_number(r) ! comment out for gcc3
    unrm=-6.0
    DO k=1,12
        !      R(K)=UNIF(IX)         ! use for gcc3
        unrm=unrm+r(k)
    END DO
END

!> Simple uniform random numbers
FUNCTION unif(ix)
    ! portable random number generator using the recursion:
    ! IX = 16807 * IX MOD (2**31-1)
    k1 = ix/127773
    ix = 16807* (ix - k1*127773) - k1 * 2836
    IF (ix < 0) ix = ix + 2147483647
    unif = ix*4.656612875E-10
    RETURN
END FUNCTION unif

