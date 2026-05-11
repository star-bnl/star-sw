
      SUBROUTINE PHO_READPDS0(Nu)
      IMPLICIT NONE
      DOUBLE PRECISION aa , aimass , alambda , ALFaq , ALScteq , AMAss , 
     &                 dr , dummy , fl , fswitch , QALfa , QBAse , 
     &                 qbase1 , qbase2 , QINi , QMAx , TV , UPD , XMIn , 
     &                 XV
      INTEGER i , IORder , IPDsset , IPK , iret , ISEtch , MAXVAL , 
     &        MXF , MXPQX , MXQ , MXVal , MXX , n0 , nblk , NFL , NFMx , 
     &        ng , npts , NT , Nu
      INTEGER NX
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      CHARACTER line*80
      INTEGER IPDsformat
      PARAMETER (MXX=201,MXQ=40,MXF=6,MAXVAL=4)
      PARAMETER (MXPQX=(MXF+1+MAXVAL)*MXQ*MXX)
      DOUBLE PRECISION qv(0:MXQ)
 
      COMMON /CTQPAR1/ QBAse , XV(0:MXX) , TV(0:MXQ) , UPD(MXPQX) , 
     &                 ALScteq(0:MXQ)/CTQPAR2/ NX , NT , NFMx , 
     &                 MXVal/XQRANGE/ QINi , QMAx , 
     &                 XMIn/MASSTBL/ AMAss(6)/QCDTBL/ ALFaq , QALfa , 
     &                 IPK , IORder , NFL/SETCHANGE/ ISEtch , IPDsset , 
     &                 IPDsformat                 !for external use
 
      READ (Nu,'(A)') line
      READ (Nu,'(A)') line
 
      IF ( line(1:11).EQ.'  ipk, Ordr' ) THEN !post-CT10 .pds format;
C Set alphas(MZ) at scale Zm, quark masses, and evolution type
         IPDsformat = 10          !Post-CT10 .pds format
         READ (Nu,*) IPK , dr , QALfa , ALFaq , (AMAss(i),i=1,6)
         IORder = NINT(dr)
         READ (Nu,'(A)') line
         IF ( line(1:7).EQ.'  IMASS' ) THEN
            IPDsformat = 11       !CT14 .pds format
            READ (Nu,*) aimass , fswitch , n0 , n0 , n0 , NFMx , MXVal
            NFL = NFMx
         ELSE                     !Pre-CT14 format
            READ (Nu,*) n0 , n0 , n0 , NFMx , MXVal
         END IF                   !Line(1:7)
 
      ELSE                        !old .pds format;
         IPDsformat = 6           !CTEQ6.6 .pds format; alpha_s  is not specified
         READ (Nu,*) dr , fl , alambda , (AMAss(i),i=1,6)
                                                        !set Lambda_QCD
         IORder = NINT(dr)
 
         READ (Nu,'(A)') line
         READ (Nu,*) dummy , dummy , dummy , NFMx , MXVal , n0
      END IF                      !Line(1:11...
 
      READ (Nu,'(A)') line
      READ (Nu,*) NX , NT , n0 , ng , n0
 
      IF ( ng.GT.0 ) READ (Nu,'(A)') (line,i=1,ng+1)
 
      READ (Nu,'(A)') line
      IF ( IPDsformat.GE.11 ) THEN
                                  !CT14 format with alpha_s values
         READ (Nu,*) QINi , QMAx , (qv(i),TV(i),ALScteq(i),i=0,NT)
      ELSE                        !pre-CT14 format
         READ (Nu,*) QINi , QMAx , (qv(i),TV(i),i=0,NT)
      END IF                      !ipdsformat.ge.11
 
C check that qBase is consistent with the definition of Tv(0:nQ) for 2 values of Qv
      qbase1 = qv(1)/EXP(EXP(TV(1)))
      qbase2 = qv(NT)/EXP(EXP(TV(NT)))
      IF ( ABS(qbase1-qbase2).GT.1E-5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'pho_Readpds0: something wrong with qbase'
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 'qbase1, qbase2=' , 
     &        qbase1 , qbase2
         STOP
      ELSE
         QBAse = (qbase1+qbase2)/2.0D0
      END IF                    !abs(qbase1...
 
      READ (Nu,'(A)') line
      READ (Nu,*) XMIn , aa , (XV(i),i=1,NX)
      XV(0) = 0D0
 
      nblk = (NX+1)*(NT+1)
      npts = nblk*(NFMx+1+MXVal)
      READ (Nu,'(A)') line
      READ (Nu,*,IOSTAT=iret) (UPD(i),i=1,npts)
 
C                        ****************************
      END SUBROUTINE
