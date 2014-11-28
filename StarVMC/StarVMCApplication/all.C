// COMMON /TRCOM3/ 
static TRMatrix A(5,5);
static TRMatrix B(5,5);
static TRSymMatrix S(5);
static TVector3 TN;
static TRVector T(5);
static Double_t cosl, sinl, cosp, sinp, cosl1;
static Int_t    new = 0;

#if 0
// Codes from StarVMC/geant3/erpremc/*.F
//________________________________________________________________________________
/************************************************************************
 *     SUBR. TRPRFN(X1,P1,H1,X2,P2,H2,CH,XL,*R*,Mvar,Iflag,Itran,Ierr*)
 *     Origin W.Wittek    EMCSW/81/18
 *     Finite step length case coded by V.Innocente ( Feb. 88 )
 *     code improved:                   V.Innocente ( April. 90 )
 *                   inline code replaces external function
 *     code improved:                   V.Innocente ( January 91 )
 *                   effect of energy loss added              
 *_______________________________________________________________________
 * Error propagation along a particle trajectory in a magnetic field
 *     routine assumes that in the interval (x1,x2) the quantities 1/p
 *     and (hx,hy,hz) are constant.
 *
 * *** Iflag  =  -1   initialization, transformation of error matrix from
 *                    external to SC variables
 *            =   0   error propagation from x1 to x2
 *            =   1   transformation of error matrix from SC to
 *                    external variables
 *
 *     Itran          used for Iflag == 0 or 1 only
 *            =   0   transformation matrix is updated ,but error matrix is not
 *                    transformed
 *           =    1   transf. matrix is updated  and error matrix is transformed
 *
 *     Mvar           specifies type of external variables
 *            =   0   ( 1/p,lambda,phi,yt, zt ;   SC   )
 *            =   1   ( 1/p,  y',  z',  y,  z ; spline )
 *
 * *** x1, p1, h1     x,y,z components of position, momentum and magnetic   input
 *                    field vector/gradient at starting point of interval
 *     x2, p2, h2     ......  at end point of interval                      input
 *     ch             charge of particle                                    input
 *     xl             pathlength from x1 to x2   ( negative if opposite
 *                    to actual movement of particle )                      input
 *     r              error matrix  (triangle)                       input/output
 *     b              5 * 5 transformation matrix for errors in
 *                    SC variables                                         output
 *
 * *** Ierr   =  1    illegal value of Mvar                                output
 *               2    momentum is zero
 *               3    h*alpha/p at x1 and x2 differ too much
 *               4    particle moves in z - direction
 *
 ************************************************************************/
Int_t StGeanePropagator::Trprfn(Double_t *x1,Double_t *p1,Double_t *h1,
				Double_t *x2,Double_t *p2,Double_t *h2,
				Double_t  ch,Double_t  xL,Double_t *r,
				Int_t Mvar,Int_t Iflag,Int_t Itran) {
  TVector3 X1(x1);  TVector3 P1(p1);
  TVector3 X2(x2);  TVector3 P2(p2);
  TVector3 H1(h1);  TVector3 H2(h2);
  TRSymMatrix R(5,r);
  return Ierr = Trprfn(X1,P1,H1,X2,P2,H2,ch,xL,R,Mvar,Iflag,Itran);
}
//________________________________________________________________________________
Int_t StGeanePropagator::Trprfn(TVector3 &X1,TVector3 &P1,TVector3 &H1,
				TVector3 &X2,TVector3 &P2,TVector3 &H2,
				Double_t  ch,Double_t  xL,TRSymMatrix &R,
				Int_t Mvar,Int_t Iflag,Int_t Itran) {
  static Int_t Init = 0;
  static Int_t new = 0;
  if (Iflag < 0) {// Transform Error Matrix From External To Internal Variables;
    new=1;
    if (Mvar == 1) {
      if (P1[0] == 0.) return 4;
      Double_t pa1 = P1.Mag();
      if (pa1 == 0.) return 2;
      TVector3 PS(1./pa1, P1[1]/P1[0], P1[2]/P1[0]);
      Double_t spx=1.;
      if (P1[0] < 0.) spx=-1.;
      return Trspsc(PS,R,PC,R,H1,ch,spx);
    } else {if (Mvar != 0) return 1;}
    return 0;
  }
  if (Iflag > 0) {// Transform Error Matrix From Internal To External Variables;
    new=1;
    if (Mvar != 1) return 1;
    IF (TMath::Abs(P2[0])  <  1e-30) P2[0] = 1e-30;
    Double_t pm2 = 1./P2.Mag();
    TVector3 PC(pm2, TMath::ASin(P2[2]*PC[0]), TMath::ATan2(P2[1],P2[0]));
    return Trscsp(PC,R,PS,R,H2,ch,Ierr,spx);
  } 
  // Error Propagation on a Helix Assuming SC Variables
  Double_t pa1 = P1.Mag();
  Double_t pa2 = P2.Mag();
  if (pa1*pa2 == 0.) return 2;
  Double_t pm1=1./pa1;
  Double_t pm2=1./pa2;
  Double_t dpm = pm2 - pm1;
  TVectro3 T1 = pm1*P1;
  TVector3 T2 = pm2*P2;
  
  Double_t sinl=T2[2];
  Double_t sinl0=T1[2];
  Double_t cosl=TMath::Sqrt(TMath::Abs(1.-sinl*sinl));
  if (cosl == 0.) return 4;
  Double_t cosl1=1./cosl;
  Double_t cosl0=TMath::Sqrt(TMath::Abs(1.-sinl0**2));
  // Define Transformation Matrix Between X1 And X2 For
  // Neutral Particle Or Fieldfree Region
  TRMatrix A(5,5,
	     1., 0.,      0., 0., 0.,    // 1/p
	     0., 1.,      0., 0., 0.,    // tY
	     0., 0.,      1., 0., 0.,    // tZ
	     0., 0., xL*cosl, 1., 0.);    // Y
  
  if (ch != 0.) {
    Double_t ha1 = H1.Mag();
    Double_t ha2 = H2.Mag();
    Double_t ham1=ha1*pm1;
    Double_t ham2=ha2*pm2;
    hamx=TMath::Max(ham1,ham2);
    if (hamx != 0.) {
      // Check Whether H*Alpha/P Is Too Different At X1 And X2
      Double_t gam;
      if (ha2 != 0.) gam = H2*T2/ha2;
      else           gam = H1*T1/ha1;
      Double_t alpha2=1.-gam*gam;
      TVector3 Hs1 = pm1*H1;
      TVector3 Hs2 = pm2*H2;
      TVector3 dH2 = Hs1 - Hs2;
      Double_t dh2= dH2.Mag2();
      if (dh2*alpha2 > Delhp6*Delhp6) return 3;
      // Define Average Magnetic Field And Gradient
      pm12=(pm1+pm2)*0.5;
      p12=1./(2.*pm12);
      TVector3 HN = p12*ch*CFACT8*(Hs1 + Hs2);
      Double_t hm = HN.Mag();
      HN /= hm;
      Double_t pav = .5*(pa1+pa2);
      Double_t q = - hm/pav;
      Double_t theta = q*xL;
      Double_t sint = TMath::Sin(theta);
      Double_t cost = TMath::Cos(theta);
      Double_t gamma= HN*T2;
      TVector3 AN2 = HN.Cross(T2);
      TVector3 Z(0,0,1);
      TVector3 U1 = Z.Cross(T1);  U1 /= T1.Pt(); TVector3 V1 = T1.Cross(U1);
      TVector3 U2 = Z.Cross(T2);  U2 /= T2.Pt(); TVector3 V2 = T2.Corss(U2);
      TVector3 DX = X1 - X2;
      // Complete Transformation Matrix Between Errors At X1 And X2
      // Field Gradient Perpendicular To Track Is Presently Not
      // Taken Into Account
      Double_t qp  = q*pav;
      Double_t anv = -HN*U2;
      Double_t anu =  HN*V2;
      Double_t omcost = 1.-cost;
      Double_t tmsint = theta-sint;
      TVectro3 HU1 = HN.Cross(U1);
      TVector3 HV1 = HN.Cross(V1);
      //   1/P
      A(1,1) = 1.    -dpm*pav*(1.+T2*DX/xL) +2.*dpm*pav;
      A(1,2) =       -dpm/theta*(tmsint*gamma*HN*V1 + sint*V1*T2 + omcost*HV1*T2);
      A(1,3) = -cosl0*dpm/theta*( tmsint*gamma*HN*U1+ sint*U1*T2 + omcost*HU1*T2);
      A(1,4) =       -dpm/xL*U1*T2;
      A(1,5) =       -dpm/xL*V1*T2;
      //   Lambda
      A(2,1) = -qp*anv*T2*DX*(1.+dpm*pav);
      A(2,2) = cost*V1*V2 +  sint*HV1*V2 + omcost*(HN*V1)*(HN*V2) + anv*( -sint*V1*T2 + omcost*V1*AN2 - tmsint*gamma*HN*V1);
      A(2,3) = cost*U1*V2 +  sint*HU1*V2 + omcost*(HN*U1)*(HN*V2) + anv*( -sint*U1*T2 + omcost*U1*AN2 - tmsint*gamma*HN*U1);
      A(2,3) = cosl0*A(2,3);
      A(2,4) = -q*anv*U1*T2;
      A(2,5) = -q*anv*V1*T2;
      //   Phi
      A(3,1) = -qp*anu*T2*DX*cosl1*(1.+dpm*pav);
      A(3,2) = cost*V1*U2 + sint*HV1*U2 + omcost*(HN*V1)*(HN*U2) + anu*( -sint*V1*T2 + omcost*V1*AN2 - tmsint*gamma*HN*V1);
      A(3,2) = cosl1*A(3,2);
      A(3,3) = cost*U1*U2 + sint*HU1*U2 + omcost*(HN*U1)*(HN*U2) + anu*( -sint*U1*T2 + omcost*U1*AN2 - tmsint*gamma*HN*U1);
      A(3,3) = cosl1*cosl0*A(3,3);
      A(3,4) = -q*anu*U1*T2*cosl1;
      A(3,5) = -q*anu*V1*T2*cosl1;
      //   Yt
      A(4,1) = pav*U2*DX*(1.+dpm*pav);
      A(4,2) =                                                         (  sint*V1*U2 + omcost*HV1*U2 + tmsint*(HN*U2)*(HN*V1))/q;
      A(4,3) =                                                   cosl0*(  sint*U1*U2 + omcost*HU1*U2 + tmsint*(HN*U2)*(HN*U1))/q;
      A(4,4) = U1*U2;
      A(4,5) = V1*U2;
      //   Zt
      A(5,1) = pav*V2*DX*(1.+dpm*pav);
      A(5,2) =                                                         (  sint*V1*V2 + omcost*HV1*V2 + tmsint*(HN*V2)*(HN*V1))/q;
      A(5,3) =                                                   cosl0*(  sint*U1*V2 + omcost*HU1*V2 + tmsint*(HN*V2)*(HN*U1))/q;
      A(5,4) = U1*V2;
      A(5,5) = V1*V2;
    }
  }
  // new = 0  Transformation Matrix Is Updated
  //       1  Transformation Matrix Is Initialized
  if (new != 0) {
    new=0;
    TRMatrix B(A,TRArray::kTransposed);
  } else {
    TRMatrix D(A);
    TRMatrix B(D,TRArray::kAxB,A);
  }
  if (Itran != 0) {
    TRSymMatrix S(R);
    // Transform Error Matrix
    R = TRSymMatrix(B,TRArray::kATxSxA,S);
    new = 1;
  }
  return 0;
}
//________________________________________________________________________________
/*******************************************************************************
 * Error propagation along a particle trajectory in a magnetic field
 *     routine assumes that in the interval (x1,x2) the quantities 1/p
 *     and (hx,hy,hz) are rather constant. delta(phi) must not be too large
 *     Authors: A. Haas and W. Wittek
 * *** Iflag  =  -1   initialization, transformation of error matrix from
 *                    external to sc variables
 *            =   0   error propagation from x1 to x2
 *            =   1   transformation of error matrix from sc to
 *                    external variables
 *     Itran          used for Iflag = 0 or 1 only
 *            =   0   transformation matrix is updated ,but error matrix is not
 *                    transformed
 *           =    1   transf. matrix is updated  and error matrix is transformed
 *
 *     Mvar           specifies type of external variables
 *            =   0   ( 1/p,lambda,phi,yt, zt ;   sc   )
 *            =   1   ( 1/p,  y',  z',  y,  z ; spline )
 *
 * *** x1, p1, h1     x,y,z components of position, momentum and magnetic   input
 *                    field vector/gradient at starting point of interval
 *     x2, p2, h2     ......  at end point of interval                      input
 *     ch             charge of particle                                    input
 *     xL             pathlength from x1 to x2   ( negative if opposite
 *                    to actual movement of particle )                      input
 *     r              error matrix  (triangle)                       input/output
 *     b              5 * 5 transformation matrix for errors in
 *                    sc variables                                         output
 *
 * *** Ierr   =  1    illegal value of Mvar                                output
 *               2    momentum is zero
 *               3    h*alpha/p at x1 and x2 differ too much
 *                    or delta phi is too large
 *               4    particle moves in z - direction
 ******************************************************************************/
Int_t StGeanePropagator::Trprop(Double_t x1[3],Double_t p1[3],Double_t h1[3],
				Double_t x2[3],Double_t p2[3],Double_t h2[3],
				Double_t ch,Double_t xL,Double_t r[15],
				Int_t Mvar,Int_t Iflag,Int_t Itran) {

  TVector3 X1(x1);  TVector3 P1(p1); TVector3 H1(h1);
  TVector3 X2(x2);  TVector3 P2(p2); TVector3 H2(h2);
  TRSymMatrix R(5,r);
  return StGeanePropagator::Trprop(X1,P1,H1,X2,P2,H2,ch,xL,R,Mvar,Iflag,Itran);
}
//________________________________________________________________________________
Int_t StGeanePropagator::Trprop(TVector3 &X1,TVector3 &P1,TVector3 &H1,
				TVector3 &X2,TVector3 &P2,TVector3 &H2,
				Double_t  ch,Double_t  xL,TRSymMatrix &R,
				Int_t Mvar,Int_t Iflag,Int_t Itran) {
  static Int_t Init = 0;
  Int_t new = 0;
  if (Iflag < 0) { // transform error matrix from external to internal variables
    return Trprfn(X1,P1,H1,X2,P2,H2,ch,xL,R,Mvar,Iflag,Itran);
  }
C
C ERROR propagation ON A HELIX ASSUMING SC VARIABLES
 
C
   20 pa1=TMath::Sqrt(P1[0]**2+P1[1]**2+P1[2]**2)
      pa2=TMath::Sqrt(P2[0]**2+P2[1]**2+P2[2]**2)
      if (pa1*pa2 == 0.) return 2;
      pm1=1./pa1
      pm2=1./pa2
C
      TN[0]=P1[0]+P2[0]
      TN[1]=P1[1]+P2[1]
      TN[2]=P1[2]+P2[2]
      pm12=1./TMath::Sqrt(TN[0]**2+TN[1]**2+TN[2]**2)
      TN[0]=TN[0]*pm12
      TN[1]=TN[1]*pm12
      TN[2]=TN[2]*pm12
C
      sinl=TN[2]
      cosl=TMath::Sqrt(TMath::Abs(1.-sinl*sinl))
      if (cosl == 0.) return 4;
      cosl1=1./cosl
      SINP=TN[1]*cosl1
      COSP=TN[0]*cosl1
C
C *** DEFINE TRANSFORMATION MATRIX BETWEEN X1 AND X2 FOR
C *** NEUTRAL paRTICLE OR FIELDFREE REGION
C
      DO 26 I=1,5
         DO 15 K=1,5
            A(I,K)=0.
   15    CONTINUE
         A(I,I)=1.
   26 CONTINUE
      A(4,3)=xL*cosl
      A(5,2)=xL
C
      if (ch == 0.) GO TO 45
      Double_t ha1 = H1.Mag();
	  Double_t ha2 = H2.Mag();   
      ham1=ha1*pm1
      ham2=ha2*pm2
      hamx = TMath::Max(ham1,ham2)
      if (hamx == 0.) GO TO 45
C
C *** DEFINE AVERAGE MAGNETIC FIELD AND GRADIENT
C
      pm12=(pm1+pm2)*0.5
      p12=1./(2.*pm12)
      HN[0]=(H1[0]*pm1+H2[0]*pm2)*p12*ch*CFACT8
      HN[1]=(H1[1]*pm1+H2[1]*pm2)*p12*ch*CFACT8
      HN[2]=(H1[2]*pm1+H2[2]*pm2)*p12*ch*CFACT8
      HN(4)=(H1(4)*pm1+H2(4)*pm2)*p12*ch*CFACT8
      HN(5)=(H1(5)*pm1+H2(5)*pm2)*p12*ch*CFACT8
      HN(6)=(H1(6)*pm1+H2(6)*pm2)*p12*ch*CFACT8
      HN(7)=(H1(7)*pm1+H2(7)*pm2)*p12*ch*CFACT8
      HN(8)=(H1(8)*pm1+H2(8)*pm2)*p12*ch*CFACT8
      HN(9)=(H1(9)*pm1+H2(9)*pm2)*p12*ch*CFACT8
C
      B0=HN[0]*COSP+HN[1]*SINP
      B2=-HN[0]*SINP+HN[1]*COSP
      B3=-B0*sinl+HN[2]*cosl
      TGL=sinl*cosl1
C
C
C *** chECK WHETHER H*alpha/P IS TOO DIFFERENT AT X1 AND X2
C     AND WHETHER chANGE OF TRACK DIRECTION DUE TO MAG.FIELD IS TOO LARGE
C
C
      if (ha2 == 0.) GO TO 29
 
      gam=(H2[0]*TN[0]+H2[1]*TN[1]+H2[2]*TN[2])/ha2
      GO TO 28
   29 gam=(H1[0]*TN[0]+H1[1]*TN[1]+H1[2]*TN[2])/ha1
   28 CONTINUE
      alpha=TMath::Sqrt(TMath::Abs(1.-gam**2))
C
      dh2=(H1[0]*pm1-H2[0]*pm2)**2+
     1    (H1[1]*pm1-H2[1]*pm2)**2+
     1    (H1[2]*pm1-H2[2]*pm2)**2
      if (dh2*alpha**2 > Delhp6**2) GO TO 903
      alphaQ=-alpha*CFACT8*(ham1+ham2)*0.5
      DFI=TMath::Abs(xL*alphaQ)
      if (DFI > Delfi6) GO TO 903
C
C *** COMPLETE TRANSFORMATION MATRIX BETWEEN ERRORS AT X1 AND X2
C *** TAKING INTO ACCOUNT  FIELD GRADIENT PERPENDICULAR TO TRACK
C
   30 COSP2=COSP*COSP
      SINP2=SINP*SINP
      COSIP=COSP*SINP
C
      G22=SINP2*HN(9)+COSP2*HN(8)-2.0*COSIP*HN(7)
      G33=sinl*sinl*(COSP2*HN(9)+SINP2*HN(8)+2.0*COSIP*HN(7))
     ++cosl*(cosl*HN(6)-2.0*sinl*(COSP*HN(4)+SINP*HN(5)))
      G23=sinl*(COSIP*(HN(9)-HN(8))+(SINP2-COSP2)*HN(7))
     ++cosl*(COSP*HN(5)-SINP*HN(4))
C
      A(2,1)=xL*B2
      A(2,3)=-B0*xL*pm12
      A(2,4)=(B2*B3*pm12+G22)*xL*pm12
      A(2,5)=(-B2*B2*pm12+G23)*xL*pm12
C
      A(3,1)=-xL*B3*cosl1
      A(3,2)=B0*xL*pm12*cosl1**2
      A(3,3)=1.+TGL*B2*xL*pm12
      A(3,4)=(-B3*B3*pm12-G23)*xL*pm12*cosl1
      A(3,5)=(B3*B2*pm12-G33)*xL*pm12*cosl1
C
      A(4,5)=-B3*TGL*xL*pm12
      A(5,4)=B3*TGL*xL*pm12
C
   45 CONTINUE
C
C *** new = 0  TRANSFORMATION MATRIX IS UPDATED
C           1  TRANSFORMATION MATRIX IS INITIALIZED
C
      if (new == 0) GO TO 23
      new=0
      DO 25 I=1,5
         DO 24 K=1,5
            B(I,K)=A(I,K)
   24    CONTINUE
   25 CONTINUE
      GO TO 27
   23 CONTINUE
C
      CALL XMM55(A,B,B)
C
   27 CONTINUE
   80 if (Itran == 0) GO TO 90
C
C
      J=0
      DO 22 I=1,5
         DO 21 K=I,5
            J=J+1
            S(J)=R(J)
   21    CONTINUE
   22 CONTINUE
C
C
C *** TRANSFORM ERROR MATRIX
C
      CALL SSMT5T(B,S,S)
C
      new=1
 
      J=0
      DO 41 I=1,5
         DO 40 K=I,5
            J=J+1
            R(J)=S(J)
   40    CONTINUE
   41 CONTINUE
C
   90 if (Iflag <= 0) GO TO 900
C
C
C *** TRANSFORM ERROR MATRIX FROM INTERNAL TO EXTERNAL VARIABLES;
C
C
      new=1
      if (Mvar != 1) GO TO 91
      PC[0]=pm2
      PC[1]=TMath::ASin(P2[2]*PC[0])
      IF (TMath::Abs (P2[0])  <  1.E-30) P2[0] = 1.E-30
      PC[2] = TMath::ATan2 (P2[1],P2[0])
      CALL TRSCSP(PC,R,PS,R,H2,ch,Ierr,spx)
      GO TO 900
C
   91 if (Mvar != 0) GO TO 901
      GO TO 900
C
C ERROR EXITS
C
  901 Ierr=1
      GO TO 999
  902 Ierr=2
      GO TO 999
  903 Ierr=3
      if (INIT != 0) GO TO 30
      INIT=1
      GO TO 30
  904 Ierr=4
  999 WRITE (LOUT, 1000) Ierr
 1000 FORMAT(1H ,' *** S/R ERPROP   Ierr =',I5)
C
  900 RETURN
      END
//________________________________________________________________________________
/*******************************************************************************
 * transforms error matrix
 *     from   sc   variables (1/pt,lambda,phi,yt,zt)
 *     to     sc   variables (1/p,lambda,phi,yt,zt)
 *******************************************************************************/
Int_t StGeanePropagator::TRPTSC(PC,RC,PD,RD,Ierr)
 
 
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,RC,RD
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC[2],PD[2],RC(15),RD(15)
*
*______________________________________________________________________
*
      Ierr = 0
      cosl  = TMath::Cos(PC[1])
      IF (TMath::Abs(cosl) == 0) GO TO 901
      sinl  =    TMath::Sin(PC[1])
*
      PD[0] = PC[0]*cosl
      PD[1] = PC[1]
      PD[2] = PC[2]
*
      J=0
*
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
*
      A(1,1) = cosl
      A(2,2) = 1.D0
      A(3,3) = 1.D0
      A(4,4) = 1.D0
      A(5,5) = 1.D0
*
      A(1,2) = -PC[0]*sinl
C
      CALL SSMT5T(A,S,S)
C
      DO 25 J=1,15
        RD(J)=S(J)
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
  901 Ierr=1
  910 CONTINUE
C
      END
//______________________________________________________________________________
/*******************************************************************************
 * transforms error matrix
 *     from        variables (1/pt,v',w',v,w)
 *     from        variables (1/p, v',w',v,w)
 *******************************************************************************/
Int_t StGeanePropagator::TRPTSD(PD,RD,PC,RC,H,ch,Ierr,SPU,DJ,DK)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,ch,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD[2],PC[2],H[2],RC(15),RD(15),DJ[2],DK[2]
      DIMENSION UN[2],VN[2],DI[2],TVW[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
*
**_____________________________________________________________________
*
      Ierr=0
      TVW[0]=1./TMath::Sqrt(1.+PD[1]**2+PD[2]**2)
      if (SPU < 0.) TVW[0]=-TVW[0]
      TVW[1]=PD[1]*TVW[0]
      TVW[2]=PD[2]*TVW[0]
C
      DI[0]=DJ[1]*DK[2]-DJ[2]*DK[1]
      DI[1]=DJ[2]*DK[0]-DJ[0]*DK[2]
      DI[2]=DJ[0]*DK[1]-DJ[1]*DK[0]
C
      DO 5 I=1,3
         TN(I)=TVW[0]*DI(I)+TVW[1]*DJ(I)+TVW[2]*DK(I)
    5 CONTINUE
C
      cosl=TMath::Sqrt(TMath::Abs(1.-TN[2]**2))
      IF (cosl  <  1.E-30) cosl = 1.E-30
      cosl1=1./cosl
      sinl  = TN[2]
*
      PC[0]=PD[0]*cosl
      PC[1]=PD[1]
      PC[2]=PD[2]
      pm=PC[0]
*
      IF (TMath::Abs (TN[0])  <  1.E-30) TN[0] = 1.E-30
C
      UN[0]=-TN[1]*cosl1
      UN[1]=TN[0]*cosl1
      UN[2]=0.
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
 
C
      UJ=UN[0]*DJ[0]+UN[1]*DJ[1]+UN[2]*DJ[2]
      UK=UN[0]*DK[0]+UN[1]*DK[1]+UN[2]*DK[2]
      VJ=VN[0]*DJ[0]+VN[1]*DJ[1]+VN[2]*DJ[2]
      VK=VN[0]*DK[0]+VN[1]*DK[1]+VN[2]*DK[2]
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
C
      Q=-ham*CFACT8
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]+H[2]*UN[2])*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      A(1,4)=Q*TVW[1]*SINZ*(sinl*PD[0])
      A(1,5)=Q*TVW[2]*SINZ*(sinl*PD[0])
C
6     continue
      A(1,1) = cosl
      A(2,2) = 1.
      A(3,3) = 1.
      A(4,4) = 1.
      A(5,5) = 1.
*
      A(1,2)=-TVW[0]*VJ*(sinl*PD[0])
      A(1,3)=-TVW[0]*VK*(sinl*PD[0])
C
      CALL SSMT5T(A,S,S)
C
      DO J=1,15
        RC(J)=S(J)
      ENDDO
*
      END
//________________________________________________________________________________
/*******************************************************************************
 * *** transforms error matrix
 *     from        variables (1/p,v1',w1',v1,w1)
 *      to         variables (1/p,v2',w2',v2,w2)
 *     Authors: A. Haas And W. Wittek
 * *** pd1[2]    1/p,v1',w1'                             input
 *     pd2[2]    1/p,v2',w2'                            output
 *     h[2]      magnetic field                          input
 *     rd1(15)   error matrix in 1/p,v1',w1',v1,w1       input      (triangle)
 *     rd2(15)   error matrix in 1/p,v2',w2',v2,w2      output      (triangle)
 *     ch        charge of particle                      input
 *               charge and magnetic field are needed
 *               for correlation terms (v2',v1),(v2',w1),(w2',v1),(w2',w1)
 *               these correlation terms appear because rd1 is assumed
 *               to be the error matrix for fixed u1
 *               and rd2 for fixed u2
 *     sp1       sign of u1-component of particle momentum     input
 *     sp2       sign of u2-component of particle momentum    output
 *     dj1[2]    unit vector in v1-direction
 *     dk1[2]    unit vector in w1-direction    of system 1
 *     dj2[2]    unit vector in v2-direction
 *     dk2[2]    unit vector in w2-direction    of system 2
 *     Ierr      = 0    transformation ok
 *               = 1    momentum perpendicular to u2-direction (v2',w2' not defin
 *               = 2    momentum perpendicular to x-axis
 ******************************************************************************/
Int_t StGeanePropagator::TRS1S2 (PD1,RD1,PD2,RD2,H,ch,Ierr,SP1,SP2
     1,                  DJ1,DK1,DJ2,DK2)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD1,PD2,RD1,RD2,H,ch,SP1,SP2,DJ1,DK1,DJ2,DK2
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD1[2],PD2[2],RD1(15),RD2(15),H[2],DJ1[2],DK1[2]
     +,DJ2[2],DK2[2],UN[2],VN[2],DI1[2],DI2[2],TVW1[2],TVW2[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      Ierr=0
      pm=PD1[0]
      TVW1[0]=1./TMath::Sqrt(1.+PD1[1]**2+PD1[2]**2)
      if (SP1 < 0.) TVW1[0]=-TVW1[0]
      TVW1[1]=PD1[1]*TVW1[0]
      TVW1[2]=PD1[2]*TVW1[0]
C
      DI1[0]=DJ1[1]*DK1[2]-DJ1[2]*DK1[1]
      DI1[1]=DJ1[2]*DK1[0]-DJ1[0]*DK1[2]
      DI1[2]=DJ1[0]*DK1[1]-DJ1[1]*DK1[0]
C
      DO 5 I=1,3
         TN(I)=TVW1[0]*DI1(I)+TVW1[1]*DJ1(I)+TVW1[2]*DK1(I)
    5 CONTINUE
C
      DI2[0]=DJ2[1]*DK2[2]-DJ2[2]*DK2[1]
      DI2[1]=DJ2[2]*DK2[0]-DJ2[0]*DK2[2]
      DI2[2]=DJ2[0]*DK2[1]-DJ2[1]*DK2[0]
C
      TVW2[0]=TN[0]*DI2[0]+TN[1]*DI2[1]+TN[2]*DI2[2]
      TVW2[1]=TN[0]*DJ2[0]+TN[1]*DJ2[1]+TN[2]*DJ2[2]
      TVW2[2]=TN[0]*DK2[0]+TN[1]*DK2[1]+TN[2]*DK2[2]
C
      if (TVW2[0] == 0.) GO TO 901
      TR=1./TVW2[0]
      SP2=1.
      if (TVW2[0] < 0.) SP2=-1.
      PD2[0]=PD1[0]
      PD2[1]=TVW2[1]*TR
      PD2[2]=TVW2[2]*TR
C
      cosl=TMath::Sqrt(TMath::Abs(1.-TN[2]**2))
      if (cosl == 0.) return 2;
      cosl1=1./cosl
      UN[0]=-TN[1]*cosl1
      UN[1]=TN[0]*cosl1
      UN[2]=0.
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
C
      UJ1=UN[0]*DJ1[0]+UN[1]*DJ1[1]+UN[2]*DJ1[2]
      UK1=UN[0]*DK1[0]+UN[1]*DK1[1]+UN[2]*DK1[2]
      VJ1=VN[0]*DJ1[0]+VN[1]*DJ1[1]+VN[2]*DJ1[2]
      VK1=VN[0]*DK1[0]+VN[1]*DK1[1]+VN[2]*DK1[2]
C
      UJ2=UN[0]*DJ2[0]+UN[1]*DJ2[1]+UN[2]*DJ2[2]
      UK2=UN[0]*DK2[0]+UN[1]*DK2[1]+UN[2]*DK2[2]
      VJ2=VN[0]*DJ2[0]+VN[1]*DJ2[1]+VN[2]*DJ2[2]
      VK2=VN[0]*DK2[0]+VN[1]*DK2[1]+VN[2]*DK2[2]
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD1(J)
    4    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
C
      Q=-ham*CFACT8
      TT=-Q*TR**3
      SJ1I2=DJ1[0]*DI2[0]+DJ1[1]*DI2[1]+DJ1[2]*DI2[2]
      SK1I2=DK1[0]*DI2[0]+DK1[1]*DI2[1]+DK1[2]*DI2[2]
      SK2U=DK2[0]*UN[0]+DK2[1]*UN[1]+DK2[2]*UN[2]
      SK2V=DK2[0]*VN[0]+DK2[1]*VN[1]+DK2[2]*VN[2]
      SJ2U=DJ2[0]*UN[0]+DJ2[1]*UN[1]+DJ2[2]*UN[2]
      SJ2V=DJ2[0]*VN[0]+DJ2[1]*VN[1]+DJ2[2]*VN[2]
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]+H[2]*UN[2])*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      A(2,4)=-TT*SJ1I2*(SK2U*SINZ-SK2V*COSZ)
      A(2,5)=-TT*SK1I2*(SK2U*SINZ-SK2V*COSZ)
      A(3,4)= TT*SJ1I2*(SJ2U*SINZ-SJ2V*COSZ)
      A(3,5)= TT*SK1I2*(SJ2U*SINZ-SJ2V*COSZ)
C
    6 A(1,1)=1.
      A(4,4)=TR*(UJ1*VK2-VJ1*UK2)
      A(4,5)=TR*(UK1*VK2-VK1*UK2)
      A(5,4)=TR*(VJ1*UJ2-UJ1*VJ2)
      A(5,5)=TR*(VK1*UJ2-UK1*VJ2)
C
      TS=TR*TVW1[0]
      A(2,2)=A(4,4)*TS
      A(2,3)=A(4,5)*TS
      A(3,2)=A(5,4)*TS
      A(3,3)=A(5,5)*TS
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RD2(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
  901 Ierr=1
      GO TO 910
  902 Ierr=2
  910 RETURN
      END
//________________________________________________________________________________
/*  transforms error matrix
 *     from   sc   variables (1/p,lambda,phi,yt,zt)
 *     from   sc   variables (1/pt,lambda,phi,yt,zt)   */
Int_t StGeanePropagator::TRSCPT(PC,RC,PD,RD,Ierr)
 
 
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,RC,RD
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC[2],PD[2],RC(15),RD(15)
*
*______________________________________________________________________
*
      Ierr = 0
      cosl  = TMath::Cos(PC[1])
      IF (TMath::Abs(cosl) == 0) GO TO 901
      cosl1 = 1./cosl
      TANL  =    TAN(PC[1])
*
      PD[0] = PC[0]*cosl1
      PD[1] = PC[1]
      PD[2] = PC[2]
*
      J=0
*
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
*
      A(1,1) = cosl1
      A(2,2) = 1.D0
      A(3,3) = 1.D0
      A(4,4) = 1.D0
      A(5,5) = 1.D0
*
      A(1,2) = PD[0]*TANL
C
      CALL SSMT5T(A,S,S)
C
      DO 25 J=1,15
        RD(J)=S(J)
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
  901 Ierr=1
  910 CONTINUE
C
      END
//________________________________________________________________________________
/* *** transforms error matrix
 *     from   sc   variables (1/p,lambda,phi,yt,zt)
 *      to         variables (1/p,v',w',v,w)
 *     authors: a. haas and w. wittek
 * *** pc[2]     1/p,lambda,phi                          input
 *     pd[2]     1/p,v',w'                              output
 *     h[2]      magnetic field                          input
 *     rc(15)    error matrix in   sc   variables        input     (triangle)
 *     rd(15)    error matrix in 1/p,v',w',v,w          output     (triangle)
 *     ch        charge of particle                      input
 *               charge and magnetic field are needed
 *               for correlation terms (v',yt),(v',zt),(w',yt),(w',zt)
 *               these correlation terms appear because rc is assumed
 *               to be the error matrix for fixed s (path length)
 *               and rd for fixed u
 *     dj[2]     unit vector in v-direction
 *     dk[2]     unit vector in w-direction    of detector system
 *     Ierr  =   1       particle moves perpendicular to u-axis
 *                      ( v',w' are not defined )
 *     spu       sign of u-component of particle momentum   output */
Int_t StGeanePropagator::TRSCSD(PC,RC,PD,RD,H,ch,Ierr,SPU,DJ,DK)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL  PC,PD,H,RC,RD,ch,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC[2],PD[2],H[2],RC(15),RD(15),DJ[2],DK[2]
      DIMENSION UN[2],VN[2],DI[2],TVW[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      Ierr=0
      pm=PC[0]
      cosl=TMath::Cos(PC[1])
      SINP=TMath::Sin(PC[2])
      COSP=TMath::Cos(PC[2])
C
      TN[0]=cosl*COSP
      TN[1]=cosl*SINP
      TN[2]=TMath::Sin(PC[1])
C
      DI[0]=DJ[1]*DK[2]-DJ[2]*DK[1]
      DI[1]=DJ[2]*DK[0]-DJ[0]*DK[2]
      DI[2]=DJ[0]*DK[1]-DJ[1]*DK[0]
C
      TVW[0]=TN[0]*DI[0]+TN[1]*DI[1]+TN[2]*DI[2]
      SPU=1.
      if (TVW[0] < 0.) SPU=-1.
      TVW[1]=TN[0]*DJ[0]+TN[1]*DJ[1]+TN[2]*DJ[2]
      TVW[2]=TN[0]*DK[0]+TN[1]*DK[1]+TN[2]*DK[2]
      if (TVW[0] == 0.) GO TO 901
C
      T1R=1./TVW[0]
      PD[0]=PC[0]
      PD[1]=TVW[1]*T1R
      PD[2]=TVW[2]*T1R
C
      UN[0]=-SINP
      UN[1]=COSP
      UN[2]=0.
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
C
      UJ=UN[0]*DJ[0]+UN[1]*DJ[1]+UN[2]*DJ[2]
      UK=UN[0]*DK[0]+UN[1]*DK[1]+UN[2]*DK[2]
      VJ=VN[0]*DJ[0]+VN[1]*DJ[1]+VN[2]*DJ[2]
      VK=VN[0]*DK[0]+VN[1]*DK[1]+VN[2]*DK[2]
C
C
      J=0
 
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
      Q=-ham*CFACT8
C
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]+H[2]*UN[2])*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      T3R=Q*T1R**3
      UI=UN[0]*DI[0]+UN[1]*DI[1]+UN[2]*DI[2]
      VI=VN[0]*DI[0]+VN[1]*DI[1]+VN[2]*DI[2]
      A(2,4)=-UI*(VK*COSZ-UK*SINZ)*T3R
      A(2,5)=-VI*(VK*COSZ-UK*SINZ)*T3R
      A(3,4)= UI*(VJ*COSZ-UJ*SINZ)*T3R
      A(3,5)= VI*(VJ*COSZ-UJ*SINZ)*T3R
C
    6 T2R=T1R**2
C
      A(1,1)=1.
      A(2,2)=-UK*T2R
      A(2,3)=VK*cosl*T2R
      A(3,2)=UJ*T2R
      A(3,3)=-VJ*cosl*T2R
      A(4,4)=VK*T1R
      A(4,5)=-UK*T1R
      A(5,4)=-VJ*T1R
      A(5,5)=UJ*T1R
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RD(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
  901 Ierr=1
  910 CONTINUE
C
      RETURN
      END
//________________________________________________________________________________
/* *** transforms error matrix
 *     from   sc   variables (1/p,lambda,phi,yt,zt)
 *      to  spline variables (1/p,y',z',y,z)
 *     authors: a. haas and w. wittek
 * *** pc[2]     1/p,lambda,phi                          input
 *     ps[2]     1/p,y',z'                              output
 *     h[2]      magnetic field                          input
 *     rc(15)    error matrix in   sc   variables        input     (triangle)
 *     rs(15)    error matrix in spline variables       output     (triangle)
 *     ch        charge of particle                      input
 *               charge and magnetic field are needed
 *               for correlation terms (y',yt),(y',zt),(z',yt),(z',zt)
 *               these correlation terms appear because rc is assumed
 *               to be the error matrix for fixed s (path length)
 *               and rs for fixed x
 *     Ierr  =   1       particle moves perpendicular to x-axis
 *                      ( y',z' are not defined )
 *     spx       sign of x-component of particle momentum   output  */
Int_t StGeanePropagator::TRSCSP(PC,RC,PS,RS,H,ch,Ierr,spx)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PC,PS,H,RC,RS,ch,spx
#endif
#include "geant321/trcom3.inc"
      DIMENSION PC[2],PS[2],H[2],RC(15),RS(15)
      DIMENSION UN[2],VN[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      Ierr=0
      pm=PC[0]
      cosl=TMath::Cos(PC[1])
      SINP=TMath::Sin(PC[2])
      COSP=TMath::Cos(PC[2])
C
      TN[0]=cosl*COSP
      spx=1.
      if (TN[0] < 0.) spx=-1.
      if (TN[0] == 0.) GO TO 901
      TN[1]=cosl*SINP
      TN[2]=TMath::Sin(PC[1])
C
      T1R=1./TN[0]
      PS[0]=PC[0]
      PS[1]=SINP/COSP
      PS[2]=TN[2]*T1R
C
      UN[0]=-SINP
      UN[1]=COSP
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
C
      J=0
      DO 10 I=1,5
         DO 5 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RC(J)
    5    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
      Q=-ham*CFACT8
C
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]           )*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
 
      T3R=Q*T1R**3
      A(2,4)=-UN[0]*(VN[2]*COSZ           )*T3R
      A(2,5)=-VN[0]*(VN[2]*COSZ           )*T3R
      A(3,4)=UN[0]*(VN[1]*COSZ-UN[1]*SINZ)*T3R
      A(3,5)=VN[0]*(VN[1]*COSZ-UN[1]*SINZ)*T3R
C
    6 T2R=T1R**2
C
      A(1,1)=1.
      A(2,3)=VN[2]*cosl*T2R
      A(3,2)=UN[1]*T2R
      A(3,3)=-VN[1]*cosl*T2R
      A(4,4)=VN[2]*T1R
      A(5,4)=-VN[1]*T1R
      A(5,5)=UN[1]*T1R
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RS(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
  901 Ierr=1
  910 CONTINUE
C
      RETURN
      END
//________________________________________________________________________________
/*  transforms error matrix
 *     from        variables (1/p,v',w',v,w)
 *     from        variables (1/pt,v',w',v,w) */
Int_t StGeanePropagator::TRSDPT(PD,RD,PC,RC,H,ch,Ierr,SPU,DJ,DK)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,ch,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD[2],PC[2],H[2],RC(15),RD(15),DJ[2],DK[2]
      DIMENSION UN[2],VN[2],DI[2],TVW[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
*
**_____________________________________________________________________
*
      Ierr=0
      pm=PD[0]
      TVW[0]=1./TMath::Sqrt(1.+PD[1]**2+PD[2]**2)
      if (SPU < 0.) TVW[0]=-TVW[0]
      TVW[1]=PD[1]*TVW[0]
      TVW[2]=PD[2]*TVW[0]
C
      DI[0]=DJ[1]*DK[2]-DJ[2]*DK[1]
      DI[1]=DJ[2]*DK[0]-DJ[0]*DK[2]
      DI[2]=DJ[0]*DK[1]-DJ[1]*DK[0]
C
      DO 5 I=1,3
         TN(I)=TVW[0]*DI(I)+TVW[1]*DJ(I)+TVW[2]*DK(I)
    5 CONTINUE
C
      cosl=TMath::Sqrt(TMath::Abs(1.-TN[2]**2))
      IF (cosl  <  1.E-30) cosl = 1.E-30
      cosl1=1./cosl
      TANL  = TN[2]*cosl1
*
      PC[0]=PD[0]*cosl1
      PC[1]=PD[1]
      PC[2]=PD[2]
*
      IF (TMath::Abs (TN[0])  <  1.E-30) TN[0] = 1.E-30
C
      UN[0]=-TN[1]*cosl1
      UN[1]=TN[0]*cosl1
      UN[2]=0.
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
 
C
      UJ=UN[0]*DJ[0]+UN[1]*DJ[1]+UN[2]*DJ[2]
      UK=UN[0]*DK[0]+UN[1]*DK[1]+UN[2]*DK[2]
      VJ=VN[0]*DJ[0]+VN[1]*DJ[1]+VN[2]*DJ[2]
      VK=VN[0]*DK[0]+VN[1]*DK[1]+VN[2]*DK[2]
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
C
      Q=-ham*CFACT8
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]+H[2]*UN[2])*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      A(1,4)=-Q*TVW[1]*SINZ*(TANL*PC[0])
      A(1,5)=-Q*TVW[2]*SINZ*(TANL*PC[0])
C
6     continue
      A(1,1) = cosl1
      A(2,2) = 1.
      A(3,3) = 1.
      A(4,4) = 1.
      A(5,5) = 1.
*
      A(1,2)=TVW[0]*VJ*(TANL*PC[0])
      A(1,3)=TVW[0]*VK*(TANL*PC[0])
C
      CALL SSMT5T(A,S,S)
C
      DO J=1,15
        RC(J)=S(J)
      ENDDO
*
      END
//________________________________________________________________________________
/*  transforms error matrix
 *     from        variables (1/p,v',w',v,w)
 *      to    sc   variables (1/p,lambda,phi,yt,zt)
 *     authors: a. haas and w. wittek
 * *** pd[2]     1/p,v',w'                               input
 *     pc[2]     1/p,lambda,phi                         output
 *     h[2]      magnetic field                          input
 *     rd(15)    error matrix in 1/p,v',w',v,w           input      (triangle)
 *     rc(15)    error matrix in   sc   variables       output      (triangle)
 *     ch        charge of particle                      input
 *               charge and magnetic field are needed
 *               for correlation terms (lambda,v),(lambda,w),(phi,v),(phi,w)
 *               these correlation terms appear because rc is assumed
 *               to be the error matrix for fixed s (path length)
 *               and rd for fixed u
 *     dj[2]     unit vector in v-direction
 *     dk[2]     unit vector in w-direction    of detector system
 *     Ierr              not used
 *     spu       sign of u-component of particle momentum    input */
 
Int_t StGeanePropagator::TRSDSC(PD,RD,PC,RC,H,ch,Ierr,SPU,DJ,DK)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PD,PC,H,RC,RD,ch,DJ,DK,SPU
#endif
#include "geant321/trcom3.inc"
      DIMENSION PD[2],PC[2],H[2],RC(15),RD(15),DJ[2],DK[2]
      DIMENSION UN[2],VN[2],DI[2],TVW[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      Ierr=0
      pm=PD[0]
      TVW[0]=1./TMath::Sqrt(1.+PD[1]**2+PD[2]**2)
      if (SPU < 0.) TVW[0]=-TVW[0]
      TVW[1]=PD[1]*TVW[0]
      TVW[2]=PD[2]*TVW[0]
C
      DI[0]=DJ[1]*DK[2]-DJ[2]*DK[1]
      DI[1]=DJ[2]*DK[0]-DJ[0]*DK[2]
      DI[2]=DJ[0]*DK[1]-DJ[1]*DK[0]
C
      DO 5 I=1,3
         TN(I)=TVW[0]*DI(I)+TVW[1]*DJ(I)+TVW[2]*DK(I)
    5 CONTINUE
C
      PC[0]=PD[0]
      PC[1]=TMath::ASin(TN[2])
      IF (TMath::Abs (TN[0])  <  1.E-30) TN[0] = 1.E-30
      PC[2] = TMath::ATan2 (TN[1],TN[0])
C
      cosl=TMath::Sqrt(TMath::Abs(1.-TN[2]**2))
      IF (cosl  <  1.E-30) cosl = 1.E-30
      cosl1=1./cosl
      UN[0]=-TN[1]*cosl1
      UN[1]=TN[0]*cosl1
      UN[2]=0.
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
 
C
      UJ=UN[0]*DJ[0]+UN[1]*DJ[1]+UN[2]*DJ[2]
      UK=UN[0]*DK[0]+UN[1]*DK[1]+UN[2]*DK[2]
      VJ=VN[0]*DJ[0]+VN[1]*DJ[1]+VN[2]*DJ[2]
      VK=VN[0]*DK[0]+VN[1]*DK[1]+VN[2]*DK[2]
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RD(J)
    4    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
C
      Q=-ham*CFACT8
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]+H[2]*UN[2])*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      A(2,4)=-Q*TVW[1]*SINZ
      A(2,5)=-Q*TVW[2]*SINZ
      A(3,4)=-Q*TVW[1]*COSZ*cosl1
      A(3,5)=-Q*TVW[2]*COSZ*cosl1
C
    6 A(1,1)=1.
      A(2,2)=TVW[0]*VJ
      A(2,3)=TVW[0]*VK
      A(3,2)=TVW[0]*UJ*cosl1
      A(3,3)=TVW[0]*UK*cosl1
      A(4,4)=UJ
      A(4,5)=UK
      A(5,4)=VJ
      A(5,5)=VK
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RC(J)=S(J)
   20    CONTINUE
   25 CONTINUE
C
      RETURN
C
C ERROR EXITS
C
      END
//________________________________________________________________________________
/*  transforms error matrix
 *     from spline variables (1/p,y',z',y,z)
 *      to    sc   variables (1/p,lambda,phi,yt,zt)
 *     authors: a. haas and w. wittek
 * *** ps[2]     1/p,y',z'                               input
 *     pc[2]     1/p,lambda,phi                         output
 *     h[2]      magnetic field                          input
 *     rs(15)    error matrix in spline variables        input      (triangle)
 *     rc(15)    error matrix in   sc   variables       output      (triangle)
 *     ch        charge of particle                      input
 *               charge and magnetic field are needed
 *               for correlation terms (lambda,y),(lambda,z),(phi,y),(phi,z)
 *               these correlation terms appear because rc is assumed
 *               to be the error matrix for fixed s (path length)
 *               and rs for fixed x
 *     Ierr              not used
 *     spx       sign of x-component of particle momentum    input */

Int_t StGeanePropagator::TRSPSC(PS,RS,PC,RC,H,ch,Ierr,spx)
#if !defined(CERNLIB_SINGLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL   PS,PC,H,RC,RS,ch,spx
#endif
#include "geant321/trcom3.inc"
      DIMENSION PS[2],PC[2],H[2],RC(15),RS(15)
      DIMENSION UN[2],VN[2]
C
#if defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 E-4 /
#endif
#if !defined(CERNLIB_SINGLE)
      DATA CFACT8 / 2.997925 D-4 /
#endif
C
      Ierr=0
      pm=PS[0]
      TN[0]=1./TMath::Sqrt(1.+PS[1]**2+PS[2]**2)
      if (spx < 0.) TN[0]=-TN[0]
      TN[1]=PS[1]*TN[0]
      TN[2]=PS[2]*TN[0]
C
      PC[0]=PS[0]
      PC[1]=TMath::ASin(TN[2])
      IF (TMath::Abs (TN[0])  <  1.E-30) TN[0] = 1.E-30
      PC[2] = TMath::ATan2 (TN[1],TN[0])
C
      cosl=TMath::Sqrt(TMath::Abs(1.-TN[2]**2))
      IF (cosl  <  1.E-30) cosl = 1.E-30
      cosl1=1./cosl
      UN[0]=-TN[1]*cosl1
      UN[1]=TN[0]*cosl1
C
      VN[0]=-TN[2]*UN[1]
      VN[1]=TN[2]*UN[0]
      VN[2]=cosl
C
      J=0
      DO 10 I=1,5
         DO 4 K=I,5
            J=J+1
            A(I,K)=0.
            A(K,I)=0.
            S(J)=RS(J)
    4    CONTINUE
   10 CONTINUE
C
      if (ch == 0.) GO TO 6
 
      ha=TMath::Sqrt(H[0]**2+H[1]**2+H[2]**2)
      ham=ha*pm
      if (ham == 0.) GO TO 6
      hm=ch/ha
C
      Q=-ham*CFACT8
C
      SINZ=-(H[0]*UN[0]+H[1]*UN[1]           )*hm
      COSZ= (H[0]*VN[0]+H[1]*VN[1]+H[2]*VN[2])*hm
      A(2,4)=-Q*TN[1]*SINZ
      A(2,5)=-Q*TN[2]*SINZ
      A(3,4)=-Q*TN[1]*COSZ*cosl1
      A(3,5)=-Q*TN[2]*COSZ*cosl1
C
    6 A(1,1)=1.
      A(2,2)=TN[0]*VN[1]
      A(2,3)=TN[0]*VN[2]
      A(3,2)=TN[0]*UN[1]*cosl1
      A(4,4)=UN[1]
      A(5,4)=VN[1]
      A(5,5)=VN[2]
C
      CALL SSMT5T(A,S,S)
C
      J=0
      DO 25 I=1,5
         DO 20 K=I,5
            J=J+1
            RC(J)=S(J)
   20    CONTINUE
   25 CONTINUE
*
// ERROR EXITS
*
      END
#endif
