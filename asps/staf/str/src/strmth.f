        DOUBLE PRECISION FUNCTION STR_Gran(Seed)

        IMPLICIT NONE

*  Input/output:
        INTEGER SEED !Integer seed, set at least once by caller,
*                     modified "randomly" each time within this routine.
*  Returns:  Random number from a gaussian distribution, with a mean
*                 of 0 and a sigma of 1.

        DOUBLE PRECISION f
        REAL RAN

        f=RAN(SEED) !RAN should produce random no. in (0,1].
        f=2.D0*(f-0.5D0) !Remap to (-1,+1].
        IF (f.EQ.0.D0) f=1.D0 !Remap to (-1,0] & [0,+1).
        STR_Gran=SIGN(SQRT(-2.D0*LOG(ABS(f))),f)
        RETURN
        END
*
        SUBROUTINE STRFIT( GENERAL_ERRORS, COUNTING_ERRORS, TRACE_LUN
     1                   , Niter_max
     1                   , FITF, NPTS, NPAR, X, Y, WY, DA, A
     1                   , FLAMDA, SA, CHI2 )

        IMPLICIT NONE

*  Inputs:
        LOGICAL GENERAL_ERRORS  !If TRUE, the Y errors (weights) are set
                                !by the calling routine and found in WY.
                                !GENERAL_ERRORS overrides COUNTING_ERRORS.
        LOGICAL COUNTING_ERRORS !If TRUE, counting errors (sqrt(Y)) are used.
                                !If FALSE, and GENERAL_ERRORS is FALSE,
                                !constant errors (all = 1) are used.
        INTEGER TRACE_LUN       !If >0, iterations are traced on this LUN.
        INTEGER Niter_max       !Max. no. of iterations to try.
        DOUBLE PRECISION FITF   !Externally defined fit-function.
        EXTERNAL FITF
        INTEGER NPTS            !Number of (X,Y) points to fit FITF to.
        INTEGER NPAR            !Number of adjustable parameters "A" in FITF.
        DOUBLE PRECISION X(*)   !The abscissa values.
        DOUBLE PRECISION Y(*)   !The ordinate values
        DOUBLE PRECISION WY(*)  !The ordinates' weights, if GENERAL_ERRORS
        DOUBLE PRECISION DA(*)  !The differentiation step-size for the "A".

*  Input/Outputs:
        DOUBLE PRECISION A(*)   !The adjustable parameters for FITF,
                                !initialized by the caller.
        DOUBLE PRECISION FLAMDA !The "mixing" parameter, initialized externally
                                !to 1, typically, and determines the relative
                                !mix between the two "types" of fits used
                                !here;  varied as fit improves.  See Bevington.
*  Outputs:
        DOUBLE PRECISION SA(*)  !The fit-error-estimates in the A.
        DOUBLE PRECISION CHI2   !The final Chi-2 per dof of the fit.

*  Description:
*       Fit a function, FITF, to a set of data, (X,Y), with weights WY, and
*       variable parameters A.  From a set of routines in "Data reduction
*       and Error Analysis for the Physical Sciences", P.R. Bevington,
*       McGraw-Hill, 1969.   The FORTRAN has been brought up to date.


        DOUBLE PRECISION SMALL
        PARAMETER (SMALL=1.E-2) !Chi-square change max. for loop term.
        DOUBLE PRECISION SUPER_SMALL
        PARAMETER (SUPER_SMALL=1.E-6) !Chi-square change max. for immed. term.

        DOUBLE PRECISION CHISQR,CHISQO

        INTEGER I
        INTEGER ITCT,ISMCHG
        LOGICAL ITTERM

        DOUBLE PRECISION STRFIT_CHISQ

*       Initialize the measured errors:
        IF (GENERAL_ERRORS) THEN !They've been set externally.
        ELSE IF (COUNTING_ERRORS) THEN !Counting-errors.
          DO I=1,NPTS
            WY(I)=1.D0/Y(I)
          END DO
        ELSE !Constant weighting.
          DO I=1,NPTS
            WY(I)=1.0D0
          END DO
        END IF

        IF (FLAMDA.LE.0.D0) FLAMDA=0.1D0 !Start with this.

*       Initial Chi-squared:
        CHISQR=STRFIT_CHISQ(FITF,X,Y,WY,NPTS,A,NPAR)
        CHI2=CHISQR

        ITCT=0

        IF (TRACE_LUN.GT.0) THEN !Trace the initial values:
          WRITE(TRACE_LUN,105) ITCT,CHISQR,FLAMDA
          WRITE(TRACE_LUN,106) (A(I),I=1,NPAR)
        END IF

105     FORMAT(' Iteration:'I4' Chi-square:'E13.5' Flamda:'E13.5
     1            ' parameters:')
106     FORMAT(' '5E15.8)

*       Now try to fit a function to the data:
        ISMCHG=0 !Small-change-of-CHISQR counter.
        ITTERM=.FALSE.
        DO WHILE (.NOT.ITTERM)
          CHISQO=CHISQR !Save the old value, for comparison.
          ITCT=ITCT+1

*         Fit it:
          CALL STRFIT_ITER(FITF,X,Y,WY,NPTS,NPAR,A,DA,SA,FLAMDA,CHISQR)

          IF (TRACE_LUN.GT.0) THEN
            WRITE(TRACE_LUN,105) ITCT,CHISQR,FLAMDA
            WRITE(TRACE_LUN,106) (A(I),I=1,NPAR)
          END IF
          IF (ABS(CHISQO-CHISQR).LT.SUPER_SMALL) THEN !Much too small of a change.
            ITTERM=.TRUE. !Force immediate termination.
          ELSE IF (ABS(CHISQO-CHISQR).LT.SMALL) THEN !Too small of a change.
            ISMCHG=ISMCHG+1
          ELSE IF (ABS(CHISQO-CHISQR).LT.CHISQR/5.) THEN !Too small of a change.
            ISMCHG=ISMCHG+1
          ELSE !The change wasn't so small.
            ISMCHG=0 !Reset small-change-of-CHISQR counter.
          END IF
          IF (ITTERM) THEN !Already scheduled to terminate.
          ELSE IF (ISMCHG.GE.2) THEN !It seems to have converged.
            ITTERM=.TRUE. !Force termination.
          ELSE IF (CHISQR.LE.1.D-28) THEN !That's good enough.
            ITTERM=.TRUE.
          ELSE IF (ITCT.GT.Niter_max) THEN !Too many iterations.
            ITTERM=.TRUE. !Too many iterations; force termination.
          END IF
        END DO !End of iteration loop.

*       Final Chi squared:
        CHISQR=STRFIT_CHISQ(FITF,X,Y,WY,NPTS,A,NPAR)
        CHI2=CHISQR

        RETURN
        END
*
        SUBROUTINE STRFIT_DERIV( FITF, X, IPT, A, DA, NPAR, DERIV )

*  Brief description:  Calculate parameter-derivatives for general fit routine.
*       From P. R. Bevington.

        IMPLICIT NONE

        DOUBLE PRECISION FITF
        EXTERNAL FITF
        DOUBLE PRECISION X(*),A(*),DA(*),DERIV(*)
        INTEGER IPT,NPAR

        INTEGER IPAR
        DOUBLE PRECISION AI,FP,FN


        DO IPAR=1,NPAR
          IF (DA(IPAR).NE.0.D0) THEN
            AI=A(IPAR)
            A(IPAR)=AI+DA(IPAR)
            FP=FITF(X,IPT,A)
            A(IPAR)=AI-DA(IPAR)
            FN=FITF(X,IPT,A)
            A(IPAR)=AI
            DERIV(IPAR)=(FP-FN)/(2.D0*DA(IPAR))
          ELSE
            DERIV(IPAR)=0.D0
          END IF
        END DO

        RETURN
        END
*
        SUBROUTINE STRFIT_ITER( FITF, X, Y, WY, NPTS, NPAR, A, DA, SA
     1                        , FLAMDA, CHISQR )

*  Brief description:  Perform one iteration for general fit routine.
*       This is a FORTRAN-77 (and modified) version of the subroutine
*       described on page 238 of P. R. Bevington's Data Reduction and Error
*       Analysis for the Physical Sciences.

        IMPLICIT NONE

        DOUBLE PRECISION FITF
        EXTERNAL FITF
        DOUBLE PRECISION X(*),Y(*),WY(*)
        INTEGER NPTS,NPAR
        DOUBLE PRECISION A(*),DA(*),SA(*),FLAMDA,CHISQR

        INTEGER NPAR_MAX
        PARAMETER (NPAR_MAX=100) !This must match in STRFIT_MATINV.

        DOUBLE PRECISION DBLE
        DOUBLE PRECISION ARRAY(NPAR_MAX,NPAR_MAX)

        DOUBLE PRECISION ALPHA(NPAR_MAX,NPAR_MAX),BETA(NPAR_MAX)
        DOUBLE PRECISION DERIV(NPAR_MAX),B(NPAR_MAX)
        DOUBLE PRECISION GAMMA(NPAR_MAX,NPAR_MAX)

        DOUBLE PRECISION ALPH,CHISQ1,SAVE

        INTEGER I,J,K,IPT,NTOFST
        LOGICAL INVERTED

        LOGICAL STRFIT_MATINV
        DOUBLE PRECISION STRFIT_CHISQ

*       Evaluate alpha and beta matrices:
        DO J=1,NPAR !Zero everything.
          BETA(J)=0.D0
          DO K=1,J
            ALPHA(J,K)=0.D0
          END DO
        END DO

        DO IPT=1,NPTS !Cycle through the data points.
          CALL STRFIT_DERIV(FITF,X,IPT,A,DA,NPAR,DERIV)
          DO J=1,NPAR
            BETA(J)=BETA(J)+(Y(IPT)-FITF(X,IPT,A))*DERIV(J)*WY(IPT)
            DO K=1,J
              ALPHA(J,K)=ALPHA(J,K)+DERIV(J)*DERIV(K)*WY(IPT)
            END DO
          END DO
        END DO

        DO J=1,NPAR !Fill in the other side of the diagonal.
          DO K=1,J-1 !J-->>J-1  no need for ALPHA(J,J)=ALPHA(J,J).
            ALPHA(K,J)=ALPHA(J,K)
          END DO
        END DO

*       Evaluate Chi squared at the starting point:
        CHISQ1=CHISQR !Done externally.
        NTOFST=0

71      CONTINUE

*       Invert the modified curvature matrix to find the new parameters:
        DO J=1,NPAR
          DO K=1,NPAR
            IF (J.NE.K) THEN !Off-diagonal terms:
              ALPH=SQRT(ALPHA(J,J)) * SQRT(ALPHA(K,K))
              GAMMA(J,K)=ALPH !Save it.
              IF (ALPH.LE.0.) THEN
                ARRAY(J,K)=0. !Those strange zeroes...
              ELSE
                ARRAY(J,K)=DBLE(ALPHA(J,K)/ALPH)
              END IF
            ELSE !Diagonal terms:
              ARRAY(J,J)=DBLE(1.+FLAMDA)
            END IF
          END DO
        END DO

*       Invert the matrix (.FALSE. => matrix not "ordered"):
        INVERTED=STRFIT_MATINV(NPAR,.FALSE.,ARRAY)

        IF (INVERTED) THEN
         DO J=1,NPAR
          B(J)=A(J)
          DO K=1,NPAR
            ALPH=SQRT(ALPHA(J,J)) * SQRT(ALPHA(K,K))
*           ALPH=GAMMA(J,K)
            IF (ALPH.GT.0.) THEN !Those pesky zeroes.
              B(J)=B(J)+BETA(K)*ARRAY(J,K)/ALPH
            END IF
          END DO
          SAVE=A(J)
          A(J)=B(J) !The new parameters.
          B(J)=SAVE !Save the old parameters here, just in case.
         END DO

*        If Chi squared increased, increase FLAMDA and try it again:

         CHISQR=STRFIT_CHISQ(FITF,X,Y,WY,NPTS,A,NPAR)
         IF ((CHISQ1-CHISQR).LT.0.) THEN
          DO J=1,NPAR
            A(J)=B(J) !Restore old parameters.
          END DO
          NTOFST=NTOFST+1
          IF (NTOFST.LT.5) THEN
            FLAMDA=10.*FLAMDA
            GO TO 71
          END IF
         END IF

        ELSE !Matrix could not be inverted; increase FLAMDA and try it again:

          DO J=1,NPAR
            A(J)=B(J) !Restore old parameters.
          END DO
          NTOFST=NTOFST+1
          IF (NTOFST.LT.5) THEN
            FLAMDA=10.*FLAMDA
            GO TO 71
          END IF

        END IF !INVERTED
*
*       Evaluate the parameters and uncertainties:
*
        IF (NPTS.GT.NPAR) THEN
        DO J=1,NPAR
          IF (ALPHA(J,J).EQ.0.) THEN
            SA(J)=0.D0
          ELSE
            SA(J)=(ARRAY(J,J))/ALPHA(J,J)
          END IF
          IF (SA(J).LT.0.) THEN !Assume some minor floating error -- force 0:
            SA(J)=0.D0
          ELSE
            SA(J)=SQRT(SA(J))
          END IF
        END DO
        END IF

        FLAMDA=FLAMDA/10.D0
        RETURN
        END
*
        DOUBLE PRECISION FUNCTION STRFIT_CHISQ(FITF,X,Y,WY,NPTS,A,NPAR)

        IMPLICIT NONE

        DOUBLE PRECISION FITF
        EXTERNAL FITF
        DOUBLE PRECISION X(*),Y(*),WY(*),A(*)
        INTEGER NPTS,NPAR

*  Brief description:  Calculate chi-squared per DOF for general fit routine.
*       From P. R. Bevington.

        DOUBLE PRECISION CHI2,D
        INTEGER I

        CHI2=0.0D0

        DO I=1,NPTS
          D=( (Y(I)-FITF(X,I,A))**2 )*WY(I)
          CHI2=CHI2+D
        END DO

        IF (NPTS.GT.NPAR) THEN
*         Non-zero degrees-of-freedom - make chi^2/dof:
          CHI2=CHI2/FLOAT(NPTS-NPAR)
        END IF

        STRFIT_CHISQ=CHI2
        RETURN
        END
*
        LOGICAL FUNCTION STRFIT_MATINV( NORDER, ORDERED, ARRAY )

        IMPLICIT NONE

        INTEGER NPAR_MAX
        PARAMETER (NPAR_MAX=100) !This must match in STRFIT_ITER.

*  Inputs:
        INTEGER NORDER !Size of matrix to be inverted.
        LOGICAL ORDERED !If .TRUE., the pivot-strategy (ordering) is skipped.

*  Input/Output:
        DOUBLE PRECISION ARRAY(NPAR_MAX,NPAR_MAX) !Initialized by caller to
                                                  !hold matrix to be inverted.
                                                  !Returns as inverted matrix.
*  Returns:
*       .TRUE. if the matrix was successfully inverted.
*       .FALSE. if not (eg, degenerative).

*  Brief description:  Invert a matrix.

*  Description:
*       This subroutine is a very close copy of the one found in
*       appendix B-2 of P. R. Bevington.
*       Converted to FORTRAN 77  June, 1987.

        DOUBLE PRECISION AMAX,SAVE
        DOUBLE PRECISION XNORM
        INTEGER IK(NPAR_MAX),JK(NPAR_MAX)

        INTEGER I,J,K,L


        DO K=1,NORDER

          IF (.NOT.ORDERED) THEN
*           Find the largest element A(I,J) in the rest of the matrix:
            AMAX=0.D0
            DO I=K,NORDER
              DO J=K,NORDER
                IF (DABS(AMAX).LT.DABS(ARRAY(I,J))) THEN
                  AMAX=ARRAY(I,J)
                  IK(K)=I
                  JK(K)=J
                END IF
              END DO
            END DO

*           Interchange rows and columns to put AMAX in ARRAY(K,K)
            IF (AMAX.EQ.0.D0) THEN
              STRFIT_MATINV=.FALSE. !Failure.
              RETURN
            END IF
            I=IK(K)
            IF (I.GT.K) THEN !Switch rows.
              DO J=1,NORDER
                SAVE=ARRAY(K,J)
                ARRAY(K,J)=ARRAY(I,J)
                ARRAY(I,J)=-SAVE
              END DO
            END IF

            J=JK(K)
            IF (J.GT.K) THEN !Switch columns.
              DO I=1,NORDER
                SAVE=ARRAY(I,K)
                ARRAY(I,K)=ARRAY(I,J)
                ARRAY(I,J)=-SAVE
              END DO
            END IF
          ELSE !ARRAY comes "ordered";  use the first element as max:
            AMAX=ARRAY(K,K)
          END IF !Whether "ordered".


*         Accumulate elements of the inverse matrix:
          XNORM=-1.D0/AMAX !Multiplication is more efficient than division.
          DO I=1,NORDER
            IF (I.NE.K) THEN !(Except for this row.)
              ARRAY(I,K)=ARRAY(I,K)*XNORM !Normalize and make negative.
              DO J=1,NORDER
                IF (J.NE.K) THEN
                  ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
                END IF
              END DO
            END IF
          END DO

          XNORM=-XNORM !Positive.
          ARRAY(K,K)=XNORM
          DO J=1,NORDER !Normalize each column.
            IF (J.NE.K) THEN
              ARRAY(K,J)=ARRAY(K,J)*XNORM
            END IF
          END DO

        END DO

        IF (.NOT.ORDERED) THEN !Done if ordering not disturbed.

*         Restore the ordering of the matrix:
          DO L=1,NORDER
            K=NORDER-L+1
            J=IK(K)
            IF (J.GT.K) THEN
              DO I=1,NORDER
                SAVE=ARRAY(I,K)
                ARRAY(I,K)=-ARRAY(I,J)
                ARRAY(I,J)=SAVE
              END DO
            END IF

            I=JK(K)
            IF (I.GT.K) THEN
              DO J=1,NORDER
                SAVE=ARRAY(K,J)
                ARRAY(K,J)=-ARRAY(I,J)
                ARRAY(I,J)=SAVE
              END DO
            END IF

          END DO

        END IF !.NOT.ORDERED

        STRFIT_MATINV=.TRUE. !Success.

        RETURN
        END
*
        DOUBLE PRECISION FUNCTION STRFIT_F_SAMPLE( X, I, A )

        IMPLICIT NONE

        DOUBLE PRECISION X(*), A(*)
        INTEGER I

*  Description:
*        Evaluate the fit-function at point X(I) using parameters A.

*       Sample function - quadratic:
        STRFIT_F_SAMPLE=A(1)+(A(2)+A(3)*X(I))*X(I)

        RETURN
        END
