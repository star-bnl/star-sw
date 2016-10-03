       DOUBLE COMPLEX FUNCTION InProd_mass(p1,m1,l1,p2,m2,l2)
c////////////////////////////////////////////////////////////////
c                                                              //
c  Inner product for massive spinors: Ub(p1,m1,l1)*U(p2,m2,l2) //
c                                                              //
c////////////////////////////////////////////////////////////////
              
          IMPLICIT NONE
          INTEGER          l1,l2
          DOUBLE PRECISION p1(0:3),p2(0:3),m1,m2
          DOUBLE PRECISION sqrt1,sqrt2,forSqrt1
          DOUBLE COMPLEX   InProd_zero
          EXTERNAL         InProd_zero
          INCLUDE 'MC_Declare.h' 

          IF ((l1.EQ.+1).AND.(l2.EQ.+1)) THEN                
             forSqrt1    = (p1(0)-p1(1))/(p2(0)-p2(1))
             sqrt1       = dsqrt(forSqrt1)
             sqrt2       = 1d0/sqrt1
             InProd_mass = Dcmplx(m1*sqrt2+m2*sqrt1,0d0)

          ELSEIF ((l1.EQ.+1).AND.(l2.EQ.-1)) THEN                             
             InProd_mass = InProd_zero(p1,+1,p2,-1)

          ELSEIF ((l1.EQ.-1).AND.(l2.EQ.+1)) THEN                        
             InProd_mass = InProd_zero(p1,-1,p2,+1)               

          ELSEIF ((l1.EQ.-1).AND.(l2.EQ.-1)) THEN                             
             forSqrt1    = (p1(0)-p1(1))/(p2(0)-p2(1))
             sqrt1       = dsqrt(forSqrt1)
             sqrt2       = 1d0/sqrt1
             InProd_mass = Dcmplx(m1*sqrt2+m2*sqrt1,0d0)

          ELSE        
             WRITE(MCLUN,*) " "             
             WRITE(MCLUN,*) " ERROR IN InProd_mass.."
             WRITE(MCLUN,*) "       WRONG VALUES FOR l1,l2: l1,l2 = -1,+1"
             WRITE(MCLUN,*) " "             
             STOP
          ENDIF       

       RETURN
       END
