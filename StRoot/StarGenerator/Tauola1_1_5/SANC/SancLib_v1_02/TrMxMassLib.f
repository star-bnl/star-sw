        FUNCTION TrMatrix_mass(p1,m1,l1,k,m,s,p2,m2,l2)
c//////////////////////////////////////////////////////////////
c          transition matrix for massive boson               //
c                                                            // 
c                                                            //
c                         \ eps(k,m,s)                       //
c                         /                                  // 
c                        _\                                  //
c                         /\ k                               // 
c                         \                                  //
c             <-- p1      /         <-- p2                   //                       
c           ---<----------\----------<---                    //
c       Ub(p1,m1,l1)                  U(p2,m2,l2)            //
c                                                            // 
c//////////////////////////////////////////////////////////////                         
           IMPLICIT NONE
           INTEGER          l1,l2,s,i
           DOUBLE PRECISION forSqrt1,forSqrt2
           DOUBLE PRECISION m,m1,m2
           DOUBLE PRECISION k_1(0:3),k_2(0:3),p1(0:3),p2(0:3),k(0:3)
           DOUBLE PRECISION forSqrt3,forSqrt4,sqrt3,sqrt1,sqrt2,sqrt4
           DOUBLE COMPLEX   InProd_zero,inPr1,inPr2,inPr3,inPr4
           DOUBLE COMPLEX   TrMatrix_mass
           EXTERNAL         InProd_zero
           INCLUDE 'MC_Declare.h'

           DO i=0,3
             k_1(i) = 1d0/2d0*(k(i) - m*spV(i))
             k_2(i) = 1d0/2d0*(k(i) + m*spV(i))                                
           ENDDO

           IF ( (l1.EQ.+1).AND.(l2.EQ.+1).AND.(s.EQ.0) ) THEN 
                
              inPr1 = InProd_zero(p1,+1,k_2,-1)
              inPr2 = InProd_zero(p2,-1,k_2,+1)
              inPr3 = InProd_zero(p1,+1,k_1,-1)
              inPr4 = InProd_zero(p2,-1,k_1,+1)
              sqrt1 = Dsqrt(p1(0)-p1(1))
              sqrt2 = Dsqrt(p2(0)-p2(1))
              sqrt3 = m1*m2/sqrt1/sqrt2

              TrMatrix_mass =                 
     &                      (inPr1*inPr2-inPr3*inPr4)*(vf+af)/m 
     &          + (k_1(0)-k_2(0)-k_1(1)+k_2(1))*sqrt3*(vf-af)/m         
                 
           ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1).AND.(s.EQ.0) ) THEN 

              inPr1 = InProd_zero(p1,+1,k_1,-1)
              inPr2 = InProd_zero(p1,+1,k_2,-1)
              inPr3 = InProd_zero(p2,+1,k_2,-1)
              inPr4 = InProd_zero(p2,+1,k_1,-1)

              forSqrt1 = (k_1(0)-k_1(1))/(p2(0)-p2(1))
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))
              forSqrt3 = (k_2(0)-k_2(1))/(p1(0)-p1(1))
              forSqrt4 = (k_1(0)-k_1(1))/(p1(0)-p1(1))
              sqrt1 = Dsqrt(forSqrt1)
              sqrt2 = Dsqrt(forSqrt2)
              sqrt3 = Dsqrt(forSqrt3)
              sqrt4 = Dsqrt(forSqrt4)     

              TrMatrix_mass = 
     &                 (inPr1*sqrt1 - inPr2*sqrt2)*(vf+af)*m2/m
     &               + (inPr3*sqrt3 - inPr4*sqrt4)*(vf-af)*m1/m
                        
           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1).AND.(s.EQ.0) ) THEN 

              inPr1 = InProd_zero(p1,-1,k_1,+1)
              inPr2 = InProd_zero(p1,-1,k_2,+1)
              inPr3 = InProd_zero(p2,-1,k_2,+1)
              inPr4 = InProd_zero(p2,-1,k_1,+1)

              forSqrt1 = (k_1(0)-k_1(1))/(p2(0)-p2(1))
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))
              forSqrt3 = (k_2(0)-k_2(1))/(p1(0)-p1(1))
              forSqrt4 = (k_1(0)-k_1(1))/(p1(0)-p1(1))
              sqrt1 = Dsqrt(forSqrt1)
              sqrt2 = Dsqrt(forSqrt2)
              sqrt3 = Dsqrt(forSqrt3)
              sqrt4 = Dsqrt(forSqrt4)     
        
              TrMatrix_mass = 
     &                 (inPr1*sqrt1 - inPr2*sqrt2)*(vf-af)*m2/m
     &               + (inPr3*sqrt3 - inPr4*sqrt4)*(vf+af)*m1/m

           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1).AND.(s.EQ.0) ) THEN 

              inPr1 = InProd_zero(p2,+1,k_2,-1)
              inPr2 = InProd_zero(p1,-1,k_2,+1)
              inPr3 = InProd_zero(p2,+1,k_1,-1)
              inPr4 = InProd_zero(p1,-1,k_1,+1)
              sqrt1 = Dsqrt(p1(0)-p1(1))
              sqrt2 = Dsqrt(p2(0)-p2(1))
              sqrt3 = m1*m2/sqrt1/sqrt2

              TrMatrix_mass =                    
     &                      (inPr1*inPr2 - inPr3*inPr4)*(vf-af)/m  
     &            + (k_1(0)-k_2(0)-k_1(1)+k_2(1))*sqrt3*(vf+af)/m
        
           ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.+1).AND.(s.EQ.+1) ) THEN 

              inPr1 = InProd_zero(p1,+1,k_1,-1)
              inPr2 = InProd_zero(k_2,-1,p2,+1)
              inPr3 = inPr1*inPr2

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))  
              sqrt1 = Dsqrt(forSqrt1)                   
              sqrt2 = Dsqrt(forSqrt2)                   
              sqrt3 = m1*m2*sqrt1*sqrt2

              TrMatrix_mass =
     &                 Dsqrt(2d0)/m*(inPr3*(vf+af)+sqrt3*(vf-af))

           ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1).AND.(s.EQ.+1) ) THEN 

              inPr1 = InProd_zero(p1,+1,k_1,-1)
              inPr2 = InProd_zero(p2,+1,k_1,-1) 

              forSqrt1 = (k_2(0)-k_2(1))/(p2(0)-p2(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p1(0)-p1(1))                       
              sqrt1 = m2*Dsqrt(forSqrt1)                   
              sqrt2 = m1*Dsqrt(forSqrt2)                                     
                     
              TrMatrix_mass =
     &                 Dsqrt(2d0)/m*( + inPr1*sqrt1*(vf+af)
     &                                - inPr2*sqrt2*(vf-af)
     &                                )

           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1).AND.(s.EQ.+1) ) THEN 

              inPr1 = InProd_zero(k_2,-1,p2,+1)
              inPr2 = InProd_zero(k_2,-1,p1,+1)

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_1(0)-k_1(1))/(p2(0)-p2(1))                       
              sqrt1 = m1*Dsqrt(forSqrt1)                   
              sqrt2 = m2*Dsqrt(forSqrt2)                                     
                     
              TrMatrix_mass =
     &                 Dsqrt(2d0)/m*( + inPr1*sqrt1*(vf+af)
     &                                - inPr2*sqrt2*(vf-af)
     &                                )

           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1).AND.(s.EQ.+1) ) THEN 

              inPr1 = InProd_zero(p2,+1,k_1,-1)
              inPr2 = InProd_zero(k_2,-1,p1,+1)
              inPr3 = inPr1*inPr2

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))  
              sqrt1 = Dsqrt(forSqrt1)                   
              sqrt2 = Dsqrt(forSqrt2)                   
              sqrt3 = m1*m2*sqrt1*sqrt2

              TrMatrix_mass = 
     &                 Dsqrt(2d0)/m*(inPr3*(vf-af)+sqrt3*(vf+af))

           ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.+1).AND.(s.EQ.-1) ) THEN 

              inPr1 = InProd_zero(p2,-1,k_1,+1)
              inPr2 = InProd_zero(k_2,+1,p1,-1)
              inPr3 = inPr1*inPr2

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))  
              sqrt1 = Dsqrt(forSqrt1)                   
              sqrt2 = Dsqrt(forSqrt2)                   
              sqrt3 = m1*m2*sqrt1*sqrt2

              TrMatrix_mass =               
     &                 Dsqrt(2d0)/m*(inPr3*(vf+af)+sqrt3*(vf-af))

           ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1).AND.(s.EQ.-1) ) THEN 

              inPr1 = InProd_zero(k_2,+1,p2,-1)
              inPr2 = InProd_zero(k_2,+1,p1,-1)

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_1(0)-k_1(1))/(p2(0)-p2(1))                       
              sqrt1 = m1*Dsqrt(forSqrt1)                   
              sqrt2 = m2*Dsqrt(forSqrt2)                                     
                     
              TrMatrix_mass =
     &                 Dsqrt(2d0)/m*(+ inPr1*sqrt1*(vf-af)
     &                               - inPr2*sqrt2*(vf+af)
     &                               )

           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1).AND.(s.EQ.-1) ) THEN 

              inPr1 = InProd_zero(p1,-1,k_1,+1)
              inPr2 = InProd_zero(p2,-1,k_1,+1)

              forSqrt1 = (k_2(0)-k_2(1))/(p2(0)-p2(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p1(0)-p1(1))                       
              sqrt1 = m2*Dsqrt(forSqrt1)                   
              sqrt2 = m1*Dsqrt(forSqrt2)                                     
                     
              TrMatrix_mass =
     &                 Dsqrt(2d0)/m*(+ inPr1*sqrt1*(vf-af)
     &                               - inPr2*sqrt2*(vf+af) 
     &                               )
    
           ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1).AND.(s.EQ.-1) ) THEN 

              inPr1 = InProd_zero(p1,-1,k_1,+1)
              inPr2 = InProd_zero(k_2,+1,p2,-1)
              inPr3 = inPr1*inPr2

              forSqrt1 = (k_1(0)-k_1(1))/(p1(0)-p1(1))                       
              forSqrt2 = (k_2(0)-k_2(1))/(p2(0)-p2(1))  
              sqrt1 = Dsqrt(forSqrt1)                   
              sqrt2 = Dsqrt(forSqrt2)                   
              sqrt3 = m1*m2*sqrt1*sqrt2

              TrMatrix_mass =  
     &                 Dsqrt(2d0)/m*(inPr3*(vf-af)+sqrt3*(vf+af))

           ELSE

              WRITE(MCLUN,*) " "             
              WRITE(MCLUN,*) " ERROR IN TrMatrix_mass:   Wrong values for l1,l2,s: "
              WRITE(MCLUN,*) "          l1,l2 = -1,+1; s = -1,0,1 "
              WRITE(MCLUN,*) " "             
              STOP

           ENDIF 
         
        RETURN
        END
