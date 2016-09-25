           FUNCTION TrMatrix_zero(p1,m1,l1,k,s,p2,m2,l2)
c############################################################################# 
c                                                                            #
c                         \ eps(k,0,s)                                       # 
c                         /                                                  #   
c                        _\                                                  # 
c                         /\                                                 #
c                         \                                                  #
c                         /                                                  #
c           ---<----------\-------------<---                                 #
c       Ub(p1,m1,l1)                  U(p2,m2,l2)                            #
c                                                                            #
c                                                                            #
c             definition of arbitrary light-like vector beta!!               #
c                                                                            #
c              bet(0) = 1.d0                                                 #
c              bet(1) = 1.d0                                                 #
c              bet(2) = 0.d0      <==> bet == k0  expression becomes easy!!  #
c              bet(3) = 0.d0                                                 #
c#############################################################################
              IMPLICIT NONE
              INTEGER          l1,l2,s,i
              DOUBLE PRECISION m1,m2,forSqrt1,forSqrt2,p1(0:3)
              DOUBLE PRECISION p1_1(0:3),p2_1(0:3),k(0:3),p2(0:3)
              DOUBLE PRECISION sqrt1,sqrt2,scalProd1,scalProd2
              DOUBLE COMPLEX   InProd_zero,inPr1,inPr2,inPr3,TrMatrix_zero
              LOGICAL          equal
              EXTERNAL         InProd_zero             
              INCLUDE 'MC_Declare.h' 
              equal = .TRUE.    
              DO i=0,3
                IF (p1(i).NE.p2(i))  equal = equal.AND..FALSE.
              ENDDO
                    

              IF ( (m1.EQ.m2).AND.(equal) ) THEN
c..          
c..             when:  p1=p2=p <=> m1=m2 TrMatrix_zero is diagonal
c..               
                 IF ( (l1.EQ.+1).AND.(l2.EQ.+1) ) THEN 

                    inPr1    = InProd_zero(k,+s,p1,-s)
                    forSqrt1 = (p1(0)-p1(1))/(k(0)-k(1)) 
                    sqrt1    = Dsqrt(2d0*forSqrt1)

                    TrMatrix_zero = sqrt1*inPr1
                    GOTO 111  
 
                 ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1) ) THEN                

                    TrMatrix_zero = Dcmplx(0d0,0d0)
                    GOTO 111  

                 ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1) ) THEN                

                    TrMatrix_zero = Dcmplx(0d0,0d0)
                    GOTO 111  

                 ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1) ) THEN                

                    inPr1    = InProd_zero(k,+s,p1,-s)
                    forSqrt1 = (p1(0)-p1(1))/(k(0)-k(1)) 
                    sqrt1    = Dsqrt(2d0*forSqrt1)

                    TrMatrix_zero = sqrt1*inPr1
                    GOTO 111  
          
                 ELSE 
        
                    WRITE(MCLUN,*) ""             
                    WRITE(MCLUN,*) " ERROR IN DIAGONAL PART OF TrMatrix_zero: "
                    WRITE(MCLUN,*) "       WRONG VALUES FOR l1,l2,s : l1,l2,s = -1,+1" 
                    WRITE(MCLUN,*) ""             
                    STOP

                 ENDIF       

              ENDIF

              IF ( (l1.EQ.+1).AND.(l2.EQ.+1).AND.(s.EQ.+1) ) THEN 

                 inPr1    = InProd_zero(k,+1,p1,-1)
                 forSqrt1 = (p2(0)-p2(1))/(k(0)-k(1))
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
 
                 TrMatrix_zero = sqrt1*inPr1

              ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1).AND.(s.EQ.+1) ) THEN 
 
                 TrMatrix_zero = Dcmplx(0d0,0d0)

              ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1).AND.(s.EQ.+1) ) THEN 
  
                 forSqrt1 = (p1(0)-p1(1))/(p2(0)-p2(1))             
                 forSqrt2 = 1d0/forSqrt1
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
                 sqrt2    = Dsqrt(2d0*forSqrt2)                   
                     
                 TrMatrix_zero = Dcmplx(m2*sqrt1-m1*sqrt2,0d0)

              ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1).AND.(s.EQ.+1) ) THEN 

                 inPr1    = InProd_zero(k,+1,p2,-1)
                 forSqrt1 = (p1(0)-p1(1))/(k(0)-k(1))
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
  
                 TrMatrix_zero = inPr1*sqrt1

              ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.+1).AND.(s.EQ.-1) ) THEN 
 
                 inPr1    = -InProd_zero(k,-1,p2,+1)
                 forSqrt1 = (p1(0)-p1(1))/(k(0)-k(1))
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
 
                 TrMatrix_zero = -sqrt1*inPr1


              ELSEIF ( (l1.EQ.+1).AND.(l2.EQ.-1).AND.(s.EQ.-1) ) THEN 
           
                 forSqrt1 = (p1(0)-p1(1))/(p2(0)-p2(1))     
                 forSqrt2 = 1d0/forSqrt1
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
                 sqrt2    = Dsqrt(2d0*forSqrt2)                   
                     
                 TrMatrix_zero = Dcmplx(m2*sqrt1-m1*sqrt2,0d0)

              ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.+1).AND.(s.EQ.-1) ) THEN 

                 TrMatrix_zero = Dcmplx(0d0,0d0)

              ELSEIF ( (l1.EQ.-1).AND.(l2.EQ.-1).AND.(s.EQ.-1) ) THEN 

                 inPr1    = -InProd_zero(k,-1,p1,+1)
                 forSqrt1 = (p2(0)-p2(1))/(k(0)-k(1))
                 sqrt1    = Dsqrt(2d0*forSqrt1)                   
  
                 TrMatrix_zero = -inPr1*sqrt1

              ELSE

                 WRITE(MCLUN,*) ""
                 WRITE(MCLUN,*) " ERROR IN TrMatrix_zero: "
                 WRITE(MCLUN,*) "       WRONG VALUES FOR l1,l2,s : l1,l2,s = -1,+1"
                 WRITE(MCLUN,*) ""             
                 STOP

              ENDIF 

 111          CONTINUE           

           RETURN
           END



       DOUBLE PRECISION FUNCTION InSqrt(p,q)

           DOUBLE PRECISION q(0:3),p(0:3)
            
           InSqrt = Dsqrt( (p(0)-p(1)) / (q(0)-q(1)) )

       RETURN
       END    


       DOUBLE COMPLEX FUNCTION BsFactor(s,k,p,m)
c///////////////////////////////////////////////////////////////////
c                                                                 //
c  this is small B_{s}(k,p) function when TrMartix is diaagonal!! //
c                                                                 //
c///////////////////////////////////////////////////////////////////
          IMPLICIT NONE
          INTEGER          s
          DOUBLE PRECISION p_1(0:3),p(0:3),k(0:3),m
          DOUBLE PRECISION forSqrt1,sqrt1
          DOUBLE COMPLEX   InProd_zero,inPr1
          EXTERNAL         InProd_zero             
          INCLUDE 'MC_Declare.h'  

          IF ( s.EQ.+1 ) THEN 

             inPr1    = InProd_zero(k,+1,p,-1)
             forSqrt1 = (p(0)-p(1))/(k(0)-k(1))
             sqrt1    = Dsqrt(2d0*forSqrt1)  
             BsFactor = inPr1*sqrt1

          ELSEIF ( s.EQ.-1 ) THEN 

             inPr1    = InProd_zero(k,-1,p,+1)
             forSqrt1 = (p(0)-p(1))/(k(0)-k(1))
             sqrt1    = Dsqrt(2d0*forSqrt1)  
             BsFactor = inPr1*sqrt1

         ELSE

            WRITE(MCLUN,*) " "             
            WRITE(MCLUN,*) " ERROR IN BsFactor: "
            WRITE(MCLUN,*) "       WRONG VALUES FOR s : s = -1,+1"
            WRITE(MCLUN,*) " "             
            STOP

         ENDIF 

       RETURN
       END   

       DOUBLE COMPLEX FUNCTION SoftFactor(s,k,p1,m1,p2,m2,Gmass2)

c
c       Gauge invariant soft factor for decay!!
c       Gmass2 -- photon mass square       
c 
          IMPLICIT NONE
          INTEGER          s
          DOUBLE PRECISION p1(0:3),p2(0:3),k(0:3),Gmass2
          DOUBLE PRECISION m1,m2,ScalProd1,ScalProd2
          DOUBLE COMPLEX   BsFactor2,BsFactor1,BsFactor
          EXTERNAL         BsFactor             

          ScalProd1 = k(0)*p1(0)-k(1)*p1(1)-k(2)*p1(2)-k(3)*p1(3)
          ScalProd2 = k(0)*p2(0)-k(1)*p2(1)-k(2)*p2(2)-k(3)*p2(3)
          
          BsFactor1 = BsFactor(s,k,p1,m1)
          BsFactor2 = BsFactor(s,k,p2,m2)

          SoftFactor= + BsFactor2/2.d0/(ScalProd2-Gmass2)
     &                - BsFactor1/2.d0/(ScalProd1-Gmass2)

       RETURN
       END
