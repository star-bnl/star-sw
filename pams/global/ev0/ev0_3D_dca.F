          subroutine ev0_3D_dca(rd,p1,p2,trk1,trk2,xn1,xn2,rc1,rc2)
CC:       
C     Functional Description : Calculate the closest distance between
C                              two helix tracks after knowing the crossing
C                              point of their XY plane projection 
C    
C    
C     
C     Created  Feb 12-1999	  Hui Long ,UCLA   (long@physics.ucla.edu)
C     input          :
C     trk1(6),trk2(6):  helix  parameters of the track1 and 2
C             p1,p2  :  normalized momentum of trk1 and trk2 at the 
C                       starting point ,
C                       (the crossing point in this case)
c             rc1,rc2: the radius of track1 and 2
c             
C     output   xn1(2):
C              xn2(2) : the new coordinates of the points 
C                       at the DCA of the two tracks         
C              rd     : 3D dca of v0
          real rd,p1(3),p2(3),trk1(6),trk2(6),xn1(2),xn2(2),rc1,rc2
          real t1,t2

          real a,d,ptr1,ptr2,asq
          real temp, xn11, xn12, xn21, xn22, zdiff


         zdiff = trk1(3)-trk2(3)
         a=(p1(1)*p2(1)+p1(2)*p2(2)+p1(3)*p2(3))
         asq=a*a
         if(asq.eq.1)then
           t1=0.0
           t2=0.0
         else
           temp = zdiff/(1-asq)
           t1=(-p1(3)+p2(3)*a)*temp
           t2=(p2(3)-a*p1(3))*temp
         endif
         
         xn11 = trk1(1)*cos(trk1(2))
         xn12 = trk1(1)*sin(trk1(2))
         xn21 = trk2(1)*cos(trk2(2))
         xn22 = trk2(1)*sin(trk2(2))

         pt1=sqrt(p1(1)*p1(1)+p1(2)*p1(2))
         pt2=sqrt(p2(1)*p2(1)+p2(2)*p2(2))
         ptr1 = rc1/pt1
         ptr2 = rc2/pt2
         temp = ptr1*sin(t1/ptr1)
         xn1(1)=xn11+p1(1)*temp
         xn1(2)=xn12+p1(2)*temp
         temp = ptr2*sin(t2/ptr2)
         xn2(1)=xn21+p2(1)*temp
         xn2(2)=xn22+p2(2)*temp

         d=(zdiff+p1(3)*t1-p2(3)*t2)**2+(xn1(1)-xn2(1))**2+(xn1(2)-xn2(2))**2

         if(d.gt.rd)then
         xn1(1)=xn11
         xn1(2)=xn12
         xn2(1)=xn21
         xn2(2)=xn22
        
         else
         rd=d
         endif


         end
