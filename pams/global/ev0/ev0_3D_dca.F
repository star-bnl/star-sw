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
          real t1,t2,phi1,phi2

          real a,b,c,d
          


         a=-(p1(1)*p2(1)+p1(2)*p2(2)+p1(3)*p2(3))
         b=-(trk1(3)-trk2(3))*p1(3)
         c=(trk1(3)-trk2(3))*p2(3)
         if(a*a.eq.1)then
         t1=0.0
         t2=0.0
         else
         t1=(b-c*a)/(1-a*a)
         t2=(c-a*b)/(1-a*a)
         endif
         
         pt1=sqrt(p1(1)*p1(1)+p1(2)*p1(2))
         pt2=sqrt(p2(1)*p2(1)+p2(2)*p2(2))
         phi1=t1*pt1/rc1
         phi2=t2*pt2/rc2
         xn1(1)=trk1(1)*cos(trk1(2))+rc1*p1(1)/pt1*sin(phi1)
         xn1(2)=trk1(1)*sin(trk1(2))+rc1*p1(2)/pt1*sin(phi1) 
         xn2(1)=trk2(1)*cos(trk2(2))+rc2*p2(1)/pt2*sin(phi2)
         xn2(2)=trk2(1)*sin(trk2(2))+rc2*p2(2)/pt2*sin(phi2)
          
         d=(trk1(3)-trk2(3)+p1(3)*t1-p2(3)*t2)**2+(xn1(1)-xn2(1))**2+(xn1(2)-xn2(2))**2
         if(d.gt.rd)then
         xn1(1)=trk1(1)*cos(trk1(2))
         xn1(2)=trk1(1)*sin(trk1(2)) 
         xn2(1)=trk2(1)*cos(trk2(2))
         xn2(2)=trk2(1)*sin(trk2(2))
        
         else
         rd=d
         endif


         end
