

      subroutine pid_order(nhypo_det,gpid,prob,nsigma,weight,flag)
      implicit none
      integer i,idet,j,icol,flag
      integer nhypo_det(5), gpid(5,10), gid_num(48), nh(10)
      real prob(5,10),nsigma(5,10),weight(5,10),p(10),ns(10),w(10)
      data (gid_num(i),i=1,48)/ 10,1,1,10,2,2,10,2,2,10,
     1      3,3,10,4,4,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
     2      10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,5,6,7,10/

ccc   Arrange detector specific PID information in standard order
ccc   for prob, nsigma, and weight.  Columns 1-10 are arranged as follows:
ccc
ccc    <Particle>        <Geant id #>     <column # in array>
ccc
ccc      e+,e-               2,3                   1
ccc    mu+,mu-               5,6                   2
ccc    pi+,pi-               8,9                   2
ccc      K+,K-              11,12                  3
ccc     p,pbar              14,15                  4
ccc   deuteron                45                   5
ccc     triton                46                   6
ccc      alpha                47                   7
ccc  all others            1-48, except above     10
ccc
ccc

      flag = 0
      do 10 idet = 1,5
           do 11 j = 1,10
           p(j) = 0.0
           ns(j) = 0.0
           w(j) = 0.0
           nh(j) = 0
11         continue
      if(nhypo_det(idet) .eq. 0) go to 10

           do 12 j = 1,nhypo_det(idet)
           icol = gid_num(gpid(idet,j))
           nh(icol) = nh(icol) + 1
12         continue
      if(nh(10) .ne. 0) flag = 1
           do 13 j = 1,nhypo_det(idet)
           icol = gid_num(gpid(idet,j))
           p(icol) = p(icol) + prob(idet,j)
           ns(icol) = ns(icol) + nsigma(idet,j)
           w(icol) = w(icol) + weight(idet,j)
13         continue
           do 14 j = 1,10
           if(nh(j) .gt. 1) then
                p(j) = p(j)/float(nh(j))
                ns(j) = ns(j)/float(nh(j))
                w(j) = w(j)/float(nh(j))
           end if
14         continue
           do 15 j = 1,10
           prob(idet,j) = p(j)
           nsigma(idet,j) = ns(j)
           weight(idet,j) = w(j)
15         continue

10    continue
      return
      end
