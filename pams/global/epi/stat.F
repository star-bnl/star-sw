
      subroutine stat(prob,sharp,mean,std_dev,nhypo_det)
      implicit none
      integer nhypo_det(5), i,j,jmax
      real prob(5,10),sharp(5),mean(5),std_dev(5),pmax,pnext,sum
      do 10 i = 1,5
      if(nhypo_det(i) .eq. 0) then
           sharp(i) = 0.0
           mean(i)  = 0.0
           std_dev(i) = 0.0
           go to 10
      end if

ccc  Find largest and next largest probability
ccc
      pmax = 0.0
      do 20 j = 1,9
      if(prob(i,j) .gt. pmax) then
           jmax = j
           pmax = prob(i,j)
      end if
20    continue

      pnext = 0.0
      do 21 j = 1,9
      if(j .eq. jmax) go to 21
      if(prob(i,j) .gt. pnext) then
           pnext = prob(i,j)
      end if
21    continue

      if(pnext .ne. 0.0) then
           sharp(i) = pmax/pnext
      else
           sharp(i) = 100.0
      end if

ccc  Compute the mean index, assuming normalized probabilities
ccc
      mean(i) = 0.0
      sum = 0.0
      do 30 j = 1,9
      mean(i) = mean(i) + prob(i,j)*float(j)
      sum = sum + prob(i,j)
30    continue
      mean(i) = mean(i)/sum

ccc  Compute the standard deviation of i-th detector specific probability
ccc  distribution.
ccc
      std_dev(i) = 0.0
      sum = 0.0
      do 31 j = 1,9
      std_dev(i) = std_dev(i) + prob(i,j)*((float(j) - mean(i))**2)
      sum = sum + prob(i,j)
31    continue
      std_dev(i) = sqrt(std_dev(i)/sum)
10    continue
      return
      end
