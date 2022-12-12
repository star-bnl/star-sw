      subroutine ggderiv(x, sigma, ksi, w, alpha, D)
      implicit none
      real*8 x, sigma, ksi, w, alpha
      real*8 F(3,3), J(3), D(3)
      real*8 pi /3.14159265358979312/
      integer i,k
      F(1,1) = 1
      F(1,2) = -((-((alpha*exp(-(2*pi)/abs(alpha)))/abs(alpha))/2.0d+0
     & )-(pi**((-3.0d+0)/2.0d+0)*(4-pi)*alpha**3*(alpha**2+1)**((-3
     & .0d+0)/2.0d+0))/(sqrt(2.)*(1-(2*alpha**2)/(pi*(alpha**2+1)))**2.0d
     & +0)+(sqrt(2.)*alpha)/(sqrt(pi)*sqrt(alpha**2+1)))/sqrt(1-(2*alpha*
     & *2)/(pi*(alpha**2+1)))
        F(1,3) = (((4*alpha**3)/(pi*(alpha**2+1)**2)-(4*alpha)/(pi*(alph
     & a**2+1)))*(1-(2*alpha**2)/(pi*(alpha**2+1)))**((-3.0d+0)/2.0d+0)*
     & ((-((alpha*exp(-(2*pi)/abs(alpha)))/abs(alpha))/2.0d+0)-(pi**((-3
     & .0d+0)/2.0d+0)*(4-pi)*alpha**3*(alpha**2+1)**((-3.0d+0)/2.0d+0))/
     & (sqrt(2.)*(1-(2*alpha**2)/(pi*(alpha**2+1)))**2.0d+0)+(sqrt(2.)*alp
     & ha)/(sqrt(pi)*sqrt(alpha**2+1)))*sigma)/2.0d+0-(((-(pi*exp(-(2*pi
     & )/abs(alpha)))/alpha**2)-(3*pi**((-3.0d+0)/2.0d+0)*(4-pi)*alpha**
     & 2*(alpha**2+1)**((-3.0d+0)/2.0d+0))/(sqrt(2.)*(1-(2*alpha**2)/(pi*
     & (alpha**2+1)))**2.0d+0)+(3*pi**((-3.0d+0)/2.0d+0)*(4-pi)*alpha**4
     & *(alpha**2+1)**((-5.0d+0)/2.0d+0))/(sqrt(2.)*(1-(2*alpha**2)/(pi*(
     & alpha**2+1)))**2.0d+0)+(2.0d+0*pi**((-3.0d+0)/2.0d+0)*(4-pi)*alph
     & a**3*(alpha**2+1)**((-3.0d+0)/2.0d+0)*((4*alpha**3)/(pi*(alpha**2
     & +1)**2)-(4*alpha)/(pi*(alpha**2+1))))/(sqrt(2.)*(1-(2*alpha**2)/(p
     & i*(alpha**2+1)))**3.0d+0)+sqrt(2.)/(sqrt(pi)*sqrt(alpha**2+1))-(sq
     & rt(2.)*alpha**2*(alpha**2+1)**((-3.0d+0)/2.0d+0))/sqrt(pi))*sigma)
     & /sqrt(1-(2*alpha**2)/(pi*(alpha**2+1)))
      F(2,1) = 0
      F(2,2) = 1/sqrt(1-(2*alpha**2)/(pi*(alpha**2+1)))
      F(2,3) = -(((4*alpha**3)/(pi*(alpha**2+1)**2)-(4*alpha)/(pi*(alp
     & ha**2+1)))*(1-(2*alpha**2)/(pi*(alpha**2+1)))**((-3.0d+0)/2.0d+0)
     & *sigma)/2.0d+0
      F(3,1) = 0
      F(3,2) = 0
      F(3,3) = 1

      J(1) = ((x-ksi)*exp(-((x-ksi)**2/w**2)/2.0d+0)*(erf((alpha*(x-
     & ksi))/(sqrt(2.)*w))+1))/(sqrt(2.)*sqrt(pi)*w**3)-(alpha*exp((-((alp
     & ha**2*(x-ksi)**2)/w**2)/2.0d+0)-((x-ksi)**2/w**2)/2.0d+0))/(pi*w*
     & *2)
      J(2) = ((x-ksi)**2*exp(-((x-ksi)**2/w**2)/2.0d+0)*(
     & erf((alpha*(x-ksi))/(sqrt(2.)*w))+1))/(sqrt(2.)*sqrt(pi)*w**4)-(exp
     & (-((x-ksi)**2/w**2)/2.0d+0)*(erf((alpha*(x-ksi))/(sqrt(2.)*w))+1))
     & /(sqrt(2.)*sqrt(pi)*w**2)-(alpha*(x-ksi)*exp((-((alpha**2*(x-ksi)*
     & *2)/w**2)/2.0d+0)-((x-ksi)**2/w**2)/2.0d+0))/(pi*w**3)
      J(3) = ((x-ksi)*exp((-((alpha**2*(x-ksi)**2)/w**2)/2.0d+0)-((x-ks
     & i)**2/w**2)/2.0d+0))/(pi*w**2)
      do i = 1, 3
         D(i) = 0;
         do k = 1, 3
            D(i) = D(i) + J(k) * F(k,i) 
         end do
      end do
      end
