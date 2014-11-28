c $Id: comnorm.inc,v 1.1 2012/11/29 20:57:02 jwebb Exp $
      integer n
      parameter (n = 400)
      real*8 x_norm(0:3,1:n),y_norm(0:3,1:n)
      real*8 y2a(0:3,1:n),y2b(0:3,1:n),dx
      common /normsplin/ x_norm,y_norm,y2a,y2b,dx

