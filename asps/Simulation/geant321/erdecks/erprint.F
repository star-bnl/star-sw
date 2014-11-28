      subroutine erprint(ip)
#include "geant321/ertrio.inc"
      print *, 'plane ',iepred(ip),' reached as ',ip
      print *,'position '
      print  '(3e15.5)',(erxout(i,ip),i=1,3)
      print *,'1/p, slopes '
      print  '(3e15.5)',(erpout(i,ip),i=1,3)
      print *,'arrival error '
      print  '(5e15.5)',(errout(i,ip),i=1,15)
      print *, 'transp matrix'
      print '(5e15.5)',((erdtrp(i,j,ip),i=1,5),j=1,5)
      end
      
