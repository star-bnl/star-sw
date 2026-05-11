c $Id: paulibl.f,v 1.4 1999/01/18 09:57:11 ernst Exp $
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       logical function paulibl(i,rhob)
c
c     Revision : 1.0
c
c     This function determines wether the final state of particle i
c     is pauli-blocked ({\tt .true.}) or not ({\tt .false.}).
c     The baryon-density at the location of particle i is returned in {\tt rhob}
c
c
cinput   i :     Index of particle to be added
c
coutput rhob :   baryon density at location of i
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       implicit none
       integer i, j
       real*8 afit, bfit, rho, f, rhob, test, r2, p2, pgw, rhob0
       parameter (afit=1.49641d0, bfit=0.208736d0)

       real*8 qij(4),pij(4),pr2,q2,qp

       include 'coms.f'
       include 'comres.f'
       include 'options.f'

       rho = 0.0d0
       rhob = 0.0d0
       rhob0 = 0d0
       f = 0.0d0
       pgw = 1.0d0/hqc/hqc/gw

       if(CTOption(23).eq.0)goto 3

       do 1 j=1,nbar
          r2 = (rx(i)-rx(j))**2+(ry(i)-ry(j))**2+(rz(i)-rz(j))**2
          if ((ityp(i).eq.ityp(j)).and.(iso3(i).eq.iso3(j))) then
             p2 = (px(i)+ffermpx(i)-px(j)-ffermpx(j))**2
     &           +(py(i)+ffermpy(i)-py(j)-ffermpy(j))**2
     &           +(pz(i)+ffermpz(i)-pz(j)-ffermpz(j))**2
             p2 = 0.25d0*p2
             rho = rho + dexp(-(2.0d0*gw*r2))
             f = f + dexp(-(gw*r2)-pgw*p2)
          end if
          rhob = rhob + dexp(-(2.0d0*gw*r2))
 1     continue
       paulibl = .true.
       test = afit + bfit*rho
       if (test.gt.f) paulibl = .false.
       if (CTOption(10).eq.1) paulibl=.false.

       rhob = rhob*(2.0d0*gw/pi)**1.5/rho0
       if(ityp(i).eq.104)write(6,*)'**',rhob

       return

 3     continue
       do 108 j=1,nbar
          qij(4)=r0(i)-r0(j)
          qij(1)=rx(i)-rx(j)
          qij(2)=ry(i)-ry(j)
          qij(3)=rz(i)-rz(j)
          pij(4)=p0(i)+p0(j)
          pij(1)=px(i)+ffermpx(i)+px(j)+ffermpx(j)
          pij(2)=py(i)+ffermpy(i)+py(j)+ffermpy(j)
          pij(3)=pz(i)+ffermpz(i)+pz(j)+ffermpz(j)
          q2=qij(4)**2-qij(1)**2-qij(2)**2-qij(3)**2
          p2=pij(4)**2-pij(1)**2-pij(2)**2-pij(3)**2
          qp=qij(4)*pij(4)-qij(1)*pij(1)-qij(2)*pij(2)
     .       -qij(3)*pij(3)
          r2=qp**2/p2 - q2
          if(r2.lt.0) then
             write(6,*)'***(E) negative transverse distance !!',r2
             write(6,*)r0(j),rx(j),ry(j),rz(j),p0(j),px(j),py(j),pz(j)
             write(6,*)r0(i),rx(i),ry(i),rz(i),p0(i),px(i),py(i),pz(i)
             r2=1000.d0
          endif
          pr2 = (px(i)-px(j))**2+(py(i)-py(j))**2+(pz(i)-pz(j))**2
          if ((ityp(i).eq.ityp(j)).and.(iso3(i).eq.iso3(j))) then
             rho=rho+dexp(-(2.0d0*gw*r2))
             f=f+dexp(-(gw*r2)-.25d0*pgw*pr2)
          end if
c baryon density in rest frame of particle
         if(j.ne.lstcoll(i))rhob=rhob+dexp(-(2.0d0*gw*r2))
 108  continue
      paulibl=.true.
      test=afit+bfit*rho
      if (test.gt.f) paulibl=.false.
      if (CTOption(10).eq.1) paulibl=.false.

      rhob=max(0d0,min(.1d3,rhob*(2.0d0*gw/pi)**1.5/rho0))

      return
      end
