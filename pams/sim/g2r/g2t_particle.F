      FUNCTION  G2T_PARTICLE (particle_h, particle)
      IMPLICIT  NONE
      INTEGER   G2T_PARTICLE,link,len
*     INTEGER   IL,IDE,IP,ISTAT,IDPDG,MOTH(2),IDAU1,i
*     REAL      P(4),AMASS,TIME,V(5)
*
#include "g2t_particle.inc"
#include "geant321/gcbank.inc"
*
      G2T_PARTICLE=1
      call agfpartic(link,len)
      call ucopy(IQ(link+1),particle,15*len)

*
*      print * ,' in partiocl ',particle_h.maxlen
*      do i=1,particle_h.maxlen
*        call AgNZGETP(IL,IDE,IP,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU1,V)
*        particle_h.nok     = i            
*        particle(i).isthep = istat             ! status code of the entry
*        particle(i).idhep  = idpdg             ! particle identity, PDG 
*        Call Ucopy(moth, particle(i).jmohep,2) ! mother(s) 
*        Call Ucopy(idau1,particle(i).jdahep,1) ! first daughter 
*        Call Ucopy(p,particle(i).phep,4)       ! particl p4
*        particle(i).phep(5)= amass             ! and mass
*        Call Ucopy(v,particle(i).vhep,4)       ! production vertex and time
*      enddo
      return
      END

