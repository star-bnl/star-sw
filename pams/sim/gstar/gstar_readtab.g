      FUNCTION gstar_readtab (TAB_h,TAB_p)
      IMPLICIT NONE
#include "PAM.inc"
#include "particle.inc"
*
      INTEGER   GSTAR_READTAB
      RECORD   /TABLE_HEAD_ST/ TAB_h    
      RECORD   / PARTICLE_ST / TAB_p(*)
      Integer   ISLFLAG,NN(3)/1,1,1/
      character Cform*6 /'/6I 9F'/
*
      Call RbSTORE ('/EVNT/GENE/GENT',NN,Cform,15*TAB_h.nok,TAB_P(1).IstHep)
      Call AgGZKINE(ISLFLAG('INPU','PRIN'))
      GSTAR_READTAB = 0
      end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
old version:

      Integer   LENOCC,ge_pid,iv,ip,nv,nt,Lout/6/
      call  AGSVERT set(2)
      do ip=1,TAB_h.nok
        check TAB_P(ip).IstHep==1
        Call aPdg2Gea (TAB_P(ip).IdHep, ge_pid)
	if ge_pid<=0 
        {  if (Idebug>1) <W> TAB_P(ip).IdHep
           (' gstar_read HEPEVT unknown particle',i6)
           ge_pid = 1000000+TAB_P(ip).IdHep
        }
        Call AgSVERT ( TAB_P(ip).vhep,   0,     iv,  0,  0, nv); 
        Call AgSKINE ( TAB_P(ip).phep,  ge_pid, nv, ip,  0, nt); 
        if (nt<=0 & Idebug>0) print *,
           ' gstar_readtab: pdg code ',TAB_P(ip).IdHep,' rejected '
      enddo
      call  AGSVERT set(0)
