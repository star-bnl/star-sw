      FUNCTION gstar_readtab (TAB_h,TAB_p)
      IMPLICIT NONE
#include "PAM.inc"
#include "particle.inc"
#include "geant321/gcflag.inc"
*
      INTEGER   GSTAR_READTAB
      RECORD   /TABLE_HEAD_ST/ TAB_h    
      RECORD   / PARTICLE_ST / TAB_p(*)
      Integer   ISLFLAG,NN(3)/1,1,1/
      character Cform*6 /'/6I 9F'/
*
      If (Idebug.gt.1) print *,' GSTAR_ReadTab N=',TAB_h.nok
      Call RbSTORE ('/EVNT/GENE/GENT',NN,Cform,15*TAB_h.nok,TAB_P(1).IstHep)
      Call AgGZKINE(ISLFLAG('INPU','PRIN'))
      GSTAR_READTAB = 0
      end

