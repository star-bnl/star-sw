C    Vogelsang code
C
      SUBROUTINE denom(S,T,U,WQ1,WQ2,WQ3,WQ4,WQ5,WQ6,WQ7,WQ8)
C
C 1: qq'->qq'  2: qq->qq   3: qqbar->q'qbar'  4: qqbar->qqbar
C 5: qqbar->gg   6: gg->qqbar  7: qg->qg   8: gg->gg
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      integer pflag
      common /pol/pflag
c
      WQ1PP=8.D0*S*S/(T*T*9.D0)
      WQ1PM=8.D0*U*U/(T*T*9.D0)
      WQ2PP=8.D0*(S*S/(T*T)+S*S/(U*U)-2.D0/3.D0*S*S/(T*U))/9.D0
      WQ2PM=8.D0*(U*U/(T*T)+T*T/(U*U))/9.D0
      WQ3PP=0.D0
      WQ3PM=8.D0*(T*T+U*U)/(S*S*9.D0)
      WQ4PP=8.D0*S*S/(9.D0*T*T)
      WQ4PM=8.D0*(U*U/(T*T)+(T*T+U*U)/(S*S)-2.D0*U*U/(S*T*3.D0))/9.D0
      WQ5PP=0.D0
      WQ5PM=64.D0*(T*T+U*U)/(27.D0*U*T)-16.D0*(T*T+U*U)/(3.D0*S*S)
      WQ6PP=0.D0
      WQ6PM=(U*U+T*T)/(3.D0*U*T)-3.D0*(T*T+U*U)/(4.D0*S*S)
      WQ7PP=2.D0*S*S/(T*T)-8.D0*S*S/(9.D0*U*S)
      WQ7PM=2.D0*U*U/(T*T)-8.D0*U*U/(9.D0*U*S)
      WQ8PP=4.5D0*(2.D0*S*S/(U*T)-S*U/(T*T)-S*T/(U*U))
      WQ8PM=4.5D0*(6.D0-2.D0*S*S/(U*T)-S*U/(T*T)
     #  -S*T/(U*U)-2.D0*U*T/(S*S))
c
      pflag=0
      if(pflag.eq.0) then
        add=1.d0
      else
        add=-1.d0
      endif
c
      WQ1=0.5D0*(WQ1PP+add*WQ1PM)
      WQ2=0.5D0*(WQ2PP+add*WQ2PM)
      WQ3=0.5D0*(WQ3PP+add*WQ3PM)
      WQ4=0.5D0*(WQ4PP+add*WQ4PM)
      WQ5=0.5D0*(WQ5PP+add*WQ5PM)
      WQ6=0.5D0*(WQ6PP+add*WQ6PM)
      WQ7=0.5D0*(WQ7PP+add*WQ7PM)
      WQ8=0.5D0*(WQ8PP+add*WQ8PM)
c
      RETURN
      END
