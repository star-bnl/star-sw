      INTEGER event,BChi,BClo,Token,TrgWd,TrgId,
     +     prepost,
     +     bunchId,bunchid7bit,spinBit,
     +     VTXDSM(8),FPDDSM(8),
     +     ZDC(16),CTB(256),BBC(80),
     +     FPDENS(112),FPDETB(64),FPDWNS(112),FPDWTB(64),
     +     FPDADC(256),FPDTDC(8),FPDWEST(72),
     +     NPrimTrk,NTPCTrk,NEastFTPCTrk

      REAL Vertex(3)

      COMMON /NTP2003/ event,BChi,BClo,Token,TrgWd,TrgId,
     +     prepost,
     +     bunchId,bunchid7bit,spinBit,
     +     VTXDSM,FPDDSM,
     +     ZDC,CTB,BBC,
     +     FPDENS,FPDETB,FPDWNS,FPDWTB,
     +     FPDADC,FPDTDC,FPDWEST,
     +     NPrimTrk,NTPCTrk,NEastFTPCTrk,
     +     xVertex,yVertex,zVertex
      
      character*500 ntpdef
      ntpdef=
     $     'event:u,BChi:u,BClo:u,Token[0,4095]:u'//
     $     ',TrgWd:u,TrgId:u,prepost[-5,5]:I'//
     $     ',bunchId[0,127]:u,bunchid7bit[0,127]:u,spinBit:u'//
     $     ',VTXDSM(8):u,FPDDSM(8):u'//
     $     ',ZDC(16)[0,255]:u,CTB(256)[0,255]:u,BBC(80)[0,255]:u'//
     $     ',FPDENS(112)[0,255]:u,FPDETB(64)[0,255]:u'//
     $     ',FPDWNS(112)[0,255]:u,FPDWTB(64)[0,255]:u'//
     $     ',FPDADC(256):u,FPDTDC(8):u,FPDWEST(72):u'//
     $     ',NPrimTrk:I,NTpcTrk:I,NEastFTPCTrk:I'//
     $     ',xVertex:R,yVertex:R,zVertex:R'

