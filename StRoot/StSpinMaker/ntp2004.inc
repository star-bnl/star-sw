      parameter (MAXTRK=5000)

      INTEGER event,BChi,BClo,Token,TrgWd,TrgId,
     +     prepost,
     +     bunchId,bunchid7bit,spinBit,
     +     VTXDSM(8),FPDDSM(8),
     +     ZDC(16),ZDCSMD(32),CTB(256),BBC(80),
     +     FPDENS(112),FPDETB(64),FPDWNS(112),FPDWTB(64),
     +     FPDENSL1(8),FPDETBL1(8),FPDWNSL1(8),FPDWTBL1(8),
     +     FPDADC(256),FPDTDC(8),FPDWEST(72),
     +     NPrimTrk,NTPCTrk,NEastFTPCTrk,
     +     ntrk,trknhit(MAXTRK)
      REAL xVertex,yVertex,zvertex,
     +     trkpt(MAXTRK),trketa(MAXTRK),trkphi(MAXTRK),
     +     trkdcax(MAXTRK),trkdcay(MAXTRK),trkdcaz(MAXTRK)

      COMMON /NTP2004/ event,BChi,BClo,Token,TrgWd,TrgId,
     +     prepost,
     +     bunchId,bunchid7bit,spinBit,
     +     VTXDSM,FPDDSM,
     +     ZDC,ZDCSMD,CTB,BBC,
     +     FPDENS,FPDETB,FPDWNS,FPDWTB,
     +     FPDENSL1,FPDETBL1,FPDWNSL1,FPDWTBL1,
     +     FPDADC,FPDTDC,FPDWEST,
     +     NPrimTrk,NTPCTrk,NEastFTPCTrk,
     +     xVertex,yVertex,zVertex,
     +     ntrk,trknhit,trkpt,trketa,trkphi,
     +     trkdcax,trkdcay,trkdcaz
      
      character*900 ntpdef
      ntpdef=
     $     'event:u,BChi:u,BClo:u,Token[0,4095]:u'//
     $     ',TrgWd:u,TrgId:u,prepost[-5,5]:I'//
     $     ',bunchId[0,127]:u,bunchid7bit[0,127]:u,spinBit:u'//
     $     ',VTXDSM(8)[0,65535]:u,FPDDSM(8)[0,65535]:u'//
     $     ',ZDC(16)[0,255]:u,ZDCSMD(32)[0,255]:u'//
     $     ',CTB(256)[0,255]:u,BBC(80)[0,255]:u'//
     $     ',FPDENS(112)[0,255]:u,FPDETB(64)[0,255]:u'//
     $     ',FPDWNS(112)[0,255]:u,FPDWTB(64)[0,255]:u'//
     $     ',FPDENSL1(8)[0,65535]:u,FPDETBL1(8)[0,65535]:u'//
     $     ',FPDWNSL1(8)[0,65535]:u,FPDWTBL1(8)[0,65535]:u'//
     $     ',FPDADC(256)[0,65535]:u,FPDTDC(8)[0,65535]:u'//
     $     ',FPDWEST(72)[0,65535]:u'//
     $     ',NPrimTrk:I,NTpcTrk:I,NEastFTPCTrk:I'//
     $     ',xVertex:R,yVertex:R,zVertex:R'//
     $     ',ntrk[0,5000]:u,trknhit(ntrk):u'//
     $     ',trkpt(ntrk):R,trketa(ntrk):R,trkphi(ntrk):R'//
     $     ',trkdcax(ntrk):R,trkdcay(ntrk):R,trkdcaz(ntrk):R'



