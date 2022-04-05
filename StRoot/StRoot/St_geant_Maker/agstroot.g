*CMZ :          07/09/98  00.37.50  by  Pavel Nevski
*CMZ :  1.40/05 23/08/98  21.58.55  by  Pavel Nevski
*-- Author :    Pavel Nevski   25/11/97
***************************************************************************
*                                                                         *
            Subroutine   A G S T R O O T 
*                                                                         *
* Description: Given a path, dump the whole structure below into STAF     *
*     request a la UNIX: sys/bank - very combersome for the moment :      *
*     'standard' path form is [/DETM/]sys..., * at the end means 'all'    *
*     RECB etc alternative form is /RECB/....bank@sys                     *
*          meaning 'take documentation from sys'                          *
*     Only existing banks are dumped, alternative is commented out now    *
* Attention: Current link is equivalenced to Lpar for protection          *
***************************************************************************
*KEEP,TYPING.
      IMPLICIT NONE
*KEEP,GCBANK.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
*KEEP,GCUNIT.
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
*KEEP,GCFLAG.
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
*KEEP,AGCDOCL.
C     common for the documentation supporting links
      Integer           LDarea(2),L1Doc,L2Doc,LKDoc,Ldoc,Ldete,Lpar
      COMMON /AGCDOCL/  LDarea,   L1Doc,L2Doc,LKDoc,Ldoc,Ldete,Lpar
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
*KEND.
  INTEGER       LENOCC,TDM_MAP_TABLE,Iprin,Nun(15),LK(15),IL(15),Ist(15),
                I,J,L,K,M,N,Ia,Lc,Lp,Mj,NDDD,NCCC
  Character     Csys*4,Table*10,Cbank*4
  Character*80  Cpath,Cdest,Spec,format
  Character*4 Source,Destin
  EQUIVALENCE   (L,Lpar)
  common /agcstaffor/ nddd,nccc,format
  integer Idoc 	
*
  Source = '*';
  Destin = '*';
  Idoc   = 1;	
* reduce to the standard path and dest:
  Iprin=Idebug;         Cpath=Source;
  if (Cpath(1:1)!='/')
  { If Lenocc(Source)==4 { Cpath='/DETM/'//Source(1:4)//'/*'; }
    else                 { Cpath='/DETM/'//Source;            }
  }
  Call CLTOU(Cpath);
 
  Cdest='/dui/Run'; if (Cpath(2:5)!='DETM') Cdest='/dui/Event';
  if (Lenocc(Destin)>0) Cdest=Destin;
 
* Csys - prefix for AgKeeps
  Lc=Lenocc(Cpath); n=Index(Cpath,'@');  Csys=' ';
  if (Cpath(1:6)=='/DETM/' & Lc>=10) Csys=Cpath(7:Lc)
  if (0<n&n<Lc)  { Csys=Cpath(n+1:Lc); Lc=n-1 }
  Mj=2; if (Lenocc(Csys)>0) { Mj=0; "Call Agkeeps(Csys,Cdest,Idoc);" }
*
* Rebank path does not accept / or /_* at the end, truncate:
  m=Index(Cpath(1:Lc),'*'); Lp=Lc;
  if (m>0) Lp=min(Lp,m-1);  if (Cpath(Lp:Lp)='/') { Lp-=1; m=-1 }
  do i=1,Lp/5 { Nun(i)=1 }; Nun(Lp/5)=0;
 
:A: Call ReBANK(Cpath(:Lp),Nun,0,L,Ia)
    Call UHTOC(IQ(L-4),4,CBank,4)
 
  prin2  %L(Csys),Cpath(:lp),Cbank,lc,n,m,mj,lp
  (' AGSTRUT decoded Csys,Cpath,Cbank=',3(1x,a),' lc,n,m,mj,lp=',6i8)
  if L<=0 { <w> %L(Cpath); (' AGSTRU: Data source ',a,' not found '); Return; }
*
J=1; Loop                                  " over existing banks only "
{  If L>0
   {  Call UHTOC(IQ(L-4),4,CBank,4);
      if     J==MJ      { Csys=Cbank; "Call Agkeeps(Csys,Cdest,Idoc);" }
      elseif J> MJ & Csys!='DOCU'
      { Table=Csys(1:4)//'_'//Cbank;  Call CUTOL(Table);
        Call AGKEEPs(%L(Csys)//'/'//Cbank,%L(Cdest),Idoc)
 
*       Spec=Char(0); if (Table='hepe_gent') Spec='particle'//Char(0)
 
        K=1; if IQ(L-5)<0 { K=-IQ(L-5) } elseif Nddd>0 { K=IQ(L-1)/Nddd }
        call RootMapTable(%L(Cdest),%L(Table),Spec,K,IQ(L+1))
        prin2 %L(Cdest),%L(Table),i,k,(Q(L+i),i=1,3)
              (' TDM_MAPing_TABLE:',2(1x,a),2i5,2x,3F8.1)
        " specific bank requested " if (m==0 & Mj==0) Break;
      } Lk(j)=L;  Ist(j)=IQ(L);  IL(j)=0;
   }
*    now navigate in the structure - first through links, then to next bank
   if IQ(LK(j))!=Ist(j)  { prin2;(' AGSTRUT problem, links are lost'); Goto:A:}
   If IL(j)<IQ(LK(j)-2)  { IL(j)+=1; L=LQ(LK(j)-IL(j));  If (L >0) j+=1; }
   else   " brothers "   { If (j==1) Break; L=LQ(LK(j)); If (L<=0) j-=1; }
}
END
 
