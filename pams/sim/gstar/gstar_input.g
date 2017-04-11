* $Id: gstar_input.g,v 1.44 2011/08/24 18:45:42 fisyak Exp $
*
* $Log: gstar_input.g,v $
* Revision 1.44  2011/08/24 18:45:42  fisyak
* Dstkine option to define kinematcs from MuDst
*
* Revision 1.43  2005/11/23 20:44:16  potekhin
* Following the changed packaging of data on the Herwig side
* (changes checked in today), we correspondingly modify the
* data transfer bit in the ntuple rading code, in order to comply.
* Look for BJOR1 etc.
*
* Revision 1.42  2005/11/10 20:13:50  potekhin
* Modified handling of the event header data,
* harvesting more info from the herwig ntuple. Incompatible
* with previous version of the herwig ntuple, but much cleaner
* code, and worth it. Coordinated with the recent check in of
* herwig hwigr module.
*
* Revision 1.41  2005/10/10 16:55:43  potekhin
* Augmented the Ntuple header handling in order
* to intercept event characterization info from
* Herwig (999995)
*
* Revision 1.40  2003/12/19 17:55:09  potekhin
* Changed the debug print to a more descriptive one,
* when initializing the MPAR event header struct.
*
* Revision 1.39  2003/07/30 03:52:49  potekhin
* Add the rejected events scaler
*
* Revision 1.38  2003/05/14 23:00:54  potekhin
* This is an adapter that reads external event
* file. This has been upgraded to propagate event
* record data such as the number of binary collisions etc.
*
* Revision 1.37  2000/08/10 23:55:40  nevski
* rqmd-type cwn accepted
*
* Revision 1.36  2000/06/12 15:35:47  nevski
* debuging prints removed
*
* Revision 1.35  2000/06/10 18:58:25  nevski
* rqmd cwntuple read added
*
* Revision 1.34  2000/06/10 18:56:23  nevski
* rqmd cwntuple read added
*
* Revision 1.33  2000/02/25 16:23:19  nevski
* no header printed on skipped events
*
* Revision 1.32  2000/02/25 16:20:12  nevski
* no header printed on skipped events
*
* Revision 1.31  2000/02/25 02:26:35  nevski
* debugging print commented out
*
* Revision 1.30  2000/02/17 17:01:21  nevski
* agcuser common changed
*
* Revision 1.29  2000/02/17 00:06:12  nevski
* cscall truncates strings - corrected
*
* Revision 1.28  2000/02/15 15:45:34  nevski
* Event offset: n0
*
* Revision 1.27  2000/02/10 14:44:06  nevski
* event read offset introduced
*
* Revision 1.26  2000/01/30 21:29:44  nevsky
* ntuple input debugged
*
* Revision 1.25  2000/01/22 17:25:24  nevski
* unused ier removed
*
*
   subroutine gstar_input
+CDE,typing,gcnum.
   print *,' Not All Gstar User Input formats are activated (XDF,TX,FZ,St,MM,Nt)'
   print *,' Automatic recognition uses the first letter of a file extension'
*  print *,' To activate XDF format readout do " gexec gstar_readxdf " '
   end
*************************************************************************
  subroutine agusevent(n)
    Integer          n,InEvent,NnEvent
    Character        Table*120
    common /agcuser/ Irec(10),InEvent,NnEvent,Table
    NnEvent=n     
    if (n>0) print *,' NT READ will start with event ',n
  end
*************************************************************************
  subroutine agusopen(ifile)
*
* Description: open a TXT, EGZ(FZ) or XDF event generator data file.    *
*              Data type should corresponds to the filename extension   *
*              File types are saved in Ccommand - one letter per file   *
* /agcuser/ compensates for lack of IO descriptors in some cases:       *
*       irec   - Ntuple read needs a record number in ReadNT            *
*       Table  - XDF read needs staf table name in AgUsRead             *
*************************************************************************
   Implicit         None   
   Integer          SYSTEMF,LENOCC,CSADDR,Iadr,i,J,ier,L,N,idot,Igate,
                    Irec,Lrec,InEvent,NnEvent
   Character        ifile*(*),C*1,Table*120,CDIR*8,Zcom*256,file*256,cn*8
   common /agcuser/ Irec(10),InEvent,NnEvent,Table
*
+CDE,AGCKINE.
*
    file=ifile;  L=LENOCC(file);  Check L>0; 
    i=Index(ifile,':'); if i>0
    { cn=ifile(i+1:L);   read (cn,*) NnEvent 
      File=ifile(1:i-1); L=i-1;
    }
    If file(L-2:L)=='.gz' 
    { 
       print *,' AgUsOpen: unzipping input file ',ifile(1:L)
       J=0;   Do i=1,L-1  {  if (file(i:i)='/') J=i; }
       file='/tmp/'//ifile(J+1:L-3)
       Zcom='zcat -f '//ifile(1:L)//' > '//file(1:L-J+2)

       Ier=SYSTEMF(Zcom);  if (Ier!=0) go to :e:;  
       L=LENOCC(file)
    }

    C=' '
    Do i=1,L-1  {  check file(i:i)='.'; idot=i; C=file(i+1:i+1);  }
    Call CLTOU(C)
*
    N=LENOCC(CCOMMAND)+1;  Igate=N
    print *,' AgUsOpen: input from ',file(:L),' mode ',C,' event=',NnEvent
*
    if      C=='E'                       " egz format "
    {  Call AgzOPEN('PZ',file(:L),'EK',0,0)
       Call AgZREAD('P',ier)
       if (Ier!=0)  goto :e:
    }
    else if C=='X'                       " xdf format "
    {  J=Csaddr('XDF_OPEN')
       If ( J==0 )  goto :e:
       call CsJCAL(J,2,file(:L+3))
    }
    else if C=='T'                       "  any text  "
    {  Call ApFOPEN(21-N,file(:L),ier) 
       if (ier!=0)  goto :e:
    } 
    else if C=='N'                       " HEP Ntuple "
    {  Lrec=0;     CDIR='HEPEVNT'//CHAR(48+N);    Irec(N)=0;
       Call HREND (CDIR)
       Call HROPEN(21-N,CDIR,file(:L),'X',Lrec,ier);  if (ier!=0) goto :e:; 
       call HBSET ('BSIZE', Lrec, ier)
    } 
    else if C=='C'                       " RQMD Ntuple "
    {  Lrec=0;     CDIR='RQMD'//CHAR(48+N);    Irec(N)=0;
       Call HREND (CDIR)
       Call HROPEN(21-N,CDIR,file(:L),'Q',Lrec,ier);  if (ier!=0) goto :e:; 
       call HBSET ('BSIZE', Lrec, ier)
    } 
    else if C=='S'                       " STAF table "
    {  Table=File(1:idot-1)
    } 
    else if C=='M'                       "mickey-mouse"
    {  Iadr=CSADDR('GSTAR_MICKY');  If (Iadr!=0) Call CSJCAL(Iadr,0,0,0)
    } 
    else if C=='D'                       "DST"
    {  Iadr=CSADDR('GSTAR_DST');  
       If (Iadr!=0) Call CSJCAL(Iadr,0,0,0)
    } 
    CCOMMAND(N:N)=C
    return
:e: print *,' gstar_AgUsOPEN error openning file ',file(1:L)
   end
#if 0
*************************************************************************
   Subroutine  AgUsRead(ier)
*                                                                       *
* Description: loop over all files opened by USER/INPUT                 *
*              and call the corresponding readout routine               *
*  Igate passed to a readout routine is a bi-directional parameter:     *
*        input: gate number in a composite event                        *
*       return: error flag                                              *
*************************************************************************
   Implicit   None
+CDE,GCBANK,GCFLAG,GCNUM,SCLINK,RBBANK,AgCKINE.
   Integer          LENOCC,CSADDR,AMI_CALL,Igate,Ier,J,I
   Character        C*1, o*1, Table*120
   Integer          Irec,InEvent,NnEvent
   common /agcuser/ Irec(10),InEvent,NnEvent,Table
*
   o=CHAR(0)
   Call HEPEVNT
  print *,' AGUSREAD: doing input command :',Ccommand
   Do i=1,LENOCC(Ccommand)
     C=CCOMMAND(i:i);  Igate=i;  J=0;

     if     C=='E' { Call AGZREAD('P',ier);  
                     if (ier==0) { J=1;      call gstar_ReadEGZ(Igate)  }  }
     elseif C=='X' { IrbDIV=IxDIV;           LKARP2=LkEvnt
                     J=CsADDR('XDF_READ'); If (J!=0) call CsJCAL(J,1,Igate)}
     elseif C=='T' { J=1;                    call gstar_ReadTXT(Igate)     }
     elseif C=='N' { J=1;                    call gstar_ReadNT (Igate)     }
     elseif C=='C' { J=1;                    call gstar_ReadCNT(Igate)     }
     elseif C=='M' { J=CsADDR ('MICKINE'); IF (J!=0) Call CsJCAL(J,1,Igate)}
     elseif C=='D' { J=CsADDR ('DSTKINE'); IF (J!=0) Call CsJCAL(J,1,Igate)}
     elseif C=='S' { J=AMI_CALL ('gstar_readtab'//o,1,%L(Table)//o)-1;     }
     elseif C=='V' { J=AMI_CALL ('venus'//o,        1,%L(Table)//o)-1; 
                     J=AMI_CALL ('gstar_readtab'//o,1,%L(Table)//o)-1;     }
     elseif C=='H' { J=AMI_CALL ('hijjet'//o,       1,%L(Table)//o)-1; 
                     J=AMI_CALL ('gstar_readtab'//o,1,%L(Table)//o)-1;     }
     If Igate<=0   { Ier=1; return }
     If (NtoSkip<=0 | Idebug>0) print *,' AgUsREAD mode ',C,' Event ',Ievent,
        ': # particles in GEANT=',Ntrack,'  # vertices=',Nvertx   
     if (J==0) print *,' AgUsREAD ERROR : input processor did not work '
   enddo

*  To generate more header information:
   call GHeader
*
   End
#endif 
*************************************************************************
   Subroutine    gstar_ReadCNT(Igate)
   implicit      none
+CDE,GCUNIT,GCFLAG,GCONST.
*
   Integer           Igate,Irec,InEvent,NnEvent,NpHEP
   Real              CT/3.e11/,Comp(4)/4*0/,Hpar(4)/4*0/
   Character         Table*120,CDIR*10
   common /agcuser/  Irec(10),InEvent,NnEvent,Table
*
   Integer           Ip,Istat,Ipdg,Moth,Idau
   Real              Pxyz,Ener,mass,Vxyz,Vtime
   Common /hep_part/ Ip,Istat,Ipdg,Moth(2),Idau(2),Pxyz(3),Ener,mass,
                                                   Vxyz(3),Vtime
*
   Integer           Iprin,Id/1/,StartVx/0/,Ubuf/0/,num(4),
                     Ge_pid,nin,nv,nt,ier,L,Ia,i             
   Character         CWD*20, Cform*8/'/6I 9F'/
   Logical           First/.true./
*
c ---- Column-Wise-Ntuples ----
   integer           mnt
   parameter        (mnt = 4500)
   integer           idate,itime,ievn,imult,isub,lundata,
                     ipdgtype(mnt),ilastcoll(mnt),nrcoll(mnt)
   real              RNDM,fi,b,xspace(mnt),yspace(mnt),zspace(mnt)
   real              time(mnt),rmass(mnt),Y(mnt),pt(mnt),phi(mnt)
   COMMON/RQMD_EVENT/ievn,  b
   COMMON/RQMD_PART/ imult, ipdgtype, rmass, nrcoll, ilastcoll,
                     time, xspace, yspace, zspace, Y, pt, phi, isub
*  translate fermi to mm (? )
   real              scfermi
   parameter        (scfermi = 1.e-12)
*
   Iprin = Idebug;   prin2 igate; (' In ReadCNT igate=',i3)

   CDIR='//RQMD'//CHAR(48+Igate)
   Call RZCDIR(CWD, 'R')
   Call RZCDIR(CDIR,' ')
   Call  HCDIR(CDIR,' ')
   if (Iprin>1) Call RZCDIR(' ', 'P')
*                                               new file
   if (Irec(Igate)=<0) then
       lundata = id+Igate*1000
       Call HRIN  (Id, 99999, Igate*1000) 
*      call HGNT  (lundata,0,istat)
       call HBNAME(lundata,' ',       0,    '$CLEAR') 
       call HBNAME(lundata,'EVENT',   ievn, '$SET')
       call HBNAME(lundata,'PARTICLE',imult,'$SET')
*      call HNOENT(lundata, nloop)
       InEvent=0
   endif
* 
   Nin=0; Nphep=0; 
   Loop
   {  Irec(Igate)+=1; 
      Call HGNT(Id+Igate*1000,Irec(Igate),ier); if (ier!=0) goto :err:;

*                                                first record
      if (isub.eq.0 .or. isub.eq.1) then
         call datime(idate,itime) 
         InEvent  += 1 
         if Nphep>0 
         {  Prin0 InEvent; (' READ CNT: bad subevent sequence in event ',i8)
            Irec(Igate)-=1; Break; 
         }
*
         fi        = rndm(1.)*TwoPi
         nin       = 1
         istat     = 100
         ipdg      = 9999999
         Moth(1)   = idRun
         Moth(2)   = ievn
         Idau(1)   = Idate
         Idau(2)   = Itime
         Pxyz(1)   = b         ! impact parameter
         Pxyz(2)   = fi        ! angle reaction plane
         Pxyz(3)   = 0         ! ev.gen parameter
         Ener      = 2.*100.   ! energy of N-N CMS
         mass      = 3         ! RQMD ev.gen id
         Vxyz(1)   = 197 
         Vxyz(2)   = 79  
         Vxyz(3)   = 197
         vtime     = 79
      endif
*                                skipping
      if (InEvent<NnEvent) Next
* 
*     GENT bank was already created by HEPEVNT - no formatting is needed
      Nphep += Imult;   Num = {1,Igate,1,NpHep+1 }
      Call REBANK ('/EVNT/GENE/GENT.GENT',num,15,L,ia)
      Prin2 L,ia;      ('  Rebank done with L, ia  = ',2i8)
      Num= {1,Igate,1,0};  
      Call RbSTORE('/EVNT/GENE/GENT*',num,Cform,15,istat)
      prin3 nin,istat; ('  RBSTORE first nin,istat = ',2i8)
*
      Call VZERO(Moth,4)
      do i = 1, Imult
         istat   = 1
         ipdg    = ipdgtype(i)
         mass    = rmass(i)
         Pxyz(1) = pt(i)*cos(phi(i)+fi)
         Pxyz(2) = pt(i)*sin(phi(i)+fi)
         Pxyz(3) = sqrt(pt(i)**2+rmass(i)**2)*sinh(Y(i))
         Ener    = sqrt(pt(i)**2+rmass(i)**2)*cosh(Y(i)) 
         Vxyz(1) = (xspace(i)*cos(fi)-yspace(i)*sin(fi))*scfermi
         Vxyz(2) = (xspace(i)*sin(fi)+yspace(i)*cos(fi))*scfermi
         Vxyz(3) = zspace(i)*scfermi
         Vtime   = time(i)  *scfermi
*
         Nin+=1; num(3)=Nin;  
         Call RbSTORE ('/EVNT/GENE/GENT*',num,Cform,15,istat)
*
*        get rid of eta prime:
         if (abs(ipdg)==331) ipdg=sign(221,ipdg)
* 
         Call apdg2gea (Ipdg, ge_pid); If ge_pid<=0    
         {  Prin1 Ipdg; (' gstar_read HEPTUP unknown particle',i6)
            ge_pid = 1000000+Ipdg
         }
         Call Vscale   (Vxyz,0.1,Vxyz,3);  Vtime=Vtime/ct
         Call AgSVERT  (Vxyz,0, -Igate,Ubuf,     0,nv)  
         call AgSKINE  (Pxyz,ge_pid,nv,num(3)+0.,0,nt)
*
      enddo
      prin2 nin,istat; ('  RBSTORE last nin, istat = ',2i8)
*                                               last record
      if (isub.le.0) goto :ok:
   }
*
:err: Igate=-1
:ok:  Call RZCDIR(CWD,' ')
   end

*************************************************************************
   Subroutine    gstar_ReadNT(Igate)
   implicit      none
+CDE,GCBANK,GCUNIT,SCLINK,GCFLAG.
*
   Integer           Igate,Irec,InEvent,NnEvent,NpHEP,IdEvHep(5)
   Real              CT/3.e11/,Comp(4)/4*0/,Hpar(4)/4*0/
   Character         Table*120,CDIR*10
   common /agcuser/  Irec(10),InEvent,NnEvent,Table
*
   Integer           Ip,Istat,Ipdg,Moth,Idau
   Real              Pxyz,Ener,mass,Vxyz,Vtime
   Common /hep_part/ Ip,Istat,Ipdg,Moth(2),Idau(2),Pxyz(3),Ener,mass,
                                                   Vxyz(3),Vtime
*
   Integer           Id/999/,StartVx/0/,Ubuf/0/,num(4),
                     Ge_pid,nin,nv,nt,ier,L,Ia,i,k

   Character         CWD*20, Cform*8/'/6I 9F'/
   Logical           First/.true./
*
 structure     MPAR { int Version, int SUBPR, int Nbin, int NWE, int NWW, int Njets, int NREJ }

* careful here, a direct import from apythia's pyth_header:
 structure PYTH {int Version, int SUBPR,
                 real MANDS,  real MANDT,  real MANDU,
                 real HARDP,  real COSTH,
                 real BJOR1,  real BJOR2}


* declare the mandatory strings etc
 character*40 module/'hepevnt'/,
              mcomment/'Pythia and Herwig'/,
              bank_title,
              author/'maxim'/,
              created/'20051109'/
 integer      iprin/0/

* variables to pass on the captured data:
*              real xx1,xx2,costh,s,t,et
*              integer subpr

*
   CDIR='//'//'HEPEVNT'//CHAR(48+Igate)
   Call RZCDIR(CWD, 'R')
   Call RZCDIR(CDIR,' ')
   Call  HCDIR(CDIR,' ')
*  Call RZCDIR(' ', 'P')
   Call HRin  (Id,  99999,Igate*1000) 
   Call HRin  (Id-1,99999,Igate*1000) 
   if (Irec(Igate)=<0) then
       CALL HBNAME (id+Igate*1000,'        ', 0,'$CLEAR')
       Call HBNAME (id+Igate*1000,'particle',Ip,'$SET'  )
       InEvent=0
   endif
*        all banks are already booked, so just fill them
   Nin=0
   Loop
   {  Irec(Igate)+=1; 
      Call HGNT(Id+Igate*1000,Irec(Igate),ier); if (ier!=0) goto :err:;

      if (Istat.gt.10.and.ipdg.gt.999990) {
          if ipdg=999999 {
             if nin>0   { Irec(Igate)-=1; Break; }
             InEvent+=1; if (InEvent<NnEvent) Next;  
             NpHEP=ip;  Num = {1,Igate,1,NpHep+1 }
             do i=1,5   { IdEvHep(i) = Pxyz(i);  }

             Call REBANK('/EVNT/GENE/GENT.GENT',num,15,L,ia)
             if L<=0 {print *,' READ_NT error: cant find GENT bank'; goto :err:;}
           * print *,' Rebank done with L,ia = ',L,ia
        }
        elseif ipdg= 999998 {call Ucopy(Pxyz,Hpar,  4)}
        elseif ipdg= 999997 {call Ucopy(Pxyz,Comp,  4)}
        elseif ipdg= 999996 {
             k=999997-ipdg;
             Call REBANK('/EVNT/PASS/MPAR',num,4*(k+1),L,ia)
             if L<=0 {print *,' READ_NT error: cant find MPAR bank'; goto :err:;}

             do i=1,4 {IQ(L+4*k+i)=Pxyz(i);}
        }
        elseif ipdg=999995 {

             fill /evnt/pass/mpar(1) ! Hijing and General
                Version = 2          ! version
                SUBPR   = Moth(2)    ! subprocess id
             endfill                  

	     Fill /EVNT/PASS/PYTH    ! Pythia and Herwig
        	Version=1            ! version
	        SUBPR = Moth(2)      ! Herwig subprocess ID
*
	        MANDS = Pxyz(1)      ! Mandelstam s of the hard subprocess
	        MANDT = Pxyz(2)      ! Mandelstam t of the hard subprocess
	        MANDU = Pxyz(3)      ! Mandelstam u of the hard subprocess
*
	        HARDP = Ener         ! pT of the hard  subprocess in partonic CM frame
	        COSTH = Vxyz(3)      ! COS theta of hard subprocess in partonic CM frame
	        BJOR1 = Vxyz(1)      ! Bjorken x of 1st parton in after initial radiation
	        BJOR2 = Vxyz(2)      ! Bjorken x of 2nd particle after inital radiation

* The version prior to Thanksgiving 2005:
*	        SUBPR = Moth(2)      ! Herwig subprocess ID
*	        MANDS = Vxyz(1)      ! Mandelstam s of the hard subprocess
*	        MANDT = Vxyz(2)      ! Mandelstam t of the hard subprocess
*	        MANDU = 0            ! Mandelstam u of the hard subprocess
*	        HARDP = Vxyz(3)      ! pT of the hard  subprocess in partonic CM frame
*	        COSTH = Pxyz(3)      ! COS theta of hard subprocess in partonic CM frame
*	        BJOR1 = Pxyz(1)      ! Bjorken x of 1st parton in after initial radiation
*	        BJOR2 = Pxyz(2)      ! Bjorken x of 2nd particle after inital radiation

	     endfill
	print *, Pxyz(1),Pxyz(2), Pxyz(3), Ener, Vxyz(1), Vxyz(2), Vxyz(3)
        }
        elseif ipdg.le.999994 {
             print *,' Yet unknown HEP header ', ipdg
        }


        Call Ucopy  (IdEvHep,Moth,4)
        Call Ucopy  (Hpar,   Pxyz,4)
        Call Ucopy  (Comp,   Vxyz,4)
        Ip=0; Nin=0; mass=IdEvHep(5)
      }

*******************************************************************
*  k=1; ! obsolete
*             Call REBANK('/EVNT/PASS/MPAR',num,4*(k+1),L,ia)
*             if L<=0 {print *,' READ_NT error: cant find MPAR bank'; goto :err:;}
*  this is actually the second element in the structure:
*             IQ(L+4)=Pxyz(2);
*        elseif ipdg<=999994 {
*	     Fill /EVNT/PASS/PYTH  ! comment
*        	Version=1        ! version
*	        SUBPR = Pxyz(2)  ! Pythia subprocess ID
*	        MANDS = 0        ! Mandelstam s of the hard subprocess
*	        MANDT = 0        ! Mandelstam t of the hard subprocess
*	        MANDU = 0        ! Mandelstam u of the hard subprocess
*	        HARDP = 0        ! pT of the hard  subprocess in partonic CM frame
*	        COSTH = 0        ! COS theta of hard subprocess in partonic CM frame
*	        BJOR1 = 0        ! Bjorken x of 1st parton in after initial radiation
*	        BJOR2 = 0        ! Bjorken x of 2nd particle after inital radiation
*	     endfill
*        }
*       catch:
*******************************************************************

      if (InEvent<NnEvent) Next;  
      Nin+=1; num(3)=Nin;  
      Call RbSTORE ('/EVNT/GENE/GENT*',num,Cform,15,istat)
      * print *,' RBSTORE nin,istat=',nin,istat
    
      Check Istat==1

      Call apdg2gea (Ipdg, ge_pid); If ge_pid<=0    
      {  If (idebug>0) <W> Ipdg; (' gstar_read HEPTUP unknown particle',i6)
         ge_pid = 1000000+Ipdg
      }

      Call Vscale   (Vxyz,0.1,Vxyz,3);  Vtime=Vtime/ct
      Call AgSVERT  (Vxyz,0, -Igate,Ubuf,     0,nv)  
      call AgSKINE  (Pxyz,ge_pid,nv,num(3)+0.,0,nt)
      if (ip<0 | Ip==NpHEP) break
   }
   goto :ok:
*
:err: Igate=-1
:ok:  Call RZCDIR(CWD,' ')
   end

*************************************************************************
   Subroutine    gstar_ReadTXT(Igate)
*                                                                       *
* Description: read both NEW and OLD gstar event generator text formats *
Replace [READ[DIGIT](#)#;] with _
        [ READ(#2,ERR=:E:,END=:E:) #3
          IF (Idebug>=#1) <W> #3
        ] 
*************************************************************************
   implicit      none
+CDE,GCUNIT,GCFLAG.
*  character*8   tit, eg_name, frame 
   Character*120 line
   Integer       LENOCC,Index,li,Ieven,Ntrac,Nvert,itr,ivt,nv,nt,Igate,
                 LabelTr,LabelVx,ge_pid,StartVx,StopVx,i,k,eg_proc
   Real          version,east_z,east_a,west_z,west_a,sqrts,b_max,phi,
                 PP(3),vert(4),UBUF(10)/10*0/
   Integer       istat,eg_pid,moth,daut,num(5)
   Data          num/1,1,0,0,0/
   Character     Cform*8 /'/6I 9F'/
   Real          phep,vhep
   Common/GHEPEVT/ istat,eg_pid,moth(2),daut(2),phep(5),vhep(4)
*
*
   Li=21-Igate
 { Ntrac,Nvert } = 999;
 { itr, ivt    } =  0;
*
 WHILE itr<Ntrac | ivt<Nvert
 { 
   line='end-of-file';  read5 (li,'(a)') Line; (1x,a);  Check Line(1:1)!='*'
   If Line(1:5)=='GENER'
   {  
     read1 (line(11:),*) version,east_z,east_a,west_z,west_a,sqrts,b_max
                         (' gstar_ReadNew: GENER :',F8.3,4F8.0,F8.1,F8.3)
   }
   else If Line(1:5)=='EVENT'
   { 
     read2 (line(7:),*)  Ieven,Ntrac,Nvert
                         (' gstar_ReadNew: EVENT :',3i8,2f8.3)
     :a: if (Ieven<=-999) goto :e: " end of data "
   }
   else If Line(1:5)='TRACK'
   { 
     read5 (line(7:),*)  ge_pid,PP,LabelTr,StartVx,StopVx,eg_pid  
                         (16x,'TRACK :',i6,3F9.3,4i6)
     Call VZERO(Vert,4); Call AgSVERT(vert,-StartVx,-Igate,Ubuf,0,nv)  
     Itr += 1;           call AgSKINE(PP,ge_pid,nv,Ubuf,0,nt)
   }
   else If Line(1:6)=='VERTEX'
   { 
     read5 (line(8:),*)  Vert,LabelVx,eg_proc,moth(1)
                         (16x,'VERTEX:',4F10.6,3i6)
     Ivt += 1;           call AgSVERT(vert,-LabelVx,-Igate,Ubuf,0,nv) 
   }
   else If Line(1:6)=='HEPEVT' & itr+ivt==0  
   { *             HEPEVT text format
     read1 (line(8:),*) Ntrac,Ieven; (' gstar_Read HEPEVT:',i8,' event#',i6)
     do Itr=1,Ntrac
     {  :r: read5 (li,'(a)') Line; (1x,a);  If (Line(1:1)=='*') go to :r:
        read6 (line, * ) istat,eg_pid,moth,daut,phep,vhep; (6i5,5F8.2,4F9.3)
        num(3)=0;    If (itr==1) num(3)=1
        Call RbSTORE ('/EVNT/GENE/GENT*',num,Cform,15,istat)
        check Istat==1;       Call apdg2gea (eg_pid, ge_pid)
	if ge_pid<=0 
        {  if (Idebug>1) <W> eg_pid;(' gstar_read HEPEVT unknown particle',i6);
           ge_pid = 1000000+eg_pid
        }
        Call AgSVERT ( vhep,  0,  -Igate,  0,  0, nv)
        Call AgSKINE ( phep, ge_pid,  nv, itr, 0, nt)
     }  Break
   }
   else If Index(Line,'event')>0 & itr+ivt==0     
   { *              OLD text format
     i=Index(Line,'event');  line(i:i+5)='  ';
     read1 (line,*) Ntrac,Ieven; (' gstar_ReadOld: ',i8,' event# ',i6)
     call VZERO(vert,4); call AgSVERT(vert,-1,-Igate,Ubuf,0,nv)
     do itr=1,Ntrac
     {  :q: read5 (li,'(a)') Line; (1x,a);  If (Line(1:1)=='*') go to :q:
        read6     (line, * ) ge_pid,PP;     (16x,i6,3F8.3)
        Call AgSKINE(PP,ge_pid,nv,Ubuf,0,nt)
     }  Break
   }
   else If Index(Line,'EVENT NO.')>0 & itr+ivt==0     
   {
     i=index(line,'EVENT NO.'); 
     read1 (line(i+10:),*) Ieven,Ntrac;  (' iev,np=',2i6)
     i=index(line,'B =');  
     If i>0 { read1(line(i+3:),*) B_max; (' b=',f12.3)    }
     i=index(line,'PHI ='); 
     if i>0 { read1(line(i+5:),*) PHI,K; (' phi,k=',f10.3,i8) }
     call VZERO(Vert,4); call AgSVERT(Vert,-1,-Igate,Ubuf,0,nv)

     do itr=1,Ntrac
     {  :p: read5 (li,'(a)') Line; (1x,a);  If (Line(1:1)=='*') go to :p:
            read2 (line, * ) ge_pid,moth,daut,Phep; (i8,4i6,5f10.5) 
        call aGEA2PDG(ge_pid,eg_pid)
        if (eg_pid==0) { print *,' ? ge_pid =',ge_pid; next; }
        Call AgSKINE(PP,ge_pid,nv,Ubuf,0,nt)
     }  Break
   }
   else If LENOCC(Line)>0
   { <w> line(1:LENOCC(Line)); (' unknown line : ',a); }
 } return
*
:e:<w> line; (' gstar_readTXT error in line '/1x,a);  Igate=-1 
   end

*************************************************************************
   subroutine gstar_ReadEGZ (Igate)
*                                                                       *
* Description: copy event generator data from ZEBRA banks (CODE/EVEN)   *
*              Banks are read in a standard way on LKRUNT/LKEVNT links  *
*************************************************************************
   Implicit   None
   Character  Cname*4,Gname*8,Gnamo*8/' '/
   Real       evtver,zproj,aproj,ztarg,atarg,sqrts,bmin,bmax,bimevt,
              plab(4),UBUF(10)/10*0/,VERT(4)/4*0/
   Integer    Iprin,L,i,nptls,nptarg,nntarg,npproj,nnproj,ntry,IdPtl,
              IRC,Ipart,Nvtx,Nt,Igate,Part_Size/6/
*
+CDE,GCBANK,GCUNIT,SCLINK,GCFLAG.
*
      Iprin=Idebug
      prin1; (' gstar_EGZ called to decode RUNT/EVNT banks ')
*
*     generator run header
      check   LKRUNT>0;    call UHTOC(IQ(LKRUNT-4),4,CNAME,4)
      check  "bank length" IQ(LKRUNT-1)>=10 & "name" CNAME=='CODE'
*
      call UHTOC(IQ(LKRUNT+1),4,Gname,8)
      evtver  = Q(LKRUNT+3)
      zproj   = Q(LKRUNT+4)
      aproj   = Q(LKRUNT+5)
      ztarg   = Q(LKRUNT+6)
      atarg   = Q(LKRUNT+7)
      sqrts   = Q(LKRUNT+8)
      bmin    = Q(LKRUNT+9)
      bmax    = Q(LKRUNT+10)
*
      If Gname!=Gnamo
      { Prin1 Gname,evtver,zproj,aproj,ztarg,atarg,sqrts,bmin,bmax
        (' Gstar_EGZ: generator ',a8,' version ',f8.3/10x,
        ' beam z/a',2f5.0,' target a/z',2f5.0,' sqrt(s)=',f6.1,' b=',2f8.3)
          Gnamo=Gname
      }
*     Normal event follows
      Check   LKEVNT>0;    Call UHTOC(IQ(LKEVNT-4),4,CNAME,4)
      check  "bank length" IQ(LKEVNT-1)>=7 & "name" CNAME=='EVEN'
*
*     Do leading part of bank
      nptls   = IQ(LKEVNT+1)
      nptarg  = IQ(LKEVNT+2)
      nntarg  = IQ(LKEVNT+3)
      npproj  = IQ(LKEVNT+4)
      nnproj  = IQ(LKEVNT+5)
      ntry    = IQ(LKEVNT+6)
      bimevt  =  q(LKEVNT+7)

      prin3 nptls,npproj,nnproj,nptarg,nntarg,ntry,bimevt
      (' Gstar_EGZ: setting GEANT vertex with Npart=',i6,' (before filter)'/,
        10x,' beam p/n=',2i5,'  targ p/n=',2i5,'  ntry=',i6,'  b=',f8.3)
      Call aGSVERT(VERT,-1,-Igate,Ubuf,0,Nvtx)
*
*     Do trailing part of bank:  ID, 4-momentum, daughter/parent information
      do i=1,nptls
         L=LKEVNT+7+(i-1)*part_size
         Ipart    =  0
         Idptl    = IQ(L+1)
         Call UCOPY (Q(L+2),plab,4)
         If (Gname='VENUS'  | Gname='HIJET')    Call ISA_to_GEANT (IdPtl,Ipart)
         If (Gname='HIJING')                    Call a PDG 2 GEA  (IdPtl,Ipart)
         If (Cname='FRITIOF'| Gname='FRTIOF17') Call LUND_to_GEANT(IdPtl,Ipart)
         if (Ipart<=0) then
           prin1 Idptl,Gname
           (' Gstar_EGZ: unknown particle ',i6,' from ',a)
           next
         endif
         Call AgGZUFLT('EGZ',1,I,Idptl,Vert,Plab,IRC);  Check IRC==0
         CALL AgSKINE (Plab,Ipart,Nvtx,Ubuf,0,Nt)
      enddo
*
   END

*****************************************************************************
 module  HEPEVNT  is the HEPEVNT event header
 author  P.Nevski
 created 01/01/98
*****************************************************************************
+CDE,AGECOM,GCBANK,SCLINK,RBBANK.

 Old Structure PASS {int SYS1, int SYS2, int SYS3, int PJID,int GJID,int EVID}

 Old Structure GENE {int SYS1, int SYS2, int SYS3, int GRUN,int GEVT,
                     Char GNAM, VRTX, VRTY, VRTZ, VRTT, int WTFL, WEIG }

 Old Structure GENT {int IstHEP, int IdHEP, int JmoHEP(2), int JdaHEP(2),
                     PHEP(3), ENER, MASS, VHEP(3), TIME }
 structure     MPAR { int Version, int SUBPR, int Nbin, int NWE, int NWW, int Njets, int NREJ }

* careful here, a direct import from apythia's pyth_header:
 structure PYTH {int Version, int SUBPR,
                 real MANDS,  real MANDT,  real MANDU,
                 real HARDP,  real COSTH,
                 real BJOR1,  real BJOR2}



     If (LkEvnt==0) Call MZBOOK(IxDIV,LKAR P2,LkEvnt, 1,'EVNT',2,2,7,2,0)

     IrbDIV=IxDIV;         LKARP2=LkEvnt
*
     Fill /EVNT/PASS(1)  ! Pass Record Bank
        SYS1 = 1         !  Format flag
        SYS2 = 1         !  Member system word = 100000*NG+NM NGEN 
        SYS3 = 300003    !  Modularity system word = 100000*NW+NT
        PJID = 1         !  Pass Job ID (GJID for latest PASS bank)
        GJID = 1         !  Generator Job ID.
        EVID = 1         !  ZEBRA IDN of event read in or generated
     endfill
***
* ver: 1: just subprocess, 2: nbin and wounded, 3: rejected events
     Fill /EVNT/PASS/MPAR ! event generator parameters
        Version = 3      ! bank format version
        SUBPR   = 0      ! pythia subprocess
        Nbin    = 0      ! number of binary collisons
        NWE     = 0      ! number of wounded nucleons in the east nucleus
        NWW     = 0      ! number of wounded nucleons in the west nucleus
        NJETS   = 0      ! number of jets
        NREJ    = 0      ! number of rejected events, prior to the current one
	write(*,*) 'gstar_input: initializing the MPAR structure (event header such as Pythia process id etc)'
     endfill
***
     IrbDIV=IxDIV;         LKARP2=LkEvnt
     Fill /EVNT/GENE(1)  ! GENZ Event Bank  
       SYS1 =     1      !  Format flag = 1
       SYS2 =     0      !  Member system word = 100000*NG+NM 
       SYS3 =     0      !  Modularity system word = 100000*NW+NT
       GRUN =     0      !  Generator run number
       GEVT =     1      !  Generator event number
       GNAM = 'VENUS'    !  Generator name
       VRTX =   0.0      !  Interaction vertex position in metres
       VRTY =   0.0      !  idem
       VRTZ =   0.0      !  idem
       VRTT =   0.0      !  Interaction vertex time in seconds
       WTFL =     1      !  Interaction weight flag
       WEIG =  1.00      !  Interaction weight  
     endfill
*** 
     IrbDIV=IxDIV;             LKARP2=LkEvnt
     Fill /EVNT/GENE/GENT(1) ! HEPEVT parton level data 
       IstHEP  =   0     !  Status flag
       IdHEP   =   0     !  PDG particle code
       JmoHEP  = {0,0}   !  First and last mother (-ve gives a range)
       JdaHEP  = {0,0}   !  First and last daughter (<0 reference to GEANT)
       PHEP    = {0,0,0} !  particle momentum in GeV/c
       ENER    =  0.0    !  particle Total Energy 
       MASS    =  0.0    !  Mass in GeV/c. Not necessarily the on-shell m
       VHEP    = {0,0,0} !  particle origin in x (mm)
       TIME    =  0.0    !  Start time of particle (mm/c)
     endfill

 end


*************************************************************************
   module  GHeader defines GSTAR event header
   author  Pavel Nevski
   created sometime ago
*************************************************************************
+CDE,GCBANK,GCUNIT,GCTIME,SCLINK.
*
  old STRUCTURE EGRN {               " Event generator run structure  "_
        INT   generator              " event generator identification ",
        CHAR  eg_name(8)             " event generator name           ",
        REAL  eg_versn               " version of event generator     ",
        INT   eg_run                 " generator run number           ",
        INT   eg_rndm(2)             " generator random numbers       ",
        REAL  sqrts                  " center of mass energy          ",
        INT   is_minbias             " minimum bias flag              ",
        REAL  b_min                  " minimum impact parameter       ",
        REAL  b_max                  " maximum impact parameter       ",
        INT   east_a                 " projectile 1 mass number       ",
        INT   east_z                 " projectile 1 charge            ",
        INT   west_a                 " projectile 2 mass number       ",
        INT   west_z                 " projectile 2 charge            ",
        INT   polariza(10)           " to be defined                  "_
      }
*
  Old STRUCTURE EGEV {               " Event generator event structure"_
        INT   n_event                " eg event number                ",
        REAL  b_impact               " actual impact parameter        ",
        REAL  phi_impa               " reaction plane                 ",
        INT   ev_type                " trigger, minbias bkgd, cosmic  ",
        INT   polariza(10)           " to be defined                  ",
        INT   proteast               " number of participant protons  ",
        INT   neuteast               " number of participant neutrons ",
        INT   protwest               " number of participant protons  ",
        INT   neutwest               " number of participant neutrons ",
        INT   n_track                " # tracks                       ",
        INT   n_vertex               " # vertices                     ",
        INT   fstrack                " # final state tracks           ",
        INT   nfstrack               " # non-final state tracks       ",
        INT   prvertex               " # primary vertices             ",
        INT   fsvertex               " # non-final state vertices     ",
        INT   p_prvert               " pointer to ll of primary vertices",
        INT   p_fsvert               " pointer to ll of final state vert"_
      }
*
  Old structure uevn {                      _
        char author(20), char machine(20), 
        int date, int time, version,
        int ge_run,
        int rndmrun(2),
        int ordered,
        int rndmevt(2),
        int n_event,
        int cav(12),
        int shtk_evt,
        int mx_shtk,
        int nw_shtk,
        int firstvrt,
        int firsttrk,  
        int equals  _
    }
*
   integer iprin,Iadr
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*

   fill /EVNT/UEVN(1) ! user event buffer
      author     = { 'mea','mea' }  ! author
      machine    = { 'ibm','bla' }  ! machine
      date       = igdate           ! date
      time       = igtime           ! time
      version    = gversn           ! geant version
      equals     = 99999            ! end label
   endfill
*
   IF (LKRUNT>0) then
    FILL /EVNT/UEVN/EGRN(1)          !  Event generator run structure 
      generator  = 0                ! event generator identification
      eg_name    = { 'a','b','c' }  ! event generator name
      eg_versn   = Q(LKRUNT+3)      ! version of event generator
      eg_run     = 0                ! generator run number
      eg_rndm    = { 0,0 }          ! generator random numbers
      sqrts      = Q(LKRUNT+8)      ! center of mass energy
      is_minbias = 0                ! minimum bias flag
      b_min      = Q(LKRUNT+9)      ! minimum impact parameter
      b_max      = Q(LKRUNT+10)     ! maximum impact parameter
      east_a     = Q(LKRUNT+5)      ! projectile 1 mass number
      east_z     = Q(LKRUNT+4)      ! projectile 1 charge
      west_a     = Q(LKRUNT+7)      ! projectile 2 mass number
      west_z     = Q(LKRUNT+6)      ! projectile 2 charge
      polariza   = { 0,0,0,0,0 }    ! to be defined
    endfill
   endif
*
   If (LKEVNT>0) then
    FILL /EVNT/UEVN/EGEV(1)     !  Event generator event structure 
      n_event    = 0                ! eg event number
      b_impact   = Q (LKEVNT+7)     ! actual impact parameter
      phi_impa   = 0                ! reaction plane
      ev_type    = 0                ! trigger, minbias bkgd, cosmic, etc.
      polariza   = { 0,0,0,0,0 }    ! to be defined
      proteast   = IQ(LKEVNT+4)     ! number of participant protons
      neuteast   = IQ(LKEVNT+5)     ! number of participant neutrons
      protwest   = IQ(LKEVNT+2)     ! number of participant protons
      neutwest   = IQ(LKEVNT+3)     ! number of participant neutrons
      n_track    = IQ(LKEVNT+1)     ! number of tracks
      n_vertex   = 0                ! number of vertices
      fstrack    = 0                ! number of final state tracks
      nfstrack   = 0                ! number of non-final state tracks
      prvertex   = 0                ! number of primary vertices
      fsvertex   = 0                ! number of non-final state vertices
      p_prvert   = 0                ! pointer to ll of primary vertices
      p_fsvert   = 0                ! pointer to ll of final state vert.
    endfill
   endif
*
   Iadr = 0
   call GsHEAD (len_egrn-2, Bank_egrn(3), iadr)
   call GsHEAD (len_egev-2, Bank_egev(3), iadr)
   call GsHEAD (len_uevn-2, Bank_uevn(3), iadr)
   prin1 iadr; (' Header: geant header set with length =',i6)
*
   end


*************************************************************************
   subroutine   A p F O P E N (li,file,ier)
*
* Description: open a standard fortran formattted data file.            *
*************************************************************************
   Implicit   None
   Integer    LENOCC,Li,L,ier
   Character  file*(*)
+CDE,AgCKINE,GCKINE,GCFLAG,GCUNIT.
*
    ier=0;  close (Li,err=:o:)
:o: L=LENOCC(file);  Check L>0
*
    open (Li,file=file,form='FORMATTED',status='OLD',err=:e:)
    If (Idebug>=2) <w> file(1:L),Li;
       (' ApFOPEN: file ',a,' opened succesfully on unit',i3)
    IKineOld=IKine
    return
:e: <w> file(1:L); (' ApFOPEN error openning file ',a); ier=1
   end


