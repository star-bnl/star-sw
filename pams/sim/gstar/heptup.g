*******************************************************************************
 subroutine heptup
    call hephelp
 end

*******************************************************************************
 subroutine HEPexample
    integer mm(2)/0,0/,dd(2)/0,0/,iw(2)/90,91/,pipe,p/0/
    real    pp(3),vv(3)
    np=12000
    call HEPdense
    do j =1,3
       call hepevent ('hijing',12,np,  3.,1.5,100.,0.1, 197.,97.,197.,97.)
       do i=1,np
          pp={1,2,3*rndm()}
          vv={0,0,.1*rndm()}
          call heppart (1,2,101,mm,dd,pp,10.,1.,vv,0.)
       enddo
    enddo
    call hepend('z')
 end

*******************************************************************************

 subroutine   HEPHelp
print*,'**********************************************************************'
print*,' a utility set to write a standard HEPEVNT n-tuple 999 in evgen.run.nt'
print*,'**********************************************************************'
print*,'         mandatory Calles:'
print*,'HEPEvent (generator, run, Npart, B,F,Et,At, A1,Z1,A2,Z2) - new event'
print*,'HEPPart (ipa,ist,pdg, moth,idau,pp, Ep,Am,vv,vt) - write new particle'
print*,'HEPEnd (option) - close ntuple and compress it on "z" option'
print*,'         optional Calls:'
print*,'HEPnormal - default packing'
print*,'HEPdens  - dense packing: no mother-daughter relations, no vertex info'
print*,'HEPfat  - fat packing: precise vertex info'
print*,'HEPmax (Pdg, Ref, NPart, Vxyzt, Nbit) - for experts'
print*,'**********************************************************************'
 end

*******************************************************************************

 Subroutine HEPEvent (generator, run, Npart, B,F,Et,At, A1,Z1,A2,Z2)
 Implicit          none
 Integer           MaxIP,MaxRf,MaxPa,MaxVx,MaxNv,K
 Integer           Id/999/,Iver/11/,Icycle/0/,
                   IpMx/1000000/,MxRf/1/,MxPa/32000/,MxVx/200/,Nv/16/

* Input parameters:
 Character         generator*(*)
 Integer           run,Npart,ipa,ist,pdg,moth(2),idau(2)
 Real              B,F,Et,At,A1,Z1,A2,Z2,pp(3),Ep,Am,vv(3),vt

* Cernlib related:
 Integer           NwPAW,iPaw,LENOCC,SystemF
 Parameter         (NwPaw=100000)
 Common /pawc/     iPaw(Nwpaw)

* Hepevnt related:
 Integer           IdRun,NTrack,Itype,Is,L,Mref
 Character         cR*8, code*1, gener*20, file*20
 Integer           Np,Ievt,Idat,Itim,gen
 Common /hep_head/ Np,Ievt,Idat,Itim,gen,gener,file
 Integer           Ip,Istat,Ipdg,Mot1,Mot2,Ida1,Ida2
 Real              Pxyz,Ener,mass,Vxyz,Vtime
 Common /hep_part/ Ip,Istat,Ipdg,Mot1,Mot2,Ida1,Ida2,Pxyz(3),Ener,mass,
                                                     Vxyz(3),Vtime
* Local:
 parameter         (k=6)
 Integer           i,CC(k)
 character*20      GG(k),FF(k)
 data              (GG(i),FF(i),CC(i),i=1,k) / _
                   'venus'  , 'optns.dat'  , 6,
                   'hijing' , 'hijev.inp'  , 5,
                   'mevsim' , 'mult_gen.in', 4,
                   'rqmd'   , 'rqmd.inp'   , 3,
                   'pythia' , 'pythia.data', 1,
                   'user'   , 'user.input' , 0/
 Logical           First/.true./
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  If (First) then
    First=.false.
    Mref = MxRf*Mxpa
    Call VZERO(Ip,16)
    gener=generator
    call CUTOL(gener)

*   Is HBOOK and memory initialised ?
    If Ipaw(1)==0 { print *,' HBOOK initialised for HEP'; Call HLIMIT(NwPaw) }

    Call HEPNUMBER  (Run,cR)
    file='evgen.'//%L(cR)//'.nt'
    Call HROpen   (99,'HEPEVNT',file,'N',1024,is)
    call RZCDIR   ('//HEPEVNT', ' ')
    Call HBSet    ('BSIZE',4096, is)
    Call HBNT     (id,'HEPEVNT',' ')
    Call HepBNAME (id,Ip,   'itrac'        , 0, -1,   MxPa)
    Call HepBNAME (id,Istat,'istat'        , 0, -1,   Iver)
    Call HepBNAME (id,Ipdg, 'ipdg'         , 0, -IpMx,IpMx)
    Call HepBNAME (id,Mot1, 'moth1'        , 0, -1,   Mref)
    Call HepBNAME (id,Mot2, 'moth2'        , 0, -Mref,   1)
    Call HepBNAME (id,Ida1, 'idau1'        , 0, -1,   Mref)
    Call HepBNAME (id,Ida2, 'idau2'        , 0, -1,   Mref)
    Call HepBNAME (id,Pxyz, 'Pxyz(3)'      , 0,  0,      0)
    Call HepBNAME (id,Ener, 'ener'         , 0,  0,      0)
    Call HepBNAME (id,Mass, 'mass:R:'      ,16, -1,     10)
    Call HepBNAME (id,Vxyz, 'Vxyz(3):R:'   ,nv,  0,   MxVx)     " mm "
    Call HepBNAME (id,Vtime,'Vtime:R:'     ,nv,  0,   Mxvx)     "mm/c"
*   1 mm/c=0.33 ns;   ct=3.e11: tmax=5000 -> 17 ns
   
    Do gen=1,K-1 
    {L=min(lenocc(gener),Lenocc(GG(gen))); If(gener(1:L)==GG(gen)(1:L)) break;}
    call HEPinput(FF(gen))
  endif
*
  Ievt+=1;  Ip=NPart;  Istat=Iver;  Ipdg=IpMx;   Call DATIME (Idat,Itim);
  Ipdg-=1;  Pxyz={Run,Ievt,Idat,Itim,CC(gen)};   Call HFNT(Id)
  Ipdg-=1;  Pxyz={  B,    F,   Et,   At,   1};   Call HFNT(Id)
  Ipdg-=1;  Pxyz={ A1,   Z1,   A2,   Z2,   2};   Call HFNT(Id) 
  Np=Npart;
  return

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  entry   HEPPart (ipa,ist,pdg,moth,idau,pp,Ep,Am,vv,vt)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  call RZCDIR('//HEPEVNT',' ')
  Ip    = Ipa
  Istat = Ist
  Ipdg  = pdg
  Mot1  = moth(1)
  Mot2  = moth(2)
  Ida1  = idau(1)
  Ida2  = idau(2)
  Call Ucopy(pp,Pxyz,3)
  Call Ucopy(vv,Vxyz,3)
  Vtime = vt
  Mass  = Am
  Ener  = Ep
  if (ipa==Np) Ip=-Ipa
  Call HFNT(Id)
  return

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  entry  HEPend(code)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Call HROUT(0,ICYCLE,'NT'); 
  Call HREND('//HEPEVNT')
  if (Code=='z' | Code=='Z') i=systemF('gzip -f '//%L(file))
  return

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  entry  HEPnormal;  MxRf=1;  Nv=16;    return
  entry  HEPdense;   MxRf=0;  Nv= 1;    return
  entry  HEPfat  ;   MxRf=1;  Nv=31;    return
  entry  HEPmax (MaxIP, MaxRf, MaxPa, MaxVx, MaxNv)
         IpMx=MaxIp; Mref=MaxRf; MxPa=MaxPa; MxVx=MaxVx; Nv=MaxNv;
         Return
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  End

*************************************************************************

  subroutine   HEPinput (input)
  character    input*(*),line*128
  integer      LENOCC,li/98/,id/998/
     close        (li)
     Call  HBNT   (id,'HEPinput',' ')
     Call  HBNAMC (id,'HEPinput',line, 'line(4):C*32:')
     open         (li,file=%L(input),status='OLD',err=:er:)
     loop  { read (li,'(a)',err=:er:,end=:er:) line; call HFNT(id) }
     :er:  close  (li)
  end
*************************************************************************

  subroutine   HepBNAME(Id,var,form,nb,ia,ib)
  implicit     none
  integer      LENOCC,INDEX,nb,Id,Ia,Ib,var,L
  character    c*8,CC*80,form*(*)  
  CC=form;     L=index(form,':'); If (L>0) CC=form(1:L-1)
  if Ia!=0 | Ib!=0
  {  CC = form
     call HEPNUMBER(Nb,c)
     if (nb>0)              CC=%L(CC)//%L(c)
     if (Index(CC,':')>0)   CC=%L(CC)//':'
     call HEPNUMBER(Ia,c);  CC=%L(CC)//'['//%L(c)
     call HEPNUMBER(Ib,c);  CC=%L(CC)//','//%L(c)//']'
  }
  Call       HBNAME(Id,'particle',var,%L(CC))
  end

*************************************************************************

  subroutine   HEPNUMBER(num,cnum)
  implicit     none
  character    cnum*(*),S*8
  integer      num,L,i,i1,i2
     write (S,*) NUM;  i1=8;  i2=1; 
     do i=1,8 { check S(i:i)!=' '; i1=min(i1,i); i2=max(i2,i); }
     cnum=S(i1:i2);    L=i2-i1+1
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

* cause  Assertion failed: n == frame_size[iframe], 
* file /cern/pro/src/pawlib/paw/ntuple/qp_exec_evt.c, line 979
*   Call HepBNAME (id,Mot1, 'moth(2)'      , 0, -Mref,Mref)
*   Call HepBNAME (id,Ida1, 'idau(2)'      , 0, -1,   Mref)
     i=systemF(' rm -f evgen12.nt.gz ')
     i=systemF(' tail -f evgen.12.nt | gzip -c > evgen12.nt.gz &') 
     close (99)
     i=systemF ('sleep 2')
     i=sYstemF ('killall -4 tail ')

 end


