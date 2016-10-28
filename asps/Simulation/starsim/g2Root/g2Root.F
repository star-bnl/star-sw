*CMZ :  2.22/00 08/04/99  18.41.30  by  Rene Brun
*CMZ :  2.21/05 08/02/99  12.19.15  by  Rene Brun
*CMZ :  2.00/11 22/08/98  15.25.26  by  Rene Brun
*CMZ :  2.00/10 28/07/98  13.32.42  by  Rene Brun
*-- Author :
#if 0
      Program g2root
*
**********************************************************************
*
*      Program to convert an existing GEANT geometry/RZ file
*      into a ROOT macro (C++ file).
*
*  Author: Rene Brun
*  modified by Nikolay I. Root <nroot@inp.nsk.su> to support map_names
*  modified by Mihaela Gheata for the new geometry format
*
*  To use this conversion program (in $ROOTSYS/bin),
*        g2root [-f <map_names>] <geant_rzfile> <macro_name> [lrecl]
*  run g2root without parameters to see the usage info.
*
*  for example
*        g2root brahms.geom brahms.C
*  will convert the GEANT RZ file brahms.geom into a ROOT macro brahms.C
*
*  The default value for lrecl is 1024. The parameter lrecl must be specified
*  if the record length of the Zebra file is greater than 1024.
*
*
*  To generate the Geometry structure within Root, do:
*
*         root[0]> .x brahms.C;
*         root[0]> new TBrowser();
*
*  An interactive session
* ------------------------
*
*   Provided that a geometry was successfully built and closed , the manager class will 
*  register itself to ROOT and the logical/physical structures will become immediately 
*  browsable. The ROOT browser will display starting from the geometry folder : the list of 
*  transformations and materials, the top volume and the top logical node. These last 
*  two can be fully expanded, any intermediate volume/node in the browser being subject 
*  of direct access context menu operations (right mouse button click). All user
*  utilities of classes TGeoManager, TGeoVolume and TGeoNode can be called via the
*  context menu.
*
* see http://root.cern.ch/root/htmldoc/gif/t_browser.jpg
* 
*   --- Drawing the geometry
* 
*    Any logical volume can be drawn via TGeoVolume::Draw() member function.
*  This can be direcly accessed from the context menu of the volume object
*  directly from the browser. 
*    There are several drawing options that can be set with
*  TGeoManager::SetVisOption(Int_t opt) method :
*  opt=0 - only the content of the volume is drawn, N levels down (default N=3).
*     This is the default behavior. The number of levels to be drawn can be changed
*     via TGeoManager::SetVisLevel(Int_t level) method.
* 
* see http://root.cern.ch/root/htmldoc/gif/t_frame0.jpg
* 
*  opt=1 - the final leaves (e.g. daughters with no containment) of the branch
*     starting from volume are drawn down to the current number of levels. 
*                                      WARNING : This mode is memory consuming
*     depending of the size of geometry, so drawing from top level within this mode
*     should be handled with care for expensive geometries. In future there will be 
*     a limitation on the maximum number of nodes to be visualized.
* 
* see http://root.cern.ch/root/htmldoc/gif/t_frame1.jpg
* 
*  opt=2 - only the clicked volume is visualized. This is automatically set by
*     TGeoVolume::DrawOnly() method
*  opt=3 - only a given path is visualized. This is automatically set by
*     TGeoVolume::DrawPath(const char *path) method
* 
*     The current view can be exploded in cartesian, cylindrical or spherical
*  coordinates :
*    TGeoManager::SetExplodedView(Int_t opt). Options may be :
*  - 0  - default (no bombing)
*  - 1  - cartesian coordinates. The bomb factor on each axis can be set with
*         TGeoManager::SetBombX(Double_t bomb) and corresponding Y and Z.
*  - 2  - bomb in cylindrical coordinates. Only the bomb factors on Z and R
*         are considered
* 
* see http://root.cern.ch/root/htmldoc/gif/t_frameexpl.jpg
* 
*  - 3  - bomb in radial spherical coordinate : TGeoManager::SetBombR()
* 
*  Volumes themselves support different visualization settings :
*     - TGeoVolume::SetVisibility() : set volume visibility.
*     - TGeoVolume::VisibleDaughters() : set daughters visibility.
*  All these actions automatically updates the current view if any.
* 
*   --- Checking the geometry
* 
*   Several checking methods are accesible from the volume context menu. They
*  generally apply only to the visible parts of the drawn geometry in order to
*  ease geometry checking, and their implementation is in the TGeoChecker class
*  from the painting package.
* 
*  1. Checking a given point.
*    Can be called from TGeoManager::CheckPoint(Double_t x, Double_t y, Double_t z).
*  This method is drawing the daughters of the volume containing the point one
*  level down, printing the path to the deepest physical node holding this point.
*  It also computes the closest distance to any boundary. The point will be drawn
*  in red.
* 
* see http://root.cern.ch/root/htmldoc/gif/t_checkpoint.jpg
* 
*   2. Shooting random points.
*    Can be called from TGeoVolume::RandomPoints() (context menu function) and 
*  it will draw this volume with current visualization settings. Random points
*  are generated in the bounding box of the top drawn volume. The points are 
*  classified and drawn with the color of their deepest container. Only points
*  in visible nodes will be drawn.
* 
* see http://root.cern.ch/root/htmldoc/gif/t_random1.jpg
* 
* 
*   3. Raytracing.
*    Can be called from TGeoVolume::RandomRays() (context menu of volumes) and
*  will shoot rays from a given point in the local reference frame with random
*  directions. The intersections with displayed nodes will appear as segments
*  having the color of the touched node. Drawn geometry will be then made invisible
*  in order to enhance rays.
* 
* see http://root.cern.ch/root/htmldoc/gif/t_random2.jpg
*
*    IMPORTANT NOTE
*    To be compiled, this program requires a Fortran compiler supporting
*    recursive calls.
*
**********************************************************************
* NOT YET MAPPED FROM OLD g2root
**********************************************************************
* The following lines starting with the 2 characters ** are an example
* of (map_names> file.
* To make a valid <map_names> file, remove these 2 characters.
*
**#
**#      formal definitions :
**#
**#  'comments' - the things that are ignored by the parser.
**#               The lines starting with '#' or ' '  - comments
**#  'map'      - first two words of 'non-comments'
**#               ==> the trailing part of this lines - also 'comments'
**
**# next lines are the 'map' examples :
**
**CMD2   Detector     Names translation map for CMD2 detector
**VALL   Internals    Internal part of detector.
**VTBE   TubeBe
**VTAL   TubeAL
**VTML   DCInner
**DC0_   DCOuter
**DRIF   DCCell
**
**   first part of map ('key') consists of exactly 4 characters,
**   including trailing blanks. This part of map is a key of
**   GEANT3 volumes/detectors.
**   The second part - 'name' - is just a sequence of non-blank chars.
**   'name' is used as a replacement of 'key' in g2root output file.
**
**   'alias'    - is a map with 'key' and 'name'
**                having one 'stab character' - '%'.
**
**DCC%   DCSector%  - example of alias.
**
**    For above example - any 'keys' that have first 3 chars 'DCC'
**    would be matched by this alias.
**    The last char of 'key'  - 'stab' - used as substitution for
**    '%' in 'name' (like gmake rules, but not so complicated).
**
**    Keys are matched against aliases only if they do not
**    match any explicit 'map'. First found alias, that match
**    the key - is used for substitution.
**
**  ==> The order of aliases is important for matching !
**
**  The last alias may be like this :
**
**%      Block%       Match any key !
**
**%      %     <- assumed by default.
*
**********************************************************************

      parameter (nwpaw=4000000)
      common/pawc/paw(nwpaw)

      character *80 gname
      character *80 fname
      character *8 chtop
      character *8 crecl
      integer npar, lrecl

      
      call hlimit(nwpaw)

      npar = iargc()
      if (npar.eq.0) then
         print *, 
     +       'Invoke g2root [-f map_name] geant_name macro_name [lrecl]'
         go to 90
      endif
      narg = 1
      call getarg(narg,gname)
      narg = narg + 1
      npar = npar - 1
      if(gname .eq. "-f") then
         if(npar .eq. 0) then
            print *,'Invoke g2root [-f map_name] geant_name macro_name'
            print *,'  Parse error: you need specify <map_name>'
            go to 90
         endif
         call getarg(narg,gname)
         call create_map(gname)
         narg = narg + 1
         npar = npar - 1
         if(npar .eq. 0) then
            print *,'Invoke g2root [-f map_name] geant_name macro_name'
            print *,'  Parse error: you need specify <geant_name>'
            go to 90
         endif
         call getarg(narg,gname)
         narg = narg + 1
         npar = npar - 1
      endif
      if (npar.ge.1) then
         call getarg(narg,fname)
         narg = narg + 1
      else
         idot=index(gname,'.')
         fname = gname(1:idot-1)//'.C'
      endif

      lrecl = 1024
      if (npar.ge.2) then
         call getarg(narg,crecl)
         read (crecl,'(I6)') lrecl
      endif
      call rzopen(1,chtop,gname,'WP',lrecl,istat)
      if (istat.ne.0) then
         print *,'Cannot open file'
         go to 90
      endif
      call rzfile(1,chtop,' ')
*yf      call rzldir(' ',' ')

      call g2rin
*yf      call mapGen ! map for generic names
      call toroot(fname)
      lg = lenocc(gname)
      lf = lenocc(fname)
      print 1000,gname(1:lg),fname(1:lf)
 1000 format(' GEANT file: ',a, ' converted to ROOT macro: ',a)
  90  continue
      end
#endif

      subroutine realname(cname, pname)
      character *20 cname
      character *20 pname
      nind = 0
      pname = ' '
      n1 = lenocc(cname)
      write(pname,'(a)')cname(1:n1) 
      do i=1,n1
          nind = nind+1
          pname(nind:nind)=cname(i:i)
          if(ichar(cname(i:i)).eq.0) then 
             pname(nind:nind)=' '
             nind = nind+1
             pname(nind:nind)=' '
          endif
          if(ichar(cname(i:i)).eq.92) then 
             pname(nind:nind)=char(92)
             nind = nind+1
             pname(nind:nind)=char(92)
          endif
          if(ichar(cname(i:i)).eq.34) then
             pname(nind:nind)=char(92)
             nind = nind+1
             pname(nind:nind)=char(34)
          endif 
      enddo
*------ supress blanks
2333  if (pname(lenocc(pname):lenocc(pname)).eq.' ') then
         pname = pname(1:lenocc(pname)-1)
         goto 2333
      endif
      end      
      
      subroutine ptname(cname, pname)
      character *20 cname
      character *20 pname
      pname = ' '
      n1 = lenocc(cname)
      write(pname,'(a)')cname(1:n1) 
      do i=1,n1
          if(ichar(cname(i:i)).eq.0) then 
             pname(i:i)='_'
             !pname(5:5)='_'
          endif
          if(ichar(cname(i:i)).eq.92) then 
             pname(i:i)='a'
             !pname(5:5)='a'
          endif
          if(cname(i:i).eq.'?') then 
             pname(i:i)='b'
             !pname(5:5)='b'
          endif
          if(cname(i:i).eq.'`') then
             pname(i:i)='c'
             !pname(5:5)='c'
          endif
          if(cname(i:i).eq.' ') then
             pname(i:i)='_'
          endif
          if(cname(i:i).eq.'+') then
             pname(i:i)='d'
             !pname(5:5)='d'
          endif
          if(cname(i:i).eq.'-') then
             pname(i:i)='e'
             !pname(5:5)='e'
          endif
          if(cname(i:i).eq.'*') then
             pname(i:i)='f'
             !pname(5:5)='f'
          endif
          if(cname(i:i).eq.'/') then
             pname(i:i)='g'
             !pname(5:5)='g'
          endif
          if(cname(i:i).eq.'.') then
             pname(i:i)='h'
             !pname(5:5)='h'
          endif
          if(cname(i:i).eq.'''') then
             pname(i:i)='i'
             !pname(5:5)='i'
          endif
          if(cname(i:i).eq.';') then
             pname(i:i)='j'
             !pname(5:5)='j'
          endif
          if(cname(i:i).eq.':') then
             pname(i:i) = '_'
             !pname(i:i)='k'
             !pname(5:5)='k'
          endif
          if(cname(i:i).eq.',') then
             pname(i:i)='l'
             !pname(5:5)='l'
          endif
          if(cname(i:i).eq.'<') then
             pname(i:i)='m'
             !pname(5:5)='m'
          endif
          if(cname(i:i).eq.'>') then
             pname(i:i)='n'
             !pname(5:5)='n'
          endif
          if(cname(i:i).eq.'!') then
             pname(i:i)='o'
             !pname(5:5)='o'
          endif
          if(cname(i:i).eq.'@') then
             pname(i:i)='p'
             !pname(5:5)='p'
          endif
          if(cname(i:i).eq.'#') then
             pname(i:i)='q'
             !pname(5:5)='q'
          endif
          if(cname(i:i).eq.'$') then
             pname(i:i)='r'
             !pname(5:5)='r'
          endif
          if(cname(i:i).eq.'%') then
             pname(i:i)='s'
             !pname(5:5)='s'
          endif
          if(cname(i:i).eq.'^') then
             pname(i:i)='t'
             !pname(5:5)='t'
          endif
          if(cname(i:i).eq.'&') then
             pname(i:i)='u'
             !pname(5:5)='u'
          endif
          if(cname(i:i).eq.'(') then
             pname(i:i)='v'
             !pname(5:5)='v'
          endif
          if(cname(i:i).eq.')') then
             pname(i:i)='x'
             !pname(5:5)='x'
          endif
          if(cname(i:i).eq.'[') then
             pname(i:i)='y'
             !pname(5:5)='y'
          endif
          if(cname(i:i).eq.']') then
             pname(i:i)='z'
             !pname(5:5)='z'
          endif
          if(cname(i:i).eq.'{') then
             pname(i:i)='c'
             !pname(5:5)='a'
          endif
          if(cname(i:i).eq.'}') then
             pname(i:i)='c'
             !pname(5:5)='b'
          endif
          if(cname(i:i).eq.'=') then
             pname(i:i)='c'
             !pname(5:5)='d'
          endif
          if(cname(i:i).eq.'~') then
             pname(i:i)='c'
             !pname(5:5)='e'
          endif
          if(cname(i:i).eq.'|') then
             pname(i:i)='c'
             !pname(5:5)='f'
          endif
      enddo
      if ((ichar(pname(1:1)).ge.48).and.
     + (ichar(pname(1:1)).le.57)) then
         pname='Z'//pname(1:lenocc(pname))
      endif
      end
      
      subroutine cdnode(node)
      common/clevel/nodeold(20),nlevel
      character*20 anode
      character*20 cblank
      data cblank/' '/
      call toint(node,anode,ncd)
      if (nlevel.gt.1)then
*         write(51,1000)cblank(1:nlevel-1),anode(1:ncd)
1000     format(a,'Node',a,'->cd();')
      else
*         write(51,1001)anode(1:ncd)
1001     format('Node',a,'->cd();')
      endif
      end
      subroutine toint(i,s,nc)
      character*20 s1,s
      s1=' '
      write(s1,'(i7)')i
      do j=1,16
         if(s1(j:j).ne.' ') then
            j1 = j
            go to 10
         endif
      enddo
  10  continue
      do j=16,1,-1
         if (s1(j:j).ne.' ') then
            s=s1(j1:j)
            nc=j-j1+1
            return
         endif
      enddo
      end
      subroutine toreals(n,r,s,nc)
      character*(*) s
      character*14 s1
      dimension r(*)
      s=' '
      nc=0
      do 10 i=1,n
         call toreal(r(i),s1,nch)
         if(nc+nch.gt.128)then
*            print *,'n=',n,' nc=',nc,' nch=',nch
            return
         endif
         s(nc+1:)=','
         s(nc+2:)=s1
         nc=nc+nch+1
  10  continue
      end
      subroutine toreal(r,s,nc)
      character*14 s
      character*14 s1
      s=' '
      if(r.eq.0)then
         s1='0'
         jbeg=1
         jend=1
      else
         write(s1,'(g14.7)')r
         do k=1,14
            if(s1(k:k).ne.' ') then
               jbeg=k
               goto 10
            endif
         enddo
         jbeg=1
 10      continue
         do k=14,jbeg,-1
            if(s1(k:k).ne.' '.and.s1(k:k).ne.'0') then
               jend=k
               goto 20
            endif
         enddo
         jend=14
 20      continue
         if(s1(jend:jend).eq.'.') jend=jend-1
      endif
      nc=jend-jbeg+1
      if(nc.le.0) then
         print *, 'Should never happen'
      endif
      s(1:nc)=s1(jbeg:jend)
      read(s(1:nc),*)t
      if(abs(t-r).gt.5e-7*abs(t+r)) print *, s(1:nc), t, r
      end
      subroutine toreal_old(r,s,nc)
      character*20 s1,s
      if(r.eq.0)then
         s='0'
         nc=1
         return
      endif
      s1=' '
      write(s1,'(f14.7)')r
      j1=1
      do j=1,16
         if(s1(j:j).ne.' ') then
            j1=j
            go to 10
         endif
      enddo
  10  continue
      j2=j1+7
      if(j2.gt.16)j2=16
      do 20 j=j2,j1+1,-1
         if (s1(j:j).eq.' ')go to 20
         if (s1(j:j).ne.'0') then
            if(s1(j:j).eq.'.')then
               s=s1(j1:j-1)
               nc=j-j1
               go to 30
            endif
            s=s1(j1:j)
            nc=j-j1+1
            go to 30
         endif
  20  continue
  30  continue
      if(nc.eq.1.and.s(1:1).eq.'-')then
         s='0'
      endif
      if(nc.eq.0)then
         nc=1
         s='0'
      endif
      end
C--------------------------------------------------------------
      subroutine create_map(fname)
      character*80 fname

      Parameter (max_MAPS = 20000)
      integer nmap,nalias,id(max_MAPS),ialias(max_MAPS)
      character*20 names(max_MAPS)
      common /maps_pool/ nmap,nalias,id,ialias,names

      character*80 line
      character*1 comment,blank
      character*4 chid
      comment='#'
      blank=' '
      nmap = 0
      open(52,file=fname,status='old',ERR=100)
 10   read(52,300,ERR=100,END=100) line
 300  format(a)
      if(line(1:1) .eq. comment) goto 10
      if(line(1:1) .eq. blank)   goto 10
      nmap = nmap + 1
      if(nmap .gt. max_MAPS) then
         nmap = nmap - 1
         print *, 'Warning: Number of names exceed maximum:',nmap
         print *, 'Warning: Rest of file are ignored'
         goto 100
      endif
      chid=line(1:4)
      ialias(nmap) = index(chid,'%')
      if(ialias(nmap).ne.0) then
         nalias = nalias + 1
      endif
      call uctoh(chid,id(nmap),4,4)
*
* Find substitution word
*
      l1=5
      ll=lenocc(line)
      do while(line(l1:l1).le.blank .and. l1.le.ll)
         l1=l1+1
      enddo
      if(l1.gt.ll) then
         nmap = nmap - 1
         goto 10
      endif
      l2=l1
      do while(line(l2:l2).gt.blank .and. l2.le.ll)
         l2=l2+1
      enddo
      if(l2.gt.ll) l2=ll
*************************************************************
      names(nmap) = line(l1:l2)
      if(ialias(nmap).ne.0) then
*     Check that 'name' also has '%'
         if(index(names(nmap),'%').eq.0) then
            nmap = nmap - 1
            nalias = nalias - 1
            print *, '* Error in line  ',line(1:l2)
            print *, '* Both words should have stab symbol "%"'
            print *, '*==> Line ignored ***'
         endif
      endif
*      print 200,ialias(nmap),id(nmap),names(nmap)
* 200  format('Stab index:',i4,' ID = ',a4,'  ==> ',a)
      goto 10
 100  continue
      close(52)
      end
C-------------------------------------------------------------------
      logical function map_found(idv,name)
      integer idv
      character*20 name

      Parameter (max_MAPS = 20000)
      integer nmap,nalias,id(max_MAPS),ialias(max_MAPS)
      character*20 names(max_MAPS)
      common /maps_pool/ nmap,nalias,id,ialias,names

      character*4 chid,chidv,stab
      integer i,j,l
      map_found = .FALSE.
* First step: Search only non-alias
      do i = 1,nmap
         if(ialias(i).eq.0 .and. id(i).eq.idv) then
            name = names(i)
            map_found = .TRUE.
            return
         endif
      enddo
      if(nalias.eq.0) return
* Second step: Search only alias
      call uhtoc(idv,4,chidv,4)
      do i = 1,nmap
         if(ialias(i).eq.0) goto 100
         call uhtoc(id(i),4,chid,4)
         l = lenocc(chid)
         if(ialias(i).eq.1) then
* case 1  :  %aaa
            if(l.eq.1) then       ! single '%' match any name
               stab = chidv
               goto 50            ! accept
            endif
            if(chid(2:l) .ne. chidv(4-l+2:4)) goto 100
            stab = chidv(1:4-l+1)
         elseif(ialias(i).eq.l) then
* case 2  :  aaa%
            if(chid(1:l-1) .ne. chidv(1:l-1)) goto 100
            stab = chidv(l:4)
         else
* case 3  :  a%aa
            j=ialias(i)    ! index of '%'
            if(chid(j+1:l) .ne. chidv(4-(l-j-1):4)) goto 100
            if(chid(1:j-1) .ne. chidv(1:j-1))       goto 100
            stab = chidv(j:4-(l-j-1)-1)
         endif
 50      continue
*         print *, chidv,' matched to ',chid,' with stab ',stab
         name = names(i)
         j = index(name,'%')
         l = lenocc(name)
         if(j.eq.1 .and. l.eq.1) then
            name = stab
         elseif(j.eq.1) then
            name = stab//name(2:l)
         elseif(j.eq.l) then
            name = name(1:l-1)//stab
         else
            name = name(1:j-1)//stab//name(j+1:l)
         endif
         map_found = .TRUE.
*         print *, names(i),' ==> ',name
         return
 100     continue
      enddo
      end
C----------------------------------------------------------------------
#if 0
      SUBROUTINE G2RIN
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to read GEANT object(s) fromin the RZ file       *
C.    *         at the Current Working Directory (See RZCDIR)          *
C.    *       The data structures from disk are read in memory         *
C.    *           (VOLU,ROTM,TMED,MATE,SETS,PART,SCAN)                 *
C.    *                                                                *
C.    *     This routine is s short-cut of the GEANT routine GRIN      *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,HCBOOK.
      parameter (nwpaw=4000000)
      common/GCBANK/paw(nwpaw)

      INTEGER   IQ(2), LQ(8000)
      REAL            Q(2)
      EQUIVALENCE (LQ(1),paw(11)),(IQ(1),paw(19)),(Q(1),paw(19))

      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/QUEST/IQUEST(100)
      PARAMETER (NLINIT=9,NMKEY=22)
      DIMENSION JNAMES(20),KEYS(2),LINIT(NLINIT)
      DIMENSION LINK(NMKEY)
      EQUIVALENCE (JNAMES(1),JDIGI)
      CHARACTER*4 CKEY,KNAMES(NMKEY)
      INTEGER IKEY(1)
      DATA KNAMES/'DIGI','DRAW','HEAD','HITS','KINE','MATE','PART',
     +     'ROTM','RUNG','SETS','STAK','STAT','TMED','NULL','VERT',
     +     'VOLU','JXYZ','NULL','NULL','NULL','SCAN','NULL'/
      DATA LINIT/2,6,7,8,9,10,13,16,21/
C.
C.    ------------------------------------------------------------------
C.
      print *,' In g2rin, iquest(13)=',iquest(13)
      IQUEST(1)=0
      IDIV=2
      KVOL=JVOLUM
      CALL VZERO(JNAMES,20)
C
C                 Create a permanent link area for master pointers
C
      CALL MZLINK(0,'/GCLINK/',JDIGI,JSKLT,JDIGI)
*
      DO 20 J=1, NLINIT
         LINK(J)=LINIT(J)
   20 CONTINUE
      NLINK=NLINIT
*
      IKEY(1)=0
  130 CONTINUE
      IKEY(1)=IKEY(1)+1
      CALL RZINK(IKEY,0,'S')
      print *,' after rzink, ikey=',ikey,'iquest(1)=',iquest(1)
      IF(IQUEST(1).NE.0) THEN
         print *,' nkeys=',iquest(7),' nwkey=',iquest(8)
         if (iquest(1).ne.11) then
            IQUEST(1)=0
            GOTO 150
         endif
      ENDIF
      INDKEY = IQUEST(21)
      CALL UHTOC(INDKEY,4,CKEY,4)
      print *,'trying to read:',ckey
      DO 140 I=1,NLINK
         NKEY=ABS(LINK(I))
         IF(CKEY.EQ.KNAMES(NKEY))THEN
            KEYS(1)=IQUEST(21)
            KEYS(2)=IQUEST(22)
            IF(NKEY.LE.20)THEN
               IF(JNAMES(NKEY).NE.0)THEN
                  CALL MZDROP(IDIV,JNAMES(NKEY),'L')
                  JNAMES(NKEY)=0
               ENDIF
               CALL RZIN(IDIV,JNAMES(NKEY),1,KEYS,0,' ')
            ENDIF
         ENDIF
  140 CONTINUE
      GOTO 130
*
  150 NIN=0
*
  999 END
      SUBROUTINE RZINK(KEYU,ICYCLE,CHOPT)
*
************************************************************************
*
*         To find and decode KEYU,ICYCLE
* Input:
*   KEYU    Keyword vector of the information to be read
*   ICYCLE  Cycle number of the key to be read
*           ICYCLE > highest cycle number means read the highest cycle
*           ICYCLE = 0 means read the lowest cycle
*   CHOPT   Character variable specifying the options selected.
*           data structure
*             default
*                   Same as 'D' below
*             'A'   Read continuation of the previously read data structure
*                   with identifier KEYU,ICYCLE
*                   Given that option implies that the record was written with
*                   the same option by a call to RZOUT.
*             'C'   Provide   information   about   the   cycle   numbers
*                   associated with KEY.
*                   The  total number  of  cycles  and the  cycle  number
*                   identifiers of the 19 highest  cycles are returned in
*                   IQUEST(50) and IQUEST(51..89) respectively
*             'D'   Read the  Data structure  with the  (key,cycle)  pair
*                   specified.
*             'N'   Read the neighbouring. keys (i.e. those preceding and
*                   following KEY).
*                   The  key-vectors of  the previous  and  next key  are
*                   available   respectively   as   IQUEST(31..35)    and
*                   IQUEST(41..45), see below.
*             'R'   Read data into existing bank at LSUP,JBIAS
*             'S'   KEYU(1) contains the key serial number
*                   IQUEST(20)= serial number of the key in directory
*                   IQUEST(21..20+NWKEY)=KEY(1....NWKEY)
*
* Called by RZIN,RZVIN
*
*  Author  : R.Brun DD/US/PD
*  Written : 09.05.86
*  Last mod: 11.09.89
*          : 04.03.94 S.Banerjee (Change in cycle structure)
*          : 23.03.95 J.Shiers - check on K/C blocks is on KEY(1)
*
************************************************************************
*
*
* rzcl.inc
*
*
* mzbits.inc
*
      PARAMETER      (IQDROP=25, IQMARK=26, IQCRIT=27, IQSYSX=28)
*
* quest.inc
*
      COMMON /QUEST/ IQUEST(100)
*
* zebq.inc
*
      COMMON /ZEBQ/  IQFENC(4), LQ(100)
                              DIMENSION    IQ(92),        Q(92)
                              EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))
**#include "zebra/mzca.inc"
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM
     +,              NQFATA,NQCASE,NQTRAC
                                       EQUIVALENCE (KQSP,NQOFFS(1))
      COMMON /MZCA2/ MQTRAC(44)
                     CHARACTER  MQTRAC*8
*
* mzcb.inc
*
      COMMON /MZCB/  JQSTOR,KQT,KQS,  JQDIVI,JQDIVR
     +,              JQKIND,JQMODE,JQDIVN,JQSHAR,JQSHR1,JQSHR2,NQRESV
     +,              LQSTOR,NQFEND,NQSTRU,NQREF,NQLINK,NQMINR,LQ2END
     +,              JQDVLL,JQDVSY,NQLOGL,NQSNAM(6)
                                       DIMENSION    IQCUR(16)
                                       EQUIVALENCE (IQCUR(1),LQSTOR)
*
* mzcc.inc
*
      COMMON /MZCC/  LQPSTO,NQPFEN,NQPSTR,NQPREF,NQPLK,NQPMIN,LQP2E
     +,              JQPDVL,JQPDVS,NQPLOG,NQPNAM(6)
     +,              LQSYSS(10), LQSYSR(10), IQTDUM(22)
     +,              LQSTA(21), LQEND(20), NQDMAX(20),IQMODE(20)
     +,              IQKIND(20),IQRCU(20), IQRTO(20), IQRNO(20)
     +,              NQDINI(20),NQDWIP(20),NQDGAU(20),NQDGAF(20)
     +,              NQDPSH(20),NQDRED(20),NQDSIZ(20)
     +,              IQDN1(20), IQDN2(20),      KQFT, LQFSTA(21)
                                       DIMENSION    IQTABV(16)
                                       EQUIVALENCE (IQTABV(1),LQPSTO)
*
* rzclxx.inc
*
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
*
*          RZ - 1 structural link
*
* eqlqr.inc
*
                   EQUIVALENCE (LQRS,LQSYSS(7))
C
*
* rzclun.inc
*
      COMMON /RZCLUN/LUN,LREC,ISAVE,IMODEX,IRELAT,NHPWD,IHPWD(2)
     +,              IZRECL,IMODEC,IMODEH
C
*
* rzcout.inc
*
      COMMON /RZCOUT/IP1,IR1,IR2,IROUT,IRLOUT,IOPTRR
C
*
* rzk.inc
*
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KRZVER=16,KIRIN=17,
     +           KIROUT=18,KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,
     +           KLB=25,KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
C
*
* rzckey.inc
*
      COMMON/RZCKEY/IHEAD(3),KEY(100),KEY2(100),KEYDUM(50)
C
* rzcycle.inc
*
*
*     Pointers to cycle content
*
*     KLCYCL : length of cycle block (4,7)
*     KPPCYC : pointer to previous cycle
*     KFRCYC : first record number
*     KSRCYC : secord record number
*     KFLCYC : creation date/time and other stuff
*     KORCYC : offset in first record to data
*     KCNCYC : cycle number
*     KNWCYC : number of words in d/s
*     KKYCYC : key number to which this cycle belongs (only for version 1)
*     KVSCYC : version of RZ cycles structure (0, 1)
*
      INTEGER        KLCYCL, KPPCYC, KFRCYC, KSRCYC, KFLCYC, KORCYC,
     +               KCNCYC, KNWCYC, KKYCYC, KVSCYC
      COMMON/RZCYCLE/KLCYCL, KPPCYC, KFRCYC, KSRCYC, KFLCYC, KORCYC,
     +               KCNCYC, KNWCYC, KKYCYC, KVSCYC

      CHARACTER*(*) CHOPT
      DIMENSION KEYU(*)
      EQUIVALENCE (IOPTA,IQUEST(91)), (IOPTC,IQUEST(92))
     +,    (IOPTD,IQUEST(93)), (IOPTN,IQUEST(94)), (IOPTR,IQUEST(95))
     +,    (IOPTS,IQUEST(96))
*
*-----------------------------------------------------------------------
*
*#include "zebra/q_jbyt.inc"
*
      IQUEST(1)=0
      CALL UOPTC(CHOPT,'ACDNRS',IQUEST(91))
*
*           Search KEY and CYCLE
*
      LK=IQ(KQSP+LCDIR+KLK)
      NKEYS=IQ(KQSP+LCDIR+KNKEYS)
      NWKEY=IQ(KQSP+LCDIR+KNWKEY)
      IQUEST(7)=NKEYS
      IQUEST(8)=NWKEY
      IF(NKEYS.EQ.0)GO TO 90
*
      IF(IOPTS.NE.0)THEN
         IK1=KEYU(1)
         IK2=IK1
         IF(IK1.GT.NKEYS.OR.IK1.LE.0)THEN
            IQUEST(1)=1
            IQUEST(2)=IK1
            RETURN
         ENDIF
      ELSE
         IK1=1
         IK2=NKEYS
         DO 5 I=1,NWKEY
            IKDES=(I-1)/10
            IKBIT1=3*I-30*IKDES-2
            IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
               KEY(I)=KEYU(I)
            ELSE
               CALL ZHTOI(KEYU(I),KEY(I),1)
            ENDIF
   5     CONTINUE
      ENDIF
      DO 30 I=IK1,IK2
         LKC=LK+(NWKEY+1)*(I-1)
         IF(IOPTS.EQ.0)THEN
            DO 10 K=1,NWKEY
               IF(IQ(KQSP+LCDIR+LKC+K).NE.KEY(K))GO TO 30
  10        CONTINUE
         ELSE
            DO 15 K=1,NWKEY
               IF(K.LT.10)THEN
                  IKDES=(K-1)/10
                  IKBIT1=3*K-30*IKDES-2
                  IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).LT.3)THEN
                     IQUEST(20+K)=IQ(KQSP+LCDIR+LKC+K)
                  ELSE
                     CALL ZITOH(IQ(KQSP+LCDIR+LKC+K),IQUEST(20+K),1)
                  ENDIF
               ENDIF
  15        CONTINUE
         ENDIF
         IQUEST(20)=I
         LCYC=IQ(KQSP+LCDIR+LKC)
*
**=================WARNING=======================================
**  The guy who introduced this change was probably drunk !!
** I had to comment this block (Rene brun)
**         IF (KVSCYC.NE.0) THEN
**           IF (IQ(KQSP+LCDIR+LCYC+KKYCYC).NE.I) THEN
**
**    Check should be on content of KEY(1)
**
**            IF (IQ(KQSP+LCDIR+LCYC+KKYCYC).NE.IQ(KQSP+LCDIR+LKC+1)) THEN
**               IQUEST(1) = 11
**               GO TO 99
**            ENDIF
**         ENDIF
**===============================================================
         NC=0
  20     NC=NC+1
         ICY = JBYT(IQ(KQSP+LCDIR+LCYC+KCNCYC),21,12)
         IF(ICY.EQ.ICYCLE)GO TO 50
         IF(NC.EQ.1.AND.ICYCLE.GT.ICY)GO TO 50
         IF (KVSCYC.EQ.0) THEN
            LCOLD = JBYT(IQ(KQSP+LCDIR+LCYC+KPPCYC),1,16)
         ELSE
            LCOLD = IQ(KQSP+LCDIR+LCYC+KPPCYC)
         ENDIF
         IF(LCOLD.EQ.0.AND.LCOLD.NE.LCYC.AND.ICYCLE.EQ.0)GO TO 50
         LCYC=LCOLD
         IF(LCYC.NE.0)GO TO 20
         GO TO 90
  30  CONTINUE
      GO TO 90
*
*           Cycle has been found
*           Read record descriptor
*
  50  IF (KVSCYC.EQ.0) THEN
         IR1   = JBYT(IQ(KQSP+LCDIR+LCYC+KFRCYC),17,16)
         IR2   = JBYT(IQ(KQSP+LCDIR+LCYC+KSRCYC),17,16)
         IP1   = JBYT(IQ(KQSP+LCDIR+LCYC+KORCYC), 1,16)
         NW    = JBYT(IQ(KQSP+LCDIR+LCYC+KNWCYC), 1,20)
      ELSE
         IR1   = IQ(KQSP+LCDIR+LCYC+KFRCYC)
         IR2   = IQ(KQSP+LCDIR+LCYC+KSRCYC)
         IP1   = JBYT(IQ(KQSP+LCDIR+LCYC+KORCYC), 1,20)
         NW    = IQ(KQSP+LCDIR+LCYC+KNWCYC)
      ENDIF
      N1    = NW
      IQUEST(2)=1
      IF(IR2.NE.0)IQUEST(2)=(NW-N1-1)/LREC+2
      IQUEST(3)=IR1
      IQUEST(4)=IP1
      IQUEST(5)=IR2
      IQUEST(6)=ICY
      IQUEST(12)=NW
      IQUEST(14)=IQ(KQSP+LCDIR+LCYC+1)
      IQUEST(15)=LCYC
C
C           C option given
C
      IF(IOPTC.NE.0)THEN
         IQUEST(50)=0
         LC1=LCYC
  51     IQUEST(50)=IQUEST(50)+1
         IF (KVSCYC.EQ.0) THEN
            LCOLD = JBYT(IQ(KQSP+LCDIR+LC1+KPPCYC),1,16)
         ELSE
            LCOLD = IQ(KQSP+LCDIR+LC1+KPPCYC)
         ENDIF
         IF(IQUEST(50).LE.19)THEN
            NC=IQUEST(50)
            IQUEST(50+NC)=JBYT(IQ(KQSP+LCDIR+LC1+KCNCYC),21,12)
            IQUEST(70+NC)=IQ(KQSP+LCDIR+LC1+KFLCYC)
         ENDIF
         IF(LCOLD.NE.0.AND.LCOLD.NE.LC1)THEN
            LC1=LCOLD
            GO TO 51
         ENDIF
      ENDIF
C
C           N option given. return neighbours
C
      IF(IOPTN.NE.0)THEN
         IF(I.EQ.1)THEN
            IQUEST(30)=0
         ELSE
            IQUEST(30)=NWKEY
            DO 52 J=1,NWKEY
               IF(J.LT.10)THEN
                  LKCJ=LK+(NWKEY+1)*(I-2)
                  IQUEST(30+J)=IQ(KQSP+LCDIR+LKCJ+J)
                  IKDES=(J-1)/10
                  IKBIT1=3*J-30*IKDES-2
                  IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).GE.3)THEN
                     CALL ZITOH(IQUEST(30+J),IQUEST(30+J),1)
                  ENDIF
               ENDIF
  52        CONTINUE
         ENDIF
         IF(I.EQ.NKEYS)THEN
            IQUEST(40)=0
         ELSE
            IQUEST(40)=NWKEY
            DO 53 J=1,NWKEY
               IF(J.LT.10)THEN
                  LKCJ=LK+(NWKEY+1)*I
                  IQUEST(40+J)=IQ(KQSP+LCDIR+LKCJ+J)
                  IKDES=(J-1)/10
                  IKBIT1=3*J-30*IKDES-2
                  IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).GE.3)THEN
                     CALL ZITOH(IQUEST(40+J),IQUEST(40+J),1)
                  ENDIF
               ENDIF
  53        CONTINUE
         ENDIF
      ENDIF
      GO TO 99
*
*           Error
*
  90  IQUEST(1)=1
      IF(IOPTN.NE.0)THEN
         IF(NKEYS.GT.0)THEN
            IQUEST(30)=NWKEY
            IQUEST(40)=NWKEY
            DO 91 J=1,NWKEY
               IF(J.GE.10)GO TO 91
               LKCJ=LK+(NWKEY+1)*(NKEYS-1)
               IQUEST(30+J)=IQ(KQSP+LCDIR+LK+J)
               IQUEST(40+J)=IQ(KQSP+LCDIR+LKCJ+J)
               IKDES=(J-1)/10
               IKBIT1=3*J-30*IKDES-2
               IF(JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3).GE.3)THEN
                  CALL ZITOH(IQUEST(30+J),IQUEST(30+J),1)
                  CALL ZITOH(IQUEST(40+J),IQUEST(40+J),1)
               ENDIF
  91        CONTINUE
         ENDIF
      ENDIF
*
  99  RETURN
      END
#endif
