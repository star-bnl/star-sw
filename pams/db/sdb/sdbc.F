************************************************************************
*                                                                      *
* Set of FORTRAN routines for file manipulation                        *
* Used by SDBM and XDF2MEM packages, developed for STAR collaboration  *
* Author: Vladimir Tikhomirov                                          *
* Creation: June 1998                                                  *
*                                                                      *
************************************************************************
*-----------------------------------------------------------------------
      SUBROUTINE T_GET_ROOT(env,root)
* 
* Routine to get name of root (the last path) directory from path environment.
*
* Author: V.Tikhomirov 
*
* Creation: June-1998
*
* Input:
*       env (character) - path environment
* Output:
*        root (character) - root directory in path
*
      implicit none
      character env*(*),root*(*)
      character string*80,dir*256
      integer lenocc,l1,is,i
C.....
      root=' '
C: Get full path of environment
      l1=lenocc(env)
      string=env(1:l1)//char(0)
      CALL GET_CAL_DIR_F(string,dir)
      l1=index(dir,char(0))-1
C: Environment does not exist
      if(l1.le.0)  R E T U R N
C: Extract root - the last directory in the path
      is=0
      do i=1,l1
        if(dir(i:i).eq.'/') is=i
      enddo
      if(is.gt.0 .and. is.lt.l1) root=dir(is+1:l1)

      END
*-----------------------------------------------------------------------
      SUBROUTINE T_GET_PATH(lun,fname,troot,path,iret)
*
* Routine to get directory path from list in disk file.
* Reads disk file with paths list line by line. Each line in file should be
* some UNIX path. Routine truncates path upto troot and returns path.
*
* Author: V.Tikhomirov      

* Creation: June-1998
*
* Input:
*       lun (integer) - LUN to read file with paths list
*       fname (character) - file name of paths list
*       troot (character) - root directory in path
* Output:
*       path (character) - returned path
*       iret (integer) - return code: =0 means path is returned; =-1 - if
*                        cannot open disk file,  =-3 - EOF found and file
*                        is closed, =-4 -error during read next line in file
*
      implicit none
      integer lun,iret
      character fname*(*),troot*(*),path*(*)
      integer maxlff
      parameter (maxlff=256)   ! Maximum full file name length
      character line*(maxlff),ch*1
      integer lenocc,i,ld,ld1,irc,is,ind,iopen,ln,ld0
      data iopen/0/
      save iopen,ln,ld0
C.....
C: Open disk file with list of directory's paths, if it's not yet ready
      if(iopen.eq.0) then
        iret=-1
        ld0=lenocc(fname)
        open(unit=lun,file=fname(1:ld0),status='OLD',iostat=irc)
        if(irc.ne.0) then
          write(6,'('' *** T_GET_PATH: can not open paths list file:''
     >      ,/,1x,a,/,'' at LUN '',i5,''     Iostat='',i7)')
     >      fname(1:ld0),lun,irc
          R E T U R N
        endif
        iopen=1
        ln=0
      endif

      iret=-4
      path=' '

C: Read next line from input file
 20   ln=ln+1
      read(lun,'(a)',end=100,err=991) line
      ld=lenocc(line)

C: Skip blank line
      if(ld.le.0)  G O T O  20

C: Skip leading blanks and/or tabs in line. IS will a pointer to first nonblank
      do i=1,ld
        is=i
        ch=line(i:i)
        if(ch.ne.' ' .and. ch.ne.char(9))  G O T O  40
      enddo
      G O T O  20
 40   continue

C: Check if it's comment line and skip it
      ch=line(is:is)
      if(ch.eq.'*' .or. ch.eq.'!' .or. ch.eq.';')  G O T O  20

C: Check if troot directory is presented in path, then skip path upto troot
      ld1=lenocc(troot)
      if(ld1.gt.0) then
        ind=index(line(1:ld),troot(1:ld1))
        if(ind.gt.0) is=ind
      endif

C: Return path
      path=line(is:ld)
      iret=0
      R E T U R N

C: EOF found
 100  close(lun)
      iopen=0
      iret=-3
      R E T U R N

 991  write(6,'('' *** T_GET_PATH: error during read line #'',i5,
     >   ''  from file with list of paths:'',/,1x,a)') ln,fname(1:ld0)

      END
*-----------------------------------------------------------------------
      SUBROUTINE T_CREATE_DIR(path)
*
* Routine to build directories in memory according to path.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       path (character) - path to build
* Output:
*        none
*
      implicit none
      character path*(*)
      integer maxlff
      parameter (maxlff=256)   ! Maximum full file name length
      character dir*(maxlff)
      integer lenocc,ld,i,ind,jfirst,jlast,ndeep
C.....
C: Number of directorires to be build in this path
      ndeep=0
C: Pick up directory name from path string. Jfirst is a pointer to the first
C:  character in the string with directory name, jlast - to the last character.
      ld=lenocc(path)
      jfirst=1
 50   ind=index(path(jfirst:ld),'/')
      if(ind.gt.1) then
        jlast=jfirst+ind-2
      elseif(ind.eq.1) then
        jlast=jfirst
      else
        jlast=ld
      endif

C: Create directory in memory
      if(path(jlast:jlast).ne.'/') then
        dir=path(jfirst:jlast)//char(0)
        CALL DUI_CDIR(dir)
        ndeep=ndeep+1
        jfirst=jlast+2
      else
        jfirst=jlast+1
      endif

C: Look for the next directory name in the path
      if(jfirst.lt.ld)  G O T O  50

C: Return on top level in memory
      do i=1,ndeep
        dir='..'//char(0)
        CALL DUI_CDIR(dir)
      enddo

      END
*-----------------------------------------------------------------------
      SUBROUTINE T_OPEN_DIR(path,calenv,troot,iopt,fulldir,idir)
*
* Routine to open PWD or STAR directory.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       path (character) - path to directory after troot
*       calenv (character) - STAR environment for interested files
*       troot (character) - root directory in path
*       iopt (integer) - option: =0 - try to open PWD dir first, if not found -
*                        try STAR dir; =-1 - try PWD dir only 
* Output:
*        fulldir (character) - full path to directory
*        idir (integer) - pointer to directory structure in c-procedures,
*                          =0, if directory not found or not open
*
      implicit none
      character path*(*),calenv*(*),troot*(*),fulldir*(*)
      integer iopt,idir
      character caldir*256,string*256
      integer lenocc,ld,ld1,ld2,ind
C.....
      ld=lenocc(path)
C: Get begining path of directory (before troot): try user's directory first
      string='PWD'//char(0)
      CALL GET_CAL_DIR_F(string,caldir)
      ind=index(caldir,char(0))-1
C: Full path to directory
      fulldir=caldir(1:ind)//'/'//path(1:ld)//char(0)
C: Try to open direcory
      idir=0
      CALL OPEN_DIR_F(fulldir,idir)
C: Return, if directory is found
      if(idir.ne.0)  R E T U R N
C: If iopt=-1 - directory not found, return
      if(idir.eq.0 .and. iopt.eq.-1) then
        write(6,*) ' *** T_OPEN_DIR: cannot open directory >',
     >    fulldir(1:ind+ld+1),'< not in PWD, not in STAR path'
        fulldir=char(0)
      R E T U R N
      endif
C: If iopt=0 and directory not found in user's PWD - try STAR path
      if(iopt.eq.0) then
        ind=lenocc(calenv)
        string=calenv(1:ind)//char(0)
        CALL GET_CAL_DIR_F(string,caldir)
        ind=index(caldir,char(0))-1
C: In STAR environment root direcory 'troot' is already presented. To avoid
C:  double counting with the same name in 'path', some trick here.
        ld1=lenocc(troot)
        ld2=index(caldir(1:ind),troot(1:ld1))
        if(ld2.gt.0) ind=ld2-2
        fulldir=caldir(1:ind)//'/'//path(1:ld)//char(0)
C: Try to open direcory
        CALL OPEN_DIR_F(fulldir,idir)
	if(idir.eq.0) then
          write(6,*) ' *** T_OPEN_DIR: cannot open directory >',
     >      fulldir(1:ind+ld+1),'< not in PWD, not in STAR path'
          fulldir=char(0)
        endif
      endif

      END
*-----------------------------------------------------------------------
      SUBROUTINE T_READXDF(discfile,dirmem,iret)
*
* Routine read XDF format file from disk and put it in memory.
*  Directory path in memory should be created before CALL T_READXDF by 
*  CALL T_CREATE_DIR.
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       discfile (character) - name of XDF disk file with full path
*       dirmem (character) - directory in memory to put and unpack XDF file
* Output:
*        iret (integer) - return code =0, if OK
*
      implicit none
      character discfile*(*),dirmem*(*)
      integer iret
      character string*256
      integer lunxdf,lenocc,irc
      
C.....
      iret=-1
C: Open XDF file for read
      lunxdf=0
      string=discfile(1:lenocc(discfile))//char(0)
      call xdf_open(lunxdf,discfile,'r',irc)
      if(irc.ne.0)  R E T U R N
      iret=0

C: Read XDF file and unpack it in memory
      string=dirmem(1:lenocc(dirmem))//char(0)
      call xdf_getev(lunxdf,string,irc)
      if(irc.ne.0) iret=-2

C: Close XDF file
      call xdf_close(lunxdf,irc)
      if(irc.ne.0) iret=-3
 
      END

*-----------------------------------------------------------------------
      SUBROUTINE T_PROD_PATH_LIST(calenv,troot,flist,iopt)
*
* Routine to produce file with path list to interested files
*  Produces one list with user's PWD path, another - with STAR path,
*  merges both lists and put output to disk file in user's PWD.
*
*          !!! This routine is OS dependent !!!
*
* Author: V.Tikhomirov
* Creation: June-1998
*
* Input:
*       calenv (character) - STAR environment interested directory
*       troot (character) - root directory (last in path) of environment
*       flist (character) - name of disk file to write paths list
*       iopt (integer) - option: =0 - produce both PWD and STAR paths;
*                        =-1 - produce PWD path only
* Output:
*        none (actually output is directed to disk file 'flist'
*
      implicit none
      character calenv*(*),troot*(*),flist*(*)
      integer iopt
      character caldir*256,pwd*256,string*256
      integer lenocc,ln1,ln2,ln3,ln4
C.....
C: Delete 'flist' file, if it's already exist
      ln1=lenocc(flist)
      string='rm -f '//flist(1:ln1)
      CALL SYSTEMF(string(1:ln1+6))
      string='rm -f 1'//flist(1:ln1)
      CALL SYSTEMF(string(1:ln1+7))
      if(iopt.eq.0) then
        string='rm -f 2'//flist(1:ln1)
        CALL SYSTEMF(string(1:ln1+7))
C: Get full path to STAR directory
        ln2=lenocc(calenv)
        string=calenv(1:ln2)//char(0)
        CALL GET_CAL_DIR_F(string,caldir)
        ln2=index(caldir,char(0))-1
      endif
C: Get current working directory
      string='PWD'//char(0)
      CALL GET_CAL_DIR_F(string,pwd)
      ln3=index(pwd,char(0))-1
      ln4=lenocc(troot)
C: Produce path list of current directory
      string='find '//troot(1:ln4)//' -type d -a ! -name CVS >1'//flist(1:ln1)
      CALL SYSTEMF(string)
      if(iopt.eq.0) then
C: Goto STAR directory, produce path list, return to PWD
      string='cd '//caldir(1:ln2)//'/..; find '//troot(1:ln4)//
     >' -type d  -a ! -name CVS >'//pwd(1:ln3)//'/2'//flist(1:ln1)//'; cd '//pwd(1:ln3)
      CALL SYSTEMF(string)
      endif
C: Sort both lists to merge its
      if(iopt.eq.0) then
      string='sort 1'//flist(1:ln1)//' 2'//flist(1:ln1)//' -u '//
     >       '>'//flist(1:ln1)
      else
      string='sort 1'//flist(1:ln1)//' -u >'//flist(1:ln1)
      endif
      CALL SYSTEMF(string)
C: Delete two temporary files
      string='rm -f 1'//flist(1:ln1)
      CALL SYSTEMF(string(1:ln1+7))
      string='rm -f 2'//flist(1:ln1)
      CALL SYSTEMF(string(1:ln1+7))

      END







