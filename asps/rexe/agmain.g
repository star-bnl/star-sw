*CMZ :          30/04/98  00.45.04  by  Pavel Nevski
*-- Author :    Pavel Nevski
*****************************************************************************
*                                                                           *
      subroutine        A G M A I N (nwg,nwp,iwtyp)
*                                                                           *
*****************************************************************************
+CDE,TYPING,AGECOM,AGCKINE.
+CDE,GCFLAG,GCTIME,GCPHYS,GCTRAK.
*
      integer nwg, nwp, iwtyp
      INTEGER           NWGEA/4000000/,NWPA/500000/,p
      CHARACTER*6       PROG/'agroot'/
*
      CALL TIMEST  (3.E7)  ! set time limit for interactive mode
      if (nwg .gt. 0) NWGEA = nwg
      if (nwp .ge. 0) NWPA  = nwp
      write (*,1001) PROG,NWGEA,NWPA
1001  format(1x,54('*')/' * Starting ',a8,
     >       ' NwGEANT=',i9,' NwPAW=',i8,' *'/ 1x,54('*'))
      IDEBUG = 1
*                                        initialise packages
      CALL TIMEL   (TIMINT)
      CALL MZEBRA  (-3)
      CALL GZEBRA  (NWGEA)         ! store 0 - geant
      if (iwtyp .gt. 0) then
      CALL MZPAW   (NWPA,' ')      ! store 1 - pawc
      CALL KUINIT  (5000)
      CALL IGINIT  (10000)
      CALL IGSSE   (6,1)
      CALL HLIMIT  (0)
      CALL REBANKM (-1)
      CALL GDINIT                  ! Initialise Drawing pkg
      endif
****>
      p  = Idebug
      CALL GINIT                   "  GEANT common blocks                "
      CALL GZINIT                  "  GEANT core divisions, link areas,  "
      CALL AGZINI                  "  specific ZEBRA initialization      "
      CALL GPART                   "  Define standard particles          "
      CALL GMATE                   "  Initialize standard materials      "
      Call AGXINIT                 "  aguser menu - called here          "
      CALL GINTRI                  "  Geant MENUs and COMMANDs           "
*
      Idebug      =  p             "  restore Idebug after GINIT         "
      DPHYS1      =  0             "  oshibku oshibkoi vybivaiut         "
      %Standalone =  1             "  standalone version, not batch      "
      %IGRAP      = -1             "  no default graphic, on request only"
      IGAUTO      =  0             "  defaults GEANT tracking  off       "
      CrunType    = ' '            "  no default actions defined         "
      NkineMax    = 64 000         "  ZEBRA limit on KINE bank           "
      NhitsMax    = 10 000 000     "  a reasonable limit on hit bank     "
      %Module=' '
      call Agstand
      Call AgDummy
*
END
 




