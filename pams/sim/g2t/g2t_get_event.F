* $Id: g2t_get_event.F,v 1.10 2007/05/10 19:11:32 potekhin Exp $
*
* $Log: g2t_get_event.F,v $
* Revision 1.10  2007/05/10 19:11:32  potekhin
* a) Added MSTU72, MSTU73, MSTP111 as per Spin PWG request
* b) Removed two print statements (St_geant_Maker is a better place to report diagnostics)
* c) Improved formatting
* d) Separate error codes for the generic and Pythia-specific header read
*
* Revision 1.9  2004/08/05 16:38:05  potekhin
* New format for the common block due to Pythia work
*
* Revision 1.8  2003/07/30 03:51:32  potekhin
* Update the format of the event header, including the new
* field -- the rejected events scaler
*
* Revision 1.7  2003/05/14 22:55:53  potekhin
* Introducing new data elements in teh event record -- number of
* wounded nucleaons etc. Changed interface to the mheadrd sub.
*
* Revision 1.6  2003/03/14 17:26:19  potekhin
* Added a call to the "mheadrd", the facility which
* will allow us to read optional Zebra banks into root
* tables, at will
*
*
* Changes made:  2003/03/10 potekhin
* introduced the Pythia subprocess Id
*
* Revision 1.5  2000/01/22 23:42:11  nevski
* clean up unused variables
*
* Revision 1.4  2000/01/22 22:50:18  nevsky
* proton/neutron sharing corrected
*
*
**:>-----------------------------------------------------------------------
*  module    G2T_get event fills event header struct
*  author    Pavel Nevski
*  created   22 april 98
**:<-----------------------------------------------------------------------

      Function G2T_GET_EVENT(g2t_event_h, g2t_event)
      implicit   none

#include "g2t_get_event.inc"
      Integer             G2T_GET_EVENT,Len,NBytPW
      Integer             NwHEAD,IHEAD(100),Nu,IBUF(100),i,j,in,ip,jn,jp
      Integer             Ntra,IRUN,Ievt,IwtFL,Istat,IDPDG,MOTH(2),IDAU1

      Real                VERT(4),PP(4),TIME,AMASS,WEIGH
      Character           GeName*8
      Parameter           (NBytPW=4)

      INTEGER             NUMPRIM,NUMFINS
      COMMON /HDRNFO/     EVSTAT,SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ
      INTEGER             EVSTAT,SUB_ID,NBIN_COLL,NWE,NWW,NJETS,NREJ

      COMMON/PYTNFO/ PYSTAT,PYT_ID,
     *               MANDS,MANDT,MANDU,HARDP,COSTH,
     *               BJOR1,BJOR2,
     *               MSTU72,MSTU73,MSTP111
      INTEGER        PYSTAT,PYT_ID,MSTU72,MSTU73,MSTP111
      REAL           MANDS,MANDT,MANDU,HARDP,COSTH,BJOR1,BJOR2

      common /g2tevtcnt/  NUMPRIM,NUMFINS 
*

      G2T_GET_EVENT   = 1
      g2t_event_h.nok = 1
      Len = g2t_event_h.rbytes/NBytPW
      Call VZERO   (g2t_event(1),Len)
      call GFHEAD  (NwHead,IHEAD,Nu,IBUF)
      call AgNZGETE(1,1,  Ntra, IRUN, IEVT, GeName,VERT,IWTFL,WEIGH)
      call AgNZGETP(1,1,1,Istat,IDPDG,PP,AMASS,MOTH,TIME,IDAU1,VERT)
*
      g2t_event(1).n_event            = IHEAD(2)
      g2t_event(1).ge_rndm(1)         = IHEAD(3)
      g2t_event(1).ge_rndm(2)         = IHEAD(4)

* --max-- originally, it was EQ.10, which was incorrect
      if (Istat .ge. 10) then
         g2t_event(1).eg_label         = VERT(2) ! run number
         g2t_event(1).event_type       = VERT(1) ! event number
         i  =  AMASS                             ! Awest.Aeast
         j  = (AMASS-i)*1000+0.5
         in = (100*i)/197                        ! west
         ip = i-in
         g2t_event(1).n_part_neut_west = in
         g2t_event(1).n_part_prot_west = ip
         jn = 100*j/197                          ! east
         jp = j-jn
         g2t_event(1).n_part_neut_east = jn  
         g2t_event(1).n_part_prot_east = jp

         g2t_event(1).n_run            = MOTH(1)
         g2t_event(1).b_impact         = PP(1)
         g2t_event(1).phi_impact       = PP(2)
      endif

      g2t_event(1).n_track_eg_fs      = NUMPRIM
      g2t_event(1).n_track_prim       = NUMFINS
      g2t_event(1).prim_vertex_p      = 1
      g2t_event(1).time_offset        = 0

* event header and Pythia-specific (if it exists) data read from Zebra and put into the common blocks
      call MHEADRD()

      if(evStat.ge.0) then
         g2t_event(1).subprocess_id = SUB_ID
         g2t_event(1).n_binary      = NBIN_COLL
         g2t_event(1).n_wounded_east= NWE
         g2t_event(1).n_wounded_west= NWW
         g2t_event(1).njets         = NJETS
         g2t_event(1).n_rejected    = NREJ
      endif

* Useful debug diagnostic
*     write(*,*) 'EVSTAT,PYSTAT:',EVSTAT,',',PYSTAT

* Check the success of the Pythia header read procedure: (we reserve another decimal bit to signal the success)
      if(PYSTAT.eq.0) then
         G2T_GET_EVENT=G2T_GET_EVENT+10
      endif

      END

