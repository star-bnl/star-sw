* $Id: g2t_get_event.F,v 1.2 2000/01/22 22:49:57 nevsky Exp $
*
* $Log: g2t_get_event.F,v $
* Revision 1.2  2000/01/22 22:49:57  nevsky
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
      Integer                G2T_GET_EVENT,Len,NBytPW
      Integer                NwHEAD,IHEAD(100),Nu,IBUF(100),i,j,in,ip,jn,jp
      Integer                Ntra,IRUN,Ievt,IwtFL,Istat,IDPDG,MOTH(2),IDAU1
      Real                   VERT(4),PP(4),TIME,AMASS,WEIGH
      Character              GeName*8
      Parameter              (NBytPW=4)
      INTEGER                NUMPRIM,NUMFINS
      common /g2tevtcnt/     NUMPRIM,NUMFINS 
*
*     make g2t_event:
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

      if (Istat .eq. 10) then
        g2t_event(1).eg_label         = VERT(2)    ! run number
        g2t_event(1).event_type       = VERT(1)    ! event number
        i  =  AMASS                                ! Awest.Aeast
        j  = (AMASS-i)*10**INT(ALOG10(AMASS+.1)+1)
        in = (100*i)/197                           ! west
        ip = i-in
        g2t_event(1).n_part_neut_west = in
        g2t_event(1).n_part_prot_west = ip
        jn = 100*j/197                             ! east
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

      END

