/***************************************************************************
 *
 * $Id: g2t_tpc_hit.hh,v 1.1 1998/11/10 17:12:13 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: g2t_tpc_hit.hh,v $
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/05/21 21:27:44  lasiuk
 * Initial revision
 *
 * Revision 1.1.1.1  1998/05/19 22:33:44  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_G2T_HH
#define ST_G2T_HH

struct g2t_tpc_hit {            /* G2t_tpc_hit */ 
       long      id;            /* primary key */
       long      next_tr_hit_p; /* Id of next hit on same track */
       long      track_p;       /* Id of parent track */
       long      volume_id;     /* STAR volume identification */
       float     de;            /* energy deposition at hit */
       float     ds;            /* path length within padrow */
       float     p[3];          /* local momentum */
       float     tof;           /* time of flight */
       float     x[3];          /* coordinate (Cartesian) */
};

#endif
