#ifndef FTFVOLUME
#define FTFVOLUME
/*
 *-- Classifies space points
 */
class VOLUME {
   public:
        void print ( ) ;
        FTF_Hit *first_hit       ;   /* first hit in this volume */
        FTF_Hit *last_hit       ;   /* Id of last hit in this volume */
} ;
/*
 * To organize rows
 */
class ROW  {
   public:
        void print ( ) ;
        FTF_Hit *first_hit;   /* first hit in this row       */
        FTF_Hit *last_hit;   /* last  hit in this row       */
} ;
/*
 *    For track merging
 */
class FTF_Track ;
class AREA{
   public:
        FTF_Track *fst_track;   /* first track in track area   */
        FTF_Track *lst_track;   /* last  track in track area   */
} ;
#endif

