#ifndef FTFVOLUME
#define FTFVOLUME
/*
 *-- Classifies space points
 */
class VOLUME {
   public:
        void print ( ) ;
        FtfHit *first_hit       ;   /* first hit in this volume */
        FtfHit *last_hit       ;   /* Id of last hit in this volume */
} ;
/*
 * To organize rows
 */
class ROW  {
   public:
        void print ( ) ;
        FtfHit *first_hit;   /* first hit in this row       */
        FtfHit *last_hit;   /* last  hit in this row       */
} ;
/*
 *    For track merging
 */
class FtfTrack ;
class AREA{
   public:
        FtfTrack *fst_track;   /* first track in track area   */
        FtfTrack *lst_track;   /* last  track in track area   */
} ;
#endif

