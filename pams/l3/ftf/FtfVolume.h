#ifndef FTFVOLUME
#define FTFVOLUME
/*
 *-- Classifies space points
 */
class VOLUME {
   public:
       void print ( ) ;
       FtfHit *firstHit       ;   /* first hit in this volume */
       FtfHit *lastHit       ;   /* Id of last hit in this volume */
} ;
/*
 * To organize rows
 */
class ROW  {
   public:
        void print ( ) ;
        FtfHit *firstHit;   /* first hit in this row       */
        FtfHit *lastHit;   /* last  hit in this row       */
} ;
/*
 *    For track merging
 */
class FtfTrack ;
class AREA{
   public:
        FtfTrack *firstTrack;   /* first track in track area   */
        FtfTrack *lastTrack;   /* last  track in track area   */
} ;
#endif

