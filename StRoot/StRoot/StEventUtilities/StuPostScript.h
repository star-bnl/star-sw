/***************************************************************************
 *
 * $Id: StuPostScript.h,v 1.6 2003/09/02 17:58:09 perev Exp $
 *
 * Author: Thomas Ullrich, April 2002
 ***************************************************************************
 *
 * Description: PostScript Event Display
 *              The class has only one public static methods write()
 *              which produces an encapsulated PostScript (EPS) file.
 *              There are various options available.
 *
 * Syntax:
 * #include "StuPostScript.h"
 *
 * bool StuPostScript::write(const char* filename, const StEvent* event);
 * bool StuPostScript::write(const char* filename, const StEvent* event,
 *                           const char* options);
 * bool StuPostScript::write(const char* filename, const StEvent* event,
 *                           const char* options,
 *                           vector<StTrack*>* userTracks);
 *
 * filename:   the name of the PostScript file. The extension should
 *             be always '.eps'.
 * event:      pointer to an event.
 * options:    character string with options (see below).
 * userTracks: optional vector of pointers to tracks.
 *             If present only those tracks stored in the vector
 *             are plotted. Note that 'event' is still needed.
 *             This option allows to select and print only certain
 *             tracks.
 * Return Code: StuPostScript::write() return 'true' if successful
 *              otherwise 'false'.
 *
 * Options (case sensitive):                                          
 * b     Black background, white frame.                               
 *       Otherwise the frame is drawn in black and the                 
 *       background is white.                                         
 * p     Draw beampipe.                                               
 * c     Use 3 colors to color tracks according to their pt.        
 *       red pt>1 GeV/c, green 0.5<pt<1  GeV/c, blue<0.5  GeV/c
 * C     Use color spectra to color tracks according to their.        
 *       Blue lowest pt, red highest pt.                                    
 * f     Track drawn with fewer points (factor 4 less).
 *       Helices are approximated by a line trough many points.
 *       With fewer points the helices become slightly more
 *       chiseled but files get considerable smaller.                          
 *       Still sufficient for most cases.                              
 * t     Add text (event, run number, trigger etc.) to the plot.      
 * a     Plot all global tracks.                                      
 *       Without this option short tracks                             
 *       (<10 points) get suppressed.                                 
 * s     Plot sideview (zy)                                           
 *       Default is front view (xy)                                   
 * h     Show hits on tracks (circles)
 * H     Show all hits (squares & circles)
 *       Hits on tracks are drawn as circles
 *       otherwise as squares
 *
 * StuPostScript writes PS without a dictionary. This makes the produced
 * files slightly bigger but the code easier to maintain.
 *
 * Known Problems: The side view and text are not compatible. It works
 *                 but doesn't look nice. 
 ***************************************************************************
 *
 * $Log: StuPostScript.h,v $
 * Revision 1.6  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2002/10/11 17:34:15  ullrich
 * Hits on tracks drawn as circles not diamonds
 *
 * Revision 1.4  2002/06/25 02:43:24  ullrich
 * Added drawing of hits.
 *
 * Revision 1.3  2002/04/23 17:26:52  ullrich
 * More description.
 *
 * Revision 1.2  2002/04/23 17:17:10  ullrich
 * More description.
 *
 * Revision 1.1  2002/04/23 03:15:28  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEventTypes.h"
#include <Stiostream.h>
#include "Stiostream.h"
#include <vector>

class StuPostScript {
public:
    StuPostScript();
    virtual ~StuPostScript();

    static bool write(const char*, const StEvent*, const char* = 0, vector<StTrack*>* = 0);
    
protected:
    static void writeHeader(ostream&, const char*);
    static void writeTrailor(ostream&);
    static void writeDetectorFrame(ostream&);
    static void writeTracks(ostream&, const StEvent*);
    static void writeText(ostream&, const StEvent*);
    static void writeHits(ostream&, const StEvent*);
    static void hls2rgb(double, double, double, double&, double &, double &);

    static bool mBlackBackground;
    static bool mDrawBeamPipe;
    static bool mTracksWithManyColors;
    static bool mTracksWithFewColors;
    static bool mFewerPointsPerTrack;
    static bool mAddText;
    static bool mAllGlobalTracks;
    static bool mSideView;
    static bool mShowTrackHits;
    static bool mShowAllHits;
    static const int mllx;
    static const int mlly;
    static const int murx;
    static const int mury;
    static const int mMinFitPoints;

    static vector<StTrack*> *mUserTracks;
};
