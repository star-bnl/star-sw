/***************************************************************************
 *
 * $Id: StuPostScript.h,v 1.1 2002/04/23 03:15:28 ullrich Exp $
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
 *       Otherwise the frame is dran in black and the                 
 *       background is white.                                         
 * p     Draw beampipe.                                               
 * c     Use many colors for tracks (colored according to pt).        
 *       red pt>1, green 0.5<pt<1, blue<0.5                           
 * C     Use many colors for tracks (colored according to pt).        
 *       Blue lowest, red highest.                                    
 * f     Track drawn with fewer points (factor 4 less).               
 *       Helix becomes slightly more chiseled                         
 *       and files are considerable smaller.                          
 *       Still good for small scale eps.                              
 * t     Add text (event, run number, trigger etc.) to the plot.      
 * a     Plot all global tracks.                                      
 *       Without this option short tracks                             
 *       (<10 points) get suppressed.                                 
 * s     Plot sideview (zy)                                           
 *       Default is front view (xy)                                   
 *
 * StuPostScript writes PS without dictionary. This makes the produced
 * files slightly bigger but the code easier to maintain.
 *
 * Known Problems: The side view and text are not compatible. It works
 *                 bit doesn't look nice. 
 ***************************************************************************
 *
 * $Log: StuPostScript.h,v $
 * Revision 1.1  2002/04/23 03:15:28  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StEventTypes.h"
#include <iostream.h>
#include <fstream.h>
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
    static void hls2rgb(double, double, double, double&, double &, double &);

    static bool mBlackBackground;
    static bool mDrawBeamPipe;
    static bool mTracksWithManyColors;
    static bool mTracksWithFewColors;
    static bool mFewerPointsPerTrack;
    static bool mAddText;
    static bool mAllGlobalTracks;
    static bool mSideView;

    static const int mllx;
    static const int mlly;
    static const int murx;
    static const int mury;
    static const int mMinFitPoints;

    static vector<StTrack*> *mUserTracks;
};
