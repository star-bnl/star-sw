/***************************************************************************
 *
 * $Id: StuPostScript.cc,v 1.4 2018/06/29 17:21:24 perev Exp $
 *
 * Author: Thomas Ullrich, April 2002
 ***************************************************************************
 *
 * Description: PostScript Event Display
 *
 ***************************************************************************
 *
 * $Log: StuPostScript.cc,v $
 * Revision 1.4  2018/06/29 17:21:24  perev
 * Irakli_Jun29
 *
 * Revision 1.3  2002/10/11 17:34:14  ullrich
 * Hits on tracks drawn as circles not diamonds
 *
 * Revision 1.2  2002/06/25 02:43:12  ullrich
 * Added drawing of hits.
 *
 * Revision 1.1  2002/04/23 03:15:28  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <ctime>
#include <string>
#include "StuPostScript.h"
#include "PhysicalConstants.h"
#include "StGlobals.hh"

//
//  Initialize static data member
//
bool StuPostScript::mBlackBackground = false;
bool StuPostScript::mDrawBeamPipe = false;
bool StuPostScript::mTracksWithManyColors = false;
bool StuPostScript::mTracksWithFewColors = false;
bool StuPostScript::mFewerPointsPerTrack = false;
bool StuPostScript::mAddText = false;
bool StuPostScript::mAllGlobalTracks = false;
bool StuPostScript::mSideView = false;
bool StuPostScript::mShowTrackHits = false;
bool StuPostScript::mShowAllHits = false;
const int StuPostScript::mllx = -220;
const int StuPostScript::mlly = -220;
const int StuPostScript::murx = 220;
const int StuPostScript::mury = 220;
const int StuPostScript::mMinFitPoints = 10;
vector<StTrack*> *StuPostScript::mUserTracks = 0;

StuPostScript::StuPostScript() {/* noop*/}

StuPostScript::~StuPostScript() {/* noop*/}

bool StuPostScript::write(const char* filename, const StEvent* event, const char* opt, vector<StTrack*>* userTracks)
{
    if (!event) return false;

    //
    //  Open EPS file
    //
    ofstream ofs(filename);
    if (!ofs) return false;

    //
    //  Unpack options
    //
    string options(opt);
    mBlackBackground = (options.find('b') != string::npos);
    mDrawBeamPipe = (options.find('p') != string::npos);
    mTracksWithFewColors = (options.find('c') != string::npos);
    mTracksWithManyColors = (options.find('C') != string::npos);
    mFewerPointsPerTrack = (options.find('f') != string::npos);
    if (mTracksWithFewColors && mTracksWithManyColors) mTracksWithFewColors = false;
    mAllGlobalTracks = (options.find('a') != string::npos);
    mAddText = (options.find('t') != string::npos);
    mSideView = (options.find('s') != string::npos);
    mShowTrackHits = (options.find('h') != string::npos);
    mShowAllHits = (options.find('H') != string::npos);
    mUserTracks = userTracks;

    //
    //  Write PostSCript to file
    //
    writeHeader(ofs, filename);
    writeDetectorFrame(ofs);
    writeTracks(ofs, event);
    if (mShowTrackHits || mShowAllHits) writeHits(ofs, event);
    if (mAddText) writeText(ofs, event);
    writeTrailor(ofs);

    //
    //  Close file
    //
    ofs.close();
    cout << "StuPostScript::write(): file '" << filename << "' written." << endl;
    return true;
};

void StuPostScript::writeHeader(ostream &os, const char* filename) {

    //
    //  EPS Header
    //
    time_t now = time(0);
    os << "%!PS-Adobe-2.0 EPSF-2.0" << endl;
    os << "%%BoundingBox: " << 0 << ' ' << 0 << ' ' << murx-mllx << ' ' << mury-mlly << endl;
    os << "%%Title: " << filename << " (STAR Event Display)" << endl;
    os << "%%Creator: StuPostScript" << endl;
    os << "%%CreationDate: " << ctime(&now);
    os << "%%Pages: 0" << endl; 
    os << "%%EndComments" << endl;
    //
    //  Definitions
    //
    int  hitGrayLevel = mBlackBackground ? 1 : 0;
    const double boxSize = 1;
    os << "/Hit {gsave moveto " << -boxSize/2 << ' ' << boxSize/2
       << " rmoveto " << boxSize << " 0 rlineto 0 " << -boxSize
       << " rlineto " << -boxSize << " 0 rlineto closepath "
       << hitGrayLevel << " setgray 0.2 setlinewidth stroke grestore} def" << endl;
    os << "/tHit {gsave " << boxSize/2 << " 0 360 arc closepath "
       << hitGrayLevel << " setgray 0.2 setlinewidth stroke grestore} def" << endl;
    
    //
    //  Scaling and translation
    //
    os << "1 1 scale" << endl;
    os << -mllx << ' ' << -mlly << " translate" << endl;

    //
    //  Draw black background
    //
    if (mBlackBackground) {
	os << "gsave" << endl;
	os << "newpath" << endl;
	os << mllx << ' ' << mlly << " moveto" << endl;
	os << mllx << ' ' << mury << " lineto" << endl;
	os << murx << ' ' << mury << " lineto" << endl;
	os << murx << ' ' << mlly << " lineto" << endl;
	os << "closepath" << endl;
	os << "0 setgray" << endl;
	os << "fill" << endl;  
	os << "grestore" << endl;
    }
}

void StuPostScript::writeTrailor(ostream &os)
{
    os << "%showpage" << endl; // not for EPS files
    os << "%%EOF" << endl;
}

void StuPostScript::writeDetectorFrame(ostream &os)
{
    const double innerRadius = 50;
    const double outerRadius = 200;
    const double halfLength = 200;
    const double beamPipeRadius = 3.5;
    const int    nSectors = 12;
    const double lineWidth = 0.5;
    
    const double angleStep = twopi/nSectors;
    const double beginAngle = angleStep/2;
    double x, y, angle;

    //
    //  Handle options
    //
    int  frameGrayLevel = mBlackBackground ? 1 : 0;

    //
    //  Inner field cage
    //
    os << "gsave" << endl;
    os << "newpath" << endl;
    if (mSideView) {
	os << -halfLength  << ' ' << -innerRadius    << " moveto" << endl;
	os << 0            << ' ' << 2*innerRadius   << " rlineto" << endl;
	os << 2*halfLength << ' ' << 0               << " rlineto" << endl;
	os << 0            << ' ' << -2*innerRadius   << " rlineto" << endl;
    }
    else {
	x = innerRadius*cos(beginAngle);
	y = innerRadius*sin(beginAngle);
	os << x << ' ' << y << " moveto" << endl;
	for (angle = beginAngle+angleStep; angle < twopi; angle += angleStep) {
	    x = innerRadius*cos(angle);
	    y = innerRadius*sin(angle);
	    os << x << ' ' << y << " lineto" << endl;
	}
    }
    os << "closepath" << endl;
    os << frameGrayLevel << " setgray" << endl;
    os << lineWidth << " setlinewidth" << endl;  
    os << "stroke" << endl;  
    os << "grestore" << endl;
    
    //
    //  Outer field cage
    //
    os << "gsave" << endl;
    os << "newpath" << endl;
    if (mSideView) {
	os << -halfLength  << ' ' << -outerRadius    << " moveto" << endl;
	os << 0            << ' ' << 2*outerRadius   << " rlineto" << endl;
	os << 2*halfLength << ' ' << 0               << " rlineto" << endl;
	os << 0            << ' ' << -2*outerRadius   << " rlineto" << endl;
    }
    else {
	x = outerRadius*cos(beginAngle);
	y = outerRadius*sin(beginAngle);
	os << x << ' ' << y << " moveto" << endl;
	for (angle = beginAngle+angleStep; angle < twopi; angle += angleStep) {
	    x = outerRadius*cos(angle);
	    y = outerRadius*sin(angle);
	    os << x << ' ' << y << " lineto" << endl;
	}
    }
    os << "closepath" << endl;
    os << frameGrayLevel << " setgray" << endl;
    os << lineWidth << " setlinewidth" << endl;  
    os << "stroke" << endl;  
    os << "grestore" << endl;

    //
    //  Sector bounderies
    //
    if (!mSideView) {
	os << "gsave" << endl;
	for (angle = beginAngle; angle < twopi; angle += angleStep) {
	    x = innerRadius*cos(angle);
	    y = innerRadius*sin(angle);
	    os << x << ' ' << y << " moveto" << endl;
	    x = outerRadius*cos(angle);
	    y = outerRadius*sin(angle);
	    os << x << ' ' << y << " lineto" << endl;
	}
	os << frameGrayLevel << " setgray" << endl;
	os << lineWidth << " setlinewidth" << endl;  
	os << "stroke" << endl;  
	os << "grestore" << endl;
    }

    //
    //  Beampipe
    //
    if (mDrawBeamPipe) {
	os << "gsave" << endl;
	os << "newpath" << endl;
	if (mSideView) {
	    os << -halfLength  << ' ' << -beamPipeRadius  << " moveto" << endl;
	    os << 0            << ' ' << 2*beamPipeRadius << " rlineto" << endl;
	    os << 2*halfLength << ' ' << 0                << " rlineto" << endl;
	    os << 0            << ' ' << -2*beamPipeRadius << " rlineto" << endl;
	}
	else 
	    os << "0 0 " << beamPipeRadius << " 0 360 arc" << endl;
	os << "closepath" << endl;
	os << frameGrayLevel << " setgray" << endl;
	os << lineWidth << " setlinewidth" << endl;  
	os << "stroke" << endl;  
	os << "grestore" << endl;
    }
}

void StuPostScript::writeTracks(ostream &os, const StEvent* event)
{
    const double lineWidth    = 0.4;
    const double sstep        = mFewerPointsPerTrack ? 4 : 1;
    const double lightness    = 50;
    const double saturation   = 100;

    //
    //  Handle options
    //
    int  trackGrayLevel = mBlackBackground ? 1 : 0;
    
    StTrack *track;
    StThreeVectorD p;
    unsigned int i;
    double slast, pt, red, green, blue, hue;
    const StSPtrVecTrackNode& theNodes = event->trackNodes();
    int  minFitPoints = mAllGlobalTracks ? 1 : mMinFitPoints;
    vector<StTrack*> theTracks;

    //
    //  Prepare list of tracks to plot
    //
    if (mUserTracks) {
	for (i=0; i<mUserTracks->size(); i++)
	    theTracks.push_back((*mUserTracks)[i]);
    }
    else {
	for (i=0; i<theNodes.size(); i++) {
	  track = (StTrack *) theNodes[i]->track(global);
	    if (track) theTracks.push_back(track);
	}
    }

    //
    //  Write out tracks
    //
    for (i=0; i<theTracks.size(); i++) {
	track = theTracks[i];
	if (track && track->flag() > 0 &&
	    track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) {
	    pt = track->geometry()->momentum().perp();
	    StPhysicalHelixD helix = track->geometry()->helix();
	    p = track->detectorInfo()->lastPoint();
	    slast = helix.pathLength(p);
	    os << "gsave" << endl;
	    p = helix.origin();
	    if (mSideView) {
		os << p.z() << ' ' << p.y() << " moveto" << endl;	    
		for (double s=0; s<slast; s+= sstep) {
		    p = helix.at(s);
		    os << p.z() << ' ' << p.y() << " lineto" << endl;	    
		}
		p = helix.at(slast);
		os << p.z() << ' ' << p.y() << " lineto" << endl;
	    }
	    else {
		os << p.x() << ' ' << p.y() << " moveto" << endl;	    
		for (double s=0; s<slast; s+= sstep) {
		    p = helix.at(s);
		    os << p.x() << ' ' << p.y() << " lineto" << endl;	    
		}
		p = helix.at(slast);
		os << p.x() << ' ' << p.y() << " lineto" << endl;
	    }
	    //
	    //  Color codes
	    //
	    if (mTracksWithManyColors) {
		hue = 256.*(1.-pt/1.5);
		if (pt > 1.5 ) hue = 0;
		hls2rgb(hue, lightness, saturation, red, green, blue);
		os << red << ' ' << green << ' ' << blue << " setrgbcolor" << endl;
	    }
	    else if (mTracksWithFewColors) {
		if (pt > 1.0)
		    os << "1 0 0 setrgbcolor" << endl;  
		else if (pt > 0.5)
		    os << "0 1 0 setrgbcolor" << endl;  
		else 
		    os << "0 0 1 setrgbcolor" << endl;
	    }
	    else // B&W
		os << trackGrayLevel << " setgray" << endl;
	    
	    os << lineWidth << " setlinewidth" << endl;  
	    os << "stroke" << endl;  
	    os << "grestore" << endl;
	}
    }
}

void StuPostScript::writeHits(ostream& os, const StEvent* event)
{
    unsigned int i, m, n, h;
    const StTrack  *track;
    const StHit *hit;
    const StTpcHitCollection *theHits = event->tpcHitCollection();
        
    if (mUserTracks) {
	for (i=0; i<mUserTracks->size(); i++) {
	    track = (*mUserTracks)[i];
	    if (track->detectorInfo()) {
		const StPtrVecHit& trackHits = track->detectorInfo()->hits(kTpcId);
		for (m=0; m<trackHits.size(); m++) {
		    hit = trackHits[m];
		    os << (mSideView ? hit->position().z() : hit->position().x())
		       << ' ' << hit->position().y() << " tHit" << endl;
		}
	    }
	}
    }
    else {
	for (n=0; n<theHits->numberOfSectors(); n++) {
	    for (m=0; m<theHits->sector(n)->numberOfPadrows(); m++) { 
		for (h=0; h<theHits->sector(n)->padrow(m)->hits().size(); h++) {
		    hit = theHits->sector(n)->padrow(m)->hits()[h];
		    if (mShowAllHits || (mShowTrackHits && hit->trackReferenceCount())) {
			os << (mSideView ? hit->position().z() : hit->position().x())
			   << ' ' << hit->position().y()
			   << (hit->trackReferenceCount() ? " tHit" : " Hit") << endl;
		    }
		}
	    }
	}
    }
}

void StuPostScript::writeText(ostream& os, const StEvent* event)
{
    const int fontsize = 11;
    const int textGrayLevel = mBlackBackground ? 1 : 0;
    
    os << "/Helvetica findfont" << endl;
    os << fontsize << " scalefont setfont" << endl;
    os << "gsave" << endl;
    os << textGrayLevel << " setgray" << endl;

    //
    //   Run, event, and trigger in upper left
    //
    double x = mllx+fontsize;
    double xn = x + 3.5*fontsize;
    double y = mury-2*fontsize;
    os << x << ' ' << y << " moveto" << endl;
    os << "(run:) show" << endl;
    os << xn << ' ' << y << " moveto" << endl;
    os << "(" << event->runId() << ") show" << endl;
    y -= fontsize+1;
    os << x << ' ' << y << " moveto" << endl;
    os << "(event:) show" << endl;
    os << xn << ' ' << y << " moveto" << endl;
    os << "(" << event->id() << ") show" << endl;
    y -= fontsize+1;
    os << x << ' ' << y << " moveto" << endl;
    os << "(trigger:) show" << endl;
    os << xn << ' ' << y << " moveto" << endl;
    os << "(0x" << hex << event->l1Trigger()->triggerWord() << dec << ") show" << endl;

    //
    //  Number of tracks in upper right
    //
    xn = x + 7*fontsize;
    y = mlly+fontsize;
    os << x << ' ' << y << " moveto" << endl;
    os << "(primary tracks:) show" << endl;
    os << xn << ' ' << y << " moveto" << endl;
    if (event->primaryVertex())
	os << "(" << event->primaryVertex()->numberOfDaughters() << ") show" << endl;
    else
	os << "(0) show" << endl;
    y += fontsize+1;
    os << x << ' ' << y << " moveto" << endl;
    os << "(global tracks:) show" << endl;
    os << xn << ' ' << y << " moveto" << endl;
    os << "(" << event->trackNodes().size() << ") show" << endl;

    //
    //  Primary Vertex
    //
    y = mlly+fontsize;
    os << murx << ' ' << y << " moveto" << endl;
    os << "(vertex: ";
    if (event->primaryVertex())
	os << event->primaryVertex()->position().x() << ' '
	   << event->primaryVertex()->position().y() << ' '
	   << event->primaryVertex()->position().z();
    else
	os << "N/A";
    os << ") dup stringwidth pop -" << fontsize << " exch sub 0 rmoveto show" << endl;
    
    os << "grestore" << endl;
}

#define VALUE(n1, n2, h)  ((h < 60) ? (n1 + (n2-n1)*h/60) : ((h<180) ? \
			  (n2) : ((h<240) ? (n1+(n2-n1)*(240-h)/60) : (n1))))

void StuPostScript::hls2rgb(double hue, double light, double sat,
			    double &red, double &green, double &blue)
{
    //
    //  Input:  hue        = [0-360]
    //          light      = [0-100]
    //          saturation = [0-100]
    //
    //  Output: red, green, blue = [0-1]
    //
    double m1, m2;
    light /= 100.;
    sat   /= 100.;
    
    if (light<= 0.5)
	m2 = light*(1 + sat);
    else
	m2 = light + sat - light*sat;
    m1 = 2*light - m2;
    if ((sat == 0) && (hue == 0)) {
	red = light; green = light; blue = light;
    }
    else {
	red   = VALUE(m1, m2, (((hue+120) > 360) ? (hue-240) : (hue+120)));
	green = VALUE(m1, m2, hue);
	blue  = VALUE(m1, m2, (((hue-120) < 0) ? (hue+240) : (hue-120)));
    }
}
#undef VALUE
