//StiRootDisplayManager.h
//M.L. Miller (Yale Software)
//04/01

//This  class manages the Sti display

#ifndef StiRootDisplayManager_HH
#define StiRootDisplayManager_HH

#include <string>
#include <map>

#include "Sti/StiDisplayManager.h"

using std::map;
using std::multimap;
using std::string;

class TCanvas;
class TShape;
class TVolume;
class StiDrawable;

class StiRootDisplayManager : public StiDisplayManager
{
public:
    friend class nobody;
    
    static const int skeletonView;
    static const int zoomSkeletonView;

    typedef multimap<string, StiDrawable*> stidrawablemap;
    typedef stidrawablemap::value_type stiDrawableMapValType;
    //Singleton access
    static StiDisplayManager* instance(TCanvas* c1=0);
    void addDrawable(StiDrawable*);
    void reset();
    void cd();
    void update();
    void draw();
    void setVisible(bool value);
    void setVisible(const StiDrawable*, bool value);
    void setVisible(const char * name, bool value);
    void setView(int view);
    void print() const;

protected:
    
private:

    static StiRootDisplayManager* sinstance;

    //dx, dy, and dz define the volume within which objects are drawn.
    //The volume is a rectangle of lengths 2*dx, 2*dy, 2*dz
    enum StiCanvasSize {kXmin=200, kXmax=600, kYmin=100, kYmax=500};
    enum StiMainVolumeSize {kdx=200, kdy=200, kdz=240};
    
    StiRootDisplayManager(TCanvas*);
    StiRootDisplayManager(); //Not implemented
    StiRootDisplayManager(const StiRootDisplayManager&); //Not implemented
    
    virtual ~StiRootDisplayManager();
    
    void setup();

    TCanvas* mcanvas;
    TShape* mzone;
    TVolume* mnode;

    stidrawablemap mmap;
    
};

#endif
