#ifndef StiMapUtilities_h
#define StiMapUtilities_h

#include <string>
using std::string;
class StiDetector;
class StiHit;
class StTpcHit;
class StiTrackNode;
template<class NodeType> class StiCompositeTreeNode;

//Structure for hit map key
struct HitMapKey {
    bool operator==(const HitMapKey&) const;
    double refangle;
    double position;
};

//Functor for ordering hit map key
struct MapKeyLessThan{
    MapKeyLessThan() : reftolerance(.01), postolerance(.01) {};
    bool operator() (const HitMapKey&, const HitMapKey&) const;
    double reftolerance;
    double postolerance;
};

// Structure for material, shape, or detector name map key
struct NameMapKey {
    NameMapKey(const string& str){ name = str; }
    NameMapKey(){}
    bool operator==(const NameMapKey&) const;
    bool operator<(const NameMapKey&) const;
    string name;
};

//Detector sorter
struct StiDetectorNodePositionLessThan
{
    bool operator() (const StiCompositeTreeNode<StiDetector> *, const StiCompositeTreeNode<StiDetector> *) const;
};

struct SetHitUsed
{
    void operator() (StiTrackNode&);
};
struct SetHitUnused
{
    void operator() (StiTrackNode&);
};

#endif


