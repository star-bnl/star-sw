//TpcMapUtilities.h
//M.L. Miller, Yale Software, 4/13

#ifndef TpcMapUtilities_h
#define TpcMapUtilities_h

class StHit;
class StiHit;

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

//Structures for detector map key
struct DetectorMapKey {
  bool operator==(const DetectorMapKey&) const;
  bool operator<(const DetectorMapKey&) const;
  double position;
  double refangle;
  double z;
};

// Structure for material, shape, or detector name map key
struct NameMapKey {
    NameMapKey::NameMapKey(const char *str){ strncpy(name, str, 127); }
    NameMapKey::NameMapKey(){}
    bool operator==(const NameMapKey&) const;
    bool operator<(const NameMapKey&) const;
    char name[128];
};

//Functors for ordering hits
struct StHitRadiusLessThan
{
    bool operator() (const StHit*, const StHit*) const;
};

struct StHitRadiusGreaterThan
{
    bool operator() (const StHit*, const StHit*) const;
};

struct StiHitLessThan
{
    bool operator() (const StiHit*, const StiHit*) const;
};

struct StidHitLessThan
{
    bool operator() (const StiHit*, const StiHit*) const;
};

struct StizHitLessThan
{
    bool operator() (const StiHit*, const StiHit*) const;
};


#endif


