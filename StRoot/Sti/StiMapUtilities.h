//TpcMapUtilities.h
//M.L. Miller, Yale Software, 4/13

#ifndef TpcMapUtilities_h
#define TpcMapUtilities_h

class StiHit;

//Structure for hit map key
struct HitMapKey {
    bool operator==(const HitMapKey&) const;
    double refangle;
    double position;
};

//Functor for ordering hit map key
struct MapKeyLessThan{
    bool operator() (const HitMapKey&, const HitMapKey&) const;
};

//Structure for detector map key
struct DetectorMapKey {
  bool operator==(const DetectorMapKey&) const;
  bool operator<(const DetectorMapKey&) const;
  double position;
  double refangle;
  double z;
};

// Structure for material map key
struct MaterialMapKey {
    MaterialMapKey::MaterialMapKey(const char *str):name(str){}
    MaterialMapKey::MaterialMapKey():name(0){}
    bool operator==(const MaterialMapKey&) const;
    bool operator<(const MaterialMapKey&) const;
    const char *name;
};

//Non member functions
ostream& operator<<(ostream&, const HitMapKey&);
ostream& operator<<(ostream&, const DetectorMapKey&);
ostream& operator<<(ostream&, const MaterialMapKey&);

//Functors for ordering hits
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


