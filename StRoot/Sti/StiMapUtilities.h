//TpcMapUtilities.h
//M.L. Miller, Yale Software, 4/13

#ifndef TpcMapUtilities_h
#define TpcMapUtilities_h

class StiHit;

//Structure for hit map key
struct HitMapKey {
    bool operator==(const HitMapKey&) const;
    unsigned int sector;
    unsigned int padrow;
};

//Functor for ordering hit map key
struct MapKeyLessThan{
    bool operator() (const HitMapKey&, const HitMapKey&) const;
};

//Structure for detector map key
struct DetectorMapKey {
  bool operator==(const DetectorMapKey&) const;
  double position;
  double refangle;
  double z;
};
 
//Functor for ordering detector map key
struct DetectorMapKeyLessThan {
  bool operator() (const DetectorMapKey&, const DetectorMapKey&) const;
};

//Non member functions
ostream& operator<<(ostream&, const HitMapKey&);
ostream& operator<<(ostream&, const DetectorMapKey&);

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


