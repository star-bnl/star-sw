//StiHitMapUtilities.h
//M.L. Miller, Yale Software, 7/00

#ifndef StiHitMapUtilities_h
#define StiHitMapUtilities_h

//Structure for map key
struct HitMapKey {
    bool operator==(const HitMapKey&) const;
    int sector;
    int padrow;
    void print() const;
};

//Functor for ordering map key
struct MapKeyLessThan{
    bool operator() (const HitMapKey&, const HitMapKey&) const;
};

ostream& operator<<(ostream&, const HitMapKey&);

struct StidHitLessThan
{
    bool operator() (const StHit*, const StHit*) const;
};

struct StizHitLessThan
{
    bool operator() (const StHit*, const StHit*) const;
};


#endif
