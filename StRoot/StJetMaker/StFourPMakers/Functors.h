//Functors.h
//M.L. Miller (MIT)
//9/04

#ifndef STJMFunctors_HH
#define STJMFunctors_HH

struct EmcHitLT
{
    bool operator()(const EmcHit* lhs, const EmcHit* rhs) const;
};

#endif
