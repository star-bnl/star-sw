#ifndef VertexFinderOptions_h
#define VertexFinderOptions_h

#include <iostream>
#include <string>


namespace star_vertex {

/// Options used to define the type of vertex fit performed in a concrete
/// implementation
enum class VertexFit_t : int
{
   Unspecified, NoBeamline, BeamlineNoFit, Beamline1D, Beamline3D
};

/// Options to select vertex seed finder
enum class SeedFinder_t : int
{
   Unspecified, MinuitVF, PPVLikelihood, TSpectrum
};


bool requiresBeamline(VertexFit_t& vertex_fit);


std::istream& operator>>(std::istream &in, VertexFit_t &vertex_fit);
std::istream& operator>>(std::istream &in, SeedFinder_t &seed_finder);

std::ostream& operator<<(std::ostream &out, const VertexFit_t &vertex_fit);
std::ostream& operator<<(std::ostream &out, const SeedFinder_t &seed_finder);

}

#endif
