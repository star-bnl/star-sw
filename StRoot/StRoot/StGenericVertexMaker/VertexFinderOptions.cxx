#include <stdexcept>

#include "StGenericVertexMaker/VertexFinderOptions.h"

namespace star_vertex {


bool requiresBeamline(VertexFit_t& vertex_fit)
{
   switch(vertex_fit)
   {
   case VertexFit_t::BeamlineNoFit:
   case VertexFit_t::Beamline1D:
   case VertexFit_t::Beamline3D:
      return true;
   default:
      return false;
   }
}


std::istream& operator>>(std::istream &in, VertexFit_t &vertex_fit)
{
   std::string token;
   in >> token;

   for (auto & c : token) c = toupper(c);

   if (token == "NOBEAMLINE")
   {
      vertex_fit = VertexFit_t::NoBeamline;
   }
   else if (token == "BEAMLINENOFIT")
   {
      vertex_fit = VertexFit_t::BeamlineNoFit;
   }
   else if (token == "BEAMLINE1D")
   {
      vertex_fit = VertexFit_t::Beamline1D;
   }
   else if (token == "BEAMLINE3D")
   {
      vertex_fit = VertexFit_t::Beamline3D;
   }
   else
   {
      vertex_fit = VertexFit_t::Unspecified;
   }

   return in;
}



std::istream& operator>>(std::istream &in, SeedFinder_t &seed_finder)
{
   std::string token;
   in >> token;

   for (auto & c : token) c = toupper(c);

   if (token == "MINUITVF")
   {
      seed_finder = SeedFinder_t::MinuitVF;
   }
   else if (token == "PPVLIKELIHOOD")
   {
      seed_finder = SeedFinder_t::PPVLikelihood;
   }
   else if (token == "TSPECTRUM")
   {
      seed_finder = SeedFinder_t::TSpectrum;
   }
   else
   {
      seed_finder = SeedFinder_t::Unspecified;
   }

   return in;
}


std::ostream& operator<<(std::ostream &out, const VertexFit_t &vertex_fit)
{
   switch(vertex_fit)
   {
   case VertexFit_t::NoBeamline:
      out << "NoBeamline";
      break;
   case VertexFit_t::BeamlineNoFit:
      out << "BeamlineNoFit";
      break;
   case VertexFit_t::Beamline1D:
      out << "Beamline1D";
      break;
   case VertexFit_t::Beamline3D:
      out << "Beamline3D";
      break;
   default:
      out << "Unspecified";
   }

   return out;
}


std::ostream& operator<<(std::ostream &out, const SeedFinder_t &seed_finder)
{
   switch(seed_finder)
   {
   case SeedFinder_t::MinuitVF:
      out << "MinuitVF";
      break;
   case SeedFinder_t::PPVLikelihood:
      out << "PPVLikelihood";
      break;
   case SeedFinder_t::TSpectrum:
      out << "TSpectrum";
      break;
   default:
      out << "Unspecified";
   }

   return out;
}

}
