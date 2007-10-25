//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// March 19, 2006
//

#include "Histogram.hh"

Histogram::Histogram(const string& name, const string& title, int nbins, float min, float max)
{
  fName = name;
  fTitle = title;
  fNbins = nbins;
  fMin = min;
  fMax = max;
  fEntries = 0;
  fBins.resize(nbins+2);
}

int Histogram::fill(float x, float w)
{
  ++fEntries;
  if (x < fMin) {
    fBins[0] += w;
    return 0;
  }
  if (!(x < fMax)) {
    fBins[fNbins+1] += w;
    return fNbins+1;
  }
  int i = (int)((x-fMin)/(fMax-fMin)*fNbins);
  fBins[i+1] += w;
  return i+1;
}

void Histogram::reset()
{
  fEntries = 0;
  std::fill(fBins.begin(), fBins.end(), 0);
}

//
// Serialization
//
istream& Histogram::read(istream& in)
{
  // Read name
  unsigned int size;
  if (!in.read((char*)&size, sizeof(size))) return in;
  char name[size];
  if (!in.read(name, size)) return in;
  fName.assign(name, size);
  // Read title
  if (!in.read((char*)&size, sizeof(size))) return in;
  char title[size];
  if (!in.read(title, size)) return in;
  fTitle.assign(title, size);
  // Read number of bins
  if (!in.read((char*)&fNbins, sizeof(fNbins))) return in;
  // Read min
  if (!in.read((char*)&fMin, sizeof(fMin))) return in;
  // Read max
  if (!in.read((char*)&fMax, sizeof(fMax))) return in;
  // Read entries
  if (!in.read((char*)&fEntries, sizeof(fEntries))) return in;
  // Read bin contents
  fBins.resize(fNbins+2);
  for (unsigned int i = 0; i < fBins.size(); ++i)
    if (!in.read((char*)&fBins[i], sizeof(float))) return in;
  return in;
}

ostream& Histogram::write(ostream& out)
{
  // Write name
  unsigned int size;
  size = fName.size();
  if (!out.write((char*)&size, sizeof(size))) return out;
  if (!out.write(fName.c_str(), size)) return out;
  // Write title
  size = fTitle.size();
  if (!out.write((char*)&size, sizeof(size))) return out;
  if (!out.write(fTitle.c_str(), size)) return out;
  // Write number of bins
  if (!out.write((char*)&fNbins, sizeof(fNbins))) return out;
  // Write min
  if (!out.write((char*)&fMin, sizeof(fMin))) return out;
  // Write max
  if (!out.write((char*)&fMax, sizeof(fMax))) return out;
  // Write entries
  if (!out.write((char*)&fEntries, sizeof(fEntries))) return out;
  // Write bin contents
  for (unsigned int i = 0; i < fBins.size(); ++i)
    if (!out.write((char*)&fBins[i], sizeof(float))) return out;
  return out;
}
