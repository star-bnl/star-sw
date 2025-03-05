/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef KFPHISTOGRAMSET
#define KFPHISTOGRAMSET

#include "KFPHistogram1D.h"
#include "KFParticle.h"

/** @class KFPHistogramSet
 ** @brief A set of histograms collected at the external devise.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class defines a set of histograms to be collect in the environment,
 ** where ROOT is not available, for example at Intel Xeon Phi cards.
 ** It contains a set of one-dimensional histograms.
 ** Also,Calculates the needed amount of memory to be allocated
 **/

class KFPHistogramSet
{
 public:
  KFPHistogramSet(int iPart=0);
  ~KFPHistogramSet() {}
  
  void Fill(const KFParticle& particle);
  
  inline int GetNHisto1D() const { return NHisto1D; } ///< Returns a number of one dimensional histograms in the set.
  inline int DataSize() const 
  {
    int dataSize = 0;
    for(int i=0; i<NHisto1D; i++)
      dataSize += fKFPHistogram1D[i].DataSize();
    return dataSize;
  } ///< Returns the size of the memory in blocks of integer (or 4 bytes, or 32 bits) to be allocated for the histogram set. \see KFPHistogram, where memory is allocated.
  KFPHistogram1D GetHistogram1D(int iHistogram) const { return fKFPHistogram1D[iHistogram]; } ///< Returns one dimensional histogram with index "iHistogram".
  
  /**Sets bin content of the histogram "iHisto" to a given value.
   ** \param[in] iHisto - index of the histogram in the set
   ** \param[in] iBin - number of the bin
   ** \param[in] value - value to be set
   **/
  inline void SetHisto1DBinContent(int iHisto, int iBin, int value)   { fKFPHistogram1D[iHisto].SetBinContent(iBin,value); }
  
  inline void operator += ( const KFPHistogramSet &h )
  {
    for(int i=0; i<NHisto1D; i++)
      fKFPHistogram1D[i] += h.fKFPHistogram1D[i];
  } ///< Adds all histograms bin-by-bin from the histogram set "h" to the current set.
  
  void SetHistogramMemory(int* pointer);
  
 private:
  static const int NHisto1D = 17; ///< Number of histogram per each particle specie.
  KFPHistogram1D fKFPHistogram1D[NHisto1D]; ///< A set of the one dimensional histograms.
};

#endif
