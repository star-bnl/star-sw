
/** \file
 *  Create Millepede-II C-binary record.
 *
 *  \author Gero Flucke, University Hamburg, 2006
 *
 *  \copyright
 *  Copyright (c) 2009 - 2015 Deutsches Elektronen-Synchroton,
 *  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version. \n\n
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details. \n\n
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program (see the file COPYING.LIB for more
 *  details); if not, write to the Free Software Foundation, Inc.,
 *  675 Mass Ave, Cambridge, MA 02139, USA.
 */

/**
 *  author    : Gero Flucke, University Hamburg, 2006
 *  date      : October 2006
 *  $Revision: 1.3 $
 *  $Date: 2007/04/16 17:47:38 $
 *  (last update by $Author: flucke $)
 */

#include "Mille.h"

#include <fstream>
#include <iostream>

//___________________________________________________________________________

/// Opens outFileName (by default as binary file).
/**
 * \param[in] outFileName  file name
 * \param[in] asBinary     flag for binary
 * \param[in] writeZero    flag for keeping of zeros
 */
Mille::Mille(const char *outFileName, bool asBinary, bool writeZero) : 
  myOutFile(outFileName, (asBinary ? (std::ios::binary | std::ios::out) : std::ios::out)),
  myAsBinary(asBinary), myWriteZero(writeZero), myBufferPos(-1), myHasSpecial(false)
{
  // Instead myBufferPos(-1), myHasSpecial(false) and the following two lines
  // we could call newSet() and kill()...
  myBufferInt[0]   = 0;
  myBufferFloat[0] = 0.;

  if (!myOutFile.is_open()) {
    std::cerr << "Mille::Mille: Could not open " << outFileName 
	      << " as output file." << std::endl;
  }
}

//___________________________________________________________________________
/// Closes file.
Mille::~Mille()
{
  myOutFile.close();
}

//___________________________________________________________________________
/// Add measurement to buffer.
/**
 * \param[in]    NLC    number of local derivatives
 * \param[in]    derLc  local derivatives
 * \param[in]    NGL    number of global derivatives
 * \param[in]    derGl  global derivatives
 * \param[in]    label  global labels
 * \param[in]    rMeas  measurement (residuum)
 * \param[in]    sigma  error
 */
void Mille::mille(int NLC, const float *derLc,
		  int NGL, const float *derGl, const int *label,
		  float rMeas, float sigma)
{
  if (sigma <= 0.) return;
  if (myBufferPos == -1) this->newSet(); // start, e.g. new track
  if (!this->checkBufferSize(NLC, NGL)) return;

  // first store measurement
  ++myBufferPos;
  myBufferFloat[myBufferPos] = rMeas;
  myBufferInt  [myBufferPos] = 0;

  // store local derivatives and local 'lables' 1,...,NLC
  for (int i = 0; i < NLC; ++i) {
    if (derLc[i] || myWriteZero) { // by default store only non-zero derivatives
      ++myBufferPos;
      myBufferFloat[myBufferPos] = derLc[i]; // local derivatives
      myBufferInt  [myBufferPos] = i+1;      // index of local parameter
    }
  }

  // store uncertainty of measurement in between locals and globals
  ++myBufferPos;
  myBufferFloat[myBufferPos] = sigma;
  myBufferInt  [myBufferPos] = 0;

  // store global derivatives and their labels
  for (int i = 0; i < NGL; ++i) {
    if (derGl[i] || myWriteZero) { // by default store only non-zero derivatives
      if ((label[i] > 0 || myWriteZero) && label[i] <= myMaxLabel) { // and for valid labels
	++myBufferPos;
	myBufferFloat[myBufferPos] = derGl[i]; // global derivatives
	myBufferInt  [myBufferPos] = label[i]; // index of global parameter
      } else {
	std::cerr << "Mille::mille: Invalid label " << label[i] 
		  << " <= 0 or > " << myMaxLabel << std::endl; 
      }
    }
  }
}

//___________________________________________________________________________
/// Add special data to buffer.
/**
 * \param[in]    nSpecial   number of floats/ints
 * \param[in]    floatings  floats
 * \param[in]    integers   ints
 */
void Mille::special(int nSpecial, const float *floatings, const int *integers)
{
  if (nSpecial == 0) return;
  if (myBufferPos == -1) this->newSet(); // start, e.g. new track
  if (myHasSpecial) {
    std::cerr << "Mille::special: Special values already stored for this record."
	      << std::endl; 
    return;
  }
  if (!this->checkBufferSize(nSpecial, 0)) return;
  myHasSpecial = true; // after newSet() (Note: MILLSP sets to buffer position...)

  //  myBufferFloat[.]  | myBufferInt[.]
  // ------------------------------------
  //      0.0           |      0
  //  -float(nSpecial)  |      0
  //  The above indicates special data, following are nSpecial floating and nSpecial integer data.

  ++myBufferPos; // zero pair
  myBufferFloat[myBufferPos] = 0.;
  myBufferInt  [myBufferPos] = 0;

  ++myBufferPos; // nSpecial and zero
  myBufferFloat[myBufferPos] = -nSpecial; // automatic conversion to float
  myBufferInt  [myBufferPos] = 0;

  for (int i = 0; i < nSpecial; ++i) {
    ++myBufferPos;
    myBufferFloat[myBufferPos] = floatings[i];
    myBufferInt  [myBufferPos] = integers[i];
  }
}

//___________________________________________________________________________
/// Reset buffers, i.e. kill derivatives accumulated for current set.
void Mille::kill()
{
  myBufferPos = -1;
}

//___________________________________________________________________________
/// Write buffer (set of derivatives with same local parameters) to file.
void Mille::end()
{
  if (myBufferPos > 0) { // only if anything stored...
    const int numWordsToWrite = (myBufferPos + 1)*2;

    if (myAsBinary) {
      myOutFile.write(reinterpret_cast<const char*>(&numWordsToWrite), 
		      sizeof(numWordsToWrite));
      myOutFile.write(reinterpret_cast<char*>(myBufferFloat), 
		      (myBufferPos+1) * sizeof(myBufferFloat[0]));
      myOutFile.write(reinterpret_cast<char*>(myBufferInt), 
		      (myBufferPos+1) * sizeof(myBufferInt[0]));
    } else {
      myOutFile << numWordsToWrite << "\n";
      for (int i = 0; i < myBufferPos+1; ++i) {
	myOutFile << myBufferFloat[i] << " ";
      }
      myOutFile << "\n";
      
      for (int i = 0; i < myBufferPos+1; ++i) {
	myOutFile << myBufferInt[i] << " ";
      }
      myOutFile << "\n";
    }
  }
  myBufferPos = -1; // reset buffer for next set of derivatives
}

//___________________________________________________________________________
/// Initialize for new set of locals, e.g. new track.
void Mille::newSet()
{
  myBufferPos = 0;
  myHasSpecial = false;
  myBufferFloat[0] = 0.0;
  myBufferInt  [0] = 0;   // position 0 used as error counter
}

//___________________________________________________________________________
/// Enough space for next nLocal + nGlobal derivatives incl. measurement?
/**
 * \param[in]   nLocal  number of local derivatives
 * \param[in]   nGlobal number of global derivatives
 * \return      true if sufficient space available (else false)
 */
bool Mille::checkBufferSize(int nLocal, int nGlobal)
{
  if (myBufferPos + nLocal + nGlobal + 2 >= myBufferSize) {
    ++(myBufferInt[0]); // increase error count
    std::cerr << "Mille::checkBufferSize: Buffer too short (" 
	      << myBufferSize << "),"
	      << "\n need space for nLocal (" << nLocal<< ")"
	      << "/nGlobal (" << nGlobal << ") local/global derivatives, " 
	      << myBufferPos + 1 << " already stored!"
	      << std::endl;
    return false;
  } else {
    return true;
  }
}
