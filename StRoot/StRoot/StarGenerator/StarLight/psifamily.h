///////////////////////////////////////////////////////////////////////////
//
//    Copyright 2010
//
//    This file is part of starlight.
//
//    starlight is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    starlight is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with starlight. If not, see <http://www.gnu.org/licenses/>.
//
///////////////////////////////////////////////////////////////////////////
//
// File and Version Information:
// $Rev::                             $: revision of last commit
// $Author: jwebb $: author of last commit
// $Date: 2012/11/27 22:27:32 $: date of last commit
//
// Description:
//
//
//
///////////////////////////////////////////////////////////////////////////


#ifndef PSIFAMILY_H
#define PSIFAMILY_H

#include <vector>

#include "starlightconstants.h"
#include "beambeamsystem.h"
#include "randomgenerator.h"
#include "gammaavm.h"


class psiFamily : public Gammaanarrowvm
{
	public:
		psiFamily(inputParameters& input, beamBeamSystem& bbsystem);
		~psiFamily();
		double getTheta(starlightConstants::particleTypeEnum ipid);
		double getDaughterMass(starlightConstants::particleTypeEnum &ipid);
	private:
		double _width;
		double _mass;
};

#endif //PSIFAMILY_H
