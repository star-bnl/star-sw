#ifndef STARKINEGENERATOR_H
#define STARKINEGENERATOR_H

/* $Id: StarKineGenerator.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

///////////////////////////////////////////////////////////
//                                                       //
//  Class to generate kinematic tracks for the MC        //
//                                                       //
///////////////////////////////////////////////////////////

#include <TArrayF.h>
#include <TLorentzVector.h>
#include <TMCProcess.h>

// #include "StarRndm.h"

#include "StarGenerator.h"

//class StarVertexGenerator;
//class StarCollisionGeometry;
//class StarStack;


class StarKineGenerator : public StarGenerator
{

 public:
    StarKineGenerator();
    StarKineGenerator(Int_t npart);
    StarKineGenerator(const StarGenerator &gen);

    virtual ~StarKineGenerator();

    virtual void Generate(void);

 protected:

    ClassDef(StarKineGenerator,1) // class for kine track event generators
};

#endif
