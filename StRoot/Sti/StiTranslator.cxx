//StiTranslator.cxx
//M.L. Miller (Yale Software)
//04/01

//StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

//Sti
#include "StiTranslator.h"

StiTranslator::StiTranslator()
{
    mtpctransformer = new StTpcCoordinateTransform(gStTpcDb);
}

StiTranslator::~StiTranslator()
{
    delete mtpctransformer;
    mtpctransformer = 0;
}
