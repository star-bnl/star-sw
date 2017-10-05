#if defined(__CINT__) || defined(__CLING__)
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
// wait till dictionary will be created for StarClassLibrary template classes
#pragma link C++ class StTpcCoordinate!-;
#pragma link C++ class StTpcLocalCoordinate!-;
#pragma link C++ class StTpcLocalDirection!-;
#pragma link C++ class StTpcLocalSectorAlignedCoordinate!-;
#pragma link C++ class StTpcLocalSectorAlignedDirection!-;
#pragma link C++ class StTpcLocalSectorCoordinate!-;
#pragma link C++ class StTpcLocalSectorDirection!-;
#pragma link C++ class StGlobalCoordinate!-;
#pragma link C++ class StGlobalDirection!-;
#pragma link C++ class StTpcPadCoordinate!-;
#pragma link C++ class StTpcCoordinateTransform!-;

/* #pragma link C++ class StSvtCoordinateTransform!-; */
/* #pragma link C++ class StSvtLocalCoordinate!-; */
/* #pragma link C++ class StSvtWaferCoordinate!-; */

#pragma link C++ function operator<<(ostream&, const StTpcLocalCoordinate&);
#pragma link C++ function operator<<(ostream&, const StTpcLocalDirection&);
#pragma link C++ function operator<<(ostream&, const StTpcLocalSectorAlignedCoordinate&);
#pragma link C++ function operator<<(ostream&, const StTpcLocalSectorAlignedDirection&);
#pragma link C++ function operator<<(ostream&, const StTpcLocalSectorCoordinate&);
#pragma link C++ function operator<<(ostream&, const StTpcLocalSectorDirection&);
#pragma link C++ function operator<<(ostream&, const StGlobalCoordinate&);
#pragma link C++ function operator<<(ostream&, const StGlobalDirection&);
#pragma link C++ function operator<<(ostream&, const StTpcPadCoordinate&);

#pragma link C++ class StTpcCoordinateTransform!-;
#ifndef __CLING__
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate&, StTpcPadCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcPadCoordinate&, StTpcLocalSectorCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate&, StTpcPadCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcPadCoordinate&, StTpcLocalCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate&, StTpcLocalSectorCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate&, StTpcLocalCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate&, StTpcLocalSectorAlignedCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedCoordinate&, StTpcLocalSectorCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection&, StTpcLocalDirection&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection&, StTpcLocalSectorAlignedDirection&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorAlignedDirection&, StTpcLocalSectorDirection&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorCoordinate&, StGlobalCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StGlobalCoordinate& ,StTpcLocalSectorCoordinate&,Int_t,Int_t);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalSectorDirection&, StGlobalDirection&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StGlobalDirection& ,StTpcLocalSectorDirection&,Int_t, Int_t);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalCoordinate&, StGlobalCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StGlobalCoordinate&, StTpcLocalCoordinate&,Int_t, Int_t);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcLocalDirection&, StGlobalDirection&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StGlobalDirection&, StTpcLocalDirection&,Int_t, Int_t);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StTpcPadCoordinate&, StGlobalCoordinate&);
#pragma link C++ function   StTpcCoordinateTransform::operator()(const StGlobalCoordinate&, StTpcPadCoordinate&);
#endif
#pragma link C++ enum  Prime;
#pragma link C++ enum  DistortSelect;
#endif
