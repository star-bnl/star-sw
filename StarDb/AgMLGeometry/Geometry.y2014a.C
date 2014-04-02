#include "CreateGeometry.h"
TDataSet *CreateTable() 
{
  // Name of the macro including the full path
  TString myname = gInterpreter->GetCurrentMacroName();
  
  // Get an array of TString separated by "/"
  TObjArray *array = myname.Tokenize("/");

  // Get the name of the macro itself
  TString name = ((TObjString *)array->Last())->GetString();

  // Isolate the geometry tag
  array = name.Tokenize(".");

  // Get the geometry tag
  TString tag = ((TObjString *)array->At(1))->GetString();

  // Ensure that we are not looking at the template
  if ( tag=="C" )
    {
      std::cout << "Please use one of the Geometry.[TAG].C macros" << std::endl;
      std::cout << "to instantiate the geometry specified by the " << std::endl;
      std::cout << "given [TAG]." << std::endl;
      return NULL;
    }

  // Return the requested geometry
  return CreateGeometry(tag);

}

