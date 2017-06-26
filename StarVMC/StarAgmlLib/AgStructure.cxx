#include "AgStructure.h"
#include "AgModule.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "TList.h"
#include "TBrowser.h"
#include "TROOT.h"

#include "StMessMgr.h"


TFolder AgStructure::_top("DETP","Detector parameters");
TFolder *_commands = 0;

struct __AgStructure_Initialization__
{
  __AgStructure_Initialization__() 
  {
    // Register a .commands folder at the top of the structure
    AgStructure::top().AddFolder(".commands","AgDetp manipulation at end of fill");
    // Register structures folder with all browsers 
    TIter next( gROOT->GetListOfBrowsers() );
    TBrowser *b = 0;
    while ((b=(TBrowser*)next()))
      {
	b->Add( &AgStructure::top() );
      }
  }
} __agstructure_initialziation__;

TString folderName( const Char_t *moduleName )
{
  TString name(moduleName);
  return name(0,4);
}

TFolder *getFolder()
{
  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module = folderName(module);
  
  // Get the sub folder for this module
  TFolder *folder = (TFolder*)AgStructure::top().FindObject(module.Data());  

  return folder;
}

AgStructure::AgStructure():TNamed()
{
  _inUse=false;
}

AgStructure::AgStructure(const Char_t *name, const Char_t *title)
  : TNamed(name,title)
{
  _inUse = false;
  _use   = 0;
}

void AgStructure::fill()
{

  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module = folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // If the folder doesn't exist, create it
  if ( !folder ) 
    {
      folder = _top.AddFolder(module.Data(),Form("Parameters for %s",module.Data()));
    }

  // Set the AgML index
  _index = Count() + 1;

  //
  // TODO:
  //
  // Check for AgDetpNew/Add commands which have been issued for this
  // structure in this module.  If we find one, overwrite the specified
  // data memeber.
  //
  
  // Create a clone of this structure and register it
  // with the folder
  AgStructure *_clone = (AgStructure *)Clone( GetName() );
  _clone -> _index = _index; // why do I need to do this?

  // Add the clone to the folder
  folder->Add(_clone);

  // And Use() the clone (ensures _inUse flag is set, initiates AgDetp copy)
  Use("_index",_index);

}

Int_t AgStructure::Count()
{
  // Get the folder
  TFolder *folder = getFolder();
  
  // Iterate over all objects w/in and count the number of
  // instancs of this structure
  TString name = GetName();
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Int_t count = 0;
  while ( (obj=next()) )
    {
      if ( name == obj->GetName() )
	{
	  count++;
	}
    }
  return count;    
}

Bool_t AgStructure::Last()
{

  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  //
  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  //
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;
  
  Int_t highest = -999;

  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();//obj->Class()->GetName();
      if ( oname != _class->GetName() ) 
	{
	  continue;
	}

      // Recast to the concrete structure
      AgStructure *_struct = (AgStructure *)obj;

      // Mark all other data structures as not in use
      _struct->_inUse = false;

      if ( _struct->_index > highest )
	{
	  result = CopyDataMembers( _struct );
	  _struct -> _inUse = true;
	  highest = _struct -> _index;
	}

    }
  
  _use++;
  return result;

}


Bool_t AgStructure::Use()
{

  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  //
  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  //
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;

  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();
      TString cname = _class->GetName();
      if ( !cname.Contains( oname ) )
	{
	  continue;
	}

      // Recast to the concrete structure
      AgStructure *_struct = (AgStructure *)obj;

      // Mark all other data structures as not in use
      _struct->_inUse = false;

      if ( _struct->_index == 1 )
	{
	  result = CopyDataMembers( _struct );
	  _struct -> _inUse = true;
	}

    }
  
  _use++;
  return result;

}

#if 1
Bool_t AgStructure::Use( const Char_t *member, Int_t value )
{

  Int_t _current = -999;
  
  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  //
  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  //
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;

  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();
      TString cname = _class->GetName();
      if ( !cname.Contains( oname ) )
	{
	  continue;
	}

      // Obtain the member from the stored structure
      AgStructure *_struct = (AgStructure *)obj;
      _struct->_inUse = false;

      if ( _struct->GetMember(member,_current) )
	{
	  if ( _current == value ) result = CopyDataMembers( _struct );
	  _struct -> _inUse = result;
	}
      else
	{
	  AgModule::module()->Warning( GetName(), Form("Invalid request for member %s",member) );
	  //Warning(GetName(),Form("request for %s=(int)%i failed.",member,value));
	}

    }

  // Could find no matching structure so we return false,
  // and leave the structure in it's current state.

  _use++;
  return result;

}


Bool_t AgStructure::Use( const Char_t *member, Float_t value )
{

  // Early return if value matches current state
  Float_t _current = -999;

  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;
  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();
      TString cname = _class->GetName();
      if ( !cname.Contains( oname ) )
	{
	  continue;
	}

      // Obtain the member from the stored structure
      AgStructure *_struct = (AgStructure *)obj;
      _struct -> _inUse = false;

      if ( _struct->GetMember(member,_current) )
	{
	  if ( _current == value ) result = CopyDataMembers( _struct );
	  _struct -> _inUse = result;
	}

    }

  if ( !result ) // Falls through to Int_t
    {
      // try int
      Int_t ivalue = value;
      return Use(member,ivalue);
    }

  _use++;
  return result;

}
#endif

Bool_t AgStructure::Use( const Char_t *member, Double_t value )
{

  // Early return if value matches current state
  Double_t _current = -999;

  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;
  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();
      TString cname = _class->GetName();
      if ( !cname.Contains( oname ) )
	{
	  continue;
	}

      // Obtain the member from the stored structure
      AgStructure *_struct = (AgStructure *)obj;
      _struct -> _inUse = false;

      if ( _struct->GetMember(member,_current) )
	{
	  if ( _current == value ) result = CopyDataMembers( _struct );
	  _struct -> _inUse = result;
	}

    }

  if ( !result ) { // Fall through to float
    // Try float
    Float_t fvalue = value;
    return Use(member,fvalue);
  }

  _use++;
  return result;

}

Bool_t AgStructure::Use( const Char_t *member, TString value )
{

  // Early return if value matches current state
  TString _current = "nada";
  
  // Get the name of the current module
  TString module = AgModule::module()->GetName();
  module=folderName(module);

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)_top.FindObject(module.Data());

  // Get the class of the current structure
  TClass *_class = TClass::GetClass( GetName() );

  // Resolve any AgDetp instructions for this structure
  AgDetpSet();

  // Loop over all objects in the folder until we find
  // one which has the requested value for the requested
  // data member.  When we find it, copy the values
  // from all members.
  TIter next( folder->GetListOfFolders() );
  TObject *obj = 0;
  Bool_t result = false;

  while ( (obj=next()) )
    {
      
      // Skip objects which do not match this class
      TString oname = obj->GetName();
      TString cname = _class->GetName();


      if ( !cname.Contains( oname ) )
	{
	  continue;
	}

      // Obtain the member from the stored structure
      AgStructure *_struct = (AgStructure *)obj;
      _struct -> _inUse = false;

      if ( _struct->GetMember(member,_current) )
	{
	  if ( _current == value ) result = CopyDataMembers( _struct );
	  _struct -> _inUse = result;
	}

    }

  _use++;
  return result;

}



Bool_t AgStructure::CopyDataMembers( AgStructure *other, const Char_t *only, Bool_t verbose )
{

  TString module;
  if ( AgModule::module() ) {
    module = AgModule::module()->GetName();
  };

  // Get the class of the current structure
  TClass *_class  = TClass::GetClass( GetName() );
  TClass *_oclass = TClass::GetClass( other->GetName() );

  TString cname = _class -> GetName();
  if ( cname != _oclass->GetName() )
    {
      return false; // we should probably assert this, but for hubris
    }

  // Copy the AgML index for this structure
  _index = other->_index;

  // Iterate over data members, determine their type, and copy them
  TIter next( _class -> GetListOfDataMembers() );
  TDataMember *m = 0;
  while ( (m=(TDataMember*)next()) )
    {

      TString _mname = m->GetName();
      TString _mtype = m->GetTypeName();
      TString _sname = GetName(); _sname = _sname(0,4); _sname.ToLower();
      
      if ( only )
	{ // Only selected data member
	  TString fqname = TString(GetName()) + "." + _mname;
	  if ( fqname!=only ) continue;
	}

      if ( _mtype == "Int_t" or _mtype == "int" )
	{
	  Int_t _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	  if (verbose) {
	    AgModule::module()->Info
	      ( module, Form("  [ Runtime data assignment %s.%s = %i (int) ]",_sname.Data(),_mname.Data(),_value) );
	  }
	}
      if ( _mtype == "Float_t" or _mtype == "float" )
	{
	  Float_t _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	  if (verbose) {
	    AgModule::module()->Info
	      ( module, Form("  [ Runtime data assignment %s.%s = %g (float) ]",_sname.Data(),_mname.Data(),_value) );
	  }
	}
      if ( _mtype == "Double_t" or _mtype == "double" )
	{
	  Double_t _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	  if (verbose) {
	    AgModule::module()->Info
	      ( module, Form("  [ Runtime data assignment %s.%s = %g (double) ]",_sname.Data(),_mname.Data(),_value) );
	  }
	}
      if ( _mtype == "TString" )
	{
	  TString _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	  if (verbose) {
	    AgModule::module()->Info
	      ( module, Form("  [ Runtime data assignment %s.%s = %s (string) ]",_sname.Data(),_mname.Data(),_value.Data()) );
	  }

	}
      if ( _mtype == "Array_t<Int_t>" )
	{
	  Array_t<Int_t> _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	}
      if ( _mtype == "Array_t<Float_t>" )
	{
	  Array_t<Float_t> _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	}
      if ( _mtype == "Array_t<Double_t>" )
	{
	  Array_t<Double_t> _value;
	  other->GetMember( _mname, _value );
	  SetMember( _mname, _value );
	}      
    }   

  // Copy the title of the structure
  SetTitle( other->GetTitle() );

  return true;
}


std::ostream &AgStructure::Out( std::ostream &out )
{

  // Get the class description for this structure
  TClass *_class = TClass::GetClass( GetName() );
  out << Form("# %s",GetTitle()) << std::endl;
  out << Form("Structure %s:",GetName()) << std::endl;
  out << " " << this << std::endl;
  out << std::endl;
  TIter next( _class->GetListOfDataMembers() );
  TDataMember *member = 0;

  while ( (member=(TDataMember*)next()) )
    {
      TString  name    = member->GetName();
      TString  comment = member->GetTitle();
      TString  type    = member->GetTypeName();
      Long_t   offset  = _class -> GetDataMemberOffset( member->GetName() );
      void    *address = (void *)(((Long_t)this) + offset); // Address of the type (but why 6?)

      if ( !comment.Contains("!$") ) continue;
      comment = comment.ReplaceAll("!$","##");

      out << "    ";
      out << member->GetTypeName();
      out << " ";
      out << member->GetName();
      out << " ";

      assert(offset);

      if ( type.Contains("Array_t") )
	{
	  if ( type=="Array_t<Int_t>"||type=="Array_t<int>" )
	    {
	      Array_t<Int_t> array;
	      GetMember( member->GetName(), array );
	      out << "= " << array;
	    }
	  if ( type=="Array_t<Float_t>"||type=="Array_t<float>" )
	    {
	      Array_t<Float_t> array;
	      GetMember( member->GetName(), array );
	      out << "= " << array;
	    }
	  if ( type=="Array_t<Double_t>"||type=="Array_t<double>" )
	    {
	      Array_t<Double_t> array;
	      GetMember( member->GetName(), array );
	      out << "= " << array;
	    }
	}
      else
	{ 
	  if ( type=="Int_t"||type=="int" ) 
	    {
	      Int_t value = *((Int_t*)address);
	      out << "= " << value << "/i";
	      //out << "@ " << address;
	    }
	  if ( type=="Float_t"||type=="float" )
	    {
	      Float_t value = *((Float_t*)address);	      
	      out << "= " << value << "/f";
	    }
	  if ( type=="Double_t"||type=="double" )
	    {
	      Double_t value = *((Double_t*)address);
	      out << "= " << value << "/d";
	      //out << "@ " << address;
	    }
	}
      out << " ";      
      out << comment.Data();
      out << std::endl;
      
    }
  out << std::endl;

  return out;
}
























//
// ===========================================================================================
//
//                         Get/SetMember ( member, value )
//
// Below you will find 12 very similar member functions which I could not figure out how
// to template.  They provide Get and Set methods for any data member (public,private
// or protected) which is defined on the user's instance of this class.  It is important 
// that changes to any one of the methods is propagated to the other 11.  
//
//                      Y o u   h a v e   b e e n   w a r n e d.
//
// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Int_t &value )	
{									

  if ( TString(name) == "_index" )
    {
      value = _index;
      return true;
    }

  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {		
      Warning(GetName(),"Has no such member %s",name);
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Int_t" and
       _type != "int" )
    {
      Warning(GetName(),".%s is of type %s expect Int_t",name,_type.Data());
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Int_t *)_address;				

  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Int_t &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Int_t" and
       _type != "int" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Int_t *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}							

// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Float_t &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Float_t" and
       _type != "float" )
    {
      Warning(GetName(),".%s is of type %s expect Float_t",name,_type.Data());
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Float_t *)_address;				

  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Float_t &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Float_t" and
       _type != "float" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Float_t *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}							

// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Double_t &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Double_t" and
       _type != "double" )
    {
      Warning(GetName(),".%s is of type %s expect Double_t",name,_type.Data());
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Double_t *)_address;				

  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Double_t &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Double_t" and
       _type != "double" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Double_t *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}							

// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, TString &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "TString" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (TString *)_address;				

  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, TString &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "TString" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((TString *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}							

// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Array_t<Int_t> &value )	
{	
								
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Int_t>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Array_t<Int_t> *)_address;				
  
  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Array_t<Int_t> &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Int_t>" and 
       _type != "Array_t<int>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Array_t<Int_t> *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}							

// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Array_t<Float_t> &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Float_t>" and
       _type != "Array_t<float>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Array_t<Float_t> *)_address;				
 
  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Array_t<Float_t> &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Float_t>" and
       _type != "Array_t<float>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Array_t<Float_t> *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}			


// ===========================================================================================
//
Bool_t AgStructure::GetMember( const Char_t *name, Array_t<Double_t> &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Double_t>" and
       _type != "Array_t<double>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And return the value which we find there		
  value = * (Array_t<Double_t> *)_address;				
 
  // If we made it this far, then we have succeeded
  return true;						
							
}							

Bool_t AgStructure::SetMember( const Char_t *name, Array_t<Double_t> &value )	
{									
  // Get the class description for this structure			
  TClass *_class = TClass::GetClass( GetName() );			
  if ( !_class )							
    {									
      assert(false); // it damn well better not happen		
    }								
  
  // Get the requested data member					
  TDataMember *_data = _class->GetDataMember(name);			
  if ( !_data )                                                      
    {						
      return false;				
    }			

  // Ensure type compatability
  TString _type = _data->GetTypeName();
  if ( _type != "Array_t<Double_t>" and
       _type != "Array_t<double>" )
    {
      return false;
    }
  
  // And the offset to the data					
  Long_t _offset = _class -> GetDataMemberOffset( name );		
  
  // Compute the address of the data member		
  void *_address = (void*)(((Long_t)this) + _offset);	
  
  // And set the value we find there
  *((Array_t<Double_t> *)_address) = value;

  // If we made it this far, then we have succeeded
  return true;						
							
}			




//////////////////////////////////////////////////////////////////////////////////////////
//
// AgDetp style manipulation of data structures.
//
Bool_t AgStructure::AgDetpNew( const Char_t *module, const Char_t *title )
{

  // Set the namespace for this module
  TString MODULE = module; MODULE.ToUpper();

  // Make sure that the namespace is really really defined
  assert(MODULE!="NONE");

  //
  // So... this is a real hack.  May cause us problems, but we would need to
  // revisit AgStructure design to factor this out.  We are really abusing
  // ROOT, the CINT dictionary and CINT here.
  //
  gROOT -> ProcessLine(Form("using namespace %s;",MODULE.Data()));

  /// Get the /DETP/.commands folder
  _commands = (TFolder*)top().FindObject(".commands");
  TString name = folderName(module);
  TFolder *_folder = (TFolder*)_commands->FindObject(name);
  if ( !_folder )
    {
      _folder = _commands->AddFolder(name,Form("AgDetpNew %s",module));
    }
  if ( title != NULL )
    {
      _folder -> SetTitle(title);
    }
  _commands = _folder;
  return true;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Int_t value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );
    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {      

      // Add a TObjString with the format structName.memberName
      
      TString n = Form("%s.%s",name,member);
      TString t = Form("= %i",value);
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );

      return true;

    }

  return false;
}



Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Float_t value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );
    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {     

      // Add a TObjString with the format structName.memberName

      TString n = Form("%s.%s",name,member);
      TString t = Form("= %f",value);
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );


      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Double_t value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );

    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {      

      // Add a TObjString with the format structName.memberName

      TString n = Form("%s.%s",name,member);
      TString t = Form("= %f",value);
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );

      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, TString value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );

    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {

      // Add a TObjString with the format structName.memberName

      TString n = Form("%s.%s",name,member);
      TString t = Form("= %s",value.Data());
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );

      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Array_t<Int_t> value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );
    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {     

      TString n = Form("%s.%s",name,member);
      TString t = "= {";
      for ( Int_t i=0;i<value.size();i++ )
	{ t += value.at(i);
	  t += ", "; }
      t+= "}";
	  
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );

      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Array_t<Float_t> value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );
    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {     

      TString n = Form("%s.%s",name,member);
      TString t = "= {";
      for ( Int_t i=0;i<value.size();i++ )
	{ t += value.at(i);
	  t += ", "; }
      t+= "}";
	  
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );


      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpAdd( const Char_t *name, const Char_t *member, Array_t<Double_t> value )
{

  // Retrieve the requested class/structure
  TClass *_class = TClass::GetClass(name);
  if ( !_class )
    {
      std::cout << Form("AgStructure::AgDetpAdd(\"%s\",...) -- no such structure defined",name) << std::endl;
      //      Warning(GetName(),Form("Structure %s has not been defined",name));
      return false;
    }

  // Retrieve the structure from the commands folder.  If it
  // doesn't exist, create it
  AgStructure *_struct = (AgStructure *)_commands->FindObject(name);
  if ( !_struct )
    {
      _struct = (AgStructure *)_class -> New();
      _commands -> Add( _struct );
    }

  // Set the specified member variable on that class
  if ( _struct->SetMember( member, value ) )
    {     

      TString n = Form("%s.%s",name,member);
      TString t = "= {";
      for ( Int_t i=0;i<value.size();i++ )
	{ t += value.at(i);
	  t += ", "; }
      t+= "}";
	  
      _commands -> Add( new TNamed( n.Data(), t.Data() ) );

 
      return true;

    }

  return false;
}


Bool_t AgStructure::AgDetpSet()
{

  // On first call modify the the requested module data structure
  if ( _use ) 
    {      
      return false;
    }

  // Get the name of the module in which we have been called
  TString module = AgModule::module()->GetName();
  TString fname  = folderName(module);

  // Is there a folder holding AgDetp commands for this module
  TFolder *commands = (TFolder*)top().FindObject(".commands/"+fname);  
  if ( !commands ) 
    {
      return false;
    }

  // Loop over all objects in the commands directory
  TIter next( commands -> GetListOfFolders() );
  TObject *obj = 0;

  while ( (obj=next()) )
    {

      TString oname = obj->ClassName();
      TString mname = GetName();

      // Find the matching object 
      if ( oname.Contains(mname) )
	{
	  break; // but we should check selector (need to add selector)
	}     

      // Nullify before terminating loop
      obj = 0;
    }


  // This is the source structure
  AgStructure *source = (AgStructure *)obj;

  // Next retrieve the destination structure which
  // should already exist in the DETP folder
  AgStructure *destination = (AgStructure*)top().FindObject(fname+"/"+GetName());
  if ( !destination )
    {
      Warning(GetName(),"Could not find %s",(fname+"/"+GetName()).Data());
      return false;
    }
  

  while ( (obj=next()) )
    {
      
      // Only process TNamed.  Once we hit a structure punch out.
      if ( TString(obj->ClassName()) != "TNamed" )
	{
	  break;
	}

      // Get the name of the member variable
      TString member = obj->GetName();

      //    LOG_INFO << Form("[/DETP/%s/%s (%-20s)]",fname.Data(), destination->GetName(), member.Data()) << endm;

      // Now copy the member variable to the destination in the folder
      // and into the current structure
      destination -> CopyDataMembers( source, member, true  );
      this        -> CopyDataMembers( source, member  );

    }

  return true;

}


