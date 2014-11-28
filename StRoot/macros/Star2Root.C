{
  if (! strstr(gSystem->GetLibraries(),"libTable")) {
    gSystem->Load("libGeom"); gSystem->Load("libTable");
  } 
  gInterpreter->ProcessLine("typedef TCL              StCL;");              
  gInterpreter->ProcessLine("typedef TDataSet         St_DataSet ;");       
  gInterpreter->ProcessLine("typedef TDataSetIter     St_DataSetIter;");    
  gInterpreter->ProcessLine("typedef TFileSet         St_FileSet;");        
  gInterpreter->ProcessLine("typedef TVolume          St_Node;");           
  gInterpreter->ProcessLine("typedef TVolumePosition  St_NodePosition;");   
  gInterpreter->ProcessLine("typedef TVolumeView      St_NodeView;");       
  gInterpreter->ProcessLine("typedef TVolumeViewIter  St_NodeViewIter;");   
  gInterpreter->ProcessLine("typedef TObjectSet       St_ObjectSet;");      
//  gInterpreter->ProcessLine("typedef TPointPosition   St_PointPosition;");  
  gInterpreter->ProcessLine("typedef TPoints3D        St_Points3D;");       
  gInterpreter->ProcessLine("typedef TPointsArray3D   St_PointsArray3D;");  
  gInterpreter->ProcessLine("typedef TPolyLineShape   St_PolyLineShape;");  
  gInterpreter->ProcessLine("typedef TTable           St_Table;");          
  gInterpreter->ProcessLine("typedef TTable3Points    St_Table3Points;");   
  gInterpreter->ProcessLine("typedef TTableIter       St_TableIter;");      
  gInterpreter->ProcessLine("typedef TTablePoints     St_TablePoints;");    
  gInterpreter->ProcessLine("typedef TTableSorter     St_TableSorter;");    
  gInterpreter->ProcessLine("typedef TTableDescriptor St_tableDescriptor;");
}
