// This macro being called at the end of ROOT session cleares all the
// ROOT lists. In result, no ROOT objects will be deleted after ".q"
// It is made especially to avoid deleting of TGeoManager which, due to bad
// design, often leads to crash, which in its turn, is not always acceptable (VP)

void ClearRoot()
{
TCollection *C[50]={0};
int i = 0;
C[i++] = gROOT->GetListOfBrowsables() ;
C[i++] = gROOT->GetListOfBrowsers() ;
C[i++] = gROOT->GetListOfCanvases() ;
//C[i++] = gROOT->GetListOfClasses() ;
//C[i++] = gROOT->GetListOfClassGenerators() ;
C[i++] = gROOT->GetListOfCleanups() ;
C[i++] = gROOT->GetListOfColors() ;
C[i++] = gROOT->GetListOfDataSets() ;
C[i++] = gROOT->GetListOfFiles() ;
C[i++] = gROOT->GetListOfFunctions() ;
C[i++] = gROOT->GetListOfGeometries() ;
// C[i++] = gROOT->GetListOfGlobalFunctions();
C[i++] = gROOT->GetListOfGlobals();
C[i++] = gROOT->GetListOfKeys() ;
C[i++] = gROOT->GetListOfMappedFiles() ;
C[i++] = gROOT->GetListOfMessageHandlers() ;
C[i++] = gROOT->GetListOfProofs() ;
C[i++] = gROOT->GetListOfSecContexts() ;
C[i++] = gROOT->GetListOfSockets() ;
C[i++] = gROOT->GetListOfSpecials() ;
C[i++] = gROOT->GetListOfStreamerInfo() ;
C[i++] = gROOT->GetListOfStyles() ;
C[i++] = gROOT->GetListOfTasks() ;
C[i++] = gROOT->GetListOfTypes();
if (gGeoManager) {
C[i++] = gGeoManager->GetListOfGShapes() ;
C[i++] = gGeoManager->GetListOfGVolumes() ;
C[i++] = gGeoManager->GetListOfMaterials() ;
C[i++] = gGeoManager->GetListOfMatrices() ;
C[i++] = gGeoManager->GetListOfMedia() ;
C[i++] = gGeoManager->GetListOfNavigators() ;
C[i++] = gGeoManager->GetListOfNodes();
C[i++] = gGeoManager->GetListOfOverlaps();
C[i++] = gGeoManager->GetListOfPhysicalNodes();
C[i++] = gGeoManager->GetListOfShapes() ;
C[i++] = gGeoManager->GetListOfTracks() ;
C[i++] = gGeoManager->GetListOfUVolumes() ;
C[i++] = gGeoManager->GetListOfVolumes() ;
}
 for (int j=0;j<i;j++) { if (C[j]) C[j]->Clear();}
 for (int j=0;j<i;j++) { if (C[j]) C[j]->ls();}
}
