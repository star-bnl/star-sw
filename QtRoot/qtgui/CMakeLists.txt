############################################################################
# CMakeLists.txt file for building ROOT graf2d/qt package
# @author Pere Mato, CERN
############################################################################

ROOT_USE_PACKAGE(QtRoot/qt)
include(${QT_USE_FILE})

set(headers TQtGuiFactory.h        TBrowserCustom.h 
            TQtPatternSelect.h     TQtTabValidator.h
            TEmbeddedPad.h         TQtColorSelect.h
            TQtZoomPadWidget.h     TQGsiRootCanvas.h
            TQtPad2Html.h           TQtCanvas2Html.h
            TQtMarkerSelect.h      TQtPixmapBox.h )


QT4_WRAP_CPP(mocfiles  inc/inc/TQtBrowserImp.h      inc/TQtCanvasImp.h       
                       inc/TQtCanvasWidget.h        inc/TQtColorSelect.h       
                       inc/TQtColorSelectButton.h   inc/TQtContextMenuImp.h    
                       inc/TQtControlBarImp.h       inc/TQtIconBrowserImp.h   
                       inc/TQtCustomizeCanvasMenu.h inc/TQtFloatSpinBox.h    
                       inc/TQtInspectImp.h          inc/TQtRootBrowserImp.h 
                       inc/TQtObjectDialog.h        inc/TQtPatternSelect.h
                       inc/TQtPatternSelectButton.h inc/TQtRootBrowserAction.h 
                       inc/TQtZoomPadWidget.h       inc/TQtToolBar.h
                       inc/TQtMarkerSelect.h        inc/TQtPixmapBox.h 
                       inc/TQtMarkerSelectButton.h  inc/TQtStyleComboBox.h 
                       inc/TQtFloatSlider.h         inc/TQtRootCommandCombo.h 
                       qtcolorpicker.h              inc/TQtColorPickerHelper.h 
                       inc/TQtTextEditor.h 
                       qtmmlwidget.h            inc/TQtCommandPlugin.h)

ROOT_GENERATE_DICTIONARY(G__QtRootGui ${headers} LINKDEF LinkDef.h)
ROOT_GENERATE_ROOTMAP(QtGui LINKDEF LinkDef.h DEPENDENCIES GQt Gui Gpad Graf Rint)

ROOT_LINKER_LIBRARY(QtRootGui *.cxx ${mocfiles} G__QtRootGui.cxx LIBRARIES ${QT_LIBRARIES} DEPENDENCIES Gui Gpad Graf Rint)

ROOT_INSTALL_HEADERS()
