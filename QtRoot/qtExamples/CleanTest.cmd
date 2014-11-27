@echo Cleaning the Qt/Root examples: 
@echo ------------------------
@echo.

echo alias  del rm 
echo alias nmake make 

nmake distclean

cd CustomCanvasMenu
nmake distclean
del Makefile
cd ..

cd CustomWidgets
nmake distclean
del Makefile
cd ..

cd ex1
nmake distclean
del Makefile
cd ..

cd HelloCanvas 
nmake distclean
del Makefile
cd ..

cd HelloCint
nmake distclean
del Makefile
cd ..

cd HelloWord
nmake distclean
del Makefile
cd ..

cd HelloLife
nmake distclean
del Makefile
cd ..

cd QtGBrowser
nmake distclean
del Makefile
cd ..

del Makefile

@echo ---------  Cleaning  has been finished ------------------

@echo -- Recreate the examples:

qmake
nmake

@echo Execute the examples:
@echo -----------------

cd CustomCanvasMenu
echo ----------- CustomCanvasMenu  -------------
qmake
nmake
CustomCanvasMenu
cd ..

cd CustomWidgets
echo -----------   CustomWidgets   -------------
qmake
nmake
CustomWidgets
cd ..

cd ex1
echo -----------        ex1        -------------
qmake
nmake
tqrootexample
cd ..

cd HelloCanvas 
echo -----------   HelloCanvas     -------------
qmake
nmake
HelloCanvas
cd ..

cd HelloCint
echo -----------     HelloCint     -------------
qmake
nmake
root.exe HelloCint.C
cd ..

cd HelloWord
echo -----------     HelloWord     -------------
qmake
nmake
HelloWord
cd ..

cd HelloLife
echo -----------     HelloLife     -------------
qmake
nmake
HelloLife
cd ..

cd HelloPixmap
echo -----------     HelloPixmap     -------------
qmake
nmake
HelloPixmap
cd ..


cd QtGBrowser
echo -----------  Geometry Browser -------------
qmake
nmake
GeomBrowser starcomplete.root
cd ..

cd HelloZoomPad
echo -----------  HelloZoomPad -------------
root.exe -q -l %ROOTSYS%\tutorials\hsimple.C
root.exe h1draw_zoom.C
cd ..

echo -----------  QtGSI test -------------
cd qtgsi\example1
qmake
nmake
exe\QtGsiTest.exe
cd ..\..

echo ---------  Qt RootCint test -----------
cd macros
echo -- check the Qt ROOT environment setting
root.exe -q SetQtEnv.C
echo --  call a simple Qt Dialog as ROOT macro
root.exe -q QtFileDialog.C
echo --  call a simple Qt Dialog as Windows DLL
root.exe -q QtFileDialog.C++
echo -- test a set of the simple function 
echo    to input the data via graphical Qt dialogs
root.exe DialogsQ.C
echo -- test the Coin3D plug-in to crate the mixed ROOT/Coin geometries
root.exe rootgeom_coin.C
cd ..
echo All example has been performed !!!


