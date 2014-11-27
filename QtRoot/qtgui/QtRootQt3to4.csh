# Create the Qt4 aware version of qtRoot package
# $Id: QtRootQt3to4.csh,v 1.4 2013/08/30 16:00:19 perev Exp $
# Create the custom customQ3porting.xml
cat >q3porting.xml <<Q3PORTING
<?xml version="1.0" encoding="UTF-8"?>
<Include>$QTDIR/q3porting.xml</Include>
<Rules>
    <Count>3</Count>
    <item Type="RenamedToken" >
        <Qt4>
#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN</Qt4>
        <Qt3>//MOC_SKIP_BEGIN</Qt3>
    </item>

    <item Type="RenamedToken" >
        <Qt4>
//MOC_SKIP_END
#endif</Qt4>
        <Qt3>//MOC_SKIP_END</Qt3>
    </item>
    <item Type="RenamedToken" >
        <Qt4>
//MOC_SKIP_END
#endif</Qt4>
        <Qt3>// MOC_SKIP_END</Qt3>
    </item>
</Rules>
Q3PORTING
# set f=inc/TQtMarkerSelectButton.h
set f="inc/TQtFloatSlider.h src/TQtFloatSlider.cxx"

# qt3to4 qtgui.pro
echo "qt3to4 -rulesFile customQ3porting.xml qtgui.pro"
# echo "qt3to4 -rulesFile $QTPORTING qt.pro"
# qt3to4 -rulesFile ./q3porting.xml inc/TQtMarkerSelect.h
qt3to4  ${f}
# inc/TQMimeTypes.h inc/TQtEventQueue.h
rm -rf qt4
mkdir -p qt4/src
mkdir -p qt4/inc
$ foreach f ( src/*.cxx   inc/*.h )
$ echo qt4/$f


# Check against of CVS after conversion
cvs diff --ifdef=QT_VERSION ${f} | sed -e  "1,5d" | sed -e "s/ndef QT_VERSION/ QT_VERSION < 0x40000/g" |  sed -e "s/ifdef QT_VERSION/if QT_VERSION >= 0x40000/g"  >qt4/$f 

# Copy the files with no corrections
if (!( -s qt4/$f))  then
echo "        File $f was not changed."
   cp $f qt4/$f
endif
end
cp qt.pro qt4
cp inc/*.pri qt4/inc
cd qt4
qmake CONFIG+=debug
make
