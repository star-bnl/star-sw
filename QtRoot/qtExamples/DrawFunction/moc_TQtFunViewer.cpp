/****************************************************************************
** Meta object code from reading C++ file 'TQtFunViewer.h'
**
** Created: Wed Aug 28 18:37:11 2013
**      by: The Qt Meta Object Compiler version 59 (Qt 4.4.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "TQtFunViewer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'TQtFunViewer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 59
#error "This file was generated using the moc from 4.4.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_TQtFunViewer[] = {

 // content:
       1,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   10, // methods
       0,    0, // properties
       0,    0, // enums/sets

 // slots: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x0a,
      26,   22,   13,   13, 0x0a,
      63,   39,   13,   13, 0x0a,
     107,  104,   13,   13, 0x0a,
     123,   13,   13,   13, 0x2a,
     133,  104,   13,   13, 0x0a,
     148,   13,   13,   13, 0x2a,

       0        // eod
};

static const char qt_meta_stringdata_TQtFunViewer[] = {
    "TQtFunViewer\0\0Reset()\0fun\0SetFun(TF2*)\0"
    "fun,xmin,xmax,ymin,ymax\0"
    "SetFun(TF2*,double,double,double,double)\0"
    "on\0SetColZ(Bool_t)\0SetColZ()\0"
    "SetTop(Bool_t)\0SetTop()\0"
};

const QMetaObject TQtFunViewer::staticMetaObject = {
    { &QGLViewer::staticMetaObject, qt_meta_stringdata_TQtFunViewer,
      qt_meta_data_TQtFunViewer, 0 }
};

const QMetaObject *TQtFunViewer::metaObject() const
{
    return &staticMetaObject;
}

void *TQtFunViewer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_TQtFunViewer))
        return static_cast<void*>(const_cast< TQtFunViewer*>(this));
    return QGLViewer::qt_metacast(_clname);
}

int TQtFunViewer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGLViewer::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: Reset(); break;
        case 1: SetFun((*reinterpret_cast< TF2*(*)>(_a[1]))); break;
        case 2: SetFun((*reinterpret_cast< TF2*(*)>(_a[1])),(*reinterpret_cast< double(*)>(_a[2])),(*reinterpret_cast< double(*)>(_a[3])),(*reinterpret_cast< double(*)>(_a[4])),(*reinterpret_cast< double(*)>(_a[5]))); break;
        case 3: SetColZ((*reinterpret_cast< Bool_t(*)>(_a[1]))); break;
        case 4: SetColZ(); break;
        case 5: SetTop((*reinterpret_cast< Bool_t(*)>(_a[1]))); break;
        case 6: SetTop(); break;
        }
        _id -= 7;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
