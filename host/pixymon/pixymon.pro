#-------------------------------------------------
#
# Project created by QtCreator 2012-07-30T13:51:47
#
#-------------------------------------------------

QT       += core gui

TARGET = pixymon
TEMPLATE = app
RC_FILE = resources.rc

SOURCES += main.cpp\
        mainwindow.cpp \
    videowidget.cpp \
    usblink.cpp \
    console.cpp \
    interpreter.cpp \
    renderer.cpp \
    chirpmon.cpp \
    calc.cpp \
    dfu.cpp \
    connectevent.cpp \
    flash.cpp \
    reader.cpp \
    ../../common/chirp.cpp \
    ../../common/colorlut.cpp \
    ../../common/blob.cpp \
    ../../common/blobs.cpp \
    processblobs.cpp \
    ../../common/qqueue.cpp \
    configdialog.cpp

HEADERS  += mainwindow.h \
    videowidget.h \
    usblink.h \
    console.h \
    interpreter.h \
    renderer.h \
    chirpmon.h \
    calc.h \
    dfu.h \
    usb_dfu.h \
    dfu_info.h \
    connectevent.h \
    pixy.h \
    libusb.h \
    flash.h \
    reader.h \
    ../../common/pixytypes.h \
    ../../common/chirp.hpp \
    ../../common/colorlut.h \
    ../../common/blobs.h \
    ../../common/blob.h \
    ../../common/blobs.h \
    processblobs.h \
    ../../common/qqueue.h \
    pixymon.h \
    configdialog.h \
    ../../common/link.h \
    sleeper.h

INCLUDEPATH += ../../common

QMAKE_CXXFLAGS_DEBUG += -O0

FORMS    += mainwindow.ui \
    configdialog.ui

# LIBS += ./libusb-1.0.dll.a

win32 {
    LIBS += ./libusb-1.0.dll.a
}

macx {
    DEFINES += __MACOS__
#    CONFIG += x86
#    CONFIG -= x86_64
    LIBS += -L/opt/local/lib -lusb-1.0
}

unix:!macx {
    PKGCONFIG += libusb-1.0
    LIBS += -lusb-1.0
}

RESOURCES += \
    resources.qrc

























































































