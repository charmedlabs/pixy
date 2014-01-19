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
    blob.cpp \
    blobs.cpp \
    dfu.cpp \
    connectevent.cpp \
    flash.cpp \
    reader.cpp \
    ../../device/libpixy/chirp.cpp \
    colorlut.cpp

HEADERS  += mainwindow.h \
    link.h \
    videowidget.h \
    usblink.h \
    console.h \
    interpreter.h \
    renderer.h \
    chirpmon.h \
    calc.h \
    blobs.h \
    blob.h \
    dfu.h \
    usb_dfu.h \
    dfu_info.h \
    connectevent.h \
    pixy.h \
    libusb.h \
    flash.h \
    reader.h \
    ../../device/libpixy/chirp.hpp \
    colorlut.h \
    pixytypes.h

INCLUDEPATH += ../libpixy

QMAKE_CXXFLAGS_DEBUG += -O0

FORMS    += mainwindow.ui

LIBS += ./libusb-1.0.dll.a

RESOURCES += \
    resources.qrc































































