#-------------------------------------------------
#
# Project created by QtCreator 2012-07-30T13:51:47
#
#-------------------------------------------------

QT       += core gui widgets xml

TARGET = PixyMon
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
    configdialog.cpp \
    aboutdialog.cpp \
    parameters.cpp \
    paramfile.cpp

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
    flash.h \
    reader.h \
    ../../common/pixytypes.h \
    ../../common/pixydefs.h \
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
    sleeper.h \
    aboutdialog.h \
    parameters.h \
    paramfile.h

INCLUDEPATH += ../../common

QMAKE_CXXFLAGS_DEBUG += -O0
QMAKE_CXXFLAGS += -Wno-unused-parameter
QMAKE_CXXFLAGS += -mno-ms-bitfields
FORMS    += mainwindow.ui \
    configdialog.ui \
    about.ui

# LIBS += ./libusb-1.0.dll.a

win32 {
    DEFINES += __WINDOWS__
    LIBS += ../windows/libusb-1.0.dll.a
    HEADERS += ../windows/libusb.h
    INCLUDEPATH += ../windows

}

macx {
    ICON = pixy.icns
    DEFINES += __MACOS__
    #CONFIG += x86
    #CONFIG -= x86_64
    LIBS += -L/opt/local/lib -lusb-1.0
    INCLUDEPATH += /opt/local/include/libusb-1.0
    #QMAKE_MAC_SDK = /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.7.sdk
    #QMAKE_MACOSX_DEPLOYMENT_TARGET = 10.7
}

unix:!macx {
    DEFINES += __LINUX__
    PKGCONFIG += libusb-1.0
    LIBS += -lusb-1.0
    INCLUDEPATH += /usr/include/libusb-1.0
   
}

RESOURCES += \
    resources.qrc







