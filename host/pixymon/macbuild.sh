#! /bin/sh

cd "${0%/*}"
PIXYMON="${PWD}/"
PREFIX="** MACBUILD ** "


#### variables
QTBIN=/usr/local/Trolltech/Qt-4.8.4/bin
QMAKE=${QTBIN}/qmake
QDEPLOY=${QTBIN}/macdeployqt


#### clean and exit
if [ "${1}" == "clean" ]; then
	
	rm -rf ../macbuild-pixymon-{debug,release}/*	
	exit
fi


function compilePixymon() {
		
	mkdir -p ../macbuild-pixymon-${2}/
	cd ../macbuild-pixymon-${2}
	${QMAKE} ../pixymon/pixymon.pro -r -spec macx-g++ CONFIG+=${2} CONFIG+=x86_64	
	make -w
	cp ../pixymon/pixyflash.bin.hdr PixyMon.app/Contents/MacOS
	cd PixyMon.app/Contents
	defaults delete "$PWD/Info" NOTE
	defaults write "$PWD/Info" CFBundleGetInfoString "PixyMon"
	defaults write "$PWD/Info" CFBundleIconFile "pixy.icns"
	defaults write "$PWD/Info" CFBundleIdentifier "com.charmedlabs.pixymon"
	defaults write "$PWD/Info" CFBundleSignature "Clab"
		
	cd ../..
}


#### build pixymon if needed
if [ ! -f ../macbuild-pixymon-debug/PixyMon.app/Contents/MacOS/pixymon ]; then
	echo "${PREFIX} building macbuild-pixymon-debug/PixyMon.app"
	
	compilePixymon Debug debug
	
	cd "${PIXYMON}"
else
	echo "${PREFIX} nothing to do for macbuild-pixymon-debug/PixyMon.app"
fi

if [ ! -f ../macbuild-pixymon-release/PixyMon.app/Contents/MacOS/pixymon ]; then
	echo "${PREFIX} building macbuild-pixymon-release/PixyMon.app"
	
	compilePixymon Release release
	${QDEPLOY} PixyMon.app	
	cd "${PIXYMON}"
else
	echo "${PREFIX} nothing to do for macbuild-pixymon-release/PixyMon.app"
fi



